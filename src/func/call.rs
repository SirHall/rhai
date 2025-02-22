//! Implement function-calling mechanism for [`Engine`].

use super::callable_function::CallableFunction;
use super::native::FnAny;
use super::{get_builtin_binary_op_fn, get_builtin_op_assignment_fn};
use crate::api::default_limits::MAX_DYNAMIC_PARAMETERS;
use crate::ast::{Expr, FnCallHashes, Stmt};
use crate::engine::{
    KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_FN_PTR, KEYWORD_FN_PTR_CALL, KEYWORD_FN_PTR_CURRY,
    KEYWORD_IS_DEF_VAR, KEYWORD_PRINT, KEYWORD_TYPE_OF,
};
use crate::eval::{EvalState, GlobalRuntimeState};
use crate::module::Namespace;
use crate::tokenizer::Token;
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, FnArgsVec, FnPtr,
    Identifier, ImmutableString, Module, Position, RhaiResult, RhaiResultOf, Scope, ERR,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::{type_name, TypeId},
    collections::BTreeMap,
    convert::TryFrom,
    mem,
};

/// Arguments to a function call, which is a list of [`&mut Dynamic`][Dynamic].
pub type FnCallArgs<'a> = [&'a mut Dynamic];

/// A type that temporarily stores a mutable reference to a `Dynamic`,
/// replacing it with a cloned copy.
#[derive(Debug)]
struct ArgBackup<'a> {
    orig_mut: Option<&'a mut Dynamic>,
    value_copy: Dynamic,
}

impl<'a> ArgBackup<'a> {
    /// Create a new `ArgBackup`.
    pub fn new() -> Self {
        Self {
            orig_mut: None,
            value_copy: Dynamic::UNIT,
        }
    }
    /// This function replaces the first argument of a method call with a clone copy.
    /// This is to prevent a pure function unintentionally consuming the first argument.
    ///
    /// `restore_first_arg` must be called before the end of the scope to prevent the shorter lifetime from leaking.
    ///
    /// # Safety
    ///
    /// This method blindly casts a reference to another lifetime, which saves allocation and string cloning.
    ///
    /// As long as `restore_first_arg` is called before the end of the scope, the shorter lifetime
    /// will not leak.
    ///
    /// # Panics
    ///
    /// Panics when `args` is empty.
    #[inline]
    fn change_first_arg_to_copy(&mut self, args: &mut FnCallArgs<'a>) {
        // Clone the original value.
        self.value_copy = args[0].clone();

        // Replace the first reference with a reference to the clone, force-casting the lifetime.
        // Must remember to restore it later with `restore_first_arg`.
        //
        // # Safety
        //
        // Blindly casting a reference to another lifetime saves allocation and string cloning,
        // but must be used with the utmost care.
        //
        // We can do this here because, before the end of this scope, we'd restore the original reference
        // via `restore_first_arg`. Therefore this shorter lifetime does not leak.
        self.orig_mut = Some(mem::replace(&mut args[0], unsafe {
            mem::transmute(&mut self.value_copy)
        }));
    }
    /// This function restores the first argument that was replaced by `change_first_arg_to_copy`.
    ///
    /// # Safety
    ///
    /// If `change_first_arg_to_copy` has been called, this function **MUST** be called _BEFORE_ exiting
    /// the current scope.  Otherwise it is undefined behavior as the shorter lifetime will leak.
    #[inline(always)]
    fn restore_first_arg(mut self, args: &mut FnCallArgs<'a>) {
        if let Some(p) = self.orig_mut.take() {
            args[0] = p;
        }
    }
}

impl Drop for ArgBackup<'_> {
    #[inline]
    fn drop(&mut self) {
        // Panic if the shorter lifetime leaks.
        assert!(
            self.orig_mut.is_none(),
            "ArgBackup::restore_first_arg has not been called prior to existing this scope"
        );
    }
}

#[cfg(not(feature = "no_closure"))]
#[inline]
pub fn ensure_no_data_race(
    fn_name: &str,
    args: &FnCallArgs,
    is_method_call: bool,
) -> RhaiResultOf<()> {
    if let Some((n, _)) = args
        .iter()
        .enumerate()
        .skip(if is_method_call { 1 } else { 0 })
        .find(|(_, a)| a.is_locked())
    {
        return Err(ERR::ErrorDataRace(
            format!("argument #{} of function '{}'", n + 1, fn_name),
            Position::NONE,
        )
        .into());
    }

    Ok(())
}

/// _(internals)_ An entry in a function resolution cache.
/// Exported under the `internals` feature only.
#[derive(Debug, Clone)]
pub struct FnResolutionCacheEntry {
    /// Function.
    pub func: CallableFunction,
    /// Optional source.
    /// No source if the string is empty.
    pub source: Identifier,
}

/// _(internals)_ A function resolution cache.
/// Exported under the `internals` feature only.
///
/// [`FnResolutionCacheEntry`] is [`Box`]ed in order to pack as many entries inside a single B-Tree
/// level as possible.
pub type FnResolutionCache = BTreeMap<u64, Option<Box<FnResolutionCacheEntry>>>;

impl Engine {
    /// Generate the signature for a function call.
    #[inline]
    #[must_use]
    fn gen_call_signature(
        &self,
        namespace: Option<&Namespace>,
        fn_name: &str,
        args: &[&mut Dynamic],
    ) -> String {
        format!(
            "{}{}{} ({})",
            namespace.map_or_else(|| String::new(), |ns| ns.to_string()),
            if namespace.is_some() {
                Token::DoubleColon.literal_syntax()
            } else {
                ""
            },
            fn_name,
            args.iter()
                .map(|a| if a.is::<ImmutableString>() {
                    "&str | ImmutableString | String"
                } else {
                    self.map_type_name(a.type_name())
                })
                .collect::<FnArgsVec<_>>()
                .join(", ")
        )
    }

    /// Resolve a function call.
    ///
    /// Search order:
    /// 1) AST - script functions in the AST
    /// 2) Global namespace - functions registered via Engine::register_XXX
    /// 3) Global modules - packages
    /// 4) Imported modules - functions marked with global namespace
    /// 5) Global sub-modules - functions marked with global namespace
    #[must_use]
    fn resolve_fn<'s>(
        &self,
        global: &GlobalRuntimeState,
        state: &'s mut EvalState,
        lib: &[&Module],
        fn_name: &str,
        hash_script: u64,
        args: Option<&mut FnCallArgs>,
        allow_dynamic: bool,
        is_op_assignment: bool,
    ) -> Option<&'s FnResolutionCacheEntry> {
        if hash_script == 0 {
            return None;
        }

        let mut hash = args.as_ref().map_or(hash_script, |args| {
            combine_hashes(
                hash_script,
                calc_fn_params_hash(args.iter().map(|a| a.type_id())),
            )
        });

        let result = state
            .fn_resolution_cache_mut()
            .entry(hash)
            .or_insert_with(|| {
                let num_args = args.as_ref().map_or(0, |a| a.len());
                let max_bitmask = if !allow_dynamic {
                    0
                } else {
                    1usize << usize::min(num_args, MAX_DYNAMIC_PARAMETERS)
                };
                let mut bitmask = 1usize; // Bitmask of which parameter to replace with `Dynamic`

                loop {
                    let func = lib
                        .iter()
                        .find_map(|m| {
                            m.get_fn(hash).cloned().map(|func| FnResolutionCacheEntry {
                                func,
                                source: m.id_raw().clone(),
                            })
                        })
                        .or_else(|| {
                            self.global_modules.iter().find_map(|m| {
                                m.get_fn(hash).cloned().map(|func| FnResolutionCacheEntry {
                                    func,
                                    source: m.id_raw().clone(),
                                })
                            })
                        })
                        .or_else(|| {
                            global
                                .get_fn(hash)
                                .map(|(func, source)| FnResolutionCacheEntry {
                                    func: func.clone(),
                                    source: source
                                        .map_or_else(|| Identifier::new_const(), Into::into),
                                })
                        })
                        .or_else(|| {
                            self.global_sub_modules.values().find_map(|m| {
                                m.get_qualified_fn(hash).cloned().map(|func| {
                                    FnResolutionCacheEntry {
                                        func,
                                        source: m.id_raw().clone(),
                                    }
                                })
                            })
                        });

                    match func {
                        // Specific version found
                        Some(f) => return Some(Box::new(f)),

                        // Stop when all permutations are exhausted
                        None if bitmask >= max_bitmask => {
                            if num_args != 2 {
                                return None;
                            }

                            return args.and_then(|args| {
                                if !is_op_assignment {
                                    get_builtin_binary_op_fn(fn_name, &args[0], &args[1]).map(|f| {
                                        FnResolutionCacheEntry {
                                            func: CallableFunction::from_method(
                                                Box::new(f) as Box<FnAny>
                                            ),
                                            source: Identifier::new_const(),
                                        }
                                    })
                                } else {
                                    let (first_arg, rest_args) = args.split_first().unwrap();

                                    get_builtin_op_assignment_fn(fn_name, *first_arg, rest_args[0])
                                        .map(|f| FnResolutionCacheEntry {
                                            func: CallableFunction::from_method(
                                                Box::new(f) as Box<FnAny>
                                            ),
                                            source: Identifier::new_const(),
                                        })
                                }
                                .map(Box::new)
                            });
                        }

                        // Try all permutations with `Dynamic` wildcards
                        None => {
                            let hash_params = calc_fn_params_hash(
                                args.as_ref()
                                    .expect("no permutations")
                                    .iter()
                                    .enumerate()
                                    .map(|(i, a)| {
                                        let mask = 1usize << (num_args - i - 1);
                                        if bitmask & mask != 0 {
                                            // Replace with `Dynamic`
                                            TypeId::of::<Dynamic>()
                                        } else {
                                            a.type_id()
                                        }
                                    }),
                            );
                            hash = combine_hashes(hash_script, hash_params);

                            bitmask += 1;
                        }
                    }
                }
            });

        result.as_ref().map(Box::as_ref)
    }

    /// Call a native Rust function registered with the [`Engine`].
    ///
    /// # WARNING
    ///
    /// Function call arguments be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    ///
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn call_native_fn(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        name: &str,
        hash: u64,
        args: &mut FnCallArgs,
        is_ref_mut: bool,
        is_op_assign: bool,
        pos: Position,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, pos)?;

        let parent_source = global.source.clone();

        // Check if function access already in the cache
        let func = self.resolve_fn(
            global,
            state,
            lib,
            name,
            hash,
            Some(args),
            true,
            is_op_assign,
        );

        if let Some(FnResolutionCacheEntry { func, source }) = func {
            assert!(func.is_native());

            // Calling pure function but the first argument is a reference?
            let mut backup: Option<ArgBackup> = None;
            if is_ref_mut && func.is_pure() && !args.is_empty() {
                // Clone the first argument
                backup = Some(ArgBackup::new());
                backup
                    .as_mut()
                    .expect("`Some`")
                    .change_first_arg_to_copy(args);
            }

            // Run external function
            let source = match (source.as_str(), parent_source.as_str()) {
                ("", "") => None,
                ("", s) | (s, _) => Some(s),
            };

            let context = (self, name, source, &*global, lib, pos).into();

            let result = if func.is_plugin_fn() {
                func.get_plugin_fn()
                    .expect("plugin function")
                    .call(context, args)
            } else {
                func.get_native_fn().expect("native function")(context, args)
            };

            // Restore the original reference
            if let Some(bk) = backup {
                bk.restore_first_arg(args)
            }

            // Check the return value (including data sizes)
            let result = self.check_return_value(result, pos)?;

            // Check the data size of any `&mut` object, which may be changed.
            #[cfg(not(feature = "unchecked"))]
            if is_ref_mut && args.len() > 0 {
                self.check_data_size(&args[0], pos)?;
            }

            // See if the function match print/debug (which requires special processing)
            return Ok(match name {
                KEYWORD_PRINT => {
                    if let Some(ref print) = self.print {
                        let text = result.into_immutable_string().map_err(|typ| {
                            ERR::ErrorMismatchOutputType(
                                self.map_type_name(type_name::<ImmutableString>()).into(),
                                typ.into(),
                                pos,
                            )
                        })?;
                        (print(&text).into(), false)
                    } else {
                        (Dynamic::UNIT, false)
                    }
                }
                KEYWORD_DEBUG => {
                    if let Some(ref debug) = self.debug {
                        let text = result.into_immutable_string().map_err(|typ| {
                            ERR::ErrorMismatchOutputType(
                                self.map_type_name(type_name::<ImmutableString>()).into(),
                                typ.into(),
                                pos,
                            )
                        })?;
                        let source = match global.source.as_str() {
                            "" => None,
                            s => Some(s),
                        };
                        (debug(&text, source, pos).into(), false)
                    } else {
                        (Dynamic::UNIT, false)
                    }
                }
                _ => (result, func.is_method()),
            });
        }

        // Error handling

        match name {
            // index getter function not found?
            #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
            crate::engine::FN_IDX_GET => {
                assert!(args.len() == 2);

                Err(ERR::ErrorIndexingType(
                    format!(
                        "{} [{}]",
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name())
                    ),
                    pos,
                )
                .into())
            }

            // index setter function not found?
            #[cfg(any(not(feature = "no_index"), not(feature = "no_object")))]
            crate::engine::FN_IDX_SET => {
                assert!(args.len() == 3);

                Err(ERR::ErrorIndexingType(
                    format!(
                        "{} [{}] = {}",
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name()),
                        self.map_type_name(args[2].type_name())
                    ),
                    pos,
                )
                .into())
            }

            // Getter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if name.starts_with(crate::engine::FN_GET) => {
                assert!(args.len() == 1);

                Err(ERR::ErrorDotExpr(
                    format!(
                        "Unknown property '{}' - a getter is not registered for type '{}'",
                        &name[crate::engine::FN_GET.len()..],
                        self.map_type_name(args[0].type_name())
                    ),
                    pos,
                )
                .into())
            }

            // Setter function not found?
            #[cfg(not(feature = "no_object"))]
            _ if name.starts_with(crate::engine::FN_SET) => {
                assert!(args.len() == 2);

                Err(ERR::ErrorDotExpr(
                    format!(
                        "No writable property '{}' - a setter is not registered for type '{}' to handle '{}'",
                        &name[crate::engine::FN_SET.len()..],
                        self.map_type_name(args[0].type_name()),
                        self.map_type_name(args[1].type_name()),
                    ),
                    pos,
                )
                .into())
            }

            // Raise error
            _ => Err(
                ERR::ErrorFunctionNotFound(self.gen_call_signature(None, name, args), pos).into(),
            ),
        }
    }

    /// Perform an actual function call, native Rust or scripted, taking care of special functions.
    ///
    /// # WARNING
    ///
    /// Function call arguments may be _consumed_ when the function requires them to be passed by value.
    /// All function arguments not in the first position are always passed by value and thus consumed.
    ///
    /// **DO NOT** reuse the argument values unless for the first `&mut` argument - all others are silently replaced by `()`!
    pub(crate) fn exec_fn_call(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        fn_name: &str,
        hashes: FnCallHashes,
        args: &mut FnCallArgs,
        is_ref_mut: bool,
        is_method_call: bool,
        pos: Position,
        scope: Option<&mut Scope>,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        fn no_method_err(name: &str, pos: Position) -> RhaiResultOf<(Dynamic, bool)> {
            let msg = format!("'{0}' should not be called this way. Try {0}(...);", name);
            Err(ERR::ErrorRuntime(msg.into(), pos).into())
        }

        // Check for data race.
        #[cfg(not(feature = "no_closure"))]
        ensure_no_data_race(fn_name, args, is_ref_mut)?;

        let _scope = scope;
        let _level = level;
        let _is_method_call = is_method_call;

        // These may be redirected from method style calls.
        match fn_name {
            // Handle type_of()
            KEYWORD_TYPE_OF if args.len() == 1 => {
                return Ok((
                    self.map_type_name(args[0].type_name()).to_string().into(),
                    false,
                ))
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN
                if args.len() == 2 && args[0].is::<FnPtr>() && args[1].is::<crate::INT>() =>
            {
                let fn_name = args[0].read_lock::<ImmutableString>().expect("`FnPtr`");
                let num_params = args[1].as_int().expect("`INT`");

                return Ok((
                    if num_params < 0 {
                        false
                    } else {
                        let hash_script = calc_fn_hash(fn_name.as_str(), num_params as usize);
                        self.has_script_fn(Some(global), state, lib, hash_script)
                    }
                    .into(),
                    false,
                ));
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if args.len() == 1 => {
                return no_method_err(fn_name, pos)
            }

            KEYWORD_FN_PTR | KEYWORD_EVAL | KEYWORD_IS_DEF_VAR if args.len() == 1 => {
                return no_method_err(fn_name, pos)
            }

            KEYWORD_FN_PTR_CALL | KEYWORD_FN_PTR_CURRY if !args.is_empty() => {
                return no_method_err(fn_name, pos)
            }

            _ => (),
        }

        // Script-defined function call?
        #[cfg(not(feature = "no_function"))]
        if let Some(FnResolutionCacheEntry { func, mut source }) = self
            .resolve_fn(
                global,
                state,
                lib,
                fn_name,
                hashes.script,
                None,
                false,
                false,
            )
            .cloned()
        {
            // Script function call
            assert!(func.is_script());

            let func = func.get_script_fn_def().expect("script-defined function");

            if func.body.is_empty() {
                return Ok((Dynamic::UNIT, false));
            }

            let mut empty_scope;
            let scope = match _scope {
                Some(scope) => scope,
                None => {
                    empty_scope = Scope::new();
                    &mut empty_scope
                }
            };

            mem::swap(&mut global.source, &mut source);

            let result = if _is_method_call {
                // Method call of script function - map first argument to `this`
                let (first_arg, rest_args) = args.split_first_mut().unwrap();

                let level = _level + 1;

                let result = self.call_script_fn(
                    scope,
                    global,
                    state,
                    lib,
                    &mut Some(*first_arg),
                    func,
                    rest_args,
                    pos,
                    true,
                    level,
                );

                result?
            } else {
                // Normal call of script function
                // The first argument is a reference?
                let mut backup: Option<ArgBackup> = None;
                if is_ref_mut && !args.is_empty() {
                    backup = Some(ArgBackup::new());
                    backup
                        .as_mut()
                        .expect("`Some`")
                        .change_first_arg_to_copy(args);
                }

                let level = _level + 1;

                let result = self.call_script_fn(
                    scope, global, state, lib, &mut None, func, args, pos, true, level,
                );

                // Restore the original reference
                if let Some(bk) = backup {
                    bk.restore_first_arg(args)
                }

                result?
            };

            // Restore the original source
            mem::swap(&mut global.source, &mut source);

            return Ok((result, false));
        }

        // Native function call
        let hash = hashes.native;
        self.call_native_fn(
            global, state, lib, fn_name, hash, args, is_ref_mut, false, pos,
        )
    }

    /// Evaluate a list of statements with no `this` pointer.
    /// This is commonly used to evaluate a list of statements in an [`AST`] or a script function body.
    #[inline]
    pub(crate) fn eval_global_statements(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        statements: &[Stmt],
        lib: &[&Module],
        level: usize,
    ) -> RhaiResult {
        self.eval_stmt_block(
            scope, global, state, lib, &mut None, statements, false, level,
        )
        .or_else(|err| match *err {
            ERR::Return(out, _) => Ok(out),
            ERR::LoopBreak(_, _) => {
                unreachable!("no outer loop scope to break out of")
            }
            _ => Err(err),
        })
    }

    /// Call a dot method.
    #[cfg(not(feature = "no_object"))]
    pub(crate) fn make_method_call(
        &self,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        fn_name: &str,
        mut hash: FnCallHashes,
        target: &mut crate::eval::Target,
        (call_args, call_arg_pos): &mut (FnArgsVec<Dynamic>, Position),
        pos: Position,
        level: usize,
    ) -> RhaiResultOf<(Dynamic, bool)> {
        let is_ref_mut = target.is_ref();

        let (result, updated) = match fn_name {
            KEYWORD_FN_PTR_CALL if target.is::<FnPtr>() => {
                // FnPtr call
                let fn_ptr = target.read_lock::<FnPtr>().expect("`FnPtr`");
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hashes
                let new_hash = calc_fn_hash(fn_name, args_len).into();
                // Arguments are passed as-is, adding the curried arguments
                let mut curry = FnArgsVec::with_capacity(fn_ptr.num_curried());
                curry.extend(fn_ptr.curry().iter().cloned());
                let mut args = FnArgsVec::with_capacity(curry.len() + call_args.len());
                args.extend(curry.iter_mut());
                args.extend(call_args.iter_mut());

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    global, state, lib, fn_name, new_hash, &mut args, false, false, pos, None,
                    level,
                )
            }
            KEYWORD_FN_PTR_CALL => {
                if !call_args.is_empty() {
                    if !call_args[0].is::<FnPtr>() {
                        return Err(self.make_type_mismatch_err::<FnPtr>(
                            self.map_type_name(call_args[0].type_name()),
                            *call_arg_pos,
                        ));
                    }
                } else {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(target.type_name()),
                        pos,
                    ));
                }

                // FnPtr call on object
                let fn_ptr = call_args.remove(0).cast::<FnPtr>();
                // Redirect function name
                let fn_name = fn_ptr.fn_name();
                let args_len = call_args.len() + fn_ptr.curry().len();
                // Recalculate hash
                let new_hash = FnCallHashes::from_all(
                    #[cfg(not(feature = "no_function"))]
                    calc_fn_hash(fn_name, args_len),
                    calc_fn_hash(fn_name, args_len + 1),
                );
                // Replace the first argument with the object pointer, adding the curried arguments
                let mut curry = FnArgsVec::with_capacity(fn_ptr.num_curried());
                curry.extend(fn_ptr.curry().iter().cloned());
                let mut args = FnArgsVec::with_capacity(curry.len() + call_args.len() + 1);
                args.push(target.as_mut());
                args.extend(curry.iter_mut());
                args.extend(call_args.iter_mut());

                // Map it to name(args) in function-call style
                self.exec_fn_call(
                    global, state, lib, fn_name, new_hash, &mut args, is_ref_mut, true, pos, None,
                    level,
                )
            }
            KEYWORD_FN_PTR_CURRY => {
                if !target.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(target.type_name()),
                        pos,
                    ));
                }

                let fn_ptr = target.read_lock::<FnPtr>().expect("`FnPtr`");

                // Curry call
                Ok((
                    if call_args.is_empty() {
                        fn_ptr.clone()
                    } else {
                        FnPtr::new_unchecked(
                            fn_ptr.fn_name_raw().clone(),
                            fn_ptr
                                .curry()
                                .iter()
                                .cloned()
                                .chain(call_args.iter_mut().map(mem::take))
                                .collect(),
                        )
                    }
                    .into(),
                    false,
                ))
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if call_args.is_empty() => {
                return Ok((target.is_shared().into(), false));
            }

            _ => {
                let mut fn_name = fn_name;
                let _redirected;

                // Check if it is a map method call in OOP style
                #[cfg(not(feature = "no_object"))]
                if let Some(map) = target.read_lock::<crate::Map>() {
                    if let Some(val) = map.get(fn_name) {
                        if let Some(fn_ptr) = val.read_lock::<FnPtr>() {
                            // Remap the function name
                            _redirected = fn_ptr.fn_name_raw().clone();
                            fn_name = &_redirected;
                            // Add curried arguments
                            if fn_ptr.is_curried() {
                                call_args.insert_many(0, fn_ptr.curry().iter().cloned());
                            }
                            // Recalculate the hash based on the new function name and new arguments
                            hash = FnCallHashes::from_all(
                                #[cfg(not(feature = "no_function"))]
                                calc_fn_hash(fn_name, call_args.len()),
                                calc_fn_hash(fn_name, call_args.len() + 1),
                            );
                        }
                    }
                };

                // Attached object pointer in front of the arguments
                let mut args = FnArgsVec::with_capacity(call_args.len() + 1);
                args.push(target.as_mut());
                args.extend(call_args.iter_mut());

                self.exec_fn_call(
                    global, state, lib, fn_name, hash, &mut args, is_ref_mut, true, pos, None,
                    level,
                )
            }
        }?;

        // Propagate the changed value back to the source if necessary
        if updated {
            target
                .propagate_changed_value()
                .map_err(|err| err.fill_position(pos))?;
        }

        Ok((result, updated))
    }

    /// Evaluate an argument.
    #[inline]
    pub(crate) fn get_arg_value(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        level: usize,
        arg_expr: &Expr,
        constants: &[Dynamic],
    ) -> RhaiResultOf<(Dynamic, Position)> {
        Ok((
            if let Expr::Stack(slot, _) = arg_expr {
                constants[*slot].clone()
            } else if let Some(value) = arg_expr.get_literal_value() {
                value
            } else {
                self.eval_expr(scope, global, state, lib, this_ptr, arg_expr, level)?
            },
            arg_expr.position(),
        ))
    }

    /// Call a function in normal function-call style.
    pub(crate) fn make_function_call(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        fn_name: &str,
        first_arg: Option<&Expr>,
        args_expr: &[Expr],
        constants: &[Dynamic],
        hashes: FnCallHashes,
        pos: Position,
        capture_scope: bool,
        level: usize,
    ) -> RhaiResult {
        let mut first_arg = first_arg;
        let mut a_expr = args_expr;
        let mut total_args = if first_arg.is_some() { 1 } else { 0 } + a_expr.len();
        let mut curry = FnArgsVec::new_const();
        let mut name = fn_name;
        let mut hashes = hashes;
        let redirected; // Handle call() - Redirect function call

        match name {
            // Handle call()
            KEYWORD_FN_PTR_CALL if total_args >= 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, arg, constants)?;

                if !arg_value.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(arg_value.type_name()),
                        arg_pos,
                    ));
                }

                let fn_ptr = arg_value.cast::<FnPtr>();
                curry.extend(fn_ptr.curry().iter().cloned());

                // Redirect function name
                redirected = fn_ptr.take_data().0;
                name = &redirected;

                // Shift the arguments
                first_arg = a_expr.get(0);
                if !a_expr.is_empty() {
                    a_expr = &a_expr[1..];
                }
                total_args -= 1;

                // Recalculate hash
                let args_len = total_args + curry.len();
                hashes = if !hashes.is_native_only() {
                    calc_fn_hash(name, args_len).into()
                } else {
                    FnCallHashes::from_native(calc_fn_hash(name, args_len))
                };
            }
            // Handle Fn()
            KEYWORD_FN_PTR if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, arg, constants)?;

                // Fn - only in function call style
                return arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))
                    .and_then(FnPtr::try_from)
                    .map(Into::into)
                    .map_err(|err| err.fill_position(arg_pos));
            }

            // Handle curry()
            KEYWORD_FN_PTR_CURRY if total_args > 1 => {
                let first = first_arg.unwrap();
                let (arg_value, arg_pos) = self
                    .get_arg_value(scope, global, state, lib, this_ptr, level, first, constants)?;

                if !arg_value.is::<FnPtr>() {
                    return Err(self.make_type_mismatch_err::<FnPtr>(
                        self.map_type_name(arg_value.type_name()),
                        arg_pos,
                    ));
                }

                let (name, fn_curry) = arg_value.cast::<FnPtr>().take_data();

                // Append the new curried arguments to the existing list.
                let fn_curry =
                    a_expr
                        .iter()
                        .try_fold(fn_curry, |mut curried, expr| -> RhaiResultOf<_> {
                            let (value, _) = self.get_arg_value(
                                scope, global, state, lib, this_ptr, level, expr, constants,
                            )?;
                            curried.push(value);
                            Ok(curried)
                        })?;

                return Ok(FnPtr::new_unchecked(name, fn_curry).into());
            }

            // Handle is_shared()
            #[cfg(not(feature = "no_closure"))]
            crate::engine::KEYWORD_IS_SHARED if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, _) =
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, arg, constants)?;
                return Ok(arg_value.is_shared().into());
            }

            // Handle is_def_fn()
            #[cfg(not(feature = "no_function"))]
            crate::engine::KEYWORD_IS_DEF_FN if total_args == 2 => {
                let first = first_arg.unwrap();
                let (arg_value, arg_pos) = self
                    .get_arg_value(scope, global, state, lib, this_ptr, level, first, constants)?;

                let fn_name = arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))?;

                let (arg_value, arg_pos) = self.get_arg_value(
                    scope, global, state, lib, this_ptr, level, &a_expr[0], constants,
                )?;

                let num_params = arg_value
                    .as_int()
                    .map_err(|typ| self.make_type_mismatch_err::<crate::INT>(typ, arg_pos))?;

                return Ok(if num_params < 0 {
                    false
                } else {
                    let hash_script = calc_fn_hash(&fn_name, num_params as usize);
                    self.has_script_fn(Some(global), state, lib, hash_script)
                }
                .into());
            }

            // Handle is_def_var()
            KEYWORD_IS_DEF_VAR if total_args == 1 => {
                let arg = first_arg.unwrap();
                let (arg_value, arg_pos) =
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, arg, constants)?;
                let var_name = arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, arg_pos))?;
                return Ok(scope.contains(&var_name).into());
            }

            // Handle eval()
            KEYWORD_EVAL if total_args == 1 => {
                // eval - only in function call style
                let orig_scope_len = scope.len();
                let arg = first_arg.unwrap();
                let (arg_value, pos) =
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, arg, constants)?;
                let script = &arg_value
                    .into_immutable_string()
                    .map_err(|typ| self.make_type_mismatch_err::<ImmutableString>(typ, pos))?;
                let result = self.eval_script_expr_in_place(
                    scope,
                    global,
                    state,
                    lib,
                    script,
                    pos,
                    level + 1,
                );

                // IMPORTANT! If the eval defines new variables in the current scope,
                //            all variable offsets from this point on will be mis-aligned.
                if scope.len() != orig_scope_len {
                    state.always_search_scope = true;
                }

                return result.map_err(|err| {
                    ERR::ErrorInFunctionCall(
                        KEYWORD_EVAL.to_string(),
                        global.source.to_string(),
                        err,
                        pos,
                    )
                    .into()
                });
            }

            _ => (),
        }

        // Normal function call - except for Fn, curry, call and eval (handled above)
        let mut arg_values = FnArgsVec::with_capacity(total_args);
        let mut args = FnArgsVec::with_capacity(total_args + curry.len());
        let mut is_ref_mut = false;

        // Capture parent scope?
        //
        // If so, do it separately because we cannot convert the first argument (if it is a simple
        // variable access) to &mut because `scope` is needed.
        if capture_scope && !scope.is_empty() {
            first_arg
                .iter()
                .map(|&v| v)
                .chain(a_expr.iter())
                .try_for_each(|expr| {
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, expr, constants)
                        .map(|(value, _)| arg_values.push(value.flatten()))
                })?;
            args.extend(curry.iter_mut());
            args.extend(arg_values.iter_mut());

            // Use parent scope
            let scope = Some(scope);

            return self
                .exec_fn_call(
                    global, state, lib, name, hashes, &mut args, is_ref_mut, false, pos, scope,
                    level,
                )
                .map(|(v, _)| v);
        }

        // Call with blank scope
        if total_args == 0 && curry.is_empty() {
            // No arguments
        } else {
            // If the first argument is a variable, and there is no curried arguments,
            // convert to method-call style in order to leverage potential &mut first argument and
            // avoid cloning the value
            if curry.is_empty() && first_arg.map_or(false, |expr| expr.is_variable_access(false)) {
                // func(x, ...) -> x.func(...)
                let first_expr = first_arg.unwrap();

                a_expr.iter().try_for_each(|expr| {
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, expr, constants)
                        .map(|(value, _)| arg_values.push(value.flatten()))
                })?;

                let (mut target, _pos) =
                    self.search_namespace(scope, global, state, lib, this_ptr, first_expr)?;

                if target.as_ref().is_read_only() {
                    target = target.into_owned();
                }

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, _pos)?;

                #[cfg(not(feature = "no_closure"))]
                let target_is_shared = target.is_shared();
                #[cfg(feature = "no_closure")]
                let target_is_shared = false;

                if target_is_shared || target.is_temp_value() {
                    arg_values.insert(0, target.take_or_clone().flatten());
                    args.extend(arg_values.iter_mut())
                } else {
                    // Turn it into a method call only if the object is not shared and not a simple value
                    is_ref_mut = true;
                    let obj_ref = target.take_ref().expect("ref");
                    args.push(obj_ref);
                    args.extend(arg_values.iter_mut());
                }
            } else {
                // func(..., ...)
                first_arg
                    .into_iter()
                    .chain(a_expr.iter())
                    .try_for_each(|expr| {
                        self.get_arg_value(
                            scope, global, state, lib, this_ptr, level, expr, constants,
                        )
                        .map(|(value, _)| arg_values.push(value.flatten()))
                    })?;
                args.extend(curry.iter_mut());
                args.extend(arg_values.iter_mut());
            }
        }

        self.exec_fn_call(
            global, state, lib, name, hashes, &mut args, is_ref_mut, false, pos, None, level,
        )
        .map(|(v, _)| v)
    }

    /// Call a namespace-qualified function in normal function-call style.
    pub(crate) fn make_qualified_function_call(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        this_ptr: &mut Option<&mut Dynamic>,
        namespace: &Namespace,
        fn_name: &str,
        args_expr: &[Expr],
        constants: &[Dynamic],
        hash: u64,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let mut arg_values = FnArgsVec::with_capacity(args_expr.len());
        let mut args = FnArgsVec::with_capacity(args_expr.len());
        let mut first_arg_value = None;

        if args_expr.is_empty() {
            // No arguments
        } else {
            // See if the first argument is a variable (not namespace-qualified).
            // If so, convert to method-call style in order to leverage potential
            // &mut first argument and avoid cloning the value
            if !args_expr.is_empty() && args_expr[0].is_variable_access(true) {
                // func(x, ...) -> x.func(...)
                arg_values.push(Dynamic::UNIT);

                args_expr.iter().skip(1).try_for_each(|expr| {
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, expr, constants)
                        .map(|(value, _)| arg_values.push(value.flatten()))
                })?;

                // Get target reference to first argument
                let (target, _pos) =
                    self.search_scope_only(scope, global, state, lib, this_ptr, &args_expr[0])?;

                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, _pos)?;

                #[cfg(not(feature = "no_closure"))]
                let target_is_shared = target.is_shared();
                #[cfg(feature = "no_closure")]
                let target_is_shared = false;

                if target_is_shared || target.is_temp_value() {
                    arg_values[0] = target.take_or_clone().flatten();
                    args.extend(arg_values.iter_mut());
                } else {
                    // Turn it into a method call only if the object is not shared and not a simple value
                    let (first, rest) = arg_values.split_first_mut().unwrap();
                    first_arg_value = Some(first);
                    let obj_ref = target.take_ref().expect("ref");
                    args.push(obj_ref);
                    args.extend(rest.iter_mut());
                }
            } else {
                // func(..., ...) or func(mod::x, ...)
                args_expr.iter().try_for_each(|expr| {
                    self.get_arg_value(scope, global, state, lib, this_ptr, level, expr, constants)
                        .map(|(value, _)| arg_values.push(value.flatten()))
                })?;
                args.extend(arg_values.iter_mut());
            }
        }

        let module = self
            .search_imports(global, state, namespace)
            .ok_or_else(|| ERR::ErrorModuleNotFound(namespace.to_string(), namespace[0].pos))?;

        // First search in script-defined functions (can override built-in)
        let func = match module.get_qualified_fn(hash) {
            // Then search in Rust functions
            None => {
                #[cfg(not(feature = "unchecked"))]
                self.inc_operations(&mut global.num_operations, pos)?;

                let hash_params = calc_fn_params_hash(args.iter().map(|a| a.type_id()));
                let hash_qualified_fn = combine_hashes(hash, hash_params);

                module.get_qualified_fn(hash_qualified_fn)
            }
            r => r,
        };

        // Clone first argument if the function is not a method after-all
        if !func.map(|f| f.is_method()).unwrap_or(true) {
            if let Some(first) = first_arg_value {
                *first = args[0].clone();
                args[0] = first;
            }
        }

        match func {
            #[cfg(not(feature = "no_function"))]
            Some(f) if f.is_script() => {
                let fn_def = f.get_script_fn_def().expect("script-defined function");

                if fn_def.body.is_empty() {
                    Ok(Dynamic::UNIT)
                } else {
                    let new_scope = &mut Scope::new();

                    let mut source = module.id_raw().clone();
                    mem::swap(&mut global.source, &mut source);

                    let level = level + 1;

                    let result = self.call_script_fn(
                        new_scope, global, state, lib, &mut None, fn_def, &mut args, pos, true,
                        level,
                    );

                    global.source = source;

                    result
                }
            }

            Some(f) if f.is_plugin_fn() => {
                let context = (self, fn_name, module.id(), &*global, lib, pos).into();
                let result = f
                    .get_plugin_fn()
                    .expect("plugin function")
                    .clone()
                    .call(context, &mut args);
                self.check_return_value(result, pos)
            }

            Some(f) if f.is_native() => {
                let func = f.get_native_fn().expect("native function");
                let context = (self, fn_name, module.id(), &*global, lib, pos).into();
                let result = func(context, &mut args);
                self.check_return_value(result, pos)
            }

            Some(f) => unreachable!("unknown function type: {:?}", f),

            None => Err(ERR::ErrorFunctionNotFound(
                self.gen_call_signature(Some(namespace), fn_name, &args),
                pos,
            )
            .into()),
        }
    }

    /// Evaluate a text script in place - used primarily for 'eval'.
    pub(crate) fn eval_script_expr_in_place(
        &self,
        scope: &mut Scope,
        global: &mut GlobalRuntimeState,
        state: &mut EvalState,
        lib: &[&Module],
        script: &str,
        pos: Position,
        level: usize,
    ) -> RhaiResult {
        let _pos = pos;

        #[cfg(not(feature = "unchecked"))]
        self.inc_operations(&mut global.num_operations, _pos)?;

        let script = script.trim();

        if script.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Compile the script text
        // No optimizations because we only run it once
        let ast = self.compile_with_scope_and_optimization_level(
            &Scope::new(),
            &[script],
            #[cfg(not(feature = "no_optimize"))]
            crate::OptimizationLevel::None,
        )?;

        // If new functions are defined within the eval string, it is an error
        #[cfg(not(feature = "no_function"))]
        if !ast.shared_lib().is_empty() {
            return Err(crate::PERR::WrongFnDefinition.into());
        }

        let statements = ast.statements();
        if statements.is_empty() {
            return Ok(Dynamic::UNIT);
        }

        // Evaluate the AST
        self.eval_global_statements(scope, global, state, statements, lib, level)
    }
}
