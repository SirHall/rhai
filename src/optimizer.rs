//! Module implementing the [`AST`] optimizer.
#![cfg(not(feature = "no_optimize"))]

use crate::ast::{Expr, OpAssignment, Stmt, AST_OPTION_FLAGS::*};
use crate::engine::{KEYWORD_DEBUG, KEYWORD_EVAL, KEYWORD_FN_PTR, KEYWORD_PRINT, KEYWORD_TYPE_OF};
use crate::eval::{EvalState, GlobalRuntimeState};
use crate::func::builtin::get_builtin_binary_op_fn;
use crate::func::hashing::get_hasher;
use crate::tokenizer::Token;
use crate::types::dynamic::AccessMode;
use crate::{
    calc_fn_hash, calc_fn_params_hash, combine_hashes, Dynamic, Engine, FnPtr, Position, Scope,
    StaticVec, AST, INT,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    any::TypeId,
    convert::TryFrom,
    hash::{Hash, Hasher},
    mem,
};

/// Level of optimization performed.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub enum OptimizationLevel {
    /// No optimization performed.
    None,
    /// Only perform simple optimizations without evaluating functions.
    Simple,
    /// Full optimizations performed, including evaluating functions.
    /// Take care that this may cause side effects as it essentially assumes that all functions are pure.
    Full,
}

impl Default for OptimizationLevel {
    #[inline(always)]
    fn default() -> Self {
        Self::Simple
    }
}

/// Mutable state throughout an optimization pass.
#[derive(Debug, Clone)]
struct OptimizerState<'a> {
    /// Has the [`AST`] been changed during this pass?
    changed: bool,
    /// Collection of constants to use for eager function evaluations.
    variables: StaticVec<(String, AccessMode, Option<Dynamic>)>,
    /// Activate constants propagation?
    propagate_constants: bool,
    /// An [`Engine`] instance for eager function evaluation.
    engine: &'a Engine,
    /// [Module][crate::Module] containing script-defined functions.
    #[cfg(not(feature = "no_function"))]
    lib: &'a [&'a crate::Module],
    /// Optimization level.
    optimization_level: OptimizationLevel,
}

impl<'a> OptimizerState<'a> {
    /// Create a new State.
    #[inline(always)]
    pub const fn new(
        engine: &'a Engine,
        #[cfg(not(feature = "no_function"))] lib: &'a [&'a crate::Module],
        optimization_level: OptimizationLevel,
    ) -> Self {
        Self {
            changed: false,
            variables: StaticVec::new_const(),
            propagate_constants: true,
            engine,
            #[cfg(not(feature = "no_function"))]
            lib,
            optimization_level,
        }
    }
    /// Set the [`AST`] state to be dirty (i.e. changed).
    #[inline(always)]
    pub fn set_dirty(&mut self) {
        self.changed = true;
    }
    /// Set the [`AST`] state to be not dirty (i.e. unchanged).
    #[inline(always)]
    pub fn clear_dirty(&mut self) {
        self.changed = false;
    }
    /// Is the [`AST`] dirty (i.e. changed)?
    #[inline(always)]
    pub const fn is_dirty(&self) -> bool {
        self.changed
    }
    /// Prune the list of constants back to a specified size.
    #[inline(always)]
    pub fn restore_var(&mut self, len: usize) {
        self.variables.truncate(len)
    }
    /// Add a new constant to the list.
    #[inline(always)]
    pub fn push_var(
        &mut self,
        name: impl Into<String>,
        access: AccessMode,
        value: Option<Dynamic>,
    ) {
        self.variables.push((name.into(), access, value))
    }
    /// Look up a constant from the list.
    #[inline]
    pub fn find_constant(&self, name: &str) -> Option<&Dynamic> {
        if !self.propagate_constants {
            return None;
        }

        for (n, access, value) in self.variables.iter().rev() {
            if n == name {
                return match access {
                    AccessMode::ReadWrite => None,
                    AccessMode::ReadOnly => value.as_ref(),
                };
            }
        }

        None
    }
    /// Call a registered function
    #[inline]
    pub fn call_fn_with_constant_arguments(
        &self,
        fn_name: &str,
        arg_values: &mut [Dynamic],
    ) -> Option<Dynamic> {
        #[cfg(not(feature = "no_function"))]
        let lib = self.lib;
        #[cfg(feature = "no_function")]
        let lib = &[];

        self.engine
            .call_native_fn(
                &mut GlobalRuntimeState::new(),
                &mut EvalState::new(),
                lib,
                fn_name,
                calc_fn_hash(&fn_name, arg_values.len()),
                &mut arg_values.iter_mut().collect::<StaticVec<_>>(),
                false,
                false,
                Position::NONE,
            )
            .ok()
            .map(|(v, _)| v)
    }
}

// Has a system function a Rust-native override?
fn has_native_fn_override(
    engine: &Engine,
    hash_script: u64,
    arg_types: impl AsRef<[TypeId]>,
) -> bool {
    let hash_params = calc_fn_params_hash(arg_types.as_ref().iter().cloned());
    let hash = combine_hashes(hash_script, hash_params);

    // First check the global namespace and packages, but skip modules that are standard because
    // they should never conflict with system functions.
    engine.global_modules.iter().filter(|m| !m.standard).any(|m| m.contains_fn(hash))
            // Then check sub-modules
            || engine.global_sub_modules.values().any(|m| m.contains_qualified_fn(hash))
}

/// Optimize a block of [statements][Stmt].
fn optimize_stmt_block(
    mut statements: StaticVec<Stmt>,
    state: &mut OptimizerState,
    preserve_result: bool,
    is_internal: bool,
    reduce_return: bool,
) -> StaticVec<Stmt> {
    if statements.is_empty() {
        return statements;
    }

    let mut is_dirty = state.is_dirty();

    let is_pure = if is_internal {
        Stmt::is_internally_pure
    } else {
        Stmt::is_pure
    };

    // Flatten blocks
    loop {
        if let Some(n) = statements.iter().enumerate().find_map(|(i, s)| match s {
            Stmt::Block(block, _) if !block.iter().any(Stmt::is_block_dependent) => Some(i),
            _ => None,
        }) {
            let (first, second) = statements.split_at_mut(n);
            let stmt = mem::take(&mut second[0]);
            let mut stmts = match stmt {
                Stmt::Block(block, _) => block,
                stmt => unreachable!("Stmt::Block expected but gets {:?}", stmt),
            };
            statements = first
                .iter_mut()
                .map(mem::take)
                .chain(stmts.iter_mut().map(mem::take))
                .chain(second.iter_mut().skip(1).map(mem::take))
                .collect();
        } else {
            break;
        }

        is_dirty = true;
    }

    // Optimize
    loop {
        state.clear_dirty();

        let orig_constants_len = state.variables.len(); // Original number of constants in the state, for restore later
        let orig_propagate_constants = state.propagate_constants;

        // Remove everything following control flow breaking statements
        let mut dead_code = false;

        statements.retain(|stmt| {
            if dead_code {
                state.set_dirty();
                false
            } else if stmt.is_control_flow_break() {
                dead_code = true;
                true
            } else {
                true
            }
        });

        // Optimize each statement in the block
        statements.iter_mut().for_each(|stmt| {
            match stmt {
                Stmt::Var(value_expr, x, options, _) => {
                    if options.contains(AST_OPTION_CONSTANT) {
                        // Add constant literals into the state
                        optimize_expr(value_expr, state, false);

                        if value_expr.is_constant() {
                            state.push_var(
                                x.name.as_str(),
                                AccessMode::ReadOnly,
                                value_expr.get_literal_value(),
                            );
                        }
                    } else {
                        // Add variables into the state
                        optimize_expr(value_expr, state, false);
                        state.push_var(x.name.as_str(), AccessMode::ReadWrite, None);
                    }
                }
                // Optimize the statement
                _ => optimize_stmt(stmt, state, preserve_result),
            }
        });

        // Remove all pure statements except the last one
        let mut index = 0;
        let mut first_non_constant = statements
            .iter()
            .rev()
            .enumerate()
            .find_map(|(i, stmt)| match stmt {
                stmt if !is_pure(stmt) => Some(i),

                Stmt::Var(e, _, _, _) | Stmt::Expr(e) if !e.is_constant() => Some(i),

                #[cfg(not(feature = "no_module"))]
                Stmt::Import(e, _, _) if !e.is_constant() => Some(i),

                _ => None,
            })
            .map_or(0, |n| statements.len() - n - 1);

        while index < statements.len() {
            if preserve_result && index >= statements.len() - 1 {
                break;
            } else {
                match statements[index] {
                    ref stmt if is_pure(stmt) && index >= first_non_constant => {
                        state.set_dirty();
                        statements.remove(index);
                    }
                    ref stmt if stmt.is_pure() => {
                        state.set_dirty();
                        if index < first_non_constant {
                            first_non_constant -= 1;
                        }
                        statements.remove(index);
                    }
                    _ => index += 1,
                }
            }
        }

        // Remove all pure statements that do not return values at the end of a block.
        // We cannot remove anything for non-pure statements due to potential side-effects.
        if preserve_result {
            loop {
                match statements[..] {
                    // { return; } -> {}
                    [Stmt::Return(options, None, _)]
                        if reduce_return && !options.contains(AST_OPTION_BREAK_OUT) =>
                    {
                        state.set_dirty();
                        statements.clear();
                    }
                    [ref stmt] if !stmt.returns_value() && is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., ref last_stmt, Stmt::Return(options, None, _)]
                        if reduce_return
                            && !options.contains(AST_OPTION_BREAK_OUT)
                            && !last_stmt.returns_value() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return val; } -> { ...; val }
                    [.., Stmt::Return(options, ref mut expr, pos)]
                        if reduce_return && !options.contains(AST_OPTION_BREAK_OUT) =>
                    {
                        state.set_dirty();
                        *statements.last_mut().unwrap() = expr
                            .as_mut()
                            .map_or_else(|| Stmt::Noop(pos), |e| Stmt::Expr(mem::take(e)));
                    }
                    // { ...; stmt; noop } -> done
                    [.., ref second_last_stmt, Stmt::Noop(_)]
                        if second_last_stmt.returns_value() =>
                    {
                        break
                    }
                    // { ...; stmt_that_returns; pure_non_value_stmt } -> { ...; stmt_that_returns; noop }
                    // { ...; stmt; pure_non_value_stmt } -> { ...; stmt }
                    [.., ref second_last_stmt, ref last_stmt]
                        if !last_stmt.returns_value() && is_pure(last_stmt) =>
                    {
                        state.set_dirty();
                        if second_last_stmt.returns_value() {
                            *statements.last_mut().unwrap() = Stmt::Noop(last_stmt.position());
                        } else {
                            statements.pop().unwrap();
                        }
                    }
                    _ => break,
                }
            }
        } else {
            loop {
                match statements[..] {
                    [ref stmt] if is_pure(stmt) => {
                        state.set_dirty();
                        statements.clear();
                    }
                    // { ...; return; } -> { ... }
                    [.., Stmt::Return(options, None, _)]
                        if reduce_return && !options.contains(AST_OPTION_BREAK_OUT) =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    // { ...; return pure_val; } -> { ... }
                    [.., Stmt::Return(options, Some(ref expr), _)]
                        if reduce_return
                            && !options.contains(AST_OPTION_BREAK_OUT)
                            && expr.is_pure() =>
                    {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    [.., ref last_stmt] if is_pure(last_stmt) => {
                        state.set_dirty();
                        statements.pop().unwrap();
                    }
                    _ => break,
                }
            }
        }

        // Pop the stack and remove all the local constants
        state.restore_var(orig_constants_len);
        state.propagate_constants = orig_propagate_constants;

        if !state.is_dirty() {
            break;
        }

        is_dirty = true;
    }

    if is_dirty {
        state.set_dirty();
    }

    statements.shrink_to_fit();
    statements
}

/// Optimize a [statement][Stmt].
fn optimize_stmt(stmt: &mut Stmt, state: &mut OptimizerState, preserve_result: bool) {
    match stmt {
        // var = var op expr => var op= expr
        Stmt::Assignment(x, _)
            if x.1.is_none()
                && x.0.is_variable_access(true)
                && matches!(&x.2, Expr::FnCall(x2, _)
                        if Token::lookup_from_syntax(&x2.name).map(|t| t.has_op_assignment()).unwrap_or(false)
                        && x2.args.len() == 2
                        && x2.args[0].get_variable_name(true) == x.0.get_variable_name(true)
                ) =>
        {
            match x.2 {
                Expr::FnCall(ref mut x2, _) => {
                    state.set_dirty();
                    x.1 = Some(OpAssignment::new(&x2.name));

                    let value = mem::take(&mut x2.args[1]);

                    if let Expr::Stack(slot, pos) = value {
                        x.2 =
                            Expr::from_dynamic(mem::take(x2.constants.get_mut(slot).unwrap()), pos);
                    } else {
                        x.2 = value;
                    }
                }
                ref expr => unreachable!("Expr::FnCall expected but gets {:?}", expr),
            }
        }

        // expr op= expr
        Stmt::Assignment(x, _) => match x.0 {
            Expr::Variable(_, _, _) => optimize_expr(&mut x.2, state, false),
            _ => {
                optimize_expr(&mut x.0, state, false);
                optimize_expr(&mut x.2, state, false);
            }
        },

        // if expr {}
        Stmt::If(condition, x, _) if x.0.is_empty() && x.1.is_empty() => {
            state.set_dirty();

            let pos = condition.position();
            let mut expr = mem::take(condition);
            optimize_expr(&mut expr, state, false);

            *stmt = if preserve_result {
                // -> { expr, Noop }
                Stmt::Block([Stmt::Expr(expr), Stmt::Noop(pos)].into(), pos)
            } else {
                // -> expr
                Stmt::Expr(expr)
            };
        }
        // if false { if_block } -> Noop
        Stmt::If(Expr::BoolConstant(false, pos), x, _) if x.1.is_empty() => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos);
        }
        // if false { if_block } else { else_block } -> else_block
        Stmt::If(Expr::BoolConstant(false, _), x, _) => {
            state.set_dirty();
            *stmt =
                match optimize_stmt_block(mem::take(&mut *x.1), state, preserve_result, true, false)
                {
                    statements if statements.is_empty() => Stmt::Noop(x.1.position()),
                    statements => Stmt::Block(statements.into_boxed_slice(), x.1.position()),
                }
        }
        // if true { if_block } else { else_block } -> if_block
        Stmt::If(Expr::BoolConstant(true, _), x, _) => {
            state.set_dirty();
            *stmt =
                match optimize_stmt_block(mem::take(&mut *x.0), state, preserve_result, true, false)
                {
                    statements if statements.is_empty() => Stmt::Noop(x.0.position()),
                    statements => Stmt::Block(statements.into_boxed_slice(), x.0.position()),
                }
        }
        // if expr { if_block } else { else_block }
        Stmt::If(condition, x, _) => {
            optimize_expr(condition, state, false);
            *x.0 = optimize_stmt_block(mem::take(&mut *x.0), state, preserve_result, true, false);
            *x.1 = optimize_stmt_block(mem::take(&mut *x.1), state, preserve_result, true, false);
        }

        // switch const { ... }
        Stmt::Switch(match_expr, x, pos) if match_expr.is_constant() => {
            let value = match_expr.get_literal_value().unwrap();
            let hasher = &mut get_hasher();
            value.hash(hasher);
            let hash = hasher.finish();

            let table = &mut x.0;

            // First check hashes
            if let Some(block) = table.get_mut(&hash) {
                if let Some(mut condition) = mem::take(&mut block.0) {
                    // switch const { case if condition => stmt, _ => def } => if condition { stmt } else { def }
                    optimize_expr(&mut condition, state, false);

                    let def_stmt =
                        optimize_stmt_block(mem::take(&mut *x.1), state, true, true, false);

                    *stmt = Stmt::If(
                        condition,
                        Box::new((
                            mem::take(&mut block.1),
                            Stmt::Block(def_stmt.into_boxed_slice(), x.1.position().or_else(*pos))
                                .into(),
                        )),
                        match_expr.position(),
                    );
                } else {
                    // Promote the matched case
                    let statements =
                        optimize_stmt_block(mem::take(&mut *block.1), state, true, true, false);
                    *stmt = Stmt::Block(statements.into_boxed_slice(), block.1.position());
                }

                state.set_dirty();
                return;
            }

            // Then check ranges
            let ranges = &mut x.2;

            if value.is::<INT>() && !ranges.is_empty() {
                let value = value.as_int().expect("`INT`");

                // Only one range or all ranges without conditions
                if ranges.len() == 1 || ranges.iter().all(|(_, _, _, c, _)| c.is_none()) {
                    for (_, _, _, condition, stmt_block) in
                        ranges
                            .iter_mut()
                            .filter(|&&mut (start, end, inclusive, _, _)| {
                                (!inclusive && (start..end).contains(&value))
                                    || (inclusive && (start..=end).contains(&value))
                            })
                    {
                        if let Some(mut condition) = mem::take(condition) {
                            // switch const { range if condition => stmt, _ => def } => if condition { stmt } else { def }
                            optimize_expr(&mut condition, state, false);

                            let def_stmt =
                                optimize_stmt_block(mem::take(&mut *x.1), state, true, true, false);
                            *stmt = Stmt::If(
                                condition,
                                Box::new((
                                    mem::take(stmt_block),
                                    Stmt::Block(
                                        def_stmt.into_boxed_slice(),
                                        x.1.position().or_else(*pos),
                                    )
                                    .into(),
                                )),
                                match_expr.position(),
                            );
                        } else {
                            // Promote the matched case
                            let statements = mem::take(&mut **stmt_block);
                            let statements =
                                optimize_stmt_block(statements, state, true, true, false);
                            *stmt =
                                Stmt::Block(statements.into_boxed_slice(), stmt_block.position());
                        }

                        state.set_dirty();
                        return;
                    }
                } else {
                    // Multiple ranges - clear the table and just keep the right ranges
                    if !table.is_empty() {
                        state.set_dirty();
                        table.clear();
                    }

                    let old_ranges_len = ranges.len();

                    ranges.retain(|&mut (start, end, inclusive, _, _)| {
                        (!inclusive && (start..end).contains(&value))
                            || (inclusive && (start..=end).contains(&value))
                    });

                    if ranges.len() != old_ranges_len {
                        state.set_dirty();
                    }

                    for (_, _, _, condition, stmt_block) in ranges.iter_mut() {
                        let statements = mem::take(&mut **stmt_block);
                        **stmt_block =
                            optimize_stmt_block(statements, state, preserve_result, true, false);

                        if let Some(mut c) = mem::take(condition) {
                            optimize_expr(&mut c, state, false);
                            match c {
                                Expr::Unit(_) | Expr::BoolConstant(true, _) => state.set_dirty(),
                                _ => *condition = Some(c),
                            }
                        }
                    }
                    return;
                }
            }

            // Promote the default case
            state.set_dirty();
            let def_stmt = optimize_stmt_block(mem::take(&mut *x.1), state, true, true, false);
            *stmt = Stmt::Block(def_stmt.into_boxed_slice(), x.1.position().or_else(*pos));
        }
        // switch
        Stmt::Switch(match_expr, x, _) => {
            optimize_expr(match_expr, state, false);
            x.0.values_mut().for_each(|block| {
                let statements = mem::take(&mut *block.1);
                *block.1 = optimize_stmt_block(statements, state, preserve_result, true, false);

                if let Some(mut condition) = mem::take(&mut block.0) {
                    optimize_expr(&mut condition, state, false);
                    match condition {
                        Expr::Unit(_) | Expr::BoolConstant(true, _) => state.set_dirty(),
                        _ => block.0 = Some(condition),
                    }
                }
            });

            // Remove false cases
            while let Some((&key, _)) = x.0.iter().find(|(_, block)| match block.0 {
                Some(Expr::BoolConstant(false, _)) => true,
                _ => false,
            }) {
                state.set_dirty();
                x.0.remove(&key);
            }

            *x.1 = optimize_stmt_block(mem::take(&mut *x.1), state, preserve_result, true, false);
        }

        // while false { block } -> Noop
        Stmt::While(Expr::BoolConstant(false, pos), _, _) => {
            state.set_dirty();
            *stmt = Stmt::Noop(*pos)
        }
        // while expr { block }
        Stmt::While(condition, body, _) => {
            optimize_expr(condition, state, false);
            if let Expr::BoolConstant(true, pos) = condition {
                *condition = Expr::Unit(*pos);
            }
            ***body = optimize_stmt_block(mem::take(&mut **body), state, false, true, false);

            if body.len() == 1 {
                match body[0] {
                    // while expr { break; } -> { expr; }
                    Stmt::BreakLoop(options, pos) if options.contains(AST_OPTION_BREAK_OUT) => {
                        // Only a single break statement - turn into running the guard expression once
                        state.set_dirty();
                        if !condition.is_unit() {
                            let mut statements = vec![Stmt::Expr(mem::take(condition))];
                            if preserve_result {
                                statements.push(Stmt::Noop(pos))
                            }
                            *stmt = Stmt::Block(statements.into_boxed_slice(), pos);
                        } else {
                            *stmt = Stmt::Noop(pos);
                        };
                    }
                    _ => (),
                }
            }
        }
        // do { block } while false | do { block } until true -> { block }
        Stmt::Do(body, Expr::BoolConstant(x, _), options, _)
            if *x == options.contains(AST_OPTION_NEGATED) =>
        {
            state.set_dirty();
            *stmt = Stmt::Block(
                optimize_stmt_block(mem::take(&mut **body), state, false, true, false)
                    .into_boxed_slice(),
                body.position(),
            );
        }
        // do { block } while|until expr
        Stmt::Do(body, condition, _, _) => {
            optimize_expr(condition, state, false);
            ***body = optimize_stmt_block(mem::take(&mut **body), state, false, true, false);
        }
        // for id in expr { block }
        Stmt::For(iterable, x, _) => {
            optimize_expr(iterable, state, false);
            *x.2 = optimize_stmt_block(mem::take(&mut *x.2), state, false, true, false);
        }
        // let id = expr;
        Stmt::Var(expr, _, options, _) if !options.contains(AST_OPTION_CONSTANT) => {
            optimize_expr(expr, state, false)
        }
        // import expr as var;
        #[cfg(not(feature = "no_module"))]
        Stmt::Import(expr, _, _) => optimize_expr(expr, state, false),
        // { block }
        Stmt::Block(statements, pos) => {
            let statements = mem::take(statements).into_vec().into();
            let mut block = optimize_stmt_block(statements, state, preserve_result, true, false);

            match block.as_mut_slice() {
                [] => {
                    state.set_dirty();
                    *stmt = Stmt::Noop(*pos);
                }
                // Only one statement which is not block-dependent - promote
                [s] if !s.is_block_dependent() => {
                    state.set_dirty();
                    *stmt = mem::take(s);
                }
                _ => *stmt = Stmt::Block(block.into_boxed_slice(), *pos),
            }
        }
        // try { pure try_block } catch ( var ) { catch_block } -> try_block
        Stmt::TryCatch(x, _) if x.0.iter().all(Stmt::is_pure) => {
            // If try block is pure, there will never be any exceptions
            state.set_dirty();
            *stmt = Stmt::Block(
                optimize_stmt_block(mem::take(&mut *x.0), state, false, true, false)
                    .into_boxed_slice(),
                x.0.position(),
            );
        }
        // try { try_block } catch ( var ) { catch_block }
        Stmt::TryCatch(x, _) => {
            *x.0 = optimize_stmt_block(mem::take(&mut *x.0), state, false, true, false);
            *x.2 = optimize_stmt_block(mem::take(&mut *x.2), state, false, true, false);
        }
        // func(...)
        Stmt::Expr(expr @ Expr::FnCall(_, _)) => {
            optimize_expr(expr, state, false);
            match expr {
                Expr::FnCall(x, pos) => {
                    state.set_dirty();
                    *stmt = Stmt::FnCall(mem::take(x), *pos);
                }
                _ => (),
            }
        }
        // {}
        Stmt::Expr(Expr::Stmt(x)) if x.is_empty() => {
            state.set_dirty();
            *stmt = Stmt::Noop(x.position());
        }
        // {...};
        Stmt::Expr(Expr::Stmt(x)) => {
            state.set_dirty();
            *stmt = mem::take(&mut **x).into();
        }
        // expr;
        Stmt::Expr(expr) => optimize_expr(expr, state, false),
        // return expr;
        Stmt::Return(_, Some(ref mut expr), _) => optimize_expr(expr, state, false),

        // All other statements - skip
        _ => (),
    }
}

/// Optimize an [expression][Expr].
fn optimize_expr(expr: &mut Expr, state: &mut OptimizerState, chaining: bool) {
    // These keywords are handled specially
    const DONT_EVAL_KEYWORDS: &[&str] = &[
        KEYWORD_PRINT, // side effects
        KEYWORD_DEBUG, // side effects
        KEYWORD_EVAL,  // arbitrary scripts
    ];

    let _chaining = chaining;

    match expr {
        // {}
        Expr::Stmt(x) if x.is_empty() => { state.set_dirty(); *expr = Expr::Unit(x.position()) }
        // { stmt; ... } - do not count promotion as dirty because it gets turned back into an array
        Expr::Stmt(x) => {
            ***x = optimize_stmt_block(mem::take(&mut **x), state, true, true, false);

            // { Stmt(Expr) } - promote
            match x.as_mut().as_mut() {
                [ Stmt::Expr(e) ] => { state.set_dirty(); *expr = mem::take(e); }
                _ => ()
            }
        }
        // lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x,_, _) if !_chaining => match (&mut x.lhs, &mut x.rhs) {
            // map.string
            (Expr::Map(m, pos), Expr::Property(p)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                let prop = p.2.0.as_str();
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, _)| x.name == prop)
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // var.rhs
            (Expr::Variable(_, _, _), rhs) => optimize_expr(rhs, state, true),
            // lhs.rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, true); }
        }
        // ....lhs.rhs
        #[cfg(not(feature = "no_object"))]
        Expr::Dot(x,_, _) => { optimize_expr(&mut x.lhs, state, false); optimize_expr(&mut x.rhs, state, _chaining); }

        // lhs[rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x, _, _) if !_chaining => match (&mut x.lhs, &mut x.rhs) {
            // array[int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, _)) if *i >= 0 && (*i as usize) < a.len() && a.iter().all(Expr::is_pure) => {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let mut result = mem::take(&mut a[*i as usize]);
                result.set_position(*pos);
                *expr = result;
            }
            // array[-int]
            (Expr::Array(a, pos), Expr::IntegerConstant(i, _)) if *i < 0 && i.checked_abs().map(|n| n as usize <= a.len()).unwrap_or(false) && a.iter().all(Expr::is_pure) => {
                // Array literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                let index = a.len() - i.abs() as usize;
                let mut result = mem::take(&mut a[index]);
                result.set_position(*pos);
                *expr = result;
            }
            // map[string]
            (Expr::Map(m, pos), Expr::StringConstant(s, _)) if m.0.iter().all(|(_, x)| x.is_pure()) => {
                // Map literal where everything is pure - promote the indexed item.
                // All other items can be thrown away.
                state.set_dirty();
                *expr = mem::take(&mut m.0).into_iter().find(|(x, _)| x.name.as_str() == s.as_str())
                            .map(|(_, mut expr)| { expr.set_position(*pos); expr })
                            .unwrap_or_else(|| Expr::Unit(*pos));
            }
            // int[int]
            (Expr::IntegerConstant(n, pos), Expr::IntegerConstant(i, _)) if *i >= 0 && (*i as usize) < (std::mem::size_of_val(n) * 8) => {
                // Bit-field literal indexing - get the bit
                state.set_dirty();
                *expr = Expr::BoolConstant((*n & (1 << (*i as usize))) != 0, *pos);
            }
            // int[-int]
            (Expr::IntegerConstant(n, pos), Expr::IntegerConstant(i, _)) if *i < 0 && i.checked_abs().map(|i| i as usize <= (std::mem::size_of_val(n) * 8)).unwrap_or(false) => {
                // Bit-field literal indexing - get the bit
                state.set_dirty();
                *expr = Expr::BoolConstant((*n & (1 << (std::mem::size_of_val(n) * 8 - i.abs() as usize))) != 0, *pos);
            }
            // string[int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) if *i >= 0 && (*i as usize) < s.chars().count() => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().nth(*i as usize).unwrap(), *pos);
            }
            // string[-int]
            (Expr::StringConstant(s, pos), Expr::IntegerConstant(i, _)) if *i < 0 && i.checked_abs().map(|n| n as usize <= s.chars().count()).unwrap_or(false) => {
                // String literal indexing - get the character
                state.set_dirty();
                *expr = Expr::CharConstant(s.chars().rev().nth(i.abs() as usize - 1).unwrap(), *pos);
            }
            // var[rhs]
            (Expr::Variable(_, _, _), rhs) => optimize_expr(rhs, state, true),
            // lhs[rhs]
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, true); }
        },
        // ...[lhs][rhs]
        #[cfg(not(feature = "no_index"))]
        Expr::Index(x, _, _) => { optimize_expr(&mut x.lhs, state, false); optimize_expr(&mut x.rhs, state, _chaining); }
        // ``
        Expr::InterpolatedString(x, pos) if x.is_empty() => {
            state.set_dirty();
            *expr = Expr::StringConstant(state.engine.const_empty_string(), *pos);
        }
        // `...`
        Expr::InterpolatedString(x, _) if x.len() == 1 && matches!(x[0], Expr::StringConstant(_, _)) => {
            state.set_dirty();
            *expr = mem::take(&mut x[0]);
        }
        // `... ${ ... } ...`
        Expr::InterpolatedString(x, _) => {
            x.iter_mut().for_each(|expr| optimize_expr(expr, state, false));

            let mut n = 0;

            // Merge consecutive strings
            while n < x.len() - 1 {
                match (mem::take(&mut x[n]), mem::take(&mut x[n+1])) {
                    (Expr::StringConstant(mut s1, pos), Expr::StringConstant(s2, _)) => { s1 += s2; x[n] = Expr::StringConstant(s1, pos); x.remove(n+1); state.set_dirty(); }
                    (expr1, Expr::Unit(_))  => { x[n] = expr1; x.remove(n+1); state.set_dirty(); }
                    (Expr::Unit(_), expr2) => { x[n+1] = expr2; x.remove(n); state.set_dirty(); }
                    (expr1, Expr::StringConstant(s, _)) if s.is_empty() => { x[n] = expr1; x.remove(n+1); state.set_dirty(); }
                    (Expr::StringConstant(s, _), expr2) if s.is_empty()=> { x[n+1] = expr2; x.remove(n); state.set_dirty(); }
                    (expr1, expr2) => { x[n] = expr1; x[n+1] = expr2; n += 1; }
                }
            }

            x.shrink_to_fit();
        }
        // [ constant .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(_, _) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(expr.get_literal_value().unwrap().into(), expr.position());
        }
        // [ items .. ]
        #[cfg(not(feature = "no_index"))]
        Expr::Array(x, _) => x.iter_mut().for_each(|expr| optimize_expr(expr, state, false)),
        // #{ key:constant, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(_, _) if expr.is_constant() => {
            state.set_dirty();
            *expr = Expr::DynamicConstant(expr.get_literal_value().unwrap().into(), expr.position());
        }
        // #{ key:value, .. }
        #[cfg(not(feature = "no_object"))]
        Expr::Map(x, _) => x.0.iter_mut().for_each(|(_, expr)| optimize_expr(expr, state, false)),
        // lhs && rhs
        Expr::And(x, _) => match (&mut x.lhs, &mut x.rhs) {
            // true && rhs -> rhs
            (Expr::BoolConstant(true, _), rhs) => { state.set_dirty(); optimize_expr(rhs, state, false); *expr = mem::take(rhs); }
            // false && rhs -> false
            (Expr::BoolConstant(false, pos), _) => { state.set_dirty(); *expr = Expr::BoolConstant(false, *pos); }
            // lhs && true -> lhs
            (lhs, Expr::BoolConstant(true, _)) => { state.set_dirty(); optimize_expr(lhs, state, false); *expr = mem::take(lhs); }
            // lhs && rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, false); }
        },
        // lhs || rhs
        Expr::Or(ref mut x, _) => match (&mut x.lhs, &mut x.rhs) {
            // false || rhs -> rhs
            (Expr::BoolConstant(false, _), rhs) => { state.set_dirty(); optimize_expr(rhs, state, false); *expr = mem::take(rhs); }
            // true || rhs -> true
            (Expr::BoolConstant(true, pos), _) => { state.set_dirty(); *expr = Expr::BoolConstant(true, *pos); }
            // lhs || false
            (lhs, Expr::BoolConstant(false, _)) => { state.set_dirty(); optimize_expr(lhs, state, false); *expr = mem::take(lhs); }
            // lhs || rhs
            (lhs, rhs) => { optimize_expr(lhs, state, false); optimize_expr(rhs, state, false); }
        },

        // eval!
        Expr::FnCall(x, _) if x.name == KEYWORD_EVAL => {
            state.propagate_constants = false;
        }
        // Fn
        Expr::FnCall(x, pos)
            if !x.is_qualified() // Non-qualified
            && state.optimization_level == OptimizationLevel::Simple // simple optimizations
            && x.args.len() == 1
            && x.name == KEYWORD_FN_PTR
            && x.args[0].is_constant()
        => {
            let fn_name = match x.args[0] {
                Expr::Stack(slot, _) => x.constants[slot].clone(),
                Expr::StringConstant(ref s, _) => s.clone().into(),
                _ => Dynamic::UNIT
            };

            if let Ok(fn_ptr) = fn_name.into_immutable_string().map_err(|err| err.into()).and_then(FnPtr::try_from) {
                state.set_dirty();
                *expr = Expr::DynamicConstant(Box::new(fn_ptr.into()), *pos);
            } else {
                optimize_expr(&mut x.args[0], state, false);
            }
        }

        // Do not call some special keywords
        Expr::FnCall(x, _) if DONT_EVAL_KEYWORDS.contains(&x.name.as_ref()) => {
            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));
        }

        // Call built-in operators
        Expr::FnCall(x, pos)
                if !x.is_qualified() // Non-qualified
                && state.optimization_level == OptimizationLevel::Simple // simple optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
                //&& !is_valid_identifier(x.name.chars()) // cannot be scripted
        => {
            let arg_values = &mut x.args.iter().map(|e| match e {
                                                            Expr::Stack(slot, _) => x.constants[*slot].clone(),
                                                            _ => e.get_literal_value().unwrap()
                                                        }).collect::<StaticVec<_>>();

            let arg_types: StaticVec<_> = arg_values.iter().map(Dynamic::type_id).collect();

            match x.name.as_str() {
                KEYWORD_TYPE_OF if arg_values.len() == 1 => {
                    state.set_dirty();
                    *expr = Expr::from_dynamic(state.engine.map_type_name(arg_values[0].type_name()).into(), *pos);
                    return;
                }
                #[cfg(not(feature = "no_closure"))]
                crate::engine::KEYWORD_IS_SHARED if arg_values.len() == 1 => {
                    state.set_dirty();
                    *expr = Expr::from_dynamic(Dynamic::FALSE, *pos);
                    return;
                }
                // Overloaded operators can override built-in.
                _ if x.args.len() == 2 && !has_native_fn_override(state.engine, x.hashes.native, arg_types.as_ref()) => {
                    if let Some(result) = get_builtin_binary_op_fn(x.name.as_ref(), &arg_values[0], &arg_values[1])
                        .and_then(|f| {
                            #[cfg(not(feature = "no_function"))]
                            let lib = state.lib;
                            #[cfg(feature = "no_function")]
                            let lib = &[];

                            let context = (state.engine, x.name.as_str(), lib).into();
                            let (first, second) = arg_values.split_first_mut().unwrap();
                            (f)(context, &mut [ first, &mut second[0] ]).ok()
                        }) {
                            state.set_dirty();
                            *expr = Expr::from_dynamic(result, *pos);
                            return;
                        }
                }
                _ => ()
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));

            // Move constant arguments
            let constants = &mut x.constants;
            x.args.iter_mut().for_each(|arg| {
                if let Some(value) = arg.get_literal_value() {
                    state.set_dirty();
                    constants.push(value);
                    *arg = Expr::Stack(constants.len()-1, arg.position());
                }
            });
        }

        // Eagerly call functions
        Expr::FnCall(x, pos)
                if !x.is_qualified() // Non-qualified
                && state.optimization_level == OptimizationLevel::Full // full optimizations
                && x.args.iter().all(Expr::is_constant) // all arguments are constants
        => {
            // First search for script-defined functions (can override built-in)
            #[cfg(not(feature = "no_function"))]
            let has_script_fn = state.lib.iter().any(|&m| m.get_script_fn(&x.name, x.args.len()).is_some());
            #[cfg(feature = "no_function")]
            let has_script_fn = false;

            if !has_script_fn {
                let arg_values = &mut x.args.iter().map(|e| match e {
                                                                Expr::Stack(slot, _) => x.constants[*slot].clone(),
                                                                _ => e.get_literal_value().unwrap()
                                                            }).collect::<StaticVec<_>>();

                let result = match x.name.as_str() {
                    KEYWORD_TYPE_OF if arg_values.len() == 1 => Some(state.engine.map_type_name(arg_values[0].type_name()).into()),
                    #[cfg(not(feature = "no_closure"))]
                    crate::engine::KEYWORD_IS_SHARED if arg_values.len() == 1 => Some(Dynamic::FALSE),
                    _ => state.call_fn_with_constant_arguments(&x.name, arg_values)
                };

                if let Some(result) = result {
                    state.set_dirty();
                    *expr = Expr::from_dynamic(result, *pos);
                    return;
                }
            }

            x.args.iter_mut().for_each(|a| optimize_expr(a, state, false));
        }

        // id(args ..) -> optimize function call arguments
        Expr::FnCall(x, _) => for arg in x.args.iter_mut() {
            optimize_expr(arg, state, false);

            // Move constant arguments
            if let Some(value) = arg.get_literal_value() {
                state.set_dirty();
                x.constants.push(value);
                *arg = Expr::Stack(x.constants.len()-1, arg.position());
            }
        },

        // constant-name
        Expr::Variable(_, pos, x) if x.1.is_none() && state.find_constant(&x.2).is_some() => {
            // Replace constant with value
            *expr = Expr::from_dynamic(state.find_constant(&x.2).unwrap().clone(), *pos);
            state.set_dirty();
        }

        // Custom syntax
        Expr::Custom(x, _) => {
            if x.scope_may_be_changed {
                state.propagate_constants = false;
            }
            x.inputs.iter_mut().for_each(|expr| optimize_expr(expr, state, false));
        }

        // All other expressions - skip
        _ => (),
    }
}

/// Optimize a block of [statements][Stmt] at top level.
///
/// Constants and variables from the scope are added.
fn optimize_top_level(
    statements: StaticVec<Stmt>,
    engine: &Engine,
    scope: &Scope,
    #[cfg(not(feature = "no_function"))] lib: &[&crate::Module],
    optimization_level: OptimizationLevel,
) -> StaticVec<Stmt> {
    let mut statements = statements;

    // If optimization level is None then skip optimizing
    if optimization_level == OptimizationLevel::None {
        statements.shrink_to_fit();
        return statements;
    }

    // Set up the state
    let mut state = OptimizerState::new(
        engine,
        #[cfg(not(feature = "no_function"))]
        lib,
        optimization_level,
    );

    // Add constants and variables from the scope
    scope.iter().for_each(|(name, constant, value)| {
        if !constant {
            state.push_var(name, AccessMode::ReadWrite, None);
        } else {
            state.push_var(name, AccessMode::ReadOnly, Some(value));
        }
    });

    statements = optimize_stmt_block(statements, &mut state, true, false, true);
    statements
}

/// Optimize an [`AST`].
pub fn optimize_into_ast(
    engine: &Engine,
    scope: &Scope,
    statements: StaticVec<Stmt>,
    #[cfg(not(feature = "no_function"))] functions: StaticVec<
        crate::Shared<crate::ast::ScriptFnDef>,
    >,
    optimization_level: OptimizationLevel,
) -> AST {
    let mut statements = statements;

    #[cfg(not(feature = "no_function"))]
    let lib = {
        let mut module = crate::Module::new();

        if optimization_level != OptimizationLevel::None {
            // We only need the script library's signatures for optimization purposes
            let mut lib2 = crate::Module::new();

            functions
                .iter()
                .map(|fn_def| crate::ast::ScriptFnDef {
                    name: fn_def.name.clone(),
                    access: fn_def.access,
                    body: crate::ast::StmtBlock::NONE,
                    params: fn_def.params.clone(),
                    lib: None,
                    #[cfg(not(feature = "no_module"))]
                    global: None,
                    #[cfg(not(feature = "no_function"))]
                    #[cfg(feature = "metadata")]
                    comments: None,
                })
                .for_each(|fn_def| {
                    lib2.set_script_fn(fn_def);
                });

            let lib2 = &[&lib2];

            functions
                .into_iter()
                .map(|fn_def| {
                    let mut fn_def = crate::func::native::shared_take_or_clone(fn_def);

                    // Optimize the function body
                    let body = mem::take(&mut *fn_def.body);

                    *fn_def.body =
                        optimize_top_level(body, engine, scope, lib2, optimization_level);

                    fn_def
                })
                .for_each(|fn_def| {
                    module.set_script_fn(fn_def);
                });
        } else {
            functions.into_iter().for_each(|fn_def| {
                module.set_script_fn(fn_def);
            });
        }

        module
    };

    statements.shrink_to_fit();

    AST::new(
        match optimization_level {
            OptimizationLevel::None => statements,
            OptimizationLevel::Simple | OptimizationLevel::Full => optimize_top_level(
                statements,
                engine,
                &scope,
                #[cfg(not(feature = "no_function"))]
                &[&lib],
                optimization_level,
            ),
        },
        #[cfg(not(feature = "no_function"))]
        lib,
    )
}
