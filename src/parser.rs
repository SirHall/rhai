//! Main module defining the lexer and parser.

use crate::api::custom_syntax::{markers::*, CustomSyntax};
use crate::api::options::LanguageOptions;
use crate::ast::{
    BinaryExpr, CustomExpr, Expr, FnCallExpr, FnCallHashes, Ident, OpAssignment, ScriptFnDef, Stmt,
    StmtBlock, AST_OPTION_FLAGS::*,
};
use crate::engine::{Precedence, KEYWORD_THIS, OP_CONTAINS};
use crate::func::hashing::get_hasher;
use crate::module::Namespace;
use crate::tokenizer::{
    is_keyword_function, is_valid_function_name, is_valid_identifier, Token, TokenStream,
    TokenizerControl,
};
use crate::types::dynamic::AccessMode;
use crate::types::StringsInterner;
use crate::{
    calc_fn_hash, calc_qualified_fn_hash, calc_qualified_var_hash, Dynamic, Engine, ExclusiveRange,
    Identifier, ImmutableString, InclusiveRange, LexError, ParseError, Position, Scope, Shared,
    StaticVec, AST, INT, PERR,
};
#[cfg(feature = "no_std")]
use std::prelude::v1::*;
use std::{
    collections::BTreeMap,
    hash::{Hash, Hasher},
    num::{NonZeroU8, NonZeroUsize},
};

pub type ParseResult<T> = Result<T, ParseError>;

type FnLib = BTreeMap<u64, Shared<ScriptFnDef>>;

/// Invalid variable name that acts as a search barrier in a [`Scope`].
const SCOPE_SEARCH_BARRIER_MARKER: &str = "$BARRIER$";

/// The message: `TokenStream` never ends
const NEVER_ENDS: &str = "`TokenStream` never ends";

/// _(internals)_ A type that encapsulates the current state of the parser.
/// Exported under the `internals` feature only.
#[derive(Debug)]
pub struct ParseState<'e> {
    /// Reference to the scripting [`Engine`].
    pub engine: &'e Engine,
    /// Input stream buffer containing the next character to read.
    pub tokenizer_control: TokenizerControl,
    /// Interned strings.
    pub interned_strings: StringsInterner,
    /// Encapsulates a local stack with variable names to simulate an actual runtime scope.
    pub stack: StaticVec<(Identifier, AccessMode)>,
    /// Size of the local variables stack upon entry of the current block scope.
    pub entry_stack_len: usize,
    /// Tracks a list of external variables (variables that are not explicitly declared in the scope).
    #[cfg(not(feature = "no_closure"))]
    pub external_vars: BTreeMap<Identifier, Position>,
    /// An indicator that disables variable capturing into externals one single time
    /// up until the nearest consumed Identifier token.
    /// If set to false the next call to [`access_var`][ParseState::access_var] will not capture the variable.
    /// All consequent calls to [`access_var`][ParseState::access_var] will not be affected
    #[cfg(not(feature = "no_closure"))]
    pub allow_capture: bool,
    /// Encapsulates a local stack with imported [module][crate::Module] names.
    #[cfg(not(feature = "no_module"))]
    pub modules: StaticVec<Identifier>,
    /// Maximum levels of expression nesting.
    #[cfg(not(feature = "unchecked"))]
    pub max_expr_depth: Option<NonZeroUsize>,
    /// Maximum levels of expression nesting in functions.
    #[cfg(not(feature = "unchecked"))]
    #[cfg(not(feature = "no_function"))]
    pub max_function_expr_depth: Option<NonZeroUsize>,
}

impl<'e> ParseState<'e> {
    /// Create a new [`ParseState`].
    #[inline(always)]
    #[must_use]
    pub fn new(engine: &'e Engine, tokenizer_control: TokenizerControl) -> Self {
        Self {
            engine,
            tokenizer_control,
            #[cfg(not(feature = "unchecked"))]
            max_expr_depth: NonZeroUsize::new(engine.max_expr_depth()),
            #[cfg(not(feature = "unchecked"))]
            #[cfg(not(feature = "no_function"))]
            max_function_expr_depth: NonZeroUsize::new(engine.max_function_expr_depth()),
            #[cfg(not(feature = "no_closure"))]
            external_vars: BTreeMap::new(),
            #[cfg(not(feature = "no_closure"))]
            allow_capture: true,
            interned_strings: StringsInterner::new(),
            stack: StaticVec::new_const(),
            entry_stack_len: 0,
            #[cfg(not(feature = "no_module"))]
            modules: StaticVec::new_const(),
        }
    }

    /// Find explicitly declared variable by name in the [`ParseState`], searching in reverse order.
    ///
    /// If the variable is not present in the scope adds it to the list of external variables
    ///
    /// The return value is the offset to be deducted from `ParseState::stack::len()`,
    /// i.e. the top element of [`ParseState`]'s variables stack is offset 1.
    ///
    /// Return `None` when the variable name is not found in the `stack`.
    #[inline]
    #[must_use]
    pub fn access_var(&mut self, name: &str, pos: Position) -> Option<NonZeroUsize> {
        let mut barrier = false;
        let _pos = pos;

        let index = self
            .stack
            .iter()
            .rev()
            .enumerate()
            .find(|(_, (n, _))| {
                if n == SCOPE_SEARCH_BARRIER_MARKER {
                    // Do not go beyond the barrier
                    barrier = true;
                    false
                } else {
                    n == name
                }
            })
            .and_then(|(i, _)| NonZeroUsize::new(i + 1));

        #[cfg(not(feature = "no_closure"))]
        if self.allow_capture {
            if index.is_none() && !self.external_vars.contains_key(name) {
                self.external_vars.insert(name.into(), _pos);
            }
        } else {
            self.allow_capture = true
        }

        if barrier {
            None
        } else {
            index
        }
    }

    /// Find a module by name in the [`ParseState`], searching in reverse.
    ///
    /// Returns the offset to be deducted from `Stack::len`,
    /// i.e. the top element of the [`ParseState`] is offset 1.
    ///
    /// Returns `None` when the variable name is not found in the [`ParseState`].
    ///
    /// # Panics
    ///
    /// Panics when called under `no_module`.
    #[cfg(not(feature = "no_module"))]
    #[inline]
    #[must_use]
    pub fn find_module(&self, name: &str) -> Option<NonZeroUsize> {
        self.modules
            .iter()
            .rev()
            .enumerate()
            .find(|&(_, n)| n == name)
            .and_then(|(i, _)| NonZeroUsize::new(i + 1))
    }

    /// Get an interned identifier, creating one if it is not yet interned.
    #[inline(always)]
    #[must_use]
    pub fn get_identifier(&mut self, prefix: impl AsRef<str>, text: impl AsRef<str>) -> Identifier {
        self.interned_strings.get(prefix, text).into()
    }

    /// Get an interned string, creating one if it is not yet interned.
    #[inline(always)]
    #[allow(dead_code)]
    #[must_use]
    pub fn get_interned_string(
        &mut self,
        prefix: impl AsRef<str>,
        text: impl AsRef<str>,
    ) -> ImmutableString {
        self.interned_strings.get(prefix, text)
    }
}

/// A type that encapsulates all the settings for a particular parsing function.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct ParseSettings {
    /// Current position.
    pos: Position,
    /// Is the construct being parsed located at global level?
    is_global: bool,
    /// Is the construct being parsed located at function definition level?
    #[cfg(not(feature = "no_function"))]
    is_function_scope: bool,
    /// Is the construct being parsed located inside a closure?
    #[cfg(not(feature = "no_function"))]
    #[cfg(not(feature = "no_closure"))]
    is_closure: bool,
    /// Is the current position inside a loop?
    is_breakable: bool,
    /// Default language options.
    default_options: LanguageOptions,
    /// Is strict variables mode enabled?
    strict_var: bool,
    /// Is anonymous function allowed?
    #[cfg(not(feature = "no_function"))]
    allow_anonymous_fn: bool,
    /// Is `if`-expression allowed?
    allow_if_expr: bool,
    /// Is `switch` expression allowed?
    allow_switch_expr: bool,
    /// Is statement-expression allowed?
    allow_stmt_expr: bool,
    /// Current expression nesting level.
    level: usize,
}

impl ParseSettings {
    /// Create a new `ParseSettings` with one higher expression level.
    #[inline(always)]
    #[must_use]
    pub const fn level_up(&self) -> Self {
        Self {
            level: self.level + 1,
            ..*self
        }
    }
    /// Make sure that the current level of expression nesting is within the maximum limit.
    #[cfg(not(feature = "unchecked"))]
    #[inline]
    pub fn ensure_level_within_max_limit(&self, limit: Option<NonZeroUsize>) -> ParseResult<()> {
        if let Some(limit) = limit {
            if self.level > limit.get() {
                return Err(PERR::ExprTooDeep.into_err(self.pos));
            }
        }
        Ok(())
    }
}

impl Expr {
    /// Convert a [`Variable`][Expr::Variable] into a [`Property`][Expr::Property].
    /// All other variants are untouched.
    #[cfg(not(feature = "no_object"))]
    #[inline]
    #[must_use]
    fn into_property(self, state: &mut ParseState) -> Self {
        match self {
            Self::Variable(_, pos, x) if x.1.is_none() => {
                let ident = x.2;
                let getter = state.get_identifier(crate::engine::FN_GET, &ident);
                let hash_get = calc_fn_hash(&getter, 1);
                let setter = state.get_identifier(crate::engine::FN_SET, &ident);
                let hash_set = calc_fn_hash(&setter, 2);

                Self::Property(Box::new((
                    (getter, hash_get),
                    (setter, hash_set),
                    (state.get_interned_string("", &ident), pos),
                )))
            }
            _ => self,
        }
    }
    /// Raise an error if the expression can never yield a boolean value.
    fn ensure_bool_expr(self) -> ParseResult<Expr> {
        let type_name = match self {
            Expr::Unit(_) => "()",
            Expr::DynamicConstant(ref v, _) if !v.is::<bool>() => v.type_name(),
            Expr::IntegerConstant(_, _) => "a number",
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, _) => "a floating-point number",
            Expr::CharConstant(_, _) => "a character",
            Expr::StringConstant(_, _) => "a string",
            Expr::InterpolatedString(_, _) => "a string",
            Expr::Array(_, _) => "an array",
            Expr::Map(_, _) => "an object map",
            _ => return Ok(self),
        };

        Err(
            PERR::MismatchedType("a boolean expression".to_string(), type_name.to_string())
                .into_err(self.position()),
        )
    }
    /// Raise an error if the expression can never yield an iterable value.
    fn ensure_iterable(self) -> ParseResult<Expr> {
        let type_name = match self {
            Expr::Unit(_) => "()",
            Expr::BoolConstant(_, _) => "a boolean",
            Expr::IntegerConstant(_, _) => "a number",
            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, _) => "a floating-point number",
            Expr::CharConstant(_, _) => "a character",
            Expr::StringConstant(_, _) => "a string",
            Expr::InterpolatedString(_, _) => "a string",
            Expr::Map(_, _) => "an object map",
            _ => return Ok(self),
        };

        Err(
            PERR::MismatchedType("an iterable value".to_string(), type_name.to_string())
                .into_err(self.position()),
        )
    }
}

/// Make sure that the next expression is not a statement expression (i.e. wrapped in `{}`).
#[inline]
fn ensure_not_statement_expr(input: &mut TokenStream, type_name: impl ToString) -> ParseResult<()> {
    match input.peek().expect(NEVER_ENDS) {
        (Token::LeftBrace, pos) => Err(PERR::ExprExpected(type_name.to_string()).into_err(*pos)),
        _ => Ok(()),
    }
}

/// Make sure that the next expression is not a mis-typed assignment (i.e. `a = b` instead of `a == b`).
#[inline]
fn ensure_not_assignment(input: &mut TokenStream) -> ParseResult<()> {
    match input.peek().expect(NEVER_ENDS) {
        (Token::Equals, pos) => Err(LexError::ImproperSymbol(
            "=".to_string(),
            "Possibly a typo of '=='?".to_string(),
        )
        .into_err(*pos)),
        _ => Ok(()),
    }
}

/// Consume a particular [token][Token], checking that it is the expected one.
///
/// # Panics
///
/// Panics if the next token is not the expected one.
#[inline]
fn eat_token(input: &mut TokenStream, expected_token: Token) -> Position {
    let (t, pos) = input.next().expect(NEVER_ENDS);

    if t != expected_token {
        unreachable!(
            "{} expected but gets {} at {}",
            expected_token.syntax(),
            t.syntax(),
            pos
        );
    }
    pos
}

/// Match a particular [token][Token], consuming it if matched.
#[inline]
fn match_token(input: &mut TokenStream, token: Token) -> (bool, Position) {
    let (t, pos) = input.peek().expect(NEVER_ENDS);
    if *t == token {
        (true, eat_token(input, token))
    } else {
        (false, *pos)
    }
}

/// Parse a variable name.
fn parse_var_name(input: &mut TokenStream) -> ParseResult<(Box<str>, Position)> {
    match input.next().expect(NEVER_ENDS) {
        // Variable name
        (Token::Identifier(s), pos) => Ok((s, pos)),
        // Reserved keyword
        (Token::Reserved(s), pos) if is_valid_identifier(s.chars()) => {
            Err(PERR::Reserved(s.to_string()).into_err(pos))
        }
        // Bad identifier
        (Token::LexError(err), pos) => Err(err.into_err(pos)),
        // Not a variable name
        (_, pos) => Err(PERR::VariableExpected.into_err(pos)),
    }
}

/// Parse a symbol.
fn parse_symbol(input: &mut TokenStream) -> ParseResult<(Box<str>, Position)> {
    match input.next().expect(NEVER_ENDS) {
        // Symbol
        (token, pos) if token.is_standard_symbol() => Ok((token.literal_syntax().into(), pos)),
        // Reserved symbol
        (Token::Reserved(s), pos) if !is_valid_identifier(s.chars()) => Ok((s, pos)),
        // Bad identifier
        (Token::LexError(err), pos) => Err(err.into_err(pos)),
        // Not a symbol
        (_, pos) => Err(PERR::MissingSymbol(String::new()).into_err(pos)),
    }
}

/// Parse `(` expr `)`
fn parse_paren_expr(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // ( ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::LeftParen);

    if match_token(input, Token::RightParen).0 {
        return Ok(Expr::Unit(settings.pos));
    }

    let expr = parse_expr(input, state, lib, settings.level_up())?;

    match input.next().expect(NEVER_ENDS) {
        // ( xxx )
        (Token::RightParen, _) => Ok(expr),
        // ( <error>
        (Token::LexError(err), pos) => Err(err.into_err(pos)),
        // ( xxx ???
        (_, pos) => Err(PERR::MissingToken(
            Token::RightParen.into(),
            "for a matching ( in this expression".into(),
        )
        .into_err(pos)),
    }
}

/// Parse a function call.
fn parse_fn_call(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    id: Identifier,
    capture_parent_scope: bool,
    namespace: Option<Namespace>,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let (token, token_pos) = input.peek().expect(NEVER_ENDS);

    let mut namespace = namespace;
    let mut args = StaticVec::new_const();

    match token {
        // id( <EOF>
        Token::EOF => {
            return Err(PERR::MissingToken(
                Token::RightParen.into(),
                format!("to close the arguments list of this function call '{}'", id),
            )
            .into_err(*token_pos))
        }
        // id( <error>
        Token::LexError(err) => return Err(err.clone().into_err(*token_pos)),
        // id()
        Token::RightParen => {
            eat_token(input, Token::RightParen);

            let hash = if let Some(modules) = namespace.as_mut() {
                #[cfg(not(feature = "no_module"))]
                {
                    let index = state.find_module(&modules[0].name);

                    #[cfg(not(feature = "no_function"))]
                    let relax = settings.is_function_scope;
                    #[cfg(feature = "no_function")]
                    let relax = false;

                    if !relax && settings.strict_var && index.is_none() {
                        return Err(PERR::ModuleUndefined(modules[0].name.to_string())
                            .into_err(modules[0].pos));
                    }

                    modules.set_index(index);
                }

                calc_qualified_fn_hash(modules.iter().map(|m| m.name.as_str()), &id, 0)
            } else {
                calc_fn_hash(&id, 0)
            };

            let hashes = if is_valid_function_name(&id) {
                hash.into()
            } else {
                FnCallHashes::from_native(hash)
            };

            args.shrink_to_fit();

            return Ok(FnCallExpr {
                name: state.get_identifier("", id),
                capture_parent_scope,
                namespace,
                hashes,
                args,
                ..Default::default()
            }
            .into_fn_call_expr(settings.pos));
        }
        // id...
        _ => (),
    }

    let settings = settings.level_up();

    loop {
        match input.peek().expect(NEVER_ENDS) {
            // id(...args, ) - handle trailing comma
            (Token::RightParen, _) => (),
            _ => args.push(parse_expr(input, state, lib, settings)?),
        }

        match input.peek().expect(NEVER_ENDS) {
            // id(...args)
            (Token::RightParen, _) => {
                eat_token(input, Token::RightParen);

                let hash = if let Some(modules) = namespace.as_mut() {
                    #[cfg(not(feature = "no_module"))]
                    {
                        let index = state.find_module(&modules[0].name);

                        #[cfg(not(feature = "no_function"))]
                        let relax = settings.is_function_scope;
                        #[cfg(feature = "no_function")]
                        let relax = false;

                        if !relax && settings.strict_var && index.is_none() {
                            return Err(PERR::ModuleUndefined(modules[0].name.to_string())
                                .into_err(modules[0].pos));
                        }

                        modules.set_index(index);
                    }

                    calc_qualified_fn_hash(modules.iter().map(|m| m.name.as_str()), &id, args.len())
                } else {
                    calc_fn_hash(&id, args.len())
                };

                let hashes = if is_valid_function_name(&id) {
                    hash.into()
                } else {
                    FnCallHashes::from_native(hash)
                };

                args.shrink_to_fit();

                return Ok(FnCallExpr {
                    name: state.get_identifier("", id),
                    capture_parent_scope,
                    namespace,
                    hashes,
                    args,
                    ..Default::default()
                }
                .into_fn_call_expr(settings.pos));
            }
            // id(...args,
            (Token::Comma, _) => {
                eat_token(input, Token::Comma);
            }
            // id(...args <EOF>
            (Token::EOF, pos) => {
                return Err(PERR::MissingToken(
                    Token::RightParen.into(),
                    format!("to close the arguments list of this function call '{}'", id),
                )
                .into_err(*pos))
            }
            // id(...args <error>
            (Token::LexError(err), pos) => return Err(err.clone().into_err(*pos)),
            // id(...args ???
            (_, pos) => {
                return Err(PERR::MissingToken(
                    Token::Comma.into(),
                    format!("to separate the arguments to function call '{}'", id),
                )
                .into_err(*pos))
            }
        }
    }
}

/// Parse an indexing chain.
/// Indexing binds to the right, so this call parses all possible levels of indexing following in the input.
#[cfg(not(feature = "no_index"))]
fn parse_index_chain(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    lhs: Expr,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;

    let idx_expr = parse_expr(input, state, lib, settings.level_up())?;

    // Check type of indexing - must be integer or string
    match idx_expr {
        Expr::IntegerConstant(_, pos) => match lhs {
            Expr::IntegerConstant(_, _)
            | Expr::Array(_, _)
            | Expr::StringConstant(_, _)
            | Expr::InterpolatedString(_, _) => (),

            Expr::Map(_, _) => {
                return Err(PERR::MalformedIndexExpr(
                    "Object map access expects string index, not a number".into(),
                )
                .into_err(pos))
            }

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, _) => {
                return Err(PERR::MalformedIndexExpr(
                    "Only arrays, object maps and strings can be indexed".into(),
                )
                .into_err(lhs.position()))
            }

            Expr::CharConstant(_, _)
            | Expr::And(_, _)
            | Expr::Or(_, _)
            | Expr::BoolConstant(_, _)
            | Expr::Unit(_) => {
                return Err(PERR::MalformedIndexExpr(
                    "Only arrays, object maps and strings can be indexed".into(),
                )
                .into_err(lhs.position()))
            }

            _ => (),
        },

        // lhs[string]
        Expr::StringConstant(_, _) | Expr::InterpolatedString(_, _) => match lhs {
            Expr::Map(_, _) => (),

            Expr::Array(_, _) | Expr::StringConstant(_, _) | Expr::InterpolatedString(_, _) => {
                return Err(PERR::MalformedIndexExpr(
                    "Array or string expects numeric index, not a string".into(),
                )
                .into_err(idx_expr.position()))
            }

            #[cfg(not(feature = "no_float"))]
            Expr::FloatConstant(_, _) => {
                return Err(PERR::MalformedIndexExpr(
                    "Only arrays, object maps and strings can be indexed".into(),
                )
                .into_err(lhs.position()))
            }

            Expr::CharConstant(_, _)
            | Expr::And(_, _)
            | Expr::Or(_, _)
            | Expr::BoolConstant(_, _)
            | Expr::Unit(_) => {
                return Err(PERR::MalformedIndexExpr(
                    "Only arrays, object maps and strings can be indexed".into(),
                )
                .into_err(lhs.position()))
            }

            _ => (),
        },

        // lhs[float]
        #[cfg(not(feature = "no_float"))]
        x @ Expr::FloatConstant(_, _) => {
            return Err(PERR::MalformedIndexExpr(
                "Array access expects integer index, not a float".into(),
            )
            .into_err(x.position()))
        }
        // lhs[char]
        x @ Expr::CharConstant(_, _) => {
            return Err(PERR::MalformedIndexExpr(
                "Array access expects integer index, not a character".into(),
            )
            .into_err(x.position()))
        }
        // lhs[()]
        x @ Expr::Unit(_) => {
            return Err(PERR::MalformedIndexExpr(
                "Array access expects integer index, not ()".into(),
            )
            .into_err(x.position()))
        }
        // lhs[??? && ???], lhs[??? || ???]
        x @ Expr::And(_, _) | x @ Expr::Or(_, _) => {
            return Err(PERR::MalformedIndexExpr(
                "Array access expects integer index, not a boolean".into(),
            )
            .into_err(x.position()))
        }
        // lhs[true], lhs[false]
        x @ Expr::BoolConstant(_, _) => {
            return Err(PERR::MalformedIndexExpr(
                "Array access expects integer index, not a boolean".into(),
            )
            .into_err(x.position()))
        }
        // All other expressions
        _ => (),
    }

    // Check if there is a closing bracket
    match input.peek().expect(NEVER_ENDS) {
        (Token::RightBracket, _) => {
            eat_token(input, Token::RightBracket);

            // Any more indexing following?
            match input.peek().expect(NEVER_ENDS) {
                // If another indexing level, right-bind it
                (Token::LeftBracket, _) => {
                    let prev_pos = settings.pos;
                    settings.pos = eat_token(input, Token::LeftBracket);
                    // Recursively parse the indexing chain, right-binding each
                    let idx_expr =
                        parse_index_chain(input, state, lib, idx_expr, settings.level_up())?;
                    // Indexing binds to right
                    Ok(Expr::Index(
                        BinaryExpr { lhs, rhs: idx_expr }.into(),
                        false,
                        prev_pos,
                    ))
                }
                // Otherwise terminate the indexing chain
                _ => Ok(Expr::Index(
                    BinaryExpr { lhs, rhs: idx_expr }.into(),
                    true,
                    settings.pos,
                )),
            }
        }
        (Token::LexError(err), pos) => Err(err.clone().into_err(*pos)),
        (_, pos) => Err(PERR::MissingToken(
            Token::RightBracket.into(),
            "for a matching [ in this index expression".into(),
        )
        .into_err(*pos)),
    }
}

/// Parse an array literal.
#[cfg(not(feature = "no_index"))]
fn parse_array_literal(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // [ ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::LeftBracket);

    let mut arr = StaticVec::new_const();

    loop {
        const MISSING_RBRACKET: &str = "to end this array literal";

        #[cfg(not(feature = "unchecked"))]
        if state.engine.max_array_size() > 0 && arr.len() >= state.engine.max_array_size() {
            return Err(PERR::LiteralTooLarge(
                "Size of array literal".to_string(),
                state.engine.max_array_size(),
            )
            .into_err(input.peek().expect(NEVER_ENDS).1));
        }

        match input.peek().expect(NEVER_ENDS) {
            (Token::RightBracket, _) => {
                eat_token(input, Token::RightBracket);
                break;
            }
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBracket.into(), MISSING_RBRACKET.into())
                        .into_err(*pos),
                )
            }
            _ => {
                let expr = parse_expr(input, state, lib, settings.level_up())?;
                arr.push(expr);
            }
        }

        match input.peek().expect(NEVER_ENDS) {
            (Token::Comma, _) => {
                eat_token(input, Token::Comma);
            }
            (Token::RightBracket, _) => (),
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBracket.into(), MISSING_RBRACKET.into())
                        .into_err(*pos),
                )
            }
            (Token::LexError(err), pos) => return Err(err.clone().into_err(*pos)),
            (_, pos) => {
                return Err(PERR::MissingToken(
                    Token::Comma.into(),
                    "to separate the items of this array literal".into(),
                )
                .into_err(*pos))
            }
        };
    }

    arr.shrink_to_fit();

    Ok(Expr::Array(arr.into(), settings.pos))
}

/// Parse a map literal.
#[cfg(not(feature = "no_object"))]
fn parse_map_literal(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // #{ ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::MapStart);

    let mut map = StaticVec::<(Ident, Expr)>::new();
    let mut template = BTreeMap::<Identifier, crate::Dynamic>::new();

    loop {
        const MISSING_RBRACE: &str = "to end this object map literal";

        match input.peek().expect(NEVER_ENDS) {
            (Token::RightBrace, _) => {
                eat_token(input, Token::RightBrace);
                break;
            }
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBrace.into(), MISSING_RBRACE.into())
                        .into_err(*pos),
                )
            }
            _ => (),
        }

        let (name, pos) = match input.next().expect(NEVER_ENDS) {
            (Token::Identifier(s), pos) | (Token::StringConstant(s), pos) => {
                if map.iter().any(|(p, _)| p.name == &*s) {
                    return Err(PERR::DuplicatedProperty(s.to_string()).into_err(pos));
                }
                (s, pos)
            }
            (Token::InterpolatedString(_), pos) => return Err(PERR::PropertyExpected.into_err(pos)),
            (Token::Reserved(s), pos) if is_valid_identifier(s.chars()) => {
                return Err(PERR::Reserved(s.to_string()).into_err(pos));
            }
            (Token::LexError(err), pos) => return Err(err.into_err(pos)),
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBrace.into(), MISSING_RBRACE.into())
                        .into_err(pos),
                );
            }
            (_, pos) if map.is_empty() => {
                return Err(
                    PERR::MissingToken(Token::RightBrace.into(), MISSING_RBRACE.into())
                        .into_err(pos),
                );
            }
            (_, pos) => return Err(PERR::PropertyExpected.into_err(pos)),
        };

        match input.next().expect(NEVER_ENDS) {
            (Token::Colon, _) => (),
            (Token::LexError(err), pos) => return Err(err.into_err(pos)),
            (_, pos) => {
                return Err(PERR::MissingToken(
                    Token::Colon.into(),
                    format!(
                        "to follow the property '{}' in this object map literal",
                        name
                    ),
                )
                .into_err(pos))
            }
        };

        #[cfg(not(feature = "unchecked"))]
        if state.engine.max_map_size() > 0 && map.len() >= state.engine.max_map_size() {
            return Err(PERR::LiteralTooLarge(
                "Number of properties in object map literal".to_string(),
                state.engine.max_map_size(),
            )
            .into_err(input.peek().expect(NEVER_ENDS).1));
        }

        let expr = parse_expr(input, state, lib, settings.level_up())?;
        let name = state.get_identifier("", name);
        template.insert(name.clone(), crate::Dynamic::UNIT);
        map.push((Ident { name, pos }, expr));

        match input.peek().expect(NEVER_ENDS) {
            (Token::Comma, _) => {
                eat_token(input, Token::Comma);
            }
            (Token::RightBrace, _) => (),
            (Token::Identifier(_), pos) => {
                return Err(PERR::MissingToken(
                    Token::Comma.into(),
                    "to separate the items of this object map literal".into(),
                )
                .into_err(*pos))
            }
            (Token::LexError(err), pos) => return Err(err.clone().into_err(*pos)),
            (_, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBrace.into(), MISSING_RBRACE.into())
                        .into_err(*pos),
                )
            }
        }
    }

    map.shrink_to_fit();

    Ok(Expr::Map((map, template).into(), settings.pos))
}

/// Parse a switch expression.
fn parse_switch(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // switch ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::Switch);

    let item = parse_expr(input, state, lib, settings.level_up())?;

    match input.next().expect(NEVER_ENDS) {
        (Token::LeftBrace, _) => (),
        (Token::LexError(err), pos) => return Err(err.into_err(pos)),
        (_, pos) => {
            return Err(PERR::MissingToken(
                Token::LeftBrace.into(),
                "to start a switch block".into(),
            )
            .into_err(pos))
        }
    }

    let mut table = BTreeMap::<u64, Box<(Option<Expr>, StmtBlock)>>::new();
    let mut ranges = StaticVec::<(INT, INT, bool, Option<Expr>, StmtBlock)>::new();
    let mut def_pos = Position::NONE;
    let mut def_stmt = None;

    loop {
        const MISSING_RBRACE: &str = "to end this switch block";

        let (expr, condition) = match input.peek().expect(NEVER_ENDS) {
            (Token::RightBrace, _) => {
                eat_token(input, Token::RightBrace);
                break;
            }
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightBrace.into(), MISSING_RBRACE.into())
                        .into_err(*pos),
                )
            }
            (Token::Underscore, pos) if def_stmt.is_none() => {
                def_pos = *pos;
                eat_token(input, Token::Underscore);

                let (if_clause, if_pos) = match_token(input, Token::If);

                if if_clause {
                    return Err(PERR::WrongSwitchCaseCondition.into_err(if_pos));
                }

                (None, None)
            }
            (Token::Underscore, pos) => return Err(PERR::DuplicatedSwitchCase.into_err(*pos)),

            _ if def_stmt.is_some() => return Err(PERR::WrongSwitchDefaultCase.into_err(def_pos)),

            _ => {
                let case_expr = Some(parse_expr(input, state, lib, settings.level_up())?);

                let condition = if match_token(input, Token::If).0 {
                    Some(parse_expr(input, state, lib, settings.level_up())?)
                } else {
                    None
                };
                (case_expr, condition)
            }
        };

        let (hash, range) = if let Some(expr) = expr {
            let value = expr.get_literal_value().ok_or_else(|| {
                PERR::ExprExpected("a literal".to_string()).into_err(expr.position())
            })?;

            let guard = value.read_lock::<ExclusiveRange>();

            if let Some(range) = guard {
                (None, Some((range.start, range.end, false)))
            } else if let Some(range) = value.read_lock::<InclusiveRange>() {
                (None, Some((*range.start(), *range.end(), true)))
            } else if value.is::<INT>() && !ranges.is_empty() {
                return Err(PERR::WrongSwitchIntegerCase.into_err(expr.position()));
            } else {
                let hasher = &mut get_hasher();
                value.hash(hasher);
                let hash = hasher.finish();

                if table.contains_key(&hash) {
                    return Err(PERR::DuplicatedSwitchCase.into_err(expr.position()));
                }
                (Some(hash), None)
            }
        } else {
            (None, None)
        };

        match input.next().expect(NEVER_ENDS) {
            (Token::DoubleArrow, _) => (),
            (Token::LexError(err), pos) => return Err(err.into_err(pos)),
            (_, pos) => {
                return Err(PERR::MissingToken(
                    Token::DoubleArrow.into(),
                    "in this switch case".to_string(),
                )
                .into_err(pos))
            }
        };

        let stmt = parse_stmt(input, state, lib, settings.level_up())?;

        let need_comma = !stmt.is_self_terminated();

        def_stmt = match (hash, range) {
            (None, Some(range)) => {
                let is_empty = if range.2 {
                    (range.0..=range.1).is_empty()
                } else {
                    (range.0..range.1).is_empty()
                };

                if !is_empty {
                    match (range.1.checked_sub(range.0), range.2) {
                        // Unroll single range
                        (Some(1), false) | (Some(0), true) => {
                            let value = Dynamic::from_int(range.0);
                            let hasher = &mut get_hasher();
                            value.hash(hasher);
                            let hash = hasher.finish();

                            table
                                .entry(hash)
                                .or_insert_with(|| (condition.clone(), stmt.into()).into());
                        }
                        // Other range
                        _ => ranges.push((range.0, range.1, range.2, condition, stmt.into())),
                    }
                }
                None
            }
            (Some(hash), None) => {
                table.insert(hash, (condition, stmt.into()).into());
                None
            }
            (None, None) => Some(stmt.into()),
            _ => unreachable!("both hash and range in switch statement case"),
        };

        match input.peek().expect(NEVER_ENDS) {
            (Token::Comma, _) => {
                eat_token(input, Token::Comma);
            }
            (Token::RightBrace, _) => (),
            (Token::EOF, pos) => {
                return Err(
                    PERR::MissingToken(Token::RightParen.into(), MISSING_RBRACE.into())
                        .into_err(*pos),
                )
            }
            (Token::LexError(err), pos) => return Err(err.clone().into_err(*pos)),
            (_, pos) if need_comma => {
                return Err(PERR::MissingToken(
                    Token::Comma.into(),
                    "to separate the items in this switch block".into(),
                )
                .into_err(*pos))
            }
            (_, _) => (),
        }
    }

    let def_stmt_block = def_stmt.unwrap_or_else(|| Stmt::Noop(Position::NONE).into());

    Ok(Stmt::Switch(
        item,
        (table, def_stmt_block, ranges).into(),
        settings.pos,
    ))
}

/// Parse a primary expression.
fn parse_primary(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let (token, token_pos) = input.peek().expect(NEVER_ENDS);

    let mut settings = settings;
    settings.pos = *token_pos;

    let root_expr = match token {
        Token::EOF => return Err(PERR::UnexpectedEOF.into_err(settings.pos)),

        Token::IntegerConstant(_)
        | Token::CharConstant(_)
        | Token::StringConstant(_)
        | Token::True
        | Token::False => match input.next().expect(NEVER_ENDS).0 {
            Token::IntegerConstant(x) => Expr::IntegerConstant(x, settings.pos),
            Token::CharConstant(c) => Expr::CharConstant(c, settings.pos),
            Token::StringConstant(s) => {
                Expr::StringConstant(state.get_interned_string("", s), settings.pos)
            }
            Token::True => Expr::BoolConstant(true, settings.pos),
            Token::False => Expr::BoolConstant(false, settings.pos),
            token => unreachable!("token is {:?}", token),
        },
        #[cfg(not(feature = "no_float"))]
        Token::FloatConstant(x) => {
            let x = *x;
            input.next().expect(NEVER_ENDS);
            Expr::FloatConstant(x, settings.pos)
        }
        #[cfg(feature = "decimal")]
        Token::DecimalConstant(x) => {
            let x = (*x).into();
            input.next().expect(NEVER_ENDS);
            Expr::DynamicConstant(Box::new(x), settings.pos)
        }

        // { - block statement as expression
        Token::LeftBrace if settings.allow_stmt_expr => {
            match parse_block(input, state, lib, settings.level_up())? {
                block @ Stmt::Block(_, _) => Expr::Stmt(Box::new(block.into())),
                stmt => unreachable!("Stmt::Block expected but gets {:?}", stmt),
            }
        }
        // ( - grouped expression
        Token::LeftParen => parse_paren_expr(input, state, lib, settings.level_up())?,

        // If statement is allowed to act as expressions
        Token::If if settings.allow_if_expr => Expr::Stmt(Box::new(
            parse_if(input, state, lib, settings.level_up())?.into(),
        )),
        // Switch statement is allowed to act as expressions
        Token::Switch if settings.allow_switch_expr => Expr::Stmt(Box::new(
            parse_switch(input, state, lib, settings.level_up())?.into(),
        )),

        // | ...
        #[cfg(not(feature = "no_function"))]
        Token::Pipe | Token::Or if settings.allow_anonymous_fn => {
            let mut new_state = ParseState::new(state.engine, state.tokenizer_control.clone());

            #[cfg(not(feature = "unchecked"))]
            {
                new_state.max_expr_depth = new_state.max_function_expr_depth;
            }

            let new_settings = ParseSettings {
                allow_if_expr: settings.default_options.allow_if_expr,
                allow_switch_expr: settings.default_options.allow_switch_expr,
                allow_stmt_expr: settings.default_options.allow_stmt_expr,
                allow_anonymous_fn: settings.default_options.allow_anonymous_fn,
                strict_var: if cfg!(feature = "no_closure") {
                    settings.strict_var
                } else {
                    // A capturing closure can access variables not defined locally
                    false
                },
                is_global: false,
                is_function_scope: true,
                #[cfg(not(feature = "no_closure"))]
                is_closure: true,
                is_breakable: false,
                level: 0,
                ..settings
            };

            let (expr, func) = parse_anon_fn(input, &mut new_state, lib, new_settings)?;

            #[cfg(not(feature = "no_closure"))]
            new_state.external_vars.iter().try_for_each(
                |(captured_var, &pos)| -> ParseResult<_> {
                    let index = state.access_var(captured_var, pos);

                    if !settings.is_closure && settings.strict_var && index.is_none() {
                        // If the parent scope is not inside another capturing closure
                        Err(PERR::VariableUndefined(captured_var.to_string()).into_err(pos))
                    } else {
                        Ok(())
                    }
                },
            )?;

            let hash_script = calc_fn_hash(&func.name, func.params.len());
            lib.insert(hash_script, func.into());

            expr
        }

        // Interpolated string
        Token::InterpolatedString(_) => {
            let mut segments = StaticVec::<Expr>::new();

            match input.next().expect(NEVER_ENDS) {
                (Token::InterpolatedString(s), pos) => {
                    segments.push(Expr::StringConstant(s.into(), pos));
                }
                token => unreachable!("Token::InterpolatedString expected but gets {:?}", token),
            }

            loop {
                let expr = match parse_block(input, state, lib, settings.level_up())? {
                    block @ Stmt::Block(_, _) => Expr::Stmt(Box::new(block.into())),
                    stmt => unreachable!("Stmt::Block expected but gets {:?}", stmt),
                };
                segments.push(expr);

                // Make sure to parse the following as text
                let mut control = state.tokenizer_control.get();
                control.is_within_text = true;
                state.tokenizer_control.set(control);

                match input.next().expect(NEVER_ENDS) {
                    (Token::StringConstant(s), pos) => {
                        if !s.is_empty() {
                            segments.push(Expr::StringConstant(s.into(), pos));
                        }
                        // End the interpolated string if it is terminated by a back-tick.
                        break;
                    }
                    (Token::InterpolatedString(s), pos) => {
                        if !s.is_empty() {
                            segments.push(Expr::StringConstant(s.into(), pos));
                        }
                    }
                    (Token::LexError(err @ LexError::UnterminatedString), pos) => {
                        return Err(err.into_err(pos))
                    }
                    (token, _) => unreachable!(
                        "string within an interpolated string literal expected but gets {:?}",
                        token
                    ),
                }
            }

            segments.shrink_to_fit();
            Expr::InterpolatedString(segments.into(), settings.pos)
        }

        // Array literal
        #[cfg(not(feature = "no_index"))]
        Token::LeftBracket => parse_array_literal(input, state, lib, settings.level_up())?,

        // Map literal
        #[cfg(not(feature = "no_object"))]
        Token::MapStart => parse_map_literal(input, state, lib, settings.level_up())?,

        // Custom syntax.
        Token::Custom(key) | Token::Reserved(key) | Token::Identifier(key)
            if state.engine.custom_syntax.contains_key(&**key) =>
        {
            let (key, syntax) = state.engine.custom_syntax.get_key_value(&**key).unwrap();
            let (_, pos) = input.next().expect(NEVER_ENDS);
            let settings2 = settings.level_up();
            parse_custom_syntax(input, state, lib, settings2, key, syntax, pos)?
        }

        // Identifier
        Token::Identifier(_) => {
            let s = match input.next().expect(NEVER_ENDS) {
                (Token::Identifier(s), _) => s,
                token => unreachable!("Token::Identifier expected but gets {:?}", token),
            };

            match input.peek().expect(NEVER_ENDS).0 {
                // Function call
                Token::LeftParen | Token::Bang => {
                    #[cfg(not(feature = "no_closure"))]
                    {
                        // Once the identifier consumed we must enable next variables capturing
                        state.allow_capture = true;
                    }
                    Expr::Variable(
                        None,
                        settings.pos,
                        (None, None, state.get_identifier("", s)).into(),
                    )
                }
                // Namespace qualification
                #[cfg(not(feature = "no_module"))]
                Token::DoubleColon => {
                    #[cfg(not(feature = "no_closure"))]
                    {
                        // Once the identifier consumed we must enable next variables capturing
                        state.allow_capture = true;
                    }
                    Expr::Variable(
                        None,
                        settings.pos,
                        (None, None, state.get_identifier("", s)).into(),
                    )
                }
                // Normal variable access
                _ => {
                    let index = state.access_var(&s, settings.pos);

                    if settings.strict_var && index.is_none() {
                        return Err(PERR::VariableUndefined(s.to_string()).into_err(settings.pos));
                    }

                    let short_index = index.and_then(|x| {
                        if x.get() <= u8::MAX as usize {
                            NonZeroU8::new(x.get() as u8)
                        } else {
                            None
                        }
                    });
                    Expr::Variable(
                        short_index,
                        settings.pos,
                        (index, None, state.get_identifier("", s)).into(),
                    )
                }
            }
        }

        // Reserved keyword or symbol
        Token::Reserved(_) => {
            let s = match input.next().expect(NEVER_ENDS) {
                (Token::Reserved(s), _) => s,
                token => unreachable!("Token::Reserved expected but gets {:?}", token),
            };

            match input.peek().expect(NEVER_ENDS).0 {
                // Function call is allowed to have reserved keyword
                Token::LeftParen | Token::Bang if is_keyword_function(&s) => Expr::Variable(
                    None,
                    settings.pos,
                    (None, None, state.get_identifier("", s)).into(),
                ),
                // Access to `this` as a variable is OK within a function scope
                #[cfg(not(feature = "no_function"))]
                _ if &*s == KEYWORD_THIS && settings.is_function_scope => Expr::Variable(
                    None,
                    settings.pos,
                    (None, None, state.get_identifier("", s)).into(),
                ),
                // Cannot access to `this` as a variable not in a function scope
                _ if &*s == KEYWORD_THIS => {
                    let msg = format!("'{}' can only be used in functions", s);
                    return Err(LexError::ImproperSymbol(s.to_string(), msg).into_err(settings.pos));
                }
                _ => return Err(PERR::Reserved(s.to_string()).into_err(settings.pos)),
            }
        }

        Token::LexError(_) => match input.next().expect(NEVER_ENDS) {
            (Token::LexError(err), _) => return Err(err.into_err(settings.pos)),
            token => unreachable!("Token::LexError expected but gets {:?}", token),
        },

        _ => {
            return Err(LexError::UnexpectedInput(token.syntax().to_string()).into_err(settings.pos))
        }
    };

    parse_postfix(input, state, lib, root_expr, settings)
}

/// Tail processing of all possible postfix operators of a primary expression.
fn parse_postfix(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    mut lhs: Expr,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    let mut settings = settings;

    // Tail processing all possible postfix operators
    loop {
        let (tail_token, _) = input.peek().expect(NEVER_ENDS);

        if !lhs.is_valid_postfix(tail_token) {
            break;
        }

        let (tail_token, tail_pos) = input.next().expect(NEVER_ENDS);
        settings.pos = tail_pos;

        lhs = match (lhs, tail_token) {
            // Qualified function call with !
            (Expr::Variable(_, _, x), Token::Bang) if x.1.is_some() => {
                return if !match_token(input, Token::LeftParen).0 {
                    Err(LexError::UnexpectedInput(Token::Bang.syntax().to_string())
                        .into_err(tail_pos))
                } else {
                    Err(LexError::ImproperSymbol(
                        "!".to_string(),
                        "'!' cannot be used to call module functions".to_string(),
                    )
                    .into_err(tail_pos))
                };
            }
            // Function call with !
            (Expr::Variable(_, pos, x), Token::Bang) => {
                match match_token(input, Token::LeftParen) {
                    (false, pos) => {
                        return Err(PERR::MissingToken(
                            Token::LeftParen.syntax().into(),
                            "to start arguments list of function call".into(),
                        )
                        .into_err(pos))
                    }
                    _ => (),
                }

                let (_, namespace, name) = *x;
                settings.pos = pos;
                let ns = namespace.map(|(ns, _)| ns);
                parse_fn_call(input, state, lib, name, true, ns, settings.level_up())?
            }
            // Function call
            (Expr::Variable(_, pos, x), Token::LeftParen) => {
                let (_, namespace, name) = *x;
                let ns = namespace.map(|(ns, _)| ns);
                settings.pos = pos;
                parse_fn_call(input, state, lib, name, false, ns, settings.level_up())?
            }
            // module access
            #[cfg(not(feature = "no_module"))]
            (Expr::Variable(_, pos, x), Token::DoubleColon) => {
                let (id2, pos2) = parse_var_name(input)?;
                let (_, mut namespace, name) = *x;
                let var_name_def = Ident { name, pos };

                if let Some((ref mut namespace, _)) = namespace {
                    namespace.push(var_name_def);
                } else {
                    let mut ns = Namespace::new();
                    ns.push(var_name_def);
                    namespace = Some((ns, 42));
                }

                Expr::Variable(
                    None,
                    pos2,
                    (None, namespace, state.get_identifier("", id2)).into(),
                )
            }
            // Indexing
            #[cfg(not(feature = "no_index"))]
            (expr, Token::LeftBracket) => {
                parse_index_chain(input, state, lib, expr, settings.level_up())?
            }
            // Property access
            #[cfg(not(feature = "no_object"))]
            (expr, Token::Period) => {
                // Expression after dot must start with an identifier
                match input.peek().expect(NEVER_ENDS) {
                    (Token::Identifier(_), _) => {
                        #[cfg(not(feature = "no_closure"))]
                        {
                            // Prevents capturing of the object properties as vars: xxx.<var>
                            state.allow_capture = false;
                        }
                    }
                    (Token::Reserved(s), _) if is_keyword_function(s) => (),
                    (_, pos) => return Err(PERR::PropertyExpected.into_err(*pos)),
                }

                let rhs = parse_primary(input, state, lib, settings.level_up())?;
                make_dot_expr(state, expr, false, rhs, tail_pos)?
            }
            // Unknown postfix operator
            (expr, token) => unreachable!(
                "unknown postfix operator '{}' for {:?}",
                token.syntax(),
                expr
            ),
        }
    }

    // Cache the hash key for namespace-qualified variables
    let namespaced_variable = match lhs {
        Expr::Variable(_, _, ref mut x) if x.1.is_some() => Some(x.as_mut()),
        Expr::Index(ref mut x, _, _) | Expr::Dot(ref mut x, _, _) => match x.lhs {
            Expr::Variable(_, _, ref mut x) if x.1.is_some() => Some(x.as_mut()),
            _ => None,
        },
        _ => None,
    };

    if let Some((_, Some((namespace, hash)), name)) = namespaced_variable {
        *hash = calc_qualified_var_hash(namespace.iter().map(|v| v.name.as_str()), name);

        #[cfg(not(feature = "no_module"))]
        {
            let index = state.find_module(&namespace[0].name);

            #[cfg(not(feature = "no_function"))]
            let relax = settings.is_function_scope;
            #[cfg(feature = "no_function")]
            let relax = false;

            if !relax && settings.strict_var && index.is_none() {
                return Err(
                    PERR::ModuleUndefined(namespace[0].name.to_string()).into_err(namespace[0].pos)
                );
            }

            namespace.set_index(index);
        }
    }

    // Make sure identifiers are valid
    Ok(lhs)
}

/// Parse a potential unary operator.
fn parse_unary(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let (token, token_pos) = input.peek().expect(NEVER_ENDS);

    let mut settings = settings;
    settings.pos = *token_pos;

    match token {
        // -expr
        Token::Minus | Token::UnaryMinus => {
            let token = token.clone();
            let pos = eat_token(input, token);

            match parse_unary(input, state, lib, settings.level_up())? {
                // Negative integer
                Expr::IntegerConstant(num, _) => num
                    .checked_neg()
                    .map(|i| Expr::IntegerConstant(i, pos))
                    .or_else(|| {
                        #[cfg(not(feature = "no_float"))]
                        return Some(Expr::FloatConstant((-(num as crate::FLOAT)).into(), pos));
                        #[cfg(feature = "no_float")]
                        return None;
                    })
                    .ok_or_else(|| LexError::MalformedNumber(format!("-{}", num)).into_err(pos)),

                // Negative float
                #[cfg(not(feature = "no_float"))]
                Expr::FloatConstant(x, _) => Ok(Expr::FloatConstant((-(*x)).into(), pos)),

                // Call negative function
                expr => {
                    let mut args = StaticVec::new_const();
                    args.push(expr);
                    args.shrink_to_fit();

                    Ok(FnCallExpr {
                        name: state.get_identifier("", "-"),
                        hashes: FnCallHashes::from_native(calc_fn_hash("-", 1)),
                        args,
                        ..Default::default()
                    }
                    .into_fn_call_expr(pos))
                }
            }
        }
        // +expr
        Token::Plus | Token::UnaryPlus => {
            let token = token.clone();
            let pos = eat_token(input, token);

            match parse_unary(input, state, lib, settings.level_up())? {
                expr @ Expr::IntegerConstant(_, _) => Ok(expr),
                #[cfg(not(feature = "no_float"))]
                expr @ Expr::FloatConstant(_, _) => Ok(expr),

                // Call plus function
                expr => {
                    let mut args = StaticVec::new_const();
                    args.push(expr);
                    args.shrink_to_fit();

                    Ok(FnCallExpr {
                        name: state.get_identifier("", "+"),
                        hashes: FnCallHashes::from_native(calc_fn_hash("+", 1)),
                        args,
                        ..Default::default()
                    }
                    .into_fn_call_expr(pos))
                }
            }
        }
        // !expr
        Token::Bang => {
            let pos = eat_token(input, Token::Bang);
            let mut args = StaticVec::new_const();
            args.push(parse_unary(input, state, lib, settings.level_up())?);
            args.shrink_to_fit();

            Ok(FnCallExpr {
                name: state.get_identifier("", "!"),
                hashes: FnCallHashes::from_native(calc_fn_hash("!", 1)),
                args,
                ..Default::default()
            }
            .into_fn_call_expr(pos))
        }
        // <EOF>
        Token::EOF => Err(PERR::UnexpectedEOF.into_err(settings.pos)),
        // All other tokens
        _ => parse_primary(input, state, lib, settings.level_up()),
    }
}

/// Make an assignment statement.
fn make_assignment_stmt(
    op: Option<Token>,
    state: &mut ParseState,
    lhs: Expr,
    rhs: Expr,
    op_pos: Position,
) -> ParseResult<Stmt> {
    #[must_use]
    fn check_lvalue(expr: &Expr, parent_is_dot: bool) -> Option<Position> {
        match expr {
            Expr::Index(x, term, _) | Expr::Dot(x, term, _) if parent_is_dot => match x.lhs {
                Expr::Property(_) if !term => {
                    check_lvalue(&x.rhs, matches!(expr, Expr::Dot(_, _, _)))
                }
                Expr::Property(_) => None,
                // Anything other than a property after dotting (e.g. a method call) is not an l-value
                ref e => Some(e.position()),
            },
            Expr::Index(x, term, _) | Expr::Dot(x, term, _) => match x.lhs {
                Expr::Property(_) => unreachable!("unexpected Expr::Property in indexing"),
                _ if !term => check_lvalue(&x.rhs, matches!(expr, Expr::Dot(_, _, _))),
                _ => None,
            },
            Expr::Property(_) if parent_is_dot => None,
            Expr::Property(_) => unreachable!("unexpected Expr::Property in indexing"),
            e if parent_is_dot => Some(e.position()),
            _ => None,
        }
    }

    let op_info = op.map(OpAssignment::new_from_token);

    match lhs {
        // const_expr = rhs
        ref expr if expr.is_constant() => {
            Err(PERR::AssignmentToConstant("".into()).into_err(lhs.position()))
        }
        // var (non-indexed) = rhs
        Expr::Variable(None, _, ref x) if x.0.is_none() => {
            Ok(Stmt::Assignment((lhs, op_info, rhs).into(), op_pos))
        }
        // var (indexed) = rhs
        Expr::Variable(i, var_pos, ref x) => {
            let (index, _, name) = x.as_ref();
            let index = i.map_or_else(
                || index.expect("either long or short index is `None`").get(),
                |n| n.get() as usize,
            );
            match state.stack[state.stack.len() - index].1 {
                AccessMode::ReadWrite => Ok(Stmt::Assignment((lhs, op_info, rhs).into(), op_pos)),
                // Constant values cannot be assigned to
                AccessMode::ReadOnly => {
                    Err(PERR::AssignmentToConstant(name.to_string()).into_err(var_pos))
                }
            }
        }
        // xxx[???]... = rhs, xxx.prop... = rhs
        Expr::Index(ref x, term, _) | Expr::Dot(ref x, term, _) => {
            let valid_lvalue = if term {
                None
            } else {
                check_lvalue(&x.rhs, matches!(lhs, Expr::Dot(_, _, _)))
            };

            match valid_lvalue {
                None => {
                    match x.lhs {
                        // var[???] = rhs, var.??? = rhs
                        Expr::Variable(_, _, _) => {
                            Ok(Stmt::Assignment((lhs, op_info, rhs).into(), op_pos))
                        }
                        // expr[???] = rhs, expr.??? = rhs
                        ref expr => {
                            Err(PERR::AssignmentToInvalidLHS("".to_string())
                                .into_err(expr.position()))
                        }
                    }
                }
                Some(err_pos) => {
                    Err(PERR::AssignmentToInvalidLHS("".to_string()).into_err(err_pos))
                }
            }
        }
        // ??? && ??? = rhs, ??? || ??? = rhs
        Expr::And(_, _) | Expr::Or(_, _) => Err(LexError::ImproperSymbol(
            "=".to_string(),
            "Possibly a typo of '=='?".to_string(),
        )
        .into_err(op_pos)),
        // expr = rhs
        _ => Err(PERR::AssignmentToInvalidLHS("".to_string()).into_err(lhs.position())),
    }
}

/// Parse an operator-assignment expression (if any).
fn parse_op_assignment_stmt(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    lhs: Expr,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let (op, pos) = match input.peek().expect(NEVER_ENDS) {
        // var = ...
        (Token::Equals, _) => (None, eat_token(input, Token::Equals)),
        // var op= ...
        (token, _) if token.is_op_assignment() => input
            .next()
            .map(|(op, pos)| (Some(op), pos))
            .expect(NEVER_ENDS),
        // Not op-assignment
        _ => return Ok(Stmt::Expr(lhs)),
    };

    let mut settings = settings;
    settings.pos = pos;

    let rhs = parse_expr(input, state, lib, settings.level_up())?;
    make_assignment_stmt(op, state, lhs, rhs, pos)
}

/// Make a dot expression.
#[cfg(not(feature = "no_object"))]
fn make_dot_expr(
    state: &mut ParseState,
    lhs: Expr,
    terminate_chaining: bool,
    rhs: Expr,
    op_pos: Position,
) -> ParseResult<Expr> {
    match (lhs, rhs) {
        // lhs[idx_expr].rhs
        (Expr::Index(mut x, term, pos), rhs) => {
            x.rhs = make_dot_expr(state, x.rhs, term || terminate_chaining, rhs, op_pos)?;
            Ok(Expr::Index(x, false, pos))
        }
        // lhs.id
        (lhs, var_expr @ Expr::Variable(_, _, _)) if var_expr.is_variable_access(true) => {
            let rhs = var_expr.into_property(state);
            Ok(Expr::Dot(BinaryExpr { lhs, rhs }.into(), false, op_pos))
        }
        // lhs.module::id - syntax error
        (_, Expr::Variable(_, _, x)) => {
            Err(PERR::PropertyExpected.into_err(x.1.expect("`Some`").0[0].pos))
        }
        // lhs.prop
        (lhs, prop @ Expr::Property(_)) => Ok(Expr::Dot(
            BinaryExpr { lhs, rhs: prop }.into(),
            false,
            op_pos,
        )),
        // lhs.dot_lhs.dot_rhs or lhs.dot_lhs[idx_rhs]
        (lhs, rhs @ Expr::Dot(_, _, _)) | (lhs, rhs @ Expr::Index(_, _, _)) => {
            let (x, term, pos, is_dot) = match rhs {
                Expr::Dot(x, term, pos) => (x, term, pos, true),
                Expr::Index(x, term, pos) => (x, term, pos, false),
                expr => unreachable!("Expr::Dot or Expr::Index expected but gets {:?}", expr),
            };

            match x.lhs {
                Expr::Variable(_, _, _) | Expr::Property(_) => {
                    let new_lhs = BinaryExpr {
                        lhs: x.lhs.into_property(state),
                        rhs: x.rhs,
                    }
                    .into();

                    let rhs = if is_dot {
                        Expr::Dot(new_lhs, term, pos)
                    } else {
                        Expr::Index(new_lhs, term, pos)
                    };
                    Ok(Expr::Dot(BinaryExpr { lhs, rhs }.into(), false, op_pos))
                }
                Expr::FnCall(mut func, func_pos) => {
                    // Recalculate hash
                    func.hashes = FnCallHashes::from_all(
                        #[cfg(not(feature = "no_function"))]
                        calc_fn_hash(&func.name, func.args.len()),
                        calc_fn_hash(&func.name, func.args.len() + 1),
                    );

                    let new_lhs = BinaryExpr {
                        lhs: Expr::FnCall(func, func_pos),
                        rhs: x.rhs,
                    }
                    .into();

                    let rhs = if is_dot {
                        Expr::Dot(new_lhs, term, pos)
                    } else {
                        Expr::Index(new_lhs, term, pos)
                    };
                    Ok(Expr::Dot(BinaryExpr { lhs, rhs }.into(), false, op_pos))
                }
                expr => unreachable!("invalid dot expression: {:?}", expr),
            }
        }
        // lhs.nnn::func(...)
        (_, Expr::FnCall(x, _)) if x.is_qualified() => {
            unreachable!("method call should not be namespace-qualified")
        }
        // lhs.Fn() or lhs.eval()
        (_, Expr::FnCall(x, pos))
            if x.args.is_empty()
                && [crate::engine::KEYWORD_FN_PTR, crate::engine::KEYWORD_EVAL]
                    .contains(&x.name.as_ref()) =>
        {
            Err(LexError::ImproperSymbol(
                x.name.to_string(),
                format!(
                    "'{}' should not be called in method style. Try {}(...);",
                    x.name, x.name
                ),
            )
            .into_err(pos))
        }
        // lhs.func!(...)
        (_, Expr::FnCall(x, pos)) if x.capture_parent_scope => Err(PERR::MalformedCapture(
            "method-call style does not support running within the caller's scope".into(),
        )
        .into_err(pos)),
        // lhs.func(...)
        (lhs, Expr::FnCall(mut func, func_pos)) => {
            // Recalculate hash
            func.hashes = FnCallHashes::from_all(
                #[cfg(not(feature = "no_function"))]
                calc_fn_hash(&func.name, func.args.len()),
                calc_fn_hash(&func.name, func.args.len() + 1),
            );

            let rhs = Expr::FnCall(func, func_pos);
            Ok(Expr::Dot(BinaryExpr { lhs, rhs }.into(), false, op_pos))
        }
        // lhs.rhs
        (_, rhs) => Err(PERR::PropertyExpected.into_err(rhs.position())),
    }
}

/// Parse a binary expression (if any).
fn parse_binary_op(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    parent_precedence: Option<Precedence>,
    lhs: Expr,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;
    settings.pos = lhs.position();

    let mut root = lhs;

    loop {
        let (current_op, current_pos) = input.peek().expect(NEVER_ENDS);
        let precedence = match current_op {
            Token::Custom(c) => state
                .engine
                .custom_keywords
                .get(c.as_ref())
                .cloned()
                .ok_or_else(|| PERR::Reserved(c.to_string()).into_err(*current_pos))?,
            Token::Reserved(c) if !is_valid_identifier(c.chars()) => {
                return Err(PERR::UnknownOperator(c.to_string()).into_err(*current_pos))
            }
            _ => current_op.precedence(),
        };
        let bind_right = current_op.is_bind_right();

        // Bind left to the parent lhs expression if precedence is higher
        // If same precedence, then check if the operator binds right
        if precedence < parent_precedence || (precedence == parent_precedence && !bind_right) {
            return Ok(root);
        }

        let (op_token, pos) = input.next().expect(NEVER_ENDS);

        let rhs = parse_unary(input, state, lib, settings)?;

        let (next_op, next_pos) = input.peek().expect(NEVER_ENDS);
        let next_precedence = match next_op {
            Token::Custom(c) => state
                .engine
                .custom_keywords
                .get(c.as_ref())
                .cloned()
                .ok_or_else(|| PERR::Reserved(c.to_string()).into_err(*next_pos))?,
            Token::Reserved(c) if !is_valid_identifier(c.chars()) => {
                return Err(PERR::UnknownOperator(c.to_string()).into_err(*next_pos))
            }
            _ => next_op.precedence(),
        };

        // Bind to right if the next operator has higher precedence
        // If same precedence, then check if the operator binds right
        let rhs = if (precedence == next_precedence && bind_right) || precedence < next_precedence {
            parse_binary_op(input, state, lib, precedence, rhs, settings)?
        } else {
            // Otherwise bind to left (even if next operator has the same precedence)
            rhs
        };

        settings = settings.level_up();
        settings.pos = pos;

        #[cfg(not(feature = "unchecked"))]
        settings.ensure_level_within_max_limit(state.max_expr_depth)?;

        let op = op_token.syntax();
        let hash = calc_fn_hash(&op, 2);

        let op_base = FnCallExpr {
            name: state.get_identifier("", op),
            hashes: FnCallHashes::from_native(hash),
            ..Default::default()
        };

        let mut args = StaticVec::new_const();
        args.push(root);
        args.push(rhs);
        args.shrink_to_fit();

        root = match op_token {
            // '!=' defaults to true when passed invalid operands
            Token::NotEqualsTo => FnCallExpr { args, ..op_base }.into_fn_call_expr(pos),

            // Comparison operators default to false when passed invalid operands
            Token::EqualsTo
            | Token::LessThan
            | Token::LessThanEqualsTo
            | Token::GreaterThan
            | Token::GreaterThanEqualsTo => FnCallExpr { args, ..op_base }.into_fn_call_expr(pos),

            Token::Or => {
                let rhs = args.pop().unwrap();
                let current_lhs = args.pop().unwrap();
                Expr::Or(
                    BinaryExpr {
                        lhs: current_lhs.ensure_bool_expr()?,
                        rhs: rhs.ensure_bool_expr()?,
                    }
                    .into(),
                    pos,
                )
            }
            Token::And => {
                let rhs = args.pop().unwrap();
                let current_lhs = args.pop().unwrap();
                Expr::And(
                    BinaryExpr {
                        lhs: current_lhs.ensure_bool_expr()?,
                        rhs: rhs.ensure_bool_expr()?,
                    }
                    .into(),
                    pos,
                )
            }
            Token::In => {
                // Swap the arguments
                let current_lhs = args.remove(0);
                args.push(current_lhs);
                args.shrink_to_fit();

                // Convert into a call to `contains`
                FnCallExpr {
                    hashes: calc_fn_hash(OP_CONTAINS, 2).into(),
                    args,
                    name: state.get_identifier("", OP_CONTAINS),
                    ..op_base
                }
                .into_fn_call_expr(pos)
            }

            Token::Custom(s)
                if state
                    .engine
                    .custom_keywords
                    .get(s.as_ref())
                    .map_or(false, Option::is_some) =>
            {
                let hash = calc_fn_hash(&s, 2);

                FnCallExpr {
                    hashes: if is_valid_function_name(&s) {
                        hash.into()
                    } else {
                        FnCallHashes::from_native(hash)
                    },
                    args,
                    ..op_base
                }
                .into_fn_call_expr(pos)
            }

            _ => FnCallExpr { args, ..op_base }.into_fn_call_expr(pos),
        };
    }
}

/// Parse a custom syntax.
fn parse_custom_syntax(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
    key: impl Into<ImmutableString>,
    syntax: &CustomSyntax,
    pos: Position,
) -> ParseResult<Expr> {
    let mut settings = settings;
    let mut inputs = StaticVec::<Expr>::new();
    let mut segments = StaticVec::new_const();
    let mut tokens = StaticVec::new_const();

    // Adjust the variables stack
    if syntax.scope_may_be_changed {
        // Add a barrier variable to the stack so earlier variables will not be matched.
        // Variable searches stop at the first barrier.
        let marker = state.get_identifier("", SCOPE_SEARCH_BARRIER_MARKER);
        state.stack.push((marker, AccessMode::ReadWrite));
    }

    let parse_func = syntax.parse.as_ref();
    let mut required_token: ImmutableString = key.into();

    tokens.push(required_token.clone().into());
    segments.push(required_token.clone());

    loop {
        let (fwd_token, fwd_pos) = input.peek().expect(NEVER_ENDS);
        settings.pos = *fwd_pos;
        let settings = settings.level_up();

        required_token = match parse_func(&segments, &*fwd_token.syntax()) {
            Ok(Some(seg))
                if seg.starts_with(CUSTOM_SYNTAX_MARKER_SYNTAX_VARIANT)
                    && seg.len() > CUSTOM_SYNTAX_MARKER_SYNTAX_VARIANT.len() =>
            {
                inputs.push(Expr::StringConstant(
                    state.get_interned_string("", seg),
                    pos,
                ));
                break;
            }
            Ok(Some(seg)) => seg,
            Ok(None) => break,
            Err(err) => return Err(err.0.into_err(settings.pos)),
        };

        match required_token.as_str() {
            CUSTOM_SYNTAX_MARKER_IDENT => {
                let (name, pos) = parse_var_name(input)?;
                let name = state.get_identifier("", name);
                segments.push(name.clone().into());
                tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_IDENT));
                inputs.push(Expr::Variable(None, pos, (None, None, name).into()));
            }
            CUSTOM_SYNTAX_MARKER_SYMBOL => {
                let (symbol, pos) = parse_symbol(input)?;
                let symbol = state.get_interned_string("", symbol);
                segments.push(symbol.clone());
                tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_SYMBOL));
                inputs.push(Expr::StringConstant(symbol, pos));
            }
            CUSTOM_SYNTAX_MARKER_EXPR => {
                inputs.push(parse_expr(input, state, lib, settings)?);
                let keyword = state.get_identifier("", CUSTOM_SYNTAX_MARKER_EXPR);
                segments.push(keyword.clone().into());
                tokens.push(keyword);
            }
            CUSTOM_SYNTAX_MARKER_BLOCK => match parse_block(input, state, lib, settings)? {
                block @ Stmt::Block(_, _) => {
                    inputs.push(Expr::Stmt(Box::new(block.into())));
                    let keyword = state.get_identifier("", CUSTOM_SYNTAX_MARKER_BLOCK);
                    segments.push(keyword.clone().into());
                    tokens.push(keyword);
                }
                stmt => unreachable!("Stmt::Block expected but gets {:?}", stmt),
            },
            CUSTOM_SYNTAX_MARKER_BOOL => match input.next().expect(NEVER_ENDS) {
                (b @ Token::True, pos) | (b @ Token::False, pos) => {
                    inputs.push(Expr::BoolConstant(b == Token::True, pos));
                    segments.push(state.get_interned_string("", b.literal_syntax()));
                    tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_BOOL));
                }
                (_, pos) => {
                    return Err(
                        PERR::MissingSymbol("Expecting 'true' or 'false'".to_string())
                            .into_err(pos),
                    )
                }
            },
            CUSTOM_SYNTAX_MARKER_INT => match input.next().expect(NEVER_ENDS) {
                (Token::IntegerConstant(i), pos) => {
                    inputs.push(Expr::IntegerConstant(i, pos));
                    segments.push(i.to_string().into());
                    tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_INT));
                }
                (_, pos) => {
                    return Err(
                        PERR::MissingSymbol("Expecting an integer number".to_string())
                            .into_err(pos),
                    )
                }
            },
            #[cfg(not(feature = "no_float"))]
            CUSTOM_SYNTAX_MARKER_FLOAT => match input.next().expect(NEVER_ENDS) {
                (Token::FloatConstant(f), pos) => {
                    inputs.push(Expr::FloatConstant(f, pos));
                    segments.push(f.to_string().into());
                    tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_FLOAT));
                }
                (_, pos) => {
                    return Err(PERR::MissingSymbol(
                        "Expecting a floating-point number".to_string(),
                    )
                    .into_err(pos))
                }
            },
            CUSTOM_SYNTAX_MARKER_STRING => match input.next().expect(NEVER_ENDS) {
                (Token::StringConstant(s), pos) => {
                    let s = state.get_interned_string("", s);
                    inputs.push(Expr::StringConstant(s.clone(), pos));
                    segments.push(s);
                    tokens.push(state.get_identifier("", CUSTOM_SYNTAX_MARKER_STRING));
                }
                (_, pos) => {
                    return Err(PERR::MissingSymbol("Expecting a string".to_string()).into_err(pos))
                }
            },
            s => match input.next().expect(NEVER_ENDS) {
                (Token::LexError(err), pos) => return Err(err.into_err(pos)),
                (t, _) if &*t.syntax() == s => {
                    segments.push(required_token.clone());
                    tokens.push(required_token.clone().into());
                }
                (_, pos) => {
                    return Err(PERR::MissingToken(
                        s.to_string(),
                        format!("for '{}' expression", segments[0]),
                    )
                    .into_err(pos))
                }
            },
        }
    }

    inputs.shrink_to_fit();
    tokens.shrink_to_fit();

    const KEYWORD_SEMICOLON: &str = Token::SemiColon.literal_syntax();
    const KEYWORD_CLOSE_BRACE: &str = Token::RightBrace.literal_syntax();

    let self_terminated = match required_token.as_str() {
        // It is self-terminating if the last symbol is a block
        CUSTOM_SYNTAX_MARKER_BLOCK => true,
        // If the last symbol is `;` or `}`, it is self-terminating
        KEYWORD_SEMICOLON | KEYWORD_CLOSE_BRACE => true,
        _ => false,
    };

    Ok(Expr::Custom(
        CustomExpr {
            inputs,
            tokens,
            scope_may_be_changed: syntax.scope_may_be_changed,
            self_terminated,
        }
        .into(),
        pos,
    ))
}

/// Parse an expression.
fn parse_expr(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Expr> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;
    settings.pos = input.peek().expect(NEVER_ENDS).1;

    // Parse expression normally.
    let precedence = Precedence::new(1);
    let lhs = parse_unary(input, state, lib, settings.level_up())?;
    parse_binary_op(input, state, lib, precedence, lhs, settings.level_up())
}

/// Parse an if statement.
fn parse_if(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // if ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::If);

    // if guard { if_body }
    ensure_not_statement_expr(input, "a boolean")?;
    let guard = parse_expr(input, state, lib, settings.level_up())?.ensure_bool_expr()?;
    ensure_not_assignment(input)?;
    let if_body = parse_block(input, state, lib, settings.level_up())?;

    // if guard { if_body } else ...
    let else_body = if match_token(input, Token::Else).0 {
        if let (Token::If, _) = input.peek().expect(NEVER_ENDS) {
            // if guard { if_body } else if ...
            parse_if(input, state, lib, settings.level_up())?
        } else {
            // if guard { if_body } else { else-body }
            parse_block(input, state, lib, settings.level_up())?
        }
    } else {
        Stmt::Noop(Position::NONE)
    };

    Ok(Stmt::If(
        guard,
        (if_body.into(), else_body.into()).into(),
        settings.pos,
    ))
}

/// Parse a while loop.
fn parse_while_loop(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;

    // while|loops ...
    let (guard, token_pos) = match input.next().expect(NEVER_ENDS) {
        (Token::While, pos) => {
            ensure_not_statement_expr(input, "a boolean")?;
            let expr = parse_expr(input, state, lib, settings.level_up())?.ensure_bool_expr()?;
            ensure_not_assignment(input)?;
            (expr, pos)
        }
        (Token::Loop, pos) => (Expr::Unit(Position::NONE), pos),
        token => unreachable!("Token::While or Token::Loop expected but gets {:?}", token),
    };
    settings.pos = token_pos;
    settings.is_breakable = true;

    let body = parse_block(input, state, lib, settings.level_up())?;

    Ok(Stmt::While(guard, Box::new(body.into()), settings.pos))
}

/// Parse a do loop.
fn parse_do(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // do ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::Do);

    // do { body } [while|until] guard
    settings.is_breakable = true;
    let body = parse_block(input, state, lib, settings.level_up())?;

    let negated = match input.next().expect(NEVER_ENDS) {
        (Token::While, _) => AST_OPTION_NONE,
        (Token::Until, _) => AST_OPTION_NEGATED,
        (_, pos) => {
            return Err(
                PERR::MissingToken(Token::While.into(), "for the do statement".into())
                    .into_err(pos),
            )
        }
    };

    settings.is_breakable = false;

    ensure_not_statement_expr(input, "a boolean")?;
    let guard = parse_expr(input, state, lib, settings.level_up())?.ensure_bool_expr()?;
    ensure_not_assignment(input)?;

    Ok(Stmt::Do(
        Box::new(body.into()),
        guard,
        negated,
        settings.pos,
    ))
}

/// Parse a for loop.
fn parse_for(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // for ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::For);

    // for name ...
    let (name, name_pos, counter_name, counter_pos) = if match_token(input, Token::LeftParen).0 {
        // ( name, counter )
        let (name, name_pos) = parse_var_name(input)?;
        let (has_comma, pos) = match_token(input, Token::Comma);
        if !has_comma {
            return Err(PERR::MissingToken(
                Token::Comma.into(),
                "after the iteration variable name".into(),
            )
            .into_err(pos));
        }
        let (counter_name, counter_pos) = parse_var_name(input)?;

        if counter_name == name {
            return Err(PERR::DuplicatedVariable(counter_name.to_string()).into_err(counter_pos));
        }

        let (has_close_paren, pos) = match_token(input, Token::RightParen);
        if !has_close_paren {
            return Err(PERR::MissingToken(
                Token::RightParen.into(),
                "to close the iteration variable".into(),
            )
            .into_err(pos));
        }
        (name, name_pos, Some(counter_name), Some(counter_pos))
    } else {
        // name
        let (name, name_pos) = parse_var_name(input)?;
        (name, name_pos, None, None)
    };

    // for name in ...
    match input.next().expect(NEVER_ENDS) {
        (Token::In, _) => (),
        (Token::LexError(err), pos) => return Err(err.into_err(pos)),
        (_, pos) => {
            return Err(
                PERR::MissingToken(Token::In.into(), "after the iteration variable".into())
                    .into_err(pos),
            )
        }
    }

    // for name in expr { body }
    ensure_not_statement_expr(input, "a boolean")?;
    let expr = parse_expr(input, state, lib, settings.level_up())?.ensure_iterable()?;

    let prev_stack_len = state.stack.len();

    let counter_var = counter_name.map(|name| {
        let name = state.get_identifier("", name);
        let pos = counter_pos.expect("`Some`");
        state.stack.push((name.clone(), AccessMode::ReadWrite));
        Ident { name, pos }
    });

    let loop_var = state.get_identifier("", name);
    state.stack.push((loop_var.clone(), AccessMode::ReadWrite));
    let loop_var = Ident {
        name: loop_var,
        pos: name_pos,
    };

    settings.is_breakable = true;
    let body = parse_block(input, state, lib, settings.level_up())?;

    state.stack.truncate(prev_stack_len);

    Ok(Stmt::For(
        expr,
        Box::new((loop_var, counter_var, body.into())),
        settings.pos,
    ))
}

/// Parse a variable definition statement.
fn parse_let(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    var_type: AccessMode,
    is_export: bool,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // let/const... (specified in `var_type`)
    let mut settings = settings;
    settings.pos = input.next().expect(NEVER_ENDS).1;

    // let name ...
    let (name, pos) = parse_var_name(input)?;

    let name = state.get_identifier("", name);
    let var_def = Ident {
        name: name.clone(),
        pos,
    };

    // let name = ...
    let expr = if match_token(input, Token::Equals).0 {
        // let name = expr
        parse_expr(input, state, lib, settings.level_up())?
    } else {
        Expr::Unit(Position::NONE)
    };

    state.stack.push((name, var_type));

    let export = if is_export {
        AST_OPTION_PUBLIC
    } else {
        AST_OPTION_NONE
    };

    match var_type {
        // let name = expr
        AccessMode::ReadWrite => Ok(Stmt::Var(expr, var_def.into(), export, settings.pos)),
        // const name = { expr:constant }
        AccessMode::ReadOnly => Ok(Stmt::Var(
            expr,
            var_def.into(),
            AST_OPTION_CONSTANT + export,
            settings.pos,
        )),
    }
}

/// Parse an import statement.
#[cfg(not(feature = "no_module"))]
fn parse_import(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // import ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::Import);

    // import expr ...
    let expr = parse_expr(input, state, lib, settings.level_up())?;

    // import expr as ...
    if !match_token(input, Token::As).0 {
        return Ok(Stmt::Import(expr, None, settings.pos));
    }

    // import expr as name ...
    let (name, pos) = parse_var_name(input)?;
    let name = state.get_identifier("", name);
    state.modules.push(name.clone());

    Ok(Stmt::Import(
        expr,
        Some(Ident { name, pos }.into()),
        settings.pos,
    ))
}

/// Parse an export statement.
#[cfg(not(feature = "no_module"))]
fn parse_export(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;
    settings.pos = eat_token(input, Token::Export);

    match input.peek().expect(NEVER_ENDS) {
        (Token::Let, pos) => {
            let pos = *pos;
            let mut stmt = parse_let(input, state, lib, AccessMode::ReadWrite, true, settings)?;
            stmt.set_position(pos);
            return Ok(stmt);
        }
        (Token::Const, pos) => {
            let pos = *pos;
            let mut stmt = parse_let(input, state, lib, AccessMode::ReadOnly, true, settings)?;
            stmt.set_position(pos);
            return Ok(stmt);
        }
        _ => (),
    }

    let mut exports = Vec::<(Ident, Ident)>::with_capacity(4);

    loop {
        let (id, id_pos) = parse_var_name(input)?;

        let (rename, rename_pos) = if match_token(input, Token::As).0 {
            let (name, pos) = parse_var_name(input)?;
            if exports.iter().any(|(_, alias)| alias.name == name.as_ref()) {
                return Err(PERR::DuplicatedVariable(name.to_string()).into_err(pos));
            }
            (Some(name), pos)
        } else {
            (None, Position::NONE)
        };

        exports.push((
            Ident {
                name: state.get_identifier("", id),
                pos: id_pos,
            },
            Ident {
                name: state.get_identifier("", rename.as_ref().map_or("", <_>::as_ref)),
                pos: rename_pos,
            },
        ));

        match input.peek().expect(NEVER_ENDS) {
            (Token::Comma, _) => {
                eat_token(input, Token::Comma);
            }
            (Token::Identifier(_), pos) => {
                return Err(PERR::MissingToken(
                    Token::Comma.into(),
                    "to separate the list of exports".into(),
                )
                .into_err(*pos))
            }
            _ => break,
        }
    }

    Ok(Stmt::Export(exports.into_boxed_slice(), settings.pos))
}

/// Parse a statement block.
fn parse_block(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // Must start with {
    let mut settings = settings;
    settings.pos = match input.next().expect(NEVER_ENDS) {
        (Token::LeftBrace, pos) => pos,
        (Token::LexError(err), pos) => return Err(err.into_err(pos)),
        (_, pos) => {
            return Err(PERR::MissingToken(
                Token::LeftBrace.into(),
                "to start a statement block".into(),
            )
            .into_err(pos))
        }
    };

    let mut statements = Vec::with_capacity(8);

    let prev_entry_stack_len = state.entry_stack_len;
    state.entry_stack_len = state.stack.len();

    #[cfg(not(feature = "no_module"))]
    let prev_mods_len = state.modules.len();

    loop {
        // Terminated?
        match input.peek().expect(NEVER_ENDS) {
            (Token::RightBrace, _) => {
                eat_token(input, Token::RightBrace);
                break;
            }
            (Token::EOF, pos) => {
                return Err(PERR::MissingToken(
                    Token::RightBrace.into(),
                    "to terminate this block".into(),
                )
                .into_err(*pos));
            }
            _ => (),
        }

        // Parse statements inside the block
        settings.is_global = false;

        let stmt = parse_stmt(input, state, lib, settings.level_up())?;

        if stmt.is_noop() {
            continue;
        }

        // See if it needs a terminating semicolon
        let need_semicolon = !stmt.is_self_terminated();

        statements.push(stmt);

        match input.peek().expect(NEVER_ENDS) {
            // { ... stmt }
            (Token::RightBrace, _) => {
                eat_token(input, Token::RightBrace);
                break;
            }
            // { ... stmt;
            (Token::SemiColon, _) if need_semicolon => {
                eat_token(input, Token::SemiColon);
            }
            // { ... { stmt } ;
            (Token::SemiColon, _) if !need_semicolon => {
                eat_token(input, Token::SemiColon);
            }
            // { ... { stmt } ???
            (_, _) if !need_semicolon => (),
            // { ... stmt <error>
            (Token::LexError(err), err_pos) => return Err(err.clone().into_err(*err_pos)),
            // { ... stmt ???
            (_, pos) => {
                // Semicolons are not optional between statements
                return Err(PERR::MissingToken(
                    Token::SemiColon.into(),
                    "to terminate this statement".into(),
                )
                .into_err(*pos));
            }
        }
    }

    state.stack.truncate(state.entry_stack_len);
    state.entry_stack_len = prev_entry_stack_len;

    #[cfg(not(feature = "no_module"))]
    state.modules.truncate(prev_mods_len);

    Ok(Stmt::Block(statements.into_boxed_slice(), settings.pos))
}

/// Parse an expression as a statement.
fn parse_expr_stmt(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;
    settings.pos = input.peek().expect(NEVER_ENDS).1;

    let expr = parse_expr(input, state, lib, settings.level_up())?;
    let stmt = parse_op_assignment_stmt(input, state, lib, expr, settings.level_up())?;
    Ok(stmt)
}

/// Parse a single statement.
fn parse_stmt(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    use AccessMode::{ReadOnly, ReadWrite};

    let mut settings = settings;

    #[cfg(not(feature = "no_function"))]
    #[cfg(feature = "metadata")]
    let comments = {
        let mut comments = Vec::<Box<str>>::new();
        let mut comments_pos = Position::NONE;

        // Handle doc-comments.
        while let (Token::Comment(ref comment), pos) = input.peek().expect(NEVER_ENDS) {
            if comments_pos.is_none() {
                comments_pos = *pos;
            }

            if !crate::tokenizer::is_doc_comment(comment) {
                unreachable!("doc-comment expected but gets {:?}", comment);
            }

            if !settings.is_global {
                return Err(PERR::WrongDocComment.into_err(comments_pos));
            }

            match input.next().expect(NEVER_ENDS).0 {
                Token::Comment(comment) => {
                    comments.push(comment);

                    match input.peek().expect(NEVER_ENDS) {
                        (Token::Fn, _) | (Token::Private, _) => break,
                        (Token::Comment(_), _) => (),
                        _ => return Err(PERR::WrongDocComment.into_err(comments_pos)),
                    }
                }
                token => unreachable!("Token::Comment expected but gets {:?}", token),
            }
        }

        comments
    };

    let (token, token_pos) = match input.peek().expect(NEVER_ENDS) {
        (Token::EOF, pos) => return Ok(Stmt::Noop(*pos)),
        (x, pos) => (x, *pos),
    };
    settings.pos = token_pos;

    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    match token {
        // ; - empty statement
        Token::SemiColon => {
            eat_token(input, Token::SemiColon);
            Ok(Stmt::Noop(token_pos))
        }

        // { - statements block
        Token::LeftBrace => Ok(parse_block(input, state, lib, settings.level_up())?),

        // fn ...
        #[cfg(not(feature = "no_function"))]
        Token::Fn if !settings.is_global => Err(PERR::WrongFnDefinition.into_err(token_pos)),

        #[cfg(not(feature = "no_function"))]
        Token::Fn | Token::Private => {
            let access = if matches!(token, Token::Private) {
                eat_token(input, Token::Private);
                crate::FnAccess::Private
            } else {
                crate::FnAccess::Public
            };

            match input.next().expect(NEVER_ENDS) {
                (Token::Fn, pos) => {
                    let mut new_state =
                        ParseState::new(state.engine, state.tokenizer_control.clone());

                    #[cfg(not(feature = "unchecked"))]
                    {
                        new_state.max_expr_depth = new_state.max_function_expr_depth;
                    }

                    let new_settings = ParseSettings {
                        allow_if_expr: settings.default_options.allow_if_expr,
                        allow_switch_expr: settings.default_options.allow_switch_expr,
                        allow_stmt_expr: settings.default_options.allow_stmt_expr,
                        allow_anonymous_fn: settings.default_options.allow_anonymous_fn,
                        strict_var: settings.strict_var,
                        is_global: false,
                        is_function_scope: true,
                        #[cfg(not(feature = "no_closure"))]
                        is_closure: false,
                        is_breakable: false,
                        level: 0,
                        pos,
                        ..settings
                    };

                    let func = parse_fn(
                        input,
                        &mut new_state,
                        lib,
                        access,
                        new_settings,
                        #[cfg(not(feature = "no_function"))]
                        #[cfg(feature = "metadata")]
                        comments,
                    )?;

                    let hash = calc_fn_hash(&func.name, func.params.len());

                    if lib.contains_key(&hash) {
                        return Err(PERR::FnDuplicatedDefinition(
                            func.name.to_string(),
                            func.params.len(),
                        )
                        .into_err(pos));
                    }

                    lib.insert(hash, func.into());

                    Ok(Stmt::Noop(pos))
                }

                (_, pos) => Err(PERR::MissingToken(
                    Token::Fn.into(),
                    format!("following '{}'", Token::Private.syntax()),
                )
                .into_err(pos)),
            }
        }

        Token::If => parse_if(input, state, lib, settings.level_up()),
        Token::Switch => parse_switch(input, state, lib, settings.level_up()),
        Token::While | Token::Loop if settings.default_options.allow_loop => {
            parse_while_loop(input, state, lib, settings.level_up())
        }
        Token::Do if settings.default_options.allow_loop => {
            parse_do(input, state, lib, settings.level_up())
        }
        Token::For if settings.default_options.allow_loop => {
            parse_for(input, state, lib, settings.level_up())
        }

        Token::Continue if settings.default_options.allow_loop && settings.is_breakable => {
            let pos = eat_token(input, Token::Continue);
            Ok(Stmt::BreakLoop(AST_OPTION_NONE, pos))
        }
        Token::Break if settings.default_options.allow_loop && settings.is_breakable => {
            let pos = eat_token(input, Token::Break);
            Ok(Stmt::BreakLoop(AST_OPTION_BREAK_OUT, pos))
        }
        Token::Continue | Token::Break if settings.default_options.allow_loop => {
            Err(PERR::LoopBreak.into_err(token_pos))
        }

        Token::Return | Token::Throw => {
            let (return_type, token_pos) = input
                .next()
                .map(|(token, pos)| {
                    let flags = match token {
                        Token::Return => AST_OPTION_NONE,
                        Token::Throw => AST_OPTION_BREAK_OUT,
                        token => unreachable!(
                            "Token::Return or Token::Throw expected but gets {:?}",
                            token
                        ),
                    };
                    (flags, pos)
                })
                .expect(NEVER_ENDS);

            match input.peek().expect(NEVER_ENDS) {
                // `return`/`throw` at <EOF>
                (Token::EOF, _) => Ok(Stmt::Return(return_type, None, token_pos)),
                // `return`/`throw` at end of block
                (Token::RightBrace, _) if !settings.is_global => {
                    Ok(Stmt::Return(return_type, None, token_pos))
                }
                // `return;` or `throw;`
                (Token::SemiColon, _) => Ok(Stmt::Return(return_type, None, token_pos)),
                // `return` or `throw` with expression
                (_, _) => {
                    let expr = parse_expr(input, state, lib, settings.level_up())?;
                    Ok(Stmt::Return(return_type, Some(expr), token_pos))
                }
            }
        }

        Token::Try => parse_try_catch(input, state, lib, settings.level_up()),

        Token::Let => parse_let(input, state, lib, ReadWrite, false, settings.level_up()),
        Token::Const => parse_let(input, state, lib, ReadOnly, false, settings.level_up()),

        #[cfg(not(feature = "no_module"))]
        Token::Import => parse_import(input, state, lib, settings.level_up()),

        #[cfg(not(feature = "no_module"))]
        Token::Export if !settings.is_global => Err(PERR::WrongExport.into_err(token_pos)),

        #[cfg(not(feature = "no_module"))]
        Token::Export => parse_export(input, state, lib, settings.level_up()),

        _ => parse_expr_stmt(input, state, lib, settings.level_up()),
    }
}

/// Parse a try/catch statement.
fn parse_try_catch(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<Stmt> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    // try ...
    let mut settings = settings;
    settings.pos = eat_token(input, Token::Try);

    // try { body }
    let body = parse_block(input, state, lib, settings.level_up())?;

    // try { body } catch
    let (matched, catch_pos) = match_token(input, Token::Catch);

    if !matched {
        return Err(
            PERR::MissingToken(Token::Catch.into(), "for the 'try' statement".into())
                .into_err(catch_pos),
        );
    }

    // try { body } catch (
    let err_var = if match_token(input, Token::LeftParen).0 {
        let (name, pos) = parse_var_name(input)?;
        let (matched, err_pos) = match_token(input, Token::RightParen);

        if !matched {
            return Err(PERR::MissingToken(
                Token::RightParen.into(),
                "to enclose the catch variable".into(),
            )
            .into_err(err_pos));
        }

        let name = state.get_identifier("", name);
        state.stack.push((name.clone(), AccessMode::ReadWrite));
        Some(Ident { name, pos })
    } else {
        None
    };

    // try { body } catch ( var ) { catch_block }
    let catch_body = parse_block(input, state, lib, settings.level_up())?;

    if err_var.is_some() {
        // Remove the error variable from the stack
        state.stack.pop().unwrap();
    }

    Ok(Stmt::TryCatch(
        (body.into(), err_var, catch_body.into()).into(),
        settings.pos,
    ))
}

/// Parse a function definition.
#[cfg(not(feature = "no_function"))]
fn parse_fn(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    access: crate::FnAccess,
    settings: ParseSettings,
    #[cfg(not(feature = "no_function"))]
    #[cfg(feature = "metadata")]
    comments: Vec<Box<str>>,
) -> ParseResult<ScriptFnDef> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;

    let (token, pos) = input.next().expect(NEVER_ENDS);

    let name = match token.into_function_name_for_override() {
        Ok(r) => r,
        Err(Token::Reserved(s)) => return Err(PERR::Reserved(s.to_string()).into_err(pos)),
        Err(_) => return Err(PERR::FnMissingName.into_err(pos)),
    };

    match input.peek().expect(NEVER_ENDS) {
        (Token::LeftParen, _) => eat_token(input, Token::LeftParen),
        (_, pos) => return Err(PERR::FnMissingParams(name.to_string()).into_err(*pos)),
    };

    let mut params = StaticVec::new_const();

    if !match_token(input, Token::RightParen).0 {
        let sep_err = format!("to separate the parameters of function '{}'", name);

        loop {
            match input.next().expect(NEVER_ENDS) {
                (Token::RightParen, _) => break,
                (Token::Identifier(s), pos) => {
                    if params.iter().any(|(p, _)| p == &*s) {
                        return Err(
                            PERR::FnDuplicatedParam(name.to_string(), s.to_string()).into_err(pos)
                        );
                    }
                    let s = state.get_identifier("", s);
                    state.stack.push((s.clone(), AccessMode::ReadWrite));
                    params.push((s, pos))
                }
                (Token::LexError(err), pos) => return Err(err.into_err(pos)),
                (_, pos) => {
                    return Err(PERR::MissingToken(
                        Token::RightParen.into(),
                        format!("to close the parameters list of function '{}'", name),
                    )
                    .into_err(pos))
                }
            }

            match input.next().expect(NEVER_ENDS) {
                (Token::RightParen, _) => break,
                (Token::Comma, _) => (),
                (Token::LexError(err), pos) => return Err(err.into_err(pos)),
                (_, pos) => {
                    return Err(PERR::MissingToken(Token::Comma.into(), sep_err).into_err(pos))
                }
            }
        }
    }

    // Parse function body
    let body = match input.peek().expect(NEVER_ENDS) {
        (Token::LeftBrace, _) => {
            settings.is_breakable = false;
            parse_block(input, state, lib, settings.level_up())?
        }
        (_, pos) => return Err(PERR::FnMissingBody(name.to_string()).into_err(*pos)),
    }
    .into();

    let mut params: StaticVec<_> = params.into_iter().map(|(p, _)| p).collect();
    params.shrink_to_fit();

    Ok(ScriptFnDef {
        name: state.get_identifier("", name),
        access,
        params,
        body,
        lib: None,
        #[cfg(not(feature = "no_module"))]
        global: None,
        #[cfg(not(feature = "no_function"))]
        #[cfg(feature = "metadata")]
        comments: if comments.is_empty() {
            None
        } else {
            Some(comments.into())
        },
    })
}

/// Creates a curried expression from a list of external variables
#[cfg(not(feature = "no_function"))]
#[cfg(not(feature = "no_closure"))]
fn make_curry_from_externals(
    state: &mut ParseState,
    fn_expr: Expr,
    externals: StaticVec<Identifier>,
    pos: Position,
) -> Expr {
    // If there are no captured variables, no need to curry
    if externals.is_empty() {
        return fn_expr;
    }

    let num_externals = externals.len();
    let mut args = StaticVec::with_capacity(externals.len() + 1);

    args.push(fn_expr);

    args.extend(
        externals
            .iter()
            .cloned()
            .map(|x| Expr::Variable(None, Position::NONE, (None, None, x).into())),
    );

    let expr = FnCallExpr {
        name: state.get_identifier("", crate::engine::KEYWORD_FN_PTR_CURRY),
        hashes: FnCallHashes::from_native(calc_fn_hash(
            crate::engine::KEYWORD_FN_PTR_CURRY,
            num_externals + 1,
        )),
        args,
        ..Default::default()
    }
    .into_fn_call_expr(pos);

    // Convert the entire expression into a statement block, then insert the relevant
    // [`Share`][Stmt::Share] statements.
    let mut statements = StaticVec::with_capacity(externals.len() + 1);
    statements.extend(externals.into_iter().map(Stmt::Share));
    statements.push(Stmt::Expr(expr));
    Expr::Stmt(StmtBlock::new(statements, pos).into())
}

/// Parse an anonymous function definition.
#[cfg(not(feature = "no_function"))]
fn parse_anon_fn(
    input: &mut TokenStream,
    state: &mut ParseState,
    lib: &mut FnLib,
    settings: ParseSettings,
) -> ParseResult<(Expr, ScriptFnDef)> {
    #[cfg(not(feature = "unchecked"))]
    settings.ensure_level_within_max_limit(state.max_expr_depth)?;

    let mut settings = settings;
    let mut params_list = StaticVec::new_const();

    if input.next().expect(NEVER_ENDS).0 != Token::Or && !match_token(input, Token::Pipe).0 {
        loop {
            match input.next().expect(NEVER_ENDS) {
                (Token::Pipe, _) => break,
                (Token::Identifier(s), pos) => {
                    if params_list.iter().any(|p| p == &*s) {
                        return Err(
                            PERR::FnDuplicatedParam("".to_string(), s.to_string()).into_err(pos)
                        );
                    }
                    let s = state.get_identifier("", s);
                    state.stack.push((s.clone(), AccessMode::ReadWrite));
                    params_list.push(s)
                }
                (Token::LexError(err), pos) => return Err(err.into_err(pos)),
                (_, pos) => {
                    return Err(PERR::MissingToken(
                        Token::Pipe.into(),
                        "to close the parameters list of anonymous function".into(),
                    )
                    .into_err(pos))
                }
            }

            match input.next().expect(NEVER_ENDS) {
                (Token::Pipe, _) => break,
                (Token::Comma, _) => (),
                (Token::LexError(err), pos) => return Err(err.into_err(pos)),
                (_, pos) => {
                    return Err(PERR::MissingToken(
                        Token::Comma.into(),
                        "to separate the parameters of anonymous function".into(),
                    )
                    .into_err(pos))
                }
            }
        }
    }

    // Parse function body
    settings.is_breakable = false;
    let body = parse_stmt(input, state, lib, settings.level_up())?;

    // External variables may need to be processed in a consistent order,
    // so extract them into a list.
    #[cfg(not(feature = "no_closure"))]
    let externals: StaticVec<Identifier> = state
        .external_vars
        .iter()
        .map(|(name, _)| name.clone())
        .collect();

    #[cfg(not(feature = "no_closure"))]
    let mut params = StaticVec::with_capacity(params_list.len() + externals.len());
    #[cfg(feature = "no_closure")]
    let mut params = StaticVec::with_capacity(params_list.len());

    #[cfg(not(feature = "no_closure"))]
    params.extend(externals.iter().cloned());

    params.append(&mut params_list);

    // Create unique function name by hashing the script body plus the parameters.
    let hasher = &mut get_hasher();
    params.iter().for_each(|p| p.hash(hasher));
    body.hash(hasher);
    let hash = hasher.finish();
    let fn_name = state.get_identifier("", format!("{}{:016x}", crate::engine::FN_ANONYMOUS, hash));

    // Define the function
    let script = ScriptFnDef {
        name: fn_name.clone(),
        access: crate::FnAccess::Public,
        params,
        body: body.into(),
        lib: None,
        #[cfg(not(feature = "no_module"))]
        global: None,
        #[cfg(not(feature = "no_function"))]
        #[cfg(feature = "metadata")]
        comments: None,
    };

    let fn_ptr = crate::FnPtr::new_unchecked(fn_name, StaticVec::new_const());
    let expr = Expr::DynamicConstant(Box::new(fn_ptr.into()), settings.pos);

    #[cfg(not(feature = "no_closure"))]
    let expr = make_curry_from_externals(state, expr, externals, settings.pos);

    Ok((expr, script))
}

impl Engine {
    /// Parse a global level expression.
    pub(crate) fn parse_global_expr(
        &self,
        input: &mut TokenStream,
        state: &mut ParseState,
        scope: &Scope,
        #[cfg(not(feature = "no_optimize"))] optimization_level: crate::OptimizationLevel,
    ) -> ParseResult<AST> {
        let _scope = scope;
        let mut functions = BTreeMap::new();

        let settings = ParseSettings {
            default_options: self.options,
            allow_if_expr: false,
            allow_switch_expr: false,
            allow_stmt_expr: false,
            #[cfg(not(feature = "no_function"))]
            allow_anonymous_fn: false,
            strict_var: self.options.strict_var,
            is_global: true,
            #[cfg(not(feature = "no_function"))]
            is_function_scope: false,
            #[cfg(not(feature = "no_function"))]
            #[cfg(not(feature = "no_closure"))]
            is_closure: false,
            is_breakable: false,
            level: 0,
            pos: Position::NONE,
        };
        let expr = parse_expr(input, state, &mut functions, settings)?;

        assert!(functions.is_empty());

        match input.peek().expect(NEVER_ENDS) {
            (Token::EOF, _) => (),
            // Return error if the expression doesn't end
            (token, pos) => {
                return Err(LexError::UnexpectedInput(token.syntax().to_string()).into_err(*pos))
            }
        }

        let mut statements = StaticVec::new_const();
        statements.push(Stmt::Expr(expr));

        #[cfg(not(feature = "no_optimize"))]
        return Ok(crate::optimizer::optimize_into_ast(
            self,
            _scope,
            statements,
            #[cfg(not(feature = "no_function"))]
            StaticVec::new_const(),
            optimization_level,
        ));

        #[cfg(feature = "no_optimize")]
        return Ok(AST::new(
            statements,
            #[cfg(not(feature = "no_function"))]
            crate::Module::new(),
        ));
    }

    /// Parse the global level statements.
    fn parse_global_level(
        &self,
        input: &mut TokenStream,
        state: &mut ParseState,
    ) -> ParseResult<(StaticVec<Stmt>, StaticVec<Shared<ScriptFnDef>>)> {
        let mut statements = StaticVec::new_const();
        let mut functions = BTreeMap::new();

        while !input.peek().expect(NEVER_ENDS).0.is_eof() {
            let settings = ParseSettings {
                default_options: self.options,
                allow_if_expr: self.options.allow_if_expr,
                allow_switch_expr: self.options.allow_switch_expr,
                allow_stmt_expr: self.options.allow_stmt_expr,
                #[cfg(not(feature = "no_function"))]
                allow_anonymous_fn: self.options.allow_anonymous_fn,
                strict_var: self.options.strict_var,
                is_global: true,
                #[cfg(not(feature = "no_function"))]
                is_function_scope: false,
                #[cfg(not(feature = "no_function"))]
                #[cfg(not(feature = "no_closure"))]
                is_closure: false,
                is_breakable: false,
                level: 0,
                pos: Position::NONE,
            };

            let stmt = parse_stmt(input, state, &mut functions, settings)?;

            if stmt.is_noop() {
                continue;
            }

            let need_semicolon = !stmt.is_self_terminated();

            statements.push(stmt);

            match input.peek().expect(NEVER_ENDS) {
                // EOF
                (Token::EOF, _) => break,
                // stmt ;
                (Token::SemiColon, _) if need_semicolon => {
                    eat_token(input, Token::SemiColon);
                }
                // stmt ;
                (Token::SemiColon, _) if !need_semicolon => (),
                // { stmt } ???
                (_, _) if !need_semicolon => (),
                // stmt <error>
                (Token::LexError(err), pos) => return Err(err.clone().into_err(*pos)),
                // stmt ???
                (_, pos) => {
                    // Semicolons are not optional between statements
                    return Err(PERR::MissingToken(
                        Token::SemiColon.into(),
                        "to terminate this statement".into(),
                    )
                    .into_err(*pos));
                }
            }
        }

        Ok((statements, functions.into_iter().map(|(_, v)| v).collect()))
    }

    /// Run the parser on an input stream, returning an AST.
    #[inline]
    pub(crate) fn parse(
        &self,
        input: &mut TokenStream,
        state: &mut ParseState,
        scope: &Scope,
        #[cfg(not(feature = "no_optimize"))] optimization_level: crate::OptimizationLevel,
    ) -> ParseResult<AST> {
        let _scope = scope;
        let (statements, _lib) = self.parse_global_level(input, state)?;

        #[cfg(not(feature = "no_optimize"))]
        return Ok(crate::optimizer::optimize_into_ast(
            self,
            _scope,
            statements,
            #[cfg(not(feature = "no_function"))]
            _lib,
            optimization_level,
        ));

        #[cfg(feature = "no_optimize")]
        #[cfg(not(feature = "no_function"))]
        {
            let mut m = crate::Module::new();

            _lib.into_iter().for_each(|fn_def| {
                m.set_script_fn(fn_def);
            });

            return Ok(AST::new(statements, m));
        }

        #[cfg(feature = "no_optimize")]
        #[cfg(feature = "no_function")]
        return Ok(AST::new(
            statements,
            #[cfg(not(feature = "no_function"))]
            crate::Module::new(),
        ));
    }
}
