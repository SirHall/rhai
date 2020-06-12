//! Module containing error definitions for the parsing process.

use crate::result::EvalAltResult;
use crate::token::Position;

use crate::stdlib::{boxed::Box, char, error::Error, fmt, string::String};

/// Error when tokenizing the script text.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum LexError {
    /// An unexpected character is encountered when tokenizing the script text.
    UnexpectedChar(char),
    /// A string literal is not terminated before a new-line or EOF.
    UnterminatedString,
    /// An string/character/numeric escape sequence is in an invalid format.
    MalformedEscapeSequence(String),
    /// An numeric literal is in an invalid format.
    MalformedNumber(String),
    /// An character literal is in an invalid format.
    MalformedChar(String),
    /// An identifier is in an invalid format.
    MalformedIdentifier(String),
    /// Bad keyword encountered when tokenizing the script text.
    ImproperKeyword(String),
}

impl Error for LexError {}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::UnexpectedChar(c) => write!(f, "Unexpected '{}'", c),
            Self::MalformedEscapeSequence(s) => write!(f, "Invalid escape sequence: '{}'", s),
            Self::MalformedNumber(s) => write!(f, "Invalid number: '{}'", s),
            Self::MalformedChar(s) => write!(f, "Invalid character: '{}'", s),
            Self::MalformedIdentifier(s) => write!(f, "Variable name is not proper: '{}'", s),
            Self::UnterminatedString => write!(f, "Open string is not terminated"),
            Self::ImproperKeyword(s) => write!(f, "{}", s),
        }
    }
}

/// Type of error encountered when parsing a script.
///
/// Some errors never appear when certain features are turned on.
/// They still exist so that the application can turn features on and off without going through
/// massive code changes to remove/add back enum variants in match statements.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum ParseErrorType {
    /// Error in the script text. Wrapped value is the error message.
    BadInput(String),
    /// The script ends prematurely.
    UnexpectedEOF,
    /// An unknown operator is encountered. Wrapped value is the operator.
    UnknownOperator(String),
    /// Expecting a particular token but not finding one. Wrapped values are the token and description.
    MissingToken(String, String),
    /// An expression in function call arguments `()` has syntax error. Wrapped value is the error description (if any).
    MalformedCallExpr(String),
    /// An expression in indexing brackets `[]` has syntax error. Wrapped value is the error description (if any).
    ///
    /// Never appears under the `no_index` feature.
    MalformedIndexExpr(String),
    /// An expression in an `in` expression has syntax error. Wrapped value is the error description (if any).
    ///
    /// Never appears under the `no_object` and `no_index` features combination.
    MalformedInExpr(String),
    /// A map definition has duplicated property names. Wrapped value is the property name.
    ///
    /// Never appears under the `no_object` feature.
    DuplicatedProperty(String),
    /// Invalid expression assigned to constant. Wrapped value is the name of the constant.
    ForbiddenConstantExpr(String),
    /// Missing a property name for custom types and maps.
    ///
    /// Never appears under the `no_object` feature.
    PropertyExpected,
    /// Missing a variable name after the `let`, `const` or `for` keywords.
    VariableExpected,
    /// Missing an expression. Wrapped value is the expression type.
    ExprExpected(String),
    /// Defining a function `fn` in an appropriate place (e.g. inside another function).
    ///
    /// Never appears under the `no_function` feature.
    WrongFnDefinition,
    /// Missing a function name after the `fn` keyword.
    ///
    /// Never appears under the `no_function` feature.
    FnMissingName,
    /// A function definition is missing the parameters list. Wrapped value is the function name.
    ///
    /// Never appears under the `no_function` feature.
    FnMissingParams(String),
    /// A function definition has duplicated parameters. Wrapped values are the function name and parameter name.
    ///
    /// Never appears under the `no_function` feature.
    FnDuplicatedParam(String, String),
    /// A function definition is missing the body. Wrapped value is the function name.
    ///
    /// Never appears under the `no_function` feature.
    FnMissingBody(String),
    /// An export statement has duplicated names.
    ///
    /// Never appears under the `no_module` feature.
    DuplicatedExport(String),
    /// Export statement not at global level.
    ///
    /// Never appears under the `no_module` feature.
    WrongExport,
    /// Assignment to a copy of a value.
    AssignmentToCopy,
    /// Assignment to an a constant variable.
    AssignmentToConstant(String),
    /// Expression exceeding the maximum levels of complexity.
    ///
    /// Never appears under the `unchecked` feature.
    ExprTooDeep,
    /// Break statement not inside a loop.
    LoopBreak,
}

impl ParseErrorType {
    /// Make a `ParseError` using the current type and position.
    pub(crate) fn into_err(self, pos: Position) -> ParseError {
        ParseError(Box::new(self), pos)
    }

    pub(crate) fn desc(&self) -> &str {
        match self {
            Self::BadInput(p) => p,
            Self::UnexpectedEOF => "Script is incomplete",
            Self::UnknownOperator(_) => "Unknown operator",
            Self::MissingToken(_, _) => "Expecting a certain token that is missing",
            Self::MalformedCallExpr(_) => "Invalid expression in function call arguments",
            Self::MalformedIndexExpr(_) => "Invalid index in indexing expression",
            Self::MalformedInExpr(_) => "Invalid 'in' expression",
            Self::DuplicatedProperty(_) => "Duplicated property in object map literal",
            Self::ForbiddenConstantExpr(_) => "Expecting a constant",
            Self::PropertyExpected => "Expecting name of a property",
            Self::VariableExpected => "Expecting name of a variable",
            Self::ExprExpected(_) => "Expecting an expression",
            Self::FnMissingName => "Expecting name in function declaration",
            Self::FnMissingParams(_) => "Expecting parameters in function declaration",
            Self::FnDuplicatedParam(_,_) => "Duplicated parameters in function declaration",
            Self::FnMissingBody(_) => "Expecting body statement block for function declaration",
            Self::WrongFnDefinition => "Function definitions must be at global level and cannot be inside a block or another function",
            Self::DuplicatedExport(_) => "Duplicated variable/function in export statement",
            Self::WrongExport => "Export statement can only appear at global level",
            Self::AssignmentToCopy => "Only a copy of the value is change with this assignment",
            Self::AssignmentToConstant(_) => "Cannot assign to a constant value",
            Self::ExprTooDeep => "Expression exceeds maximum complexity",
            Self::LoopBreak => "Break statement should only be used inside a loop"
        }
    }
}

impl fmt::Display for ParseErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BadInput(s) | ParseErrorType::MalformedCallExpr(s) => {
                write!(f, "{}", if s.is_empty() { self.desc() } else { s })
            }
            Self::ForbiddenConstantExpr(s) => {
                write!(f, "Expecting a constant to assign to '{}'", s)
            }
            Self::UnknownOperator(s) => write!(f, "{}: '{}'", self.desc(), s),

            Self::MalformedIndexExpr(s) => {
                write!(f, "{}", if s.is_empty() { self.desc() } else { s })
            }

            Self::MalformedInExpr(s) => write!(f, "{}", if s.is_empty() { self.desc() } else { s }),

            Self::DuplicatedProperty(s) => {
                write!(f, "Duplicated property '{}' for object map literal", s)
            }

            Self::ExprExpected(s) => write!(f, "Expecting {} expression", s),

            Self::FnMissingParams(s) => write!(f, "Expecting parameters for function '{}'", s),

            Self::FnMissingBody(s) => {
                write!(f, "Expecting body statement block for function '{}'", s)
            }

            Self::FnDuplicatedParam(s, arg) => {
                write!(f, "Duplicated parameter '{}' for function '{}'", arg, s)
            }

            Self::DuplicatedExport(s) => write!(
                f,
                "Duplicated variable/function '{}' in export statement",
                s
            ),

            Self::MissingToken(token, s) => write!(f, "Expecting '{}' {}", token, s),

            Self::AssignmentToConstant(s) if s.is_empty() => write!(f, "{}", self.desc()),
            Self::AssignmentToConstant(s) => write!(f, "Cannot assign to constant '{}'", s),
            _ => write!(f, "{}", self.desc()),
        }
    }
}

/// Error when parsing a script.
#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct ParseError(pub Box<ParseErrorType>, pub Position);

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)?;

        // Do not write any position if None
        if !self.1.is_none() {
            write!(f, " ({})", self.1)?;
        }

        Ok(())
    }
}

impl From<ParseErrorType> for Box<EvalAltResult> {
    fn from(err: ParseErrorType) -> Self {
        Box::new(EvalAltResult::ErrorParsing(err, Position::none()))
    }
}

impl From<ParseError> for Box<EvalAltResult> {
    fn from(err: ParseError) -> Self {
        Box::new(EvalAltResult::ErrorParsing(*err.0, err.1))
    }
}
