use std::fmt::{Debug, Display};

use dyn_clone::{clone_trait_object, DynClone};
use miette::{Diagnostic, Report, SourceSpan};
use span::{GetSpanTrait, Span};
use span_macros::GetSpan;
use thiserror::Error;

#[derive(Clone)]
pub struct Error {
    kind: ErrorKind,
    location: ErrorLocation,
    span: Option<Span>,
}

impl Error {
    #[must_use]
    pub fn new(kind: ErrorKind, location: ErrorLocation) -> Self {
        let mut span = kind.get_option_span();
        if let Some(ref mut span) = span {
            span.set_line_col()
        }
        Error {
            kind,
            location,
            span,
        }
    }

    #[must_use]
    pub fn to_report(self) -> Report {
        let report = Report::new(self.kind);
        if let Some(span) = self.span {
            return report.with_source_code(span);
        }
        report
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut title = format!("{:?} error occurred", self.location);
        if let Some(span) = self.span.as_ref() {
            title += format!(" @ {}", span).as_str()
        };
        writeln!(f, "{}:", title)?;
        write!(f, "{:?}", self.clone().to_report())?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub enum ErrorLocation {
    /// Pre-processor errors such as file errors
    Initial,
    Lexer,
    Parser,
    Resolver,
    Evaluator,
}

#[derive(Diagnostic, Error, Debug, Clone, GetSpan)]
pub enum ErrorKind {
    #[error("Failed to open file with path: {}, message: {}", path, msg)]
    FileNotFound { path: String, msg: String },
    #[error("Failed to read from file with path: {}, message: {}", path, msg)]
    FileNotRead { path: String, msg: String },
    #[error("Found unexpected: {}, expected: {}", found, expected)]
    UnexpectedExpected {
        expected: String,
        #[label]
        #[span]
        found: Box<dyn SerializedToken>,
    },
    #[error("Found unexpected: {}", found)]
    Unexpected {
        #[label]
        #[span]
        found: Box<dyn SerializedToken>,
    },
    #[error("Found Illegal char: {}", found)]
    IllegalChar {
        #[label]
        #[span]
        found: Box<dyn SerializedToken>,
    },
    #[error("Unclosed {} found starting:", ty)]
    Unclosed {
        ty: &'static str,
        #[label]
        #[span]
        start: Span,
    },
    #[error("{} used in an unexpected context", found)]
    StmtUnexpectedContext {
        #[label]
        #[span]
        found: Box<dyn SerializedToken>,
    },
    #[error("{} is already defined in the scope", found)]
    Redeclaration {
        found: String,
        #[label]
        #[span]
        span: Span,
    },
    #[error("{} can't be found", ident)]
    NotFound {
        ident: String,
        #[label("Expected here")]
        #[span]
        required: Span,
    },
    #[error("Duplicate param idents are not allowed in this context")]
    DuplicateParam {
        #[label("Duplicate param used here")]
        #[span]
        param: Box<dyn SerializedToken>,
    },
    #[error("Operands doesn't have an impl for binary operator")]
    InvalidOperands {
        #[label("binary operator")]
        op: Box<dyn SerializedToken>,
        #[label("left hand side")]
        lhs: Box<dyn SerializedToken>,
        #[label("right hand side")]
        rhs: Box<dyn SerializedToken>,
        #[span]
        span: Span,
    },
    #[error("Found mismatched types")]
    MismatchedTypes {
        #[label("Mismatched types used here")]
        #[span]
        serialized_tok: Box<dyn SerializedToken>,
    },
    #[error("Expected arity: {} found arity: {}", expected_arity, found_arity)]
    MismatchedArity {
        expected_arity: usize,
        found_arity: usize,
        #[label("Function called here")]
        #[span]
        called: Box<dyn SerializedToken>
    }
}

pub trait SerializedToken: Debug + Display + GetSpanTrait + Sync + Send + DynClone {}

clone_trait_object!(SerializedToken);

impl From<Box<dyn SerializedToken>> for SourceSpan {
    fn from(value: Box<dyn SerializedToken>) -> Self {
        value.get_span().into()
    }
}
