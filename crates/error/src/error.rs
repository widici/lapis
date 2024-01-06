use std::fmt::{Display, format};

use miette::{Diagnostic, Report};
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
        let mut span = kind.get_option_span().cloned();
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
            let lc = span.line_col.unwrap();
            let pos = if lc.0 == lc.1 {
                format!("{}:{}", lc.0.0, lc.0.1)
            } else {
                format!("{}:{}-{}:{}", lc.0.0, lc.0.1, lc.1.0, lc.1.1)
            };
            title += format!(" @ {}", pos).as_str()
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
    #[error("Found unexpected: {}, expected: {}", found, expected)]
    Unexpected {
        expected: String,
        found: String,
        #[label]
        #[span]
        span: Span,
    },
    #[error("Found Illegal char: {}", found)]
    IllegalChar {
        found: char,
        #[label]
        #[span]
        span: Span,
    },
    #[error("Unclosed {} found starting:", ty)]
    Unclosed {
        ty: &'static str,
        #[label]
        #[span]
        start: Span,
    },
    #[error("Statement: {} used in an unexpected context", stmt)]
    StmtUnexpectedContext {
        // TODO: replace this with StatementEnum
        stmt: String,
        #[label]
        #[span]
        span: Span,
    },
    #[error("{} is already defined in the scope", ident)]
    Redefenition { ident: String },
    #[error("{} can't be found", ident)]
    NotFound {
        ident: String,
        #[label("Expected here")]
        #[span]
        required: Span,
    },
}
