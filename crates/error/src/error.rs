use span::Span;
use span_macros::GetSpan;
use miette::{Diagnostic, Report};
use thiserror::Error;

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
            span.get_src()
        }
        Error { kind, location, span }
    }

    #[must_use]
    pub fn to_report(self) -> Report {
        let report = Report::new(self.kind);
        if let Some(span) = self.span {
            return report.with_source_code(span)
        }
        report
    }
}

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
    }
}