use crate::span::{Span, GetSpanTrait};
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
        let span = kind.get_option_span();
        Error { kind, location, span }
    }

    #[must_use]
    pub fn to_report(self) -> Report {
        let mut report = Report::new(self.kind);
        match self.span {
            Some(span) => report = report.with_source_code(span),
            None => {},
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

#[derive(Diagnostic, Error, Debug, Clone)]
pub enum ErrorKind {
    #[error("Found unexpected: {}, expected: {}", found, expected)]
    Unexpected {
        expected: String,
        found: String,
        #[label]
        span: Span,
    }
}

impl GetSpanTrait for ErrorKind {
    fn get_option_span(&self) -> Option<Span> {
        match self {
            ErrorKind::Unexpected { span , .. } => Some(span.clone()),
        }
    }
}