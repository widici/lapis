use crate::span::{Span, GetSpan};
use miette::{Diagnostic, Report};
use thiserror::Error;

pub struct Error {
    kind: ErrorKind,
    span: Span,
}

impl Error {
    #[must_use]
    pub fn new(kind: ErrorKind) -> Self {
        let span = kind.get_span();
        Error { kind, span }
    }

    #[must_use]
    pub fn to_report(&self) -> Report {
        Report::new(self.kind.clone()).with_source_code(self.span.clone())
    }
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

impl GetSpan for ErrorKind {
    fn get_span(&self) -> Span {
        match self {
            ErrorKind::Unexpected { span , .. } => span.clone(),
        }
    }
}