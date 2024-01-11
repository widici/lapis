use line_col::LineColLookup;
use miette::{MietteSpanContents, SourceCode, SourceSpan};
use std::{fmt::Debug, fs::read_to_string};

pub trait GetSpanTrait {
    fn get_span(&self) -> Span;
    fn get_option_span(&self) -> Option<Span>;
}

#[derive(Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    source: Option<String>,
    pub line_col: Option<((usize, usize), (usize, usize))>,
}

impl Span {
    #[must_use]
    pub const fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            source: None,
            line_col: None,
        }
    }

    #[must_use]
    pub fn get_line_col(&mut self) -> ((usize, usize), (usize, usize)) {
        if self.source.is_none() {
            self.get_src()
        }
        let lookup = LineColLookup::new(self.source.as_ref().unwrap());
        (lookup.get(self.start), lookup.get(self.end))
    }

    pub fn set_line_col(&mut self) {
        if self.line_col.is_none() {
            self.line_col = Some(self.get_line_col())
        }
    }

    #[must_use]
    pub fn comp_line_col(&mut self, other: &mut Self) -> bool {
        self.get_line_col().0 .0 == other.get_line_col().1 .0
    }

    pub fn get_src(&mut self) {
        if self.source.is_none() {
            self.source = Some(read_to_string("./test.unamed").unwrap())
        }
    }
}

impl GetSpanTrait for Span {
    fn get_span(&self) -> Span {
        self.clone()
    }
    fn get_option_span(&self) -> Option<Span> {
        Some(self.clone())
    }
}

impl SourceCode for Span {
    fn read_span<'a>(
        &'a self,
        _span: &SourceSpan,
        _context_lines_before: usize,
        _context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
        let span: SourceSpan = (self.clone()).into();
        let contents = self.source.as_ref().unwrap().read_span(&span, 1, 1)?;
        Ok(Box::new(MietteSpanContents::new_named(
            "./test.unamed".to_string(),
            contents.data(),
            *contents.span(),
            contents.line(),
            contents.column(),
            contents.line_count(),
        )))
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Span::new(value, value)
    }
}

impl From<(usize, usize)> for Span {
    fn from(value: (usize, usize)) -> Self {
        Span::new(value.0, value.1)
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        (value.start..value.end + 1).into()
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}
