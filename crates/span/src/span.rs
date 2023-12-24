use miette::{SourceSpan, SourceCode};
use line_col::LineColLookup;
use std::{fs::read_to_string, fmt::Debug};

#[derive(Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    source: Option<String>
}

impl Span {
    #[must_use]
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end, source: None }
    }

    #[must_use]
    pub fn get_line_col(&mut self) -> ((usize, usize), (usize, usize)) {
        self.get_src();
        let lookup = LineColLookup::new(self.source.as_ref().unwrap());
        (lookup.get(self.start), lookup.get(self.end))
    }

    #[must_use]
    pub fn comp_line_col(&mut self, other: &mut Self) -> bool {
        self.get_line_col().0.0 == other.get_line_col().1.0
    }

    pub fn get_src(&mut self) {
        if self.source.is_none() {
            self.source = Some(read_to_string("./test.unamned").unwrap())
        }
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
            self.source.as_ref().unwrap().read_span(&span, 1, 1)
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
        (value.start..value.end).into()
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}