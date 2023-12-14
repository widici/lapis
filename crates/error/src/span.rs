use miette::{SourceSpan, SourceCode};
use line_col::LineColLookup;
use std::{fs::read_to_string, fmt::Debug};

#[derive(Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    /// This needs to be owned by self for read_span
    pub(crate) source: String,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end, source: Span::get_src() }
    }

    pub fn get_line_col(&self) -> ((usize, usize), (usize, usize)) {
        let lookup = LineColLookup::new(&self.source);
        (lookup.get(self.start), lookup.get(self.end))
    }

    pub fn comp_line_col(&self, other: &Self) -> bool {
        self.get_line_col().0.0 == other.get_line_col().1.0
    }

    fn get_src() -> String {
        let source = read_to_string("./test.unamned").unwrap();
        return source
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
            self.source.read_span(&span, 1, 1)
    }
}

impl From<(&Span, &Span)> for Span {
    fn from(value: (&Span, &Span)) -> Self {
        Span::new(value.0.start, value.1.end)
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        (value.start..value.end).into()
        /*
        let start: SourceOffset = SourceOffset::from_location(value.source.clone(), value.start.0, value.start.1);
        let end: SourceOffset = SourceOffset::from_location(value.source, value.end.0, value.end.1);
        println!("{:?} {:?}", value.start, value.end);
        let result: SourceSpan = (start, end).into();
        println!("{:?}", result);
        result
        */
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}..{:?}", self.start, self.end)
    }
}

pub trait GetSpan {
    fn get_span(&self) -> Span;
}