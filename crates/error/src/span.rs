use miette::{SourceSpan, SourceCode, SourceOffset};
use std::fs::read_to_string;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub(crate) source: String
}

impl Span {
    pub fn new(start: (usize, usize), end: (usize, usize)) -> Self {
        let source = read_to_string("./test.unamned").unwrap();
        Self { start, end, source }
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

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        let start: SourceOffset = SourceOffset::from_location(value.source.clone(), value.start.0, value.start.1);
        let end: SourceOffset = SourceOffset::from_location(value.source, value.end.0, value.end.1);
        println!("{:?} {:?}", value.start, value.end);
        let result = SourceSpan::new(start, end);
        println!("{:?}", result);
        result
    }
}

pub trait GetSpan {
    fn get_span(&self) -> Span;
}