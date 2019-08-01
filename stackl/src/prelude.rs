use std::convert;
use std::fmt;
use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(offset: usize, length: usize, line: usize, column: usize) -> Self {
        Span {
            offset,
            length,
            line,
            column,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Line: {}, Column: {}, Offset: {}, Length: {}",
            self.line, self.column, self.offset, self.length
        )
    }
}

impl convert::From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.offset..span.offset + span.length
    }
}

pub trait IsNewline {
    fn is_newline(self) -> bool;
}

impl IsNewline for char {
    fn is_newline(self) -> bool {
        self == '\n'
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Source {
    pub name: String,
    text: Vec<char>,
    length: usize,
}

impl Default for Source {
    fn default() -> Self {
        Self {
            name: String::new(),
            text: vec![],
            length: 0,
        }
    }
}

impl Source {
    pub fn new(name: String, text: String) -> Self {
        let text = text.chars().collect::<Vec<char>>();
        let length = text.len();
        Self { name, text, length }
    }

    pub fn from_span(&self, span: &Span) -> String {
        self.text[<Range<usize>>::from(*span)]
            .iter()
            .collect::<String>()
    }

    pub fn get(&self, index: usize) -> Option<char> {
        self.text.get(index).copied()
    }
}
