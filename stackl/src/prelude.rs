use std::convert;
use std::fmt;
use std::ops::Range;

pub type ErrorRecord = (String, Option<Span>); // maybe make this an actual struct/enum and implement From<String> for it

pub fn error<T>(message: String, span: Option<Span>) -> Result<T, (String, Option<Span>)> {
    Err((message, span))
}

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

    pub fn between(from: &Self, to: &Self) -> Self {
        let start = from.offset;
        let length = (to.offset + to.length) - start;
        let (line, column) = (from.line, from.column);
        Self::new(start, length, line, column)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            offset: usize::default(),
            length: usize::default(),
            line: usize::default(),
            column: usize::default(),
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
    number_of_lines: usize,
}

impl Default for Source {
    fn default() -> Self {
        Self {
            name: String::new(),
            text: vec![],
            length: 0,
            number_of_lines: 0,
        }
    }
}

#[allow(dead_code)]
impl Source {
    pub fn new(name: String, text: String) -> Self {
        let text = text.chars().collect::<Vec<char>>();
        let length = text.len();
        let number_of_lines = text
            .iter()
            .fold(1, |old, c| old + if c.is_newline() { 1 } else { 0 });
        Self {
            name,
            text,
            length,
            number_of_lines,
        }
    }

    pub fn from_span(&self, span: &Span) -> String {
        let mut span = *span;
        if span.offset > self.text.len() {
            span.offset = self.text.len()
        }
        self.text[<Range<usize>>::from(span)]
            .iter()
            .collect::<String>()
    }

    pub fn get(&self, index: usize) -> Option<char> {
        self.text.get(index).copied()
    }

    /// Find the nth line
    pub fn find_line(&self, n: usize) -> Option<Span> {
        let mut n = n;
        let mut pos = 0;
        let mut start = None;
        let mut end = None;
        while let Some(c) = self.text.get(pos) {
            if c.is_newline() {
                if n == 0 {
                    if start.is_some() {
                        end = Some(pos);
                        break;
                    } else {
                        start = Some(pos)
                    }
                } else {
                    n -= 1;
                }
            }
            pos += 1;
        }
        if let Some(end_pos) = end {
            let start_pos = start.unwrap();
            let length = end_pos - start_pos;
            Some(Span::new(start_pos, length, n, 0))
        } else {
            None
        }
    }

    /// get the line that a span is on
    pub fn get_line(&self, span: Span) -> Span {
        let mut end = span.offset;
        let start = if span.column == 1 {
            span.offset - 1
        } else {
            self.find_newline(span.offset, -1) + 1 // one char after prior newline
        };
        loop {
            if let Some(c) = self.get(end) {
                if c.is_newline() {
                    end += 1;
                    break;
                } else {
                    end += 1;
                }
            } else {
                end = self.length - 1;
                break;
            }
        }
        let mut length = end - (start - 1);
        if self.get(start + length).is_none() {
            length -= 1
        }
        Span::new(start, length, span.line, 1)
    }

    /// Find index of last character
    pub fn end(&self) -> usize {
        self.length - 1
    }

    /// Find the last line
    pub fn last_line(&self) -> Span {
        self.get_line(Span::new(self.end(), 0, self.number_of_lines, 0))
    }
}

impl Source {
    /// Finds the index into source such that there are n newlines before/behind the position pos
    /// Positive n means newlines after pos
    /// Negative n means newline before pos
    fn find_newline(&self, pos: usize, n: isize) -> usize {
        use convert::TryInto;
        use std::ops::{Add, Sub};
        let n = -n;
        let f = match n {
            n if n > 0 => Sub::sub,
            n if n < 0 => Add::add,
            0 => return pos,
            _ => panic!(),
        };
        let mut n = n;
        let mut pos = pos;
        while let Some(c) = self.get(pos) {
            if n == 0 {
                break;
            }
            if c.is_newline() {
                n = f(n, 1);
                if n == 0 {
                    break;
                }
            }
            pos = f(pos.try_into().unwrap(), 1).try_into().unwrap();
        }
        pos
    }
}
