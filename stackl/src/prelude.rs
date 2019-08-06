use std::convert;
use std::fmt;
use std::ops::Range;

pub type ErrorRecord = (String, Option<Span>); // maybe make this an actual struct/enum and implement From<String> for it

#[macro_export]
macro_rules! timeit {
    ( $a: expr ) => {{
        use std::time::Instant;
        let t1 = Instant::now();
        let temp = $a;
        let t2 = Instant::now();
        dbg!(t2.duration_since(t1));
        temp
    }};
}

pub fn error<T>(message: String, span: Option<Span>) -> Result<T, (String, Option<Span>)> {
    Err((message, span))
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub offset: usize,
    pub length: usize,
    line: usize,
    column: usize,
}

impl Span {
    /// Constructor - offsets start at 0, lines and columns at 1
    pub fn new(offset: usize, length: usize, line: usize, column: usize) -> Self {
        if line < 1 || column < 1 {
            panic!(
                "Columns and lines start at 1, you tried assigning line: {}, column {}",
                line, column
            )
        }
        Span {
            offset,
            length,
            line,
            column,
        }
    }

    pub fn set_line(&mut self, line: usize) {
        if line < 1 {
            panic!("Lines start at 1, you tried assigning {}", line)
        } else {
            self.line = line;
        }
    }

    pub fn set_column(&mut self, column: usize) {
        if column < 1 {
            panic!("Columns start at 1, you tried assigning {}", column)
        } else {
            self.column = column;
        }
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    /// Span starting at `from` and extending to the end of `to`
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
            line: 1,
            column: 1,
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
        if span.offset + span.length > self.text.len() {
            span.offset = self.text.len() - 1
        }
        self.text[<Range<usize>>::from(span)]
            .iter()
            .collect::<String>()
    }

    /// Get char from text buffer
    /// `index` is zero based
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

    /// get the span of a line that a span is on
    pub fn get_line(&self, span: Span) -> Span {
        let mut end = span.offset;
        let start = if span.column == 1 {
            span.offset
        } else {
            self.find_newline(span.offset, -1)
        };
        loop {
            if let Some(c) = self.get(end) {
                if c.is_newline() {
                    break;
                } else {
                    end += 1;
                }
            } else {
                end = self.length - 1;
                break;
            }
        }
        let mut length = end
            .checked_sub(start.checked_sub(1).unwrap_or(0))
            .unwrap_or(0);
        if self.get(start + length).is_none() {
            length = length.checked_sub(1).unwrap_or(1);
        }
        Span::new(start, length, span.line, 1)
    }

    /// Get offset of last character
    pub fn end(&self) -> usize {
        self.length
    }

    /// Find the last line
    pub fn last_line_span(&self) -> Span {
        if self.number_of_lines == 1 {
            Span::new(0, self.length, 1, 1)
        } else {
            self.get_line(Span::new(self.end(), 0, self.number_of_lines, 1))
        }
    }
}

impl Source {
    /// Finds the index into source such that there are n newlines before/behind the position pos
    /// Positive n means newlines after pos
    /// Negative n means newline before pos
    fn find_newline(&self, pos: usize, n: isize) -> usize {
        if self.number_of_lines == 1 {
            0 // kinda dirty fix
        } else {
            match n {
                n if n == 0 => pos,
                n if n < 0 => {
                    let mut n = n;
                    let mut pos = pos;
                    while let Some(c) = self.get(pos) {
                        if c.is_newline() {
                            n += 1;
                        }
                        if n == 0 {
                            pos += 1;
                            break;
                        }
                        let possible_pos = pos.checked_sub(1);
                        match possible_pos {
                            Some(p) => pos = p,
                            None => break,
                        }
                    }
                    pos
                }
                n if n > 0 => {
                    let mut n = n;
                    let mut pos = pos;
                    while let Some(c) = self.get(pos) {
                        if c.is_newline() {
                            n -= 1;
                        }
                        if n == 0 {
                            pos -= 1;
                            break;
                        }
                        pos += 1;
                    }
                    pos
                }
                _ => panic!("can't happen"),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_newline() {
        assert!('\n'.is_newline());
        assert!(!'a'.is_newline());
        assert!(!'2'.is_newline());
        assert!(!'\t'.is_newline());
    }

    #[test]
    fn test_span() {
        let span = Span::new(0, 0, 1, 1);
        assert_eq!(span, Span::default());

        let mut span2 = Span::new(100, 20, 3, 1); // offset 100, length 20, line 3 first column
        assert_eq!(span2.column(), 1);
        assert_eq!(span2.line(), 3);
        span2.set_column(5);
        assert_eq!(span2.column(), 5);
        let span3 = Span::between(&span, &span2);
        assert_eq!(span3, Span::new(0, 120, 1, 1));
    }

    #[test]
    #[should_panic]
    fn test_span_invariant_new() {
        Span::new(0, 0, 0, 10);
    }

    #[test]
    #[should_panic]
    fn test_span_invariant_set_column() {
        Span::new(0, 0, 1, 1).set_column(0);
    }

    #[test]
    #[should_panic]
    fn test_span_invariant_set_line() {
        Span::new(0, 0, 1, 1).set_line(0);
    }

    #[test]
    fn test_source() {
        let source = Source::new(
            "Test source".to_string(),
            "this is\njust some test\n    source.\n".to_string(),
        );
        assert_eq!(source.number_of_lines, 4);
        assert_eq!(source.length, 35);
        assert_eq!(source.end(), 35);
    }

    #[test]
    fn test_source_get_line() {
        let source = Source::new(
            "Test source".to_string(),
            "this is\njust some test\n    source.\n".to_string(),
        );
        // todo find out why some lines have a newline while others have not
        for i in 0..7 {
            let line_span = source.get_line(Span::new(i, 0, 1, i + 1));
            assert_eq!(line_span, Span::new(0, 7, 1, 1));
            assert_eq!(source.from_span(&line_span), "this is".to_string());
        }
        for i in 8..21 {
            let line_span = source.get_line(Span::new(i, 0, 1, i + 1 - 7));
            assert_eq!(line_span, Span::new(8, 15, 1, 1));
            assert_eq!(source.from_span(&line_span), "just some test\n".to_string());
        }
        for i in 23..34 {
            let line_span = source.get_line(Span::new(i, 0, 1, i + 1 - 21));
            assert_eq!(line_span, Span::new(23, 11, 1, 1));
            assert_eq!(source.from_span(&line_span), "    source.".to_string());
        }
        let line_span = source.get_line(Span::new(35, 0, 1, 1));
        assert_eq!(line_span, Span::new(35, 1, 1, 1));
        assert_eq!(source.from_span(&line_span), "\n".to_string());
    }

    #[test]
    fn test_source_last_line() {
        let source = Source::new("Test source".to_string(), "\n".to_string());
        assert_eq!(source.last_line_span(), Span::new(1, 1, 2, 1)); // I think that the length should actually be 0 here
    }
}
