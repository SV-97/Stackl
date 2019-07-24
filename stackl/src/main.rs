mod prelude;
mod tokenizer;
#[macro_use]
#[allow(dead_code)]
mod fancyterm;
mod reporter;

use fancyterm::*;
use prelude::*;
use reporter::*;
use tokenizer::*;

use std::env::args;
use std::fs::File;
use std::io;
use std::io::prelude::*;

/// Read a the contents of a file relative to the project's root directory
fn read_file(filename: &str) -> String {
    std::path::Path::new(filename);
    let path = &filename;
    let mut file = File::open(path).expect("Failed to open file");
    let mut source = String::new();
    file.read_to_string(&mut source)
        .expect("Failed to read file");
    source
}

fn main() {
    let mut args = args().collect::<Vec<String>>();
    args.push("/home/sv-97/GitHub/Stackl/stackl/examples/src1.stackl".to_string()); // only for debugging
    let filename = &args[1];
    let source = read_file(filename);

    let source = Source::new(filename.clone(), source);
    let logger = Logger::new(source.clone());
    let tokens = Tokenizer::new(source.clone(), Some(logger));
    for tok in tokens {
        /* println!(
            "{}{} {:?}",
            tok,
            colored!(":", params!(Modifier::Faint)),
            source.from_span(&tok.span)
        ); */
    }
}

/*
#[allow(dead_code)]
fn main() {
    let s = "a = 5.0\nb = 1.0\nif a + b >= 3:\n    print(a)\nend".to_string();
    let l = Logger::new(s);
    l.print_msg(Span::new(19, 5, 3, 4), "You fucked up bad, exactly here");
}

#[derive(Clone, Debug, PartialEq)]
struct Logger {
    text: Vec<char>,
}

#[allow(dead_code)]
impl Logger {
    pub fn new(text: String) -> Self {
        let text = text.chars().collect::<Vec<char>>();
        Logger { text }
    }

    /// Finds the index into text such that there are n newlines before/behind the position pos
    /// Positive n means newlines after pos
    /// Negative n means newline before pos
    fn find_newline(&self, pos: usize, n: isize) -> usize {
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
        while let Some(c) = self.text.get(pos) {
            if n == 0 {
                break;
            }
            if c.is_newline() {
                n = f(n, 1);
            }
            pos = f(pos.try_into().unwrap(), 1).try_into().unwrap();
        }
        pos
    }

    fn print_msg(&self, span: Span, msg: &str) {
        let start_pos = self.find_newline(span.offset, -1) + 1; // go one newline back
        let end_pos = self.find_newline(span.offset, 1); // go one newline forward
        print!(
            // line above error
            "{}",
            self.text[start_pos..span.offset]
                .into_iter()
                .collect::<String>()
        );
        let err_end = span.offset + span.length;
        let error = self.text[span.offset..span.offset + span.length]
            .into_iter()
            .collect::<String>();
        print_colored!(error, params!(Color::Red));
        print!(
            "{}",
            self.text[err_end..end_pos].into_iter().collect::<String>()
        );
        let mut buf = String::new();
        for _ in 0..span.column - 1 {
            buf.push(' ');
        }
        for _ in 0..span.length {
            buf.push('^');
        }
        println!("{}\n{}", buf, msg);
        println!("Line: {}, Column: {}", span.line, span.column);
        io::stdout().flush().unwrap();
    }
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
}

trait IsNewline {
    fn is_newline(self) -> bool;
}

impl IsNewline for char {
    fn is_newline(self) -> bool {
        self == '\n'
    }
}
*/
