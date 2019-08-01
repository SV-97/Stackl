use super::fancyterm::*;
use super::prelude::*;

use super::*;

use std::convert::TryInto;
use std::io::prelude::*;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub struct Logger {
    source: Rc<Source>,
}

#[derive(Debug)]
pub enum Level {
    Warning,
    Error,
}

impl Logger {
    pub fn new(source: Rc<Source>) -> Self {
        Self { source }
    }

    /// Finds the index into source such that there are n newlines before/behind the position pos
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
        while let Some(c) = self.source.get(pos) {
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

    /// Print a message at span in source with provided printing params
    fn print_msg(&self, span_error: Span, params: Vec<&dyn AnsiCode>, level: &str, message: &str) {
        let start_pos = self.find_newline(span_error.offset, -1); // find prior newline
        let end_pos = self.find_newline(span_error.offset, 1); // find next newline
        let span_prior = Span::new(start_pos + 1, span_error.column - 2, span_error.line, 1);
        let span_after = Span::new(
            span_error.offset + span_error.length,
            end_pos
                .checked_sub(span_error.offset + span_error.length)
                .unwrap_or(0),
            span_error.line,
            span_error.column + span_error.length,
        );
        // print_colored!(error, params!(Color::Red));

        self.source.from_span(&span_prior);
        self.source.from_span(&span_error);
        self.source.from_span(&span_after);

        let rendered = self.render(span_prior, span_error, span_after, params, level, message);
        println!("\n{}\n", rendered);
        io::stdout().flush().unwrap()
    }

    fn render(
        &self,
        span_prior: Span,
        span_error: Span,
        span_after: Span,
        params: Vec<&dyn AnsiCode>,
        level: &str,
        message: &str,
    ) -> String {
        let aps = params!(Color::Blue, Modifier::Bold);

        let line_no = format!("{}", span_error.line);
        let prior = self.source.from_span(&span_prior);
        let after = self.source.from_span(&span_after);
        let error = {
            /*
            use super::functional::compose;
            let g: &Fn(_) -> _ = if after.is_empty() { &(|s: &str| s.trim_end()) } else { &(|s| s) };
            let h: &Fn(_) -> _ = if prior.is_empty() { &(|s: &str| s.trim_start()) } else { &(|s| s) };
            let raw_error = self.source.from_span(&span_error);
            colored!("{}", &params, compose(g, h)(&raw_error).to_string())
            */
            let raw_error = self.source.from_span(&span_error);
            let r1 = if after.is_empty() {
                raw_error.trim_end()
            } else {
                &raw_error
            };
            let r2 = if prior.is_empty() {
                raw_error.trim_start()
            } else {
                r1
            };
            colored!("{}", &params, r2)
        };
        let mut header_p = params!(Modifier::Bold);
        header_p.extend(params.clone());
        let mut buf = String::new();
        buf.push_str(&colored!("{}: ", header_p, level));
        buf.push_str(&colored!("{}\n", params!(Modifier::Bold), message));
        buf.push_str(&colored!("--> ", &aps));
        buf.push_str(&format!(
            "{}:{}:{}\n",
            self.source.name, span_error.line, span_error.column
        ));
        let indent = line_no.len() + 2;
        for _ in 0..indent {
            buf.push(' ');
        }
        buf.push_str(&colored!("|\n", &aps));
        buf.push_str(&colored!("{}  |  ", &aps, line_no));

        buf.push_str(&format!(
            "{}{}{}\n",
            prior.trim_start(),
            error,
            after.trim_end()
        ));
        for _ in 0..indent {
            buf.push(' ');
        }
        buf.push_str(&colored!("|  ", &aps));
        for _ in 0..span_prior.length {
            buf.push(' ');
        }
        let mut squiggl = String::new();
        for _ in 0..span_error.length {
            squiggl.push('^');
        }
        buf.push_str(&colored!(squiggl, params));
        buf
    }

    pub fn log(&self, span: Span, message: &str, level: Level) {
        let (level, params) = match level {
            Level::Warning => ("Warning", params!(Color::Yellow, Modifier::Bold)),
            Level::Error => ("Error", params!(Color::Red, Modifier::Bold)),
        };
        self.print_msg(span, params, level, message);
    }
}
