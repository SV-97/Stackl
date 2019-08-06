use super::fancyterm::*;
use super::prelude::*;

use super::*;

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
    Information,
}

impl Logger {
    pub fn new(source: Rc<Source>) -> Self {
        Self { source }
    }

    /// Print a message at span in source with provided printing params
    fn print_msg(&self, span_error: Span, params: Vec<&dyn AnsiCode>, level: &str, message: &str) {
        let line = self.source.get_line(span_error);
        let start_pos = line.offset;
        let end_pos = start_pos + line.length;
        let span_prior = Span::new(
            start_pos,
            span_error.column().checked_sub(2).unwrap_or(0),
            span_error.line(),
            1,
        );
        let span_after = Span::new(
            span_error.offset + span_error.length,
            end_pos
                .checked_sub(span_error.offset + span_error.length + 1)
                .unwrap_or(0),
            span_error.line(),
            span_error.column() + span_error.length,
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

        let line_no = format!("{}", span_error.line());
        let prior = self.source.from_span(&span_prior);
        let after = self.source.from_span(&span_after);
        let error = {
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
            self.source.name, span_error.line(), span_error.column()
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
            Level::Information => ("Information", params!(Color::Cyan, Modifier::Bold)),
        };
        self.print_msg(span, params, level, message);
    }
}
