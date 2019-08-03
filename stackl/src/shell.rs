use super::fancyterm::*;

use std::io;
use std::io::Write;

/// Simple struct to allow reading strings from stdin
pub struct Shell<'a> {
    /// Name of the shell that's printed on read
    name: String,
    /// Seperator between the name and the actual text that's read
    in_seperator: String,
    /// Seperator that's put in front of output
    out_seperator: String,
    /// Modifier vectors for input, output and name printing
    in_ansi: Vec<&'a dyn AnsiCode>,
    out_ansi: Vec<&'a dyn AnsiCode>,
    name_ansi: Vec<&'a dyn AnsiCode>,
}

#[allow(dead_code)]
impl<'a> Shell<'a> {
    /// Constructor for new shells with default seperators
    /// in_seperator defaults to ">>> "
    /// out_seperator defaults to ""
    pub fn new(name: &str) -> Self {
        Shell {
            name: name.to_string(),
            in_seperator: ">>> ".to_string(),
            out_seperator: "".to_string(),
            in_ansi: vec![],
            out_ansi: vec![],
            name_ansi: vec![],
        }
    }

    pub fn set_seperators(&mut self, in_seperator: &str, out_seperator: &str) {
        self.in_seperator = in_seperator.to_string();
        self.out_seperator = out_seperator.to_string();
    }

    pub fn set_ansi(
        &mut self,
        in_ansi: Vec<&'a dyn AnsiCode>,
        out_ansi: Vec<&'a dyn AnsiCode>,
        name_ansi: Vec<&'a dyn AnsiCode>,
    ) {
        self.in_ansi = in_ansi;
        self.out_ansi = out_ansi;
        self.name_ansi = name_ansi;
    }

    /// Read a single line from stdin
    pub fn read(&self) -> String {
        print!("{}", self.colored_in_text());
        io::stdout().flush().unwrap();
        let mut input: String = "".to_string();
        io::stdin().read_line(&mut input).unwrap();
        input.trim().to_string()
    }

    /// Read until input is non empty
    /// Args:
    ///     hint: Hint that's displayed if user tries to enter an empty string
    pub fn read_force_input(&self, hint: Option<String>) -> String {
        loop {
            match self.read().as_ref() {
                "" => match hint {
                    None => continue,
                    Some(ref x) => self.writeln(x),
                },
                x => return x.to_string(),
            }
        }
    }

    /// Write text to stdout
    pub fn write<T: std::fmt::Display>(&self, text: T) {
        self.out_setup();
        print!("{}", text)
    }

    /// Write line to stdout
    pub fn writeln<T: std::fmt::Display>(&self, text: T) {
        self.out_setup();
        println!("{}", text)
    }

    fn out_setup(&self) {
        let mut buf = String::new();
        buf.push_str(&self.out_text());
        if let Some(margin) = self.in_text().len().checked_sub(self.out_text().len()) {
            for _ in 0..margin + 4 {
                buf.push(' ');
            }
        }
        print!("{}", buf);
    }

    fn out_text(&self) -> String {
        self.out_seperator.clone()
    }

    fn colored_out_text(&self) -> String {
        colored!(self.out_seperator, &self.out_ansi)
    }

    fn in_text(&self) -> String {
        format!("{} {}", self.name, self.in_seperator)
    }

    fn colored_in_text(&self) -> String {
        format!(
            "{} {}",
            colored!(self.name, &self.name_ansi),
            colored!(self.in_seperator, &self.in_ansi)
        )
    }
}
