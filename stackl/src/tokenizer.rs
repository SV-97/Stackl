use super::prelude::*;

#[macro_use]
use super::*;

use super::reporter::*;

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Tok {
    Identifier,
    Float,
    ScientificFloat, // scientific notation
    Integer,
    Bool,   // True | False
    True,
    False,
    Assign, // =

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    Not, // not
    And, // and
    Or,  // or

    LPar,
    RPar,

    Equal,     // ==
    NotEqual,  // !=
    Greater,   // >
    Less,      // <
    LessOrEq,  // <=
    GreatOrEq, // >=
    If,        // if
    Else,      // else
    End,       // end

    Indent(usize),
    Colon,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub tok_type: Tok,
    pub span: Span,
}

impl Token {
    pub fn new(tok_type: Tok, span: Span) -> Self {
        Self { tok_type, span }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "<{}> {}",
            colored!("{:?}", params!(Color::Blue), self.tok_type),
            colored!("{}", params!(Modifier::Faint), self.span)
        )
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Tokenizer {
    text: Source,
    pos: usize,
    line: usize,
    column: usize,
    current_char: Option<char>,
    logger: Option<Logger>,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Self {
            text: Default::default(),
            pos: 0,
            line: 1,
            column: 1,
            current_char: None,
            logger: None,
        }
    }
}

impl Iterator for Tokenizer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(current_char) = self.current_char {
            match current_char {
                ':' => Some(self.colon()),
                '!' => Some(self.not_equal()),
                '=' if self.peek() == Some('=') => Some(self.equal()),
                '=' => Some(self.assign()),
                '>' if self.peek() == Some('=') => Some(self.greater_or_equal()),
                '>' => Some(self.greater()),
                '<' if self.peek() == Some('=') => Some(self.less_or_equal()),
                '<' => Some(self.less()),
                '+' => Some(self.add()),
                '-' => Some(self.sub()),
                '*' => Some(self.mul()),
                '/' => Some(self.div()),
                '%' => Some(self.modulo()),
                '(' => Some(self.left_par()),
                ')' => Some(self.right_par()),
                c if c.is_newline() => {
                    self.advance();
                    self.next()
                }
                c if c.is_whitespace() // if at least two whitespace characters in succession
                    && self.peek().map(|c| c.is_whitespace()).unwrap_or(false) =>
                {
                    Some(self.indent())
                }
                c if c.is_whitespace() => {
                    self.skip_whitespace();
                    self.next()
                }
                c if c.is_digit(10) => Some(self.number()),
                c if c.is_alphabetic() || c == '_' => Some(self.identifier()),
                c => panic!("{:?}", c),
            }
        } else {
            None
        }
    }
}

impl Tokenizer {
    pub fn new(source: Source, logger: Option<Logger>) -> Self {
        let mut lex: Self = Default::default();
        lex.text = source;
        lex.line = 1;
        lex.column = 1;
        lex.current_char = lex.text.get(0);
        lex.logger = logger;
        lex
    }

    fn log(&self, span: Span, message: &str, level: Level) {
        if let Some(l) = &self.logger {
            l.log(span, message, level);
        }
    }

    fn span(&self, length: usize) -> Span {
        Span::new(self.pos, length, self.line, self.column)
    }

    fn peek(&self) -> Option<char> {
        self.text.get(self.pos + 1)
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.current_char = self.text.get(self.pos);
        if let Some(c) = self.current_char {
            match c {
                c if c.is_newline() => {
                    self.line += 1;
                    self.column = 1;
                }
                _ => self.column += 1,
            }
            self.current_char = Some(c);
        }
    }

    fn skip_whitespace(&mut self) -> usize {
        let mut n = 0;
        while let Some(c) = self.current_char {
            if c.is_whitespace() && !c.is_newline() {
                self.advance();
                n += 1;
            } else {
                break;
            }
        }
        n
    }

    fn indent(&mut self) -> Token {
        let mut span = self.span(0);
        let n = self.skip_whitespace();
        span.length = n;
        Token::new(Tok::Indent(n), span)
    }

    /// Create a token of given type width a span length of text and advance once
    fn simple(&mut self, tok_type: Tok, text: &str) -> Token {
        let span = self.span(text.len());
        self.advance();
        Token::new(tok_type, span)
    }

    fn colon(&mut self) -> Token {
        self.simple(Tok::Colon, ":")
    }

    fn assign(&mut self) -> Token {
        self.simple(Tok::Assign, "=")
    }

    fn equal(&mut self) -> Token {
        let t = self.simple(Tok::Equal, "==");
        self.advance();
        t
    }

    fn not_equal(&mut self) -> Token {
        let mut s = self.span(2);
        self.advance();
        match self.current_char {
            Some('=') => {
                let t = Token::new(Tok::NotEqual, s);
                self.advance();
                t
            }
            c => {
                s.length = 1;
                let message = if let Some(c) = c {
                    format!("Expected '=' after '!', got {:?}, inferred '!='.", c)
                } else {
                    "Expected '=' after '!', ended early instead. Did you mean '!='?"
                        .to_string()
                };
                self.log(s, &message, Level::Warning);
                let t = Token::new(Tok::NotEqual, s);
                self.advance();
                t
            }
        }
    }

    fn greater_or_equal(&mut self) -> Token {
        let t = self.simple(Tok::GreatOrEq, ">=");
        self.advance();
        t
    }

    fn greater(&mut self) -> Token {
        self.simple(Tok::Greater, ">")
    }

    fn less_or_equal(&mut self) -> Token {
        let t = self.simple(Tok::LessOrEq, "<=");
        self.advance();
        t
    }

    fn less(&mut self) -> Token {
        self.simple(Tok::Less, "<")
    }

    fn add(&mut self) -> Token {
        self.simple(Tok::Add, "+")
    }
    fn sub(&mut self) -> Token {
        self.simple(Tok::Sub, "-")
    }
    fn mul(&mut self) -> Token {
        self.simple(Tok::Mul, "*")
    }
    fn div(&mut self) -> Token {
        self.simple(Tok::Div, "/")
    }
    fn modulo(&mut self) -> Token {
        self.simple(Tok::Mod, "%")
    }

    fn left_par(&mut self) -> Token {
        self.simple(Tok::LPar, "(")
    }

    fn right_par(&mut self) -> Token {
        self.simple(Tok::RPar, ")")
    }

    fn number(&mut self) -> Token {
        let mut length = 0;
        let mut span = self.span(0);
        let mut num_type = Tok::Integer;
        let mut error_char = None;
        while let Some(c) = self.current_char {
            match c {
                c if c.is_digit(10) => length += 1,
                '.' if length > 0 => {
                    match num_type {
                        Tok::Integer => num_type = Tok::Float,
                        Tok::Float => {
                            error_char = Some('.');
                            length +=1;
                            break;
                            },
                        _ => ()
                    }
                    length += 1;
                }
                'e' if length > 0 => {
                    if num_type == Tok::ScientificFloat {
                        error_char = Some('e');
                        length +=1; 
                        break;
                    }
                    num_type = Tok::ScientificFloat;
                    length += 1;
                }
                _ => break,
            }
            self.advance();
        }
        span.length = length;
        if let Some(error_char) = error_char {
            let mut error_span = span;
            while let Some(c) = self.current_char {
                if c.is_digit(10) || c == 'e' || c == '.' {
                    self.advance();
                    length += 1;
                } else {
                    break;
                }
            }
            error_span.length = length;
            let message = format!("Invalid number literal. Contains multiple {:?}.", error_char);
            self.log(error_span, &message, Level::Error);
        }

        Token::new(num_type, span)
    }

    fn identifier(&mut self) -> Token {
        let mut span = self.span(0);
        let mut buf = String::new();
        while let Some(char_) = self.current_char {
            if char_.is_alphanumeric() || char_ == '_' {
                buf.push(char_);
                self.advance();
            } else {
                break;
            }
        }

        span.length = buf.len();
        match buf.as_ref() {
            "if" => Token::new(Tok::If, span),
            "else" => Token::new(Tok::Else, span),
            "end" => Token::new(Tok::End, span),
            "false" => Token::new(Tok::False, span),
            "true" => Token::new(Tok::True, span),
            "not" => Token::new(Tok::Not, span),
            "and" => Token::new(Tok::And, span),
            "or" => Token::new(Tok::Or, span),
            _ => Token::new(Tok::Identifier, span),
        }
    }
}
