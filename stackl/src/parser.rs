use super::ast::*;
use super::prelude::*;
use super::tokenizer::*;

use std::collections::VecDeque;
use std::convert::TryFrom;
use std::result::Result;

type ParseResult = Result<Node, String>;

pub struct Parser {
    tokenizer: Tokenizer,
    current_token: Option<Token>,
    token_buffer: VecDeque<Token>,
}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        let mut parser = Parser {
            tokenizer,
            current_token: None,
            token_buffer: VecDeque::new(),
        };
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Node {
        let node = self.program();
        node
    }
}

impl Iterator for Parser {
    type Item = ParseResult;

    fn next(&mut self) -> Option<Self::Item> {
        unimplemented!()
    }
}

impl Parser {
    // General purpose parsing methods
    fn advance(&mut self) {
        match self.token_buffer.pop_front() {
            t @ Some(_) => self.current_token = t,
            None => self.current_token = self.tokenizer.next(),
        }
    }

    fn peek(&mut self) -> Option<Token> {
        match self.token_buffer.front().cloned() {
            Some(t) => Some(t),
            None => {
                if let Some(t) = self.tokenizer.next() {
                    self.token_buffer.push_back(t);
                }
                self.token_buffer.front().cloned()
            }
        }
    }
}

impl Parser {
    // Productions
    fn program(&mut self) -> Node {
        let mut expresssions = Vec::new();

        while let Ok(node) = self.expression() {
            expresssions.push(node);
        }

        let program = NodeType::program(expresssions);
        let span = unimplemented!();
        Node::new(span, program)
    }

    fn expression(&mut self) -> ParseResult {
        unimplemented!() // self.expr()
    }

    fn expr(&mut self) -> ParseResult {
        self.or_expr()
    }

    fn or_expr(&mut self) -> ParseResult {
        let mut _l = self.and_expr();
        // todo
        unimplemented!()
    }

    fn and_expr(&mut self) -> ParseResult {
        let mut _l = self.comparison_expr();
        // todo
        unimplemented!()
    }

    fn comparison_expr(&mut self) -> ParseResult {
        let mut _l = self.equality_expr();
        // todo
        unimplemented!()
    }

    fn equality_expr(&mut self) -> ParseResult {
        let mut _l = self.line_expr();
        // todo
        unimplemented!()
    }

    fn line_expr(&mut self) -> ParseResult {
        let mut _l = self.dot_expr();
        // todo
        unimplemented!()
    }

    fn dot_expr(&mut self) -> ParseResult {
        let mut _l = self.unary_expr();
        // todo
        unimplemented!()
    }

    fn unary_expr(&mut self) -> ParseResult {
        let token = self.current_token.take().ok_or_else(ended_early)?;
        self.advance();
        match token {
            Token {
                ttype: ttype @ Tok::Not,
                span,
            }
            | Token {
                ttype: ttype @ Tok::Sub,
                span,
            } => {
                self.advance();
                let child = self.unary_expr()?;
                let op = UnOp::try_from(ttype)?;
                let _type = NodeType::unary_operation(op, child);
                Ok(Node::new(span, _type))
            }
            _ => self.primary_expr(),
        }
    }

    fn primary_expr(&mut self) -> ParseResult {
        let token = self.current_token.take().ok_or_else(ended_early)?;
        self.advance();
        match token {
            Token {
                ttype: Tok::Colon, ..
            } => self.block(),
            Token {
                ttype: Tok::LPar, ..
            } => self.parenthesized_expr(),
            Token {
                ttype: Tok::Identifier,
                ..
            } => match self.peek() {
                Some(Token {
                    ttype: Tok::Equal, ..
                }) => self.assignment_expr(),
                _ => self.literal(),
            },
            _ => Err("Couldn't match primary expression".to_string()),
        }
    }

    fn block(&mut self) -> ParseResult {
        unimplemented!()
    }

    fn parenthesized_expr(&mut self) -> ParseResult {
        unimplemented!()
    }

    fn assignment_expr(&mut self) -> ParseResult {
        unimplemented!()
    }

    fn literal(&mut self) -> ParseResult {
        unimplemented!()
    }
}

fn ended_early() -> String {
    "Output ended early".to_string()
}
