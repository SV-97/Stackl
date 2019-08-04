use super::ast::*;
use super::prelude::*;
use super::reporter::*;
use super::tokenizer::*;

use std::collections::{HashSet, VecDeque};
use std::convert::TryFrom;
use std::rc::Rc;
use std::result::Result;

type ParseResult = Result<Node, ErrorRecord>;

static COMPARISON_OPS: [BinOp; 4] = [
    BinOp::Greater,
    BinOp::GreaterOrEq,
    BinOp::Less,
    BinOp::LessOrEq,
];
static EQUALITY_OPS: [BinOp; 2] = [BinOp::Equal, BinOp::NotEqual];
static LINE_OPS: [BinOp; 2] = [BinOp::Add, BinOp::Sub];
static DOT_OPS: [BinOp; 3] = [BinOp::Mul, BinOp::Div, BinOp::Mod];

#[derive(Debug, Clone, PartialEq)]
pub struct Parser {
    tokenizer: Tokenizer,
    current_token: Option<Token>,
    token_buffer: VecDeque<Token>,
    source: Rc<Source>,
    logger: Option<Rc<Logger>>,
    comparison_set: HashSet<BinOp>,
    equality_set: HashSet<BinOp>,
    line_set: HashSet<BinOp>,
    dot_set: HashSet<BinOp>,
    or_set: HashSet<BinOp>,
    and_set: HashSet<BinOp>,
}

impl Parser {
    pub fn new(tokenizer: Tokenizer, source: Rc<Source>, logger: Option<Rc<Logger>>) -> Self {
        let mut or_set = HashSet::new();
        or_set.insert(BinOp::Or);
        let mut and_set = HashSet::new();
        and_set.insert(BinOp::And);
        let mut parser = Parser {
            tokenizer,
            current_token: None,
            token_buffer: VecDeque::new(),
            source,
            logger,
            comparison_set: COMPARISON_OPS.iter().cloned().collect::<HashSet<BinOp>>(),
            equality_set: EQUALITY_OPS.iter().cloned().collect::<HashSet<BinOp>>(),
            line_set: LINE_OPS.iter().cloned().collect::<HashSet<BinOp>>(),
            dot_set: DOT_OPS.iter().cloned().collect::<HashSet<BinOp>>(),
            or_set,
            and_set,
        };
        parser.advance();
        parser
    }

    pub fn parse(&mut self) -> Node {
        let node = self.program().expect("Parsing failed");
        node
    }

    pub fn get_tokenizer(self) -> Tokenizer {
        self.tokenizer
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

    /// Put a token that's been taken out with `take_token` back.
    /// Panics if there already is a token in `current_token`.
    fn put_back(&mut self, tok: Token) {
        if self.current_token.is_none() {
            self.current_token = Some(tok);
        } else {
            panic!("Fatal error: couldn't put token {:?} back.", tok)
        }
    }

    fn take_token(&mut self) -> Result<Token, ErrorRecord> {
        self.current_token.take().ok_or_else(|| {
            let msg = ended_early();
            let span = self.source.end();
            (msg, Some(span))
        })
    }
}

#[allow(dead_code)]
impl Parser {
    // higher order parsing stuff
    /// Parse rules like `left_expr ((alternative of validation_set) right_expr)*`
    fn binop(
        &mut self,
        mut left_expression: impl FnMut(&mut Self) -> ParseResult,
        mut right_expression: impl FnMut(&mut Self) -> ParseResult,
        validation_set: impl Fn(&Self) -> &HashSet<BinOp>,
    ) -> ParseResult {
        let mut left_expr = left_expression(self)?;
        loop {
            left_expr = if let Some(op) = self.try_binop(&validation_set) {
                let right_expr = right_expression(self)?;
                let span = Span::between(&left_expr.span(), &right_expr.span());
                let node = Node::new(span, NodeType::binary_operation(op, left_expr, right_expr));
                node
            } else {
                break;
            };
        }
        Ok(left_expr)
    }

    /// Parse rules like `left_expr ((alternative of validation_set) right_expr)?`
    fn unstackable_binop(
        &mut self,
        mut left_expression: impl FnMut(&mut Self) -> ParseResult,
        mut right_expression: impl FnMut(&mut Self) -> ParseResult,
        validation_set: impl Fn(&Self) -> &HashSet<BinOp>,
    ) -> ParseResult {
        let left_expr = left_expression(self)?;
        if let Some(op) = self.try_binop(validation_set) {
            let right_expr = right_expression(self)?;
            let span = Span::between(&left_expr.span(), &right_expr.span());
            let node = Node::new(span, NodeType::binary_operation(op, left_expr, right_expr));
            Ok(node)
        } else {
            Ok(left_expr)
        }
    }

    /// Match current token against a set of binary operations and return it if it's in there
    fn try_binop(&mut self, validation_set: impl Fn(&Self) -> &HashSet<BinOp>) -> Option<BinOp> {
        let token = self.take_token().ok()?;
        let ttype = token.ttype;
        let operation = BinOp::try_from(ttype).ok();
        if let Some(op) = operation {
            if !validation_set(self).contains(&op) {
                self.put_back(token);
                return None;
            }
            self.advance();
            Some(op)
        } else {
            self.put_back(token);
            None
        }
    }
}

impl Parser {
    // Productions
    fn program(&mut self) -> ParseResult {
        let mut expressions = Vec::new();

        loop {
            match self.expression() {
                Ok(node) => expressions.push(node),
                Err((message, span)) => {
                    if let Some(logger) = &self.logger {
                        logger.log(span.unwrap(), &message, Level::Error);
                    }
                    break;
                }
            }
        }
        let span = if expressions.len() > 0 {
            Span::between(
                &expressions.first().unwrap().span(),
                &expressions.last().unwrap().span(),
            )
        } else {
            Span::default()
        };
        let program = NodeType::program(expressions);
        let node = Node::new(span, program);
        Ok(node)
    }

    fn expression(&mut self) -> ParseResult {
        self.expr()
    }

    fn expr(&mut self) -> ParseResult {
        self.or_expr()
    }

    fn or_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.and_expr();
        let right_expr = left_expr.clone();
        self.binop(left_expr, right_expr, |s| &s.or_set)
    }

    fn and_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.comparison_expr();
        let right_expr = left_expr.clone();
        self.binop(left_expr, right_expr, |s| &s.and_set)
    }

    fn comparison_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.equality_expr();
        let right_expr = left_expr.clone();
        self.unstackable_binop(left_expr, right_expr, |s| &s.comparison_set)
    }

    fn equality_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.line_expr();
        let right_expr = left_expr.clone();
        self.unstackable_binop(left_expr, right_expr, |s| &s.equality_set)
    }

    fn line_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.dot_expr();
        let right_expr = left_expr.clone();
        self.binop(left_expr, right_expr, |s| &s.line_set)
    }

    fn dot_expr(&mut self) -> ParseResult {
        let left_expr = |s: &mut Self| s.unary_expr();
        let right_expr = left_expr.clone();
        self.binop(left_expr, right_expr, |s| &s.dot_set)
    }

    fn unary_expr(&mut self) -> ParseResult {
        let token = self.take_token()?;
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
            _ => {
                self.put_back(token);
                self.primary_expr()
            }
        }
    }

    fn primary_expr(&mut self) -> ParseResult {
        let token = self.take_token()?;
        match token {
            tok @ Token { ttype: Tok::If, .. } => {
                self.put_back(tok);
                self.if_expression()
            }
            tok @ Token {
                ttype: Tok::Colon, ..
            } => {
                self.put_back(tok);
                self.block()
            }
            tok @ Token {
                ttype: Tok::LPar, ..
            } => {
                self.put_back(tok);
                self.parenthesized_expr()
            }
            tok @ Token {
                ttype: Tok::Identifier,
                ..
            } => match self.peek() {
                Some(Token {
                    ttype: Tok::Assign, ..
                }) => {
                    self.put_back(tok);
                    self.assignment_expr()
                }
                _ => {
                    self.put_back(tok);
                    self.literal()
                }
            },
            tok @ Token {
                ttype: Tok::Integer,
                ..
            }
            | tok @ Token {
                ttype: Tok::Float, ..
            }
            | tok @ Token {
                ttype: Tok::ScientificFloat,
                ..
            }
            | tok @ Token {
                ttype: Tok::True, ..
            }
            | tok @ Token {
                ttype: Tok::False, ..
            } => {
                self.put_back(tok);
                self.literal()
            }
            tok @ Token { .. } => {
                let span = tok.span;
                self.put_back(tok);
                error("Couldn't match primary expression.".to_string(), Some(span))
            }
        }
    }

    fn if_expression(&mut self) -> ParseResult {
        // todo: add else and maybe unless
        let if_tok = self.take_token()?;
        self.advance();
        let condition = self.expression()?;
        let block = self.block()?;
        let span = Span::between(&if_tok.span, &block.span());
        let node = Node::new(span, NodeType::if_expression(condition, Some(block), None));
        Ok(node)
    }

    fn block(&mut self) -> ParseResult {
        let colon = self.take_token()?;
        self.advance();
        let mut expressions = Vec::new();
        while let Ok(expression) = self.expression() {
            expressions.push(expression);
        }
        let token = self.take_token()?;
        self.advance();
        match token {
            Token {
                ttype: Tok::End,
                span: end_span,
            } => {
                let span = Span::between(&colon.span, &end_span);
                let node = Node::new(span, NodeType::block(expressions));
                Ok(node)
            }
            t => unexpected(Tok::End, t),
        }
    }

    fn parenthesized_expr(&mut self) -> ParseResult {
        let lpar_tok = self.take_token()?; // ttype already confirmed by caller
        self.advance();
        let expression = self.expression()?;
        let token = self.take_token()?;
        self.advance();
        match token {
            Token {
                ttype: Tok::RPar,
                span: rpar_span,
            } => {
                let span = Span::between(&lpar_tok.span, &rpar_span);
                let node = Node::new(span, NodeType::expression(expression));
                Ok(node)
            }
            t => unexpected(Tok::RPar, t),
        }
    }

    fn assignment_expr(&mut self) -> ParseResult {
        let id_tok = self.take_token()?;
        let id_name = self.source.from_span(&id_tok.span);
        let identifier = Node::new(id_tok.span, NodeType::identifier(id_name));
        self.advance();
        let _equals = self.take_token()?;
        self.advance();
        let expr = self.expression()?;
        let span = Span::between(&identifier.span, &expr.span);
        let node = Node::new(span, NodeType::assignment(identifier, expr));
        Ok(node)
    }

    fn literal(&mut self) -> ParseResult {
        let token = self.take_token()?;
        self.advance();
        match token {
            Token {
                ttype: Tok::Identifier,
                span,
            } => {
                let name = self.source.from_span(&span);
                let node = Node::new(span, NodeType::identifier(name));
                Ok(node)
            }
            Token {
                ttype: Tok::Float,
                span,
            }
            | Token {
                ttype: Tok::ScientificFloat,
                span,
            } => {
                let value = self.source.from_span(&span);
                let node = Node::new(span, NodeType::float(&value)?);
                Ok(node)
            }
            Token {
                ttype: Tok::Integer,
                span,
            } => {
                let value = self.source.from_span(&span);
                let node = Node::new(span, NodeType::integer(&value)?);
                Ok(node)
            }
            Token {
                ttype: Tok::True,
                span,
            } => {
                let node = Node::new(span, NodeType::BoolLiteral(true));
                Ok(node)
            }
            Token {
                ttype: Tok::False,
                span,
            } => {
                let node = Node::new(span, NodeType::BoolLiteral(false));
                Ok(node)
            }
            Token { span, .. } => error("Couldn't match literal value.".to_string(), Some(span)),
        }
    }
}

fn ended_early() -> String {
    "Output ended early".to_string()
}

fn unexpected<T>(expected: Tok, got_instead: Token) -> Result<T, (String, Option<Span>)> {
    error(
        format!(
            "Couldn't match block expression: Expected {:?}, got {:?} instead",
            expected, got_instead
        ),
        Some(got_instead.span),
    )
}
