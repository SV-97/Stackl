use super::prelude::*;
use super::tokenizer::{Tok, Token};

use std::convert::TryFrom;

trait NodeVisitor {
    fn visit<T>(node: Node) -> T;
}

pub struct Node {
    span: Span,
    node: NodeType,
}

pub enum NodeType {
    BinaryOp {
        operation: BinOp,
        left: Box<Node>,
        right: Box<Node>,
    },
    UnaryOp {
        operation: UnOp,
        val: Box<Node>,
    },
    Program {
        expressions: Vec<Node>,
    },
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
}

pub enum UnOp {
    Not,
    Minus,
}

impl TryFrom<Tok> for UnOp {
    type Error = String;

    fn try_from(tok: Tok) -> Result<Self, Self::Error> {
        match tok {
            Tok::Not => Ok(UnOp::Not),
            Tok::Sub => Ok(UnOp::Minus),
            x => Err(format!("Failed conversion of Token {:?} to ast::UnOp", x)),
        }
    }
}

impl Node {
    pub fn new(span: Span, node: NodeType) -> Self {
        Node { span, node }
    }
}

impl NodeType {
    pub fn program(expressions: Vec<Node>) -> Self {
        NodeType::Program { expressions }
    }

    pub fn binary_operation(operation: BinOp, left: Node, right: Node) -> Self {
        NodeType::BinaryOp {
            operation,
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    pub fn unary_operation(operation: UnOp, val: Node) -> Self {
        NodeType::UnaryOp {
            operation,
            val: Box::new(val),
        }
    }
}
