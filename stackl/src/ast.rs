use super::prelude::*;
use super::tokenizer::Token;

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
