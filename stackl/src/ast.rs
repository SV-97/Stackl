use super::prelude::*;
use super::tokenizer::Tok;

use std::convert::TryFrom;
use std::str::FromStr;

trait NodeVisitor {
    fn visit<T>(node: Node) -> T;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub span: Span,
    node: NodeType,
}

#[derive(Clone, Debug, PartialEq)]
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
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Identifier {
        name: String,
    },
    Assignment {
        left_val: Box<Node>,
        right_val: Box<Node>,
    },
    Expression {
        expression: Box<Node>,
    },
    Block {
        expressions: Vec<Node>,
    },
    If {
        condition: Box<Node>,
        if_true: Option<Box<Node>>, // not currently used but may but useful for something like rubys `unless`
        if_false: Option<Box<Node>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Greater,
    Less,
    LessOrEq,
    GreaterOrEq,
}

#[derive(Clone, Debug, PartialEq)]
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

    pub fn span(&self) -> Span {
        self.span
    }
}

pub type ConversionResult = Result<NodeType, String>;

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

    pub fn identifier(name: String) -> Self {
        NodeType::Identifier { name }
    }

    pub fn float(value: &str) -> ConversionResult {
        let parsed = f64::from_str(value);
        match parsed {
            Ok(val) => Ok(NodeType::FloatLiteral(val)),
            Err(e) => Err(format!("Failed conversion of string to float. {:?}", e)),
        }
    }

    pub fn integer(value: &str) -> ConversionResult {
        let parsed = i64::from_str(value);
        match parsed {
            Ok(val) => Ok(NodeType::IntegerLiteral(val)),
            Err(e) => Err(format!("Failed conversion of string to integer. {:?}", e)),
        }
    }

    pub fn assignment(left_val: Node, right_val: Node) -> Self {
        NodeType::Assignment {
            left_val: Box::new(left_val),
            right_val: Box::new(right_val),
        }
    }

    pub fn expression(expression: Node) -> Self {
        NodeType::Expression {
            expression: Box::new(expression),
        }
    }

    pub fn block(expressions: Vec<Node>) -> Self {
        NodeType::Block {
            expressions
        }
    }

    pub fn if_expression(condition: Node, if_true: Option<Node>, if_false: Option<Node>) -> Self {
        NodeType::If {
            condition: Box::new(condition),
            if_true: if_true.map(Box::new),
            if_false: if_false.map(Box::new),
        }
    }
}
