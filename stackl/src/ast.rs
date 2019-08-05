use super::prelude::*;
use super::tokenizer::Tok;

use std::convert::TryFrom;
use std::fmt;
use std::str::FromStr;

pub type Float = f64;
pub type Int = i64;

pub trait NodeVisitor<T> {
    fn visit(&mut self, root: &Node) -> T;
}

pub trait NodeTransformer<T> {
    fn visit(&mut self, root: Node) -> T;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub span: Span,
    pub node: NodeType,
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
    IntegerLiteral(Int),
    FloatLiteral(Float),
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
    Empty,
    Error(String),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
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
    Equal,
    NotEqual,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BinOp::*;
        write!(
            f,
            "{}",
            match self {
                Add => "+",
                Sub => "-",
                Mul => "*",
                Div => "/",
                Mod => "%",
                And => "and",
                Or => "or",
                Equal => "==",
                NotEqual => "!=",
                Greater => ">",
                Less => "<",
                LessOrEq => "<=",
                GreaterOrEq => ">=",
            }
        )
    }
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use UnOp::*;
        write!(
            f,
            "{}",
            match self {
                Not => "not",
                Minus => "-",
            }
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum UnOp {
    Not,
    Minus,
}

impl TryFrom<Tok> for BinOp {
    type Error = ErrorRecord;

    fn try_from(tok: Tok) -> Result<Self, Self::Error> {
        match tok {
            Tok::Add => Ok(BinOp::Add),
            Tok::Sub => Ok(BinOp::Sub),
            Tok::Mul => Ok(BinOp::Mul),
            Tok::Div => Ok(BinOp::Div),
            Tok::Mod => Ok(BinOp::Mod),
            Tok::And => Ok(BinOp::And),
            Tok::Or => Ok(BinOp::Or),
            Tok::Greater => Ok(BinOp::Greater),
            Tok::Less => Ok(BinOp::Less),
            Tok::GreatOrEq => Ok(BinOp::GreaterOrEq),
            Tok::LessOrEq => Ok(BinOp::LessOrEq),
            Tok::Equal => Ok(BinOp::Equal),
            Tok::NotEqual => Ok(BinOp::NotEqual),
            _ => error("Invalid binary operation".to_string(), None),
        }
    }
}

impl TryFrom<Tok> for UnOp {
    type Error = ErrorRecord;

    fn try_from(tok: Tok) -> Result<Self, Self::Error> {
        match tok {
            Tok::Not => Ok(UnOp::Not),
            Tok::Sub => Ok(UnOp::Minus),
            x => error(
                format!("Failed conversion of Token {:?} to ast::UnOp", x),
                None,
            ),
        }
    }
}

impl Default for NodeType {
    fn default() -> Self {
        NodeType::Empty
    }
}

impl Default for Node {
    fn default() -> Self {
        Node::new(Span::default(), NodeType::default())
    }
}

impl Node {
    pub fn new(span: Span, node: NodeType) -> Self {
        Node { span, node }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn node_type(&self) -> &NodeType {
        &self.node
    }

    pub fn is_constant(&self) -> bool {
        self.node.is_constant()
    }
}

impl NodeType {
    pub fn is_constant(&self) -> bool {
        use NodeType::*;
        match self {
            Assignment {
                right_val,
                ..
            } => right_val.is_constant(),
            BinaryOp {
                left,
                right,
                ..
            } => left.is_constant() && right.is_constant(),
            UnaryOp {
                val,
                ..
            } => val.is_constant(),
            Block { expressions }
            | Program { expressions} => expressions.iter().all(Node::is_constant),
            Expression { expression } => expression.is_constant(),
            If { condition, .. } => condition.is_constant(),
            Empty
            | IntegerLiteral(_)
            | FloatLiteral(_)
            | StringLiteral(_)
            | BoolLiteral(_) => true,
            Error(_)
            | Identifier { .. } => false,
        }
    }
}

pub type ConversionResult = Result<NodeType, ErrorRecord>;

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
            Err(e) => error(
                format!("Failed conversion of string to float. {:?}", e),
                None,
            ),
        }
    }

    pub fn integer(value: &str) -> ConversionResult {
        let parsed = i64::from_str(value);
        match parsed {
            Ok(val) => Ok(NodeType::IntegerLiteral(val)),
            Err(e) => error(
                format!("Failed conversion of string to integer. {:?}", e),
                None,
            ),
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
        NodeType::Block { expressions }
    }

    pub fn if_expression(condition: Node, if_true: Option<Node>, if_false: Option<Node>) -> Self {
        NodeType::If {
            condition: Box::new(condition),
            if_true: if_true.map(Box::new),
            if_false: if_false.map(Box::new),
        }
    }
}
