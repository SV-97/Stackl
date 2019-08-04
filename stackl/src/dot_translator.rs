use super::ast::*;

use std::collections::HashMap;
use std::convert;

pub fn visit(root: &Node) -> String {
    let mut dot_translator = DotTranslator::new();
    dot_translator.visit(root)
}

struct DotTranslator {
    /// Emitted text
    buffer: String,
    counter: HashMap<String, usize>,
}

struct DotNode<'a> {
    root_name: &'a str,
    node: &'a NodeType,
}

impl<'a> DotNode<'a> {
    fn new(root_name: &'a str, node: &'a NodeType) -> Self {
        DotNode { root_name, node }
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
enum DotNodeType {
    BinaryOp(String),
    UnaryOp(String),
    Program,
    IntegerLiteral(String),
    FloatLiteral(String),
    StringLiteral(String),
    BoolLiteral(String),
    Identifier(String),
    Assignment,
    Expression,
    Block,
    If,
    Empty,
}

impl DotNodeType {
    fn identifier(&self) -> String {
        use DotNodeType::*;
        match self {
            BinaryOp(_) => "binary",
            UnaryOp(_) => "unary",
            Program => "program",
            IntegerLiteral(_) => "integer",
            FloatLiteral(_) => "float",
            StringLiteral(_) => "string",
            BoolLiteral(_) => "bool",
            Identifier(_) => "identifier",
            Assignment => "assignment",
            Expression => "expression",
            Block => "block",
            If => "if",
            Empty => "empty",
        }
        .to_string()
    }
}

impl convert::From<&NodeType> for DotNodeType {
    fn from(nt: &NodeType) -> Self {
        use DotNodeType as D;
        use NodeType::*;
        match nt {
            BinaryOp { operation, .. } => D::BinaryOp(format!("{}", operation)),
            UnaryOp { operation, .. } => D::UnaryOp(format!("{}", operation)),
            Program { .. } => D::Program,
            IntegerLiteral(x) => D::IntegerLiteral(format!("{}", x)),
            FloatLiteral(x) => D::FloatLiteral(format!("{}", x)),
            StringLiteral(s) => D::StringLiteral(format!("{:?}", s)),
            BoolLiteral(b) => D::BoolLiteral(format!("{}", b)),
            Expression { .. } => D::Expression,
            Block { .. } => D::Block,
            If { .. } => D::If,
            Identifier { name } => D::Identifier(name.to_string()),
            Assignment { .. } => D::Assignment,
            Empty => D::Empty,
        }
    }
}

impl convert::From<&DotNodeType> for (String, String) {
    fn from(node_type: &DotNodeType) -> Self {
        use DotNodeType::*;
        let node_type_identifier = node_type.identifier();
        let symbol = (match node_type {
            Program => "Program",
            IntegerLiteral(x) => &x,
            FloatLiteral(x) => &x,
            StringLiteral(x) => &x,
            BoolLiteral(x) => &x,
            Identifier(x) => &x,
            Assignment => "=",
            Expression => "Expression",
            Block => "Block",
            If => "If",
            BinaryOp(x) => &x, // may need modification
            UnaryOp(x) => &x,
            Empty => "",
        })
        .to_string();
        (node_type_identifier, symbol)
    }
}

impl DotTranslator {
    pub fn new() -> Self {
        use DotNodeType::*;
        let buffer = "
digraph AST {
    rankdir = TB;
    compound = true;
    labelloc = \"t\";
    label = <<B>Syntax-Tree</B><BR/>Read left to right top to bottom>;

"
        .to_string();
        let mut map = HashMap::new();
        let variants = [
            BinaryOp("".to_string()),
            UnaryOp("".to_string()),
            Program,
            IntegerLiteral("".to_string()),
            FloatLiteral("".to_string()),
            StringLiteral("".to_string()),
            BoolLiteral("".to_string()),
            Identifier("".to_string()),
            Assignment,
            Expression,
            Block,
            If,
            Empty,
        ];
        for variant in variants.iter() {
            map.insert(variant.identifier(), 0);
        }
        DotTranslator {
            buffer: buffer,
            counter: map,
        }
    }
}

impl DotTranslator {
    fn get_number(&mut self, node_type: &str) -> usize {
        let val = self.counter.get_mut(node_type).expect(&format!(
            "Unknown node type: {}. Map not properly initialized.",
            node_type
        ));
        *val += 1;
        *val
    }

    /// Write declaration to buffer and get name for node
    fn declare_node(&mut self, node: &NodeType, parameters: Option<&str>) -> String {
        let dot_node_type = DotNodeType::from(node);
        let (node_type_identifier, metadata) = <(String, String)>::from(&dot_node_type);
        let number = self.get_number(&node_type_identifier);
        let current_name = format!("{}{}", node_type_identifier, number);
        let parameters = parameters
            .map(|s| s.to_string())
            .unwrap_or(format!("label=\"{}\"", metadata));
        let declaration = format!("    {}[{}];\n", current_name, parameters);
        self.buffer.push_str(&declaration);
        current_name
    }

    fn dispatch(&mut self, name: &str, node: &NodeType) {
        use NodeType::*;

        let next_root =
            |s: &mut Self, node: &'_ Node| s.visit_helper(DotNode::new(name, node.node_type()));
        match node {
            BinaryOp { left, right, .. } => {
                next_root(self, left);
                next_root(self, right);
            }
            UnaryOp { val, .. } => next_root(self, val),
            Block { expressions } | Program { expressions } => expressions
                .iter()
                .map(|e| next_root(self, e))
                .last()
                .unwrap_or(()),
            Assignment {
                left_val,
                right_val,
            } => {
                next_root(self, left_val);
                next_root(self, right_val);
            }
            Expression { expression } => next_root(self, expression),
            If {
                condition,
                if_true,
                if_false,
            } => {
                self.buffer.push_str("  edge[tailport=if];\n");
                next_root(self, condition);
                self.buffer.push_str("  edge[tailport=then];\n");
                if let Some(if_true) = if_true {
                    next_root(self, if_true)
                } else {
                    next_root(self, &Node::default())
                }
                self.buffer.push_str("  edge[tailport=else];\n");
                if let Some(if_false) = if_false {
                    next_root(self, if_false)
                } else {
                    next_root(self, &Node::default())
                }
                self.buffer.push_str("  edge[tailport=\"\"];\n");
            }
            _ => (),
        }
    }

    fn visit_helper(&mut self, root: DotNode) {
        let DotNode { root_name, node } = root;
        let current_name = match DotNodeType::from(node) {
            DotNodeType::If => self.declare_node(
                node,
                Some("shape=record, label=\"<if>if|<then>then|<else>else\""),
            ),
            _ => self.declare_node(node, None),
        };
        let arrow = format!("    {} -> {};\n", root_name, current_name);
        self.buffer.push_str(&arrow);
        self.dispatch(&current_name, node);
    }
}

impl NodeVisitor<String> for DotTranslator {
    fn visit(&mut self, root: &Node) -> String {
        let Node { node, .. } = root;
        let root_name = self.declare_node(node, None);
        self.dispatch(&root_name, node);
        self.buffer.push_str("}\n");
        let mut buffer = "".to_string();
        std::mem::swap(&mut self.buffer, &mut buffer);
        buffer
    }
}
