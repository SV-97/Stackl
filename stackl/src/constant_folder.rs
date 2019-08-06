/// Simple transformer that evaluates all constant expressions, removes
/// duplicate unaries (like not not etc.) and brackets so e.g Expression -> Expression
use super::ast::{BinOp, Float, Int, Node, NodeTransformer, NodeType, UnOp};
use super::prelude::*;

/// Transform a tree by folding constants and evaluating constant expressions
pub fn fold_constants(root: Node) -> Node {
    let mut cf = ConstantFolder::new();
    let root = cf.visit(root);
    root
}

pub struct ConstantFolder {}

impl ConstantFolder {
    pub fn new() -> Self {
        ConstantFolder {}
    }
}

impl NodeTransformer<Node> for ConstantFolder {
    fn visit(&mut self, root: Node) -> Node {
        use NodeType::*;
        if root.is_constant() {
            root.execute()
        } else {
            match root {
                Node {
                    node:
                        BinaryOp {
                            operation,
                            left,
                            right,
                        },
                    span,
                } => {
                    let op = NodeType::binary_operation(
                        operation,
                        self.visit(*left),
                        self.visit(*right),
                    );
                    Node::new(span, op)
                }
                Node {
                    node: UnaryOp { operation, val },
                    span,
                } => {
                    let op = NodeType::unary_operation(operation, self.visit(*val));
                    Node::new(span, op)
                }
                Node {
                    node: Expression { expression },
                    ..
                } => self.visit(*expression),
                Node {
                    node: Program { expressions },
                    span,
                } => {
                    let exprs = expressions
                        .into_iter()
                        .map(|x| self.visit(x))
                        .collect::<Vec<Node>>();
                    Node::new(span, NodeType::program(exprs))
                }
                Node {
                    node:
                        If {
                            condition,
                            if_true,
                            if_false,
                        },
                    span,
                } => {
                    let condition = Box::new(self.visit(*condition));
                    let if_true = if_true.map(|x| Box::new(self.visit(*x)));
                    let if_false = if_false.map(|x| Box::new(self.visit(*x)));
                    Node::new(
                        span,
                        NodeType::If {
                            condition,
                            if_true,
                            if_false,
                        },
                    )
                }
                Node {
                    node: Block { expressions },
                    span,
                } => {
                    let exprs = expressions
                        .into_iter()
                        .map(|x| self.visit(x))
                        .collect::<Vec<Node>>();
                    Node::new(span, NodeType::block(exprs))
                }
                x => x,
            }
        }
    }
}

impl Node {
    fn execute(self) -> Node {
        use NodeType::*;
        let span = self.span;
        let new_node = |node_type| Node::new(span, node_type);
        match self.node {
            Program { expressions } => new_node(NodeType::program(
                expressions
                    .into_iter()
                    .map(Node::execute)
                    .collect::<Vec<_>>(),
            )),
            BinaryOp {
                operation,
                left,
                right,
            } => operation.execute(left.execute(), right.execute(), span),
            UnaryOp { operation, val } => operation.execute(val.execute(), span),
            Expression { expression } => expression.execute(),
            Block { expressions } => {
                if let Some(last_node) = expressions.into_iter().map(Node::execute).last() {
                    new_node(NodeType::expression(last_node)).execute()
                } else {
                    new_node(NodeType::Empty).execute()
                }
            }
            If {
                if_true: None,
                if_false: None,
                ..
            } => new_node(NodeType::Empty),
            If {
                condition,
                if_true,
                if_false,
            } => {
                let condition = condition.execute().node;
                match condition {
                    NodeType::BoolLiteral(true) => {
                        if let Some(if_true) = if_true {
                            if_true.execute()
                        } else {
                            new_node(NodeType::Empty)
                        }
                    }
                    NodeType::BoolLiteral(false) => {
                        if let Some(if_false) = if_false {
                            if_false.execute()
                        } else {
                            new_node(NodeType::Empty)
                        }
                    }
                    n => new_node(NodeType::Error(format!(
                        "Expected boolean value, got {:?}.",
                        n
                    ))),
                }
            }
            _ => self,
        }
    }
}

impl UnOp {
    fn execute(self, val: Node, span: Span) -> Node {
        match (self, val) {
            (
                UnOp::Not,
                Node {
                    node: NodeType::BoolLiteral(b),
                    ..
                },
            ) => Node::new(span, NodeType::BoolLiteral(!b)),
            (
                UnOp::Minus,
                Node {
                    node: NodeType::FloatLiteral(f),
                    ..
                },
            ) => Node::new(span, NodeType::FloatLiteral(-f)),
            (
                UnOp::Minus,
                Node {
                    node: NodeType::IntegerLiteral(i),
                    ..
                },
            ) => Node::new(span, NodeType::IntegerLiteral(-i)),
            _ => Node::new(
                span,
                NodeType::Error("Operation on incompatible types.".to_string()),
            ),
        }
    }
}

impl BinOp {
    /// Construct a new node given a NodeType constructor `g` that takes a value of type S.
    /// The NodeType constructor gets fed the result of a function `f` that combines two
    /// values `l` and `r`, that can both be converted to a type S.
    fn new_generic<T1, T2, S>(
        g: impl Fn(S) -> NodeType,
        f: impl Fn(S, S) -> S,
        l: T1,
        r: T2,
        span: Span,
    ) -> Node
    where
        S: From<T1>,
        S: From<T2>,
    {
        Node::new(span, g(f(S::from(l), S::from(r))))
    }

    /// Create a new float node given a function that combines two values that can be converted to floats
    fn new_float<T1, T2>(f: impl Fn(Float, Float) -> Float, l: T1, r: T2, span: Span) -> Node
    where
        Float: From<T1>,
        Float: From<T2>,
    {
        BinOp::new_generic(NodeType::FloatLiteral, f, l, r, span)
    }

    /// Create a new int node given a function that combines two values that can be converted to int
    fn new_int<T1, T2>(f: impl Fn(Int, Int) -> Int, l: T1, r: T2, span: Span) -> Node
    where
        Int: From<T1>,
        Int: From<T2>,
    {
        BinOp::new_generic(NodeType::IntegerLiteral, f, l, r, span)
    }

    fn execute(self, left: Node, right: Node, span: Span) -> Node {
        use std::cmp;
        use std::ops;
        use BinOp::*;
        use NodeType::*;
        match (left.execute(), self, right.execute()) {
            (
                Node {
                    node: IntegerLiteral(l),
                    ..
                },
                op,
                Node {
                    node: IntegerLiteral(r),
                    ..
                },
            ) if op == Add || op == Sub || op == Mul || op == Div || op == Mod => {
                let f = match op {
                    Add => ops::Add::add,
                    Sub => ops::Sub::sub,
                    Mul => ops::Mul::mul,
                    Div => ops::Div::div,
                    Mod => ops::Rem::rem,
                    _ => panic!("You shouldn't be here"),
                };
                BinOp::new_int(f, l, r, span)
            }
            (
                Node {
                    node: FloatLiteral(l),
                    ..
                },
                op,
                Node {
                    node: FloatLiteral(r),
                    ..
                },
            ) if op == Add || op == Sub || op == Mul || op == Div || op == Mod => {
                let f = match op {
                    Add => ops::Add::add,
                    Sub => ops::Sub::sub,
                    Mul => ops::Mul::mul,
                    Div => ops::Div::div,
                    Mod => ops::Rem::rem,
                    _ => panic!("You shouldn't be here"),
                };
                BinOp::new_float(f, l, r, span)
            }
            (
                Node {
                    node: FloatLiteral(l),
                    ..
                },
                op,
                Node {
                    node: FloatLiteral(r),
                    ..
                },
            ) if op == Greater
                || op == Less
                || op == GreaterOrEq
                || op == LessOrEq
                || op == Equal
                || op == NotEqual =>
            {
                let f = match op {
                    Greater => cmp::PartialOrd::gt,
                    Less => cmp::PartialOrd::lt,
                    GreaterOrEq => cmp::PartialOrd::ge,
                    LessOrEq => cmp::PartialOrd::le,
                    Equal => cmp::PartialEq::eq,
                    NotEqual => cmp::PartialEq::ne,
                    _ => panic!("You shouldn't be here"),
                };
                Node::new(span, NodeType::BoolLiteral(f(&l, &r)))
            }
            (
                Node {
                    node: IntegerLiteral(l),
                    ..
                },
                op,
                Node {
                    node: IntegerLiteral(r),
                    ..
                },
            ) if op == Greater
                || op == Less
                || op == GreaterOrEq
                || op == LessOrEq
                || op == Equal
                || op == NotEqual =>
            {
                let f = match op {
                    Greater => cmp::PartialOrd::gt,
                    Less => cmp::PartialOrd::lt,
                    GreaterOrEq => cmp::PartialOrd::ge,
                    LessOrEq => cmp::PartialOrd::le,
                    Equal => cmp::PartialEq::eq,
                    NotEqual => cmp::PartialEq::ne,
                    _ => panic!("You shouldn't be here"),
                };
                Node::new(span, NodeType::BoolLiteral(f(&l, &r)))
            }
            (
                Node {
                    node: FloatLiteral(_),
                    ..
                },
                _,
                Node {
                    node: IntegerLiteral(_),
                    ..
                },
            )
            | (
                Node {
                    node: IntegerLiteral(_),
                    ..
                },
                _,
                Node {
                    node: FloatLiteral(_),
                    ..
                },
            ) => Node::new(
                span,
                NodeType::Error("Operation on incompatible types.".to_string()),
            ),
            (a, b, c) => Node::new(
                span,
                NodeType::Error(format!(
                    "Binary Operation on invalid types: {:?} | {:?} | {:?}",
                    a, b, c
                )),
            ),
        }
    }
}
