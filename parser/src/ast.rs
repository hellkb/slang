use super::lexer::Span;
use std::fmt::Debug;

#[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    NotEq,
    Gt,
    Gte,
    Lt,
    Lte,

    Error(String),
}
impl Operator {
    pub fn from_opstr(op: &str) -> Operator {
        match op {
            "+" => Operator::Add,
            "-" => Operator::Sub,
            "/" => Operator::Div,
            "*" => Operator::Mul,
            "==" => Operator::Eq,
            "!=" => Operator::NotEq,

            ">" => Operator::Gt,
            ">=" => Operator::Gte,
            "<" => Operator::Lt,
            "<=" => Operator::Lte,
            _ => Operator::Error(op.into()),
        }
    }
}
pub struct Node {
    pub ntype: NodeType,
    pub span: Span,
    ty: Option<Type>,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").field("ntype", &self.ntype).finish()
    }
}

impl Node {
    pub fn new(ntype: NodeType, span: Span) -> Node {
        let ty = match ntype {
            NodeType::Comparison { .. } => Some(Type("bool".to_string())),
            _ => None,
        };
        Self { ntype, span, ty }
    }
    pub fn get_ident(&self) -> Option<(String, Span)> {
        match self {
            Node {
                ntype: NodeType::Ident(s),
                span,
                ..
            } => Some((s.clone(), *span)),
            _ => None,
        }
    }
}
#[derive(Debug)]
pub struct Type(String);
#[derive(Debug)]
pub struct Parameter(String, Type);

#[derive(Debug)]
pub enum NodeType {
    Block(Vec<Node>),
    Ident(String),
    LitNumber(String),
    LitString(String),
    BinaryExpression {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    UnaryExpr {
        op: Operator,
        rhs: Box<Node>,
    },
    Comparison {
        op: Operator,
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    Grouping(Box<Node>),
    Assignment {
        lhs: Box<Node>,
        rhs: Box<Node>,
    },
    VarDecl {
        lhs: Box<Node>,
    },
    FunctionDecl {
        name: String,
        params: Vec<Node>,
    },
    FnOrIdx {
        name: String,
        call: Vec<Node>,
    },
    FunctionCall {
        params: Vec<Node>,
    },
    ArrayIdx {
        idx: Box<Node>,
    },
    Empty,
}

enum Punct {
    Round,
    Square,
}
