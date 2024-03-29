use super::lexer::Span;
use std::{collections::HashMap, fmt::Debug};

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
    pub ty: Option<Type>,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").field("ntype", &self.ntype).finish()
    }
}
#[derive(Debug)]
pub struct Function {
    parameters: Vec<Node>,
    return_type: Type,
    body: Node,
    span: Span,
}

impl Function {
    pub fn new(parameters: Vec<Node>, return_type: Type, body: Node, span: Span) -> Self {
        Self {
            parameters,
            return_type,
            body,
            span,
        }
    }
}
#[derive(Debug)]
pub struct Globals {
    pub functions: HashMap<String, Function>,
    // structs: ...,
    // enums: ...,
    // constants: ...,
}

impl Globals {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
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
                ntype: NodeType::Ident(s, None),
                span,
                ..
            } => Some((s.clone(), *span)),
            _ => None,
        }
    }
}
#[derive(Debug)]
pub struct Type(pub String);
#[derive(Debug)]
pub struct Parameter(String, Type);

#[derive(Debug)]
pub enum NodeType {
    Block(Vec<Node>),
    Ident(String, Option<Type>),
    LitNumber(String),
    LitString(String),
    LitChar(String),
    IfExpression {
        condition: Box<Node>,
        true_case: Box<Node>,
        false_case: Option<Box<Node>>,
    },
    WhileExpression {
        condition: Box<Node>,
        body: Box<Node>,
    },

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
        rhs: Box<Node>,
    },
    FunctionDecl {
        name: String,
        params: Vec<Node>,
        ret: Type,
        body: Box<Node>, //Block
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
    Range {
        low: Option<Box<Node>>,
        high: Option<Box<Node>>,
    },
    Empty,
}

pub struct Bracket;
impl Bracket {
    pub fn get_closing(open: &str) -> &'static str {
        match open {
            "(" => ")",
            "[" => "]",
            "{" => "}",
            "<" => ">",
            _ => unreachable!(),
        }
    }
}
