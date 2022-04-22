use std::{fmt::Debug, iter::Iterator, iter::Peekable, vec::IntoIter};

use super::lexer::{Lexer, Span, Token};
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
    fn from_str(op: &str) -> Operator {
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
    ntype: NodeType,
    span: Span,
    ty: Option<String>,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Node").field("ntype", &self.ntype).finish()
    }
}

impl Node {
    fn new(ntype: NodeType, span: Span) -> Node {
        let ty = match ntype {
            NodeType::Comparison { .. } => Some("bool".to_string()),
            _ => None,
        };
        Self { ntype, span, ty }
    }
}

#[derive(Debug)]
pub enum NodeType {
    Scope(Vec<Node>),
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
    Empty,
}

type ParseResult = Result<Node, String>;
type LexIter = Peekable<IntoIter<Token>>;
pub struct Parser {
    toks: LexIter,
}
impl Parser {
    pub fn new(src: String, name: String) -> Parser {
        Self {
            toks: Lexer::new(src, name).tokens().into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> ParseResult {
        self.equality()
    }

    fn pif(&mut self) -> ParseResult {
        todo!()
    }

    fn expression(&mut self) -> ParseResult {
        let expr = self.equality()?;
        Ok(expr)
    }

    fn equality(&mut self) -> ParseResult {
        let mut lhs = self.comparison()?;
        while let Some(tok) = self.peek_op_in(&["==", "!="]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::Comparison {
                    op: Operator::from_str(&opstr),
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.comparison()?),
                },
                span,
            );
            lhs = ret;
        }

        Ok(lhs)
    }
    fn comparison(&mut self) -> ParseResult {
        let mut lhs = self.term()?;

        while let Some(tok) = self.peek_op_in(&["<", ">", "<=", ">="]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::Comparison {
                    op: Operator::from_str(&opstr),
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.term()?),
                },
                span,
            );
            lhs = ret;
        }

        Ok(lhs)
    }
    fn term(&mut self) -> ParseResult {
        let mut lhs = self.factor()?;

        while let Some(tok) = self.peek_op_in(&["+", "-"]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::BinaryExpression {
                    op: Operator::from_str(&opstr),
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.factor()?),
                },
                span,
            );
            lhs = ret;
        }

        Ok(lhs)
    }

    fn factor(&mut self) -> ParseResult {
        let mut lhs = self.unary()?;

        while let Some(tok) = self.peek_op_in(&["*", "/"]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::BinaryExpression {
                    op: Operator::from_str(&opstr),
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.unary()?),
                },
                span,
            );
            lhs = ret;
        }

        Ok(lhs)
    }
    fn unary(&mut self) -> ParseResult {
        if let Some(tok) = self.peek_op_in(&["!", "-"]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::UnaryExpr {
                    op: Operator::from_str(&opstr),

                    rhs: Box::new(self.unary()?),
                },
                span,
            );
            return Ok(ret);
        }

        self.primary()
    }
    fn primary(&mut self) -> ParseResult {
        if let Some(tok) = self.toks.next() {
            let (nt, span) = match tok {
                Token::Ident(v, s) => (NodeType::Ident(v), s),
                Token::NumLit(v, s) => (NodeType::LitNumber(v), s),
                Token::StrLit(v, s) => (NodeType::LitString(v), s),
                Token::Punct(v, s) if v == "(" => {
                    let ret = Node::new(NodeType::Grouping(Box::new(self.expression()?)), s);
                    self.consume(")", ") erwartet");
                    return Ok(ret);
                }
                _ => unreachable!(),
            };
            let ret = Node::new(nt, span);
            return Ok(ret);
        }
        Err("ksd".to_string())
    }

    fn peek_op_in(&mut self, ops: &[&str]) -> Option<Token> {
        self.toks
            .next_if(|c| ops.iter().any(|o: &&str| o == &c.get_punct()))
    }
    fn consume(&mut self, token: &str, msg: &str) {
        match self.toks.next_if(|c| c.get_punct() == token) {
            Some(_) => {}
            None => {
                println!("Error: {msg}")
            }
        }
    }
}
