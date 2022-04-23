use std::{iter::Iterator, iter::Peekable, vec::IntoIter};

use super::ast::{Node, NodeType, Operator, Parameter};
use super::lexer::{Lexer, Token};

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
        self.assign()
    }
    /// parse global scope
    /// first step: functions only
    fn global(&mut self) -> ParseResult {
        let funs: Vec<NodeType> = Vec::new();
        while let Some(tok) = self.toks.peek() {
            match tok {
                Token::Ident(c, span) if c == "fn" => if let Ok(node) = self.func() {},
                _ => todo!(),
            }
        }
        todo!()
    }

    fn func(&mut self) -> ParseResult {
        let tok_name = self.next_ident_if(None);
        if tok_name.is_none() {
            return Err("Funktionsname erwartet".to_string());
        }

        todo!()
    }

    fn func_params(&mut self) -> ParseResult {
        todo!()
    }

    fn assign(&mut self) -> ParseResult {
        let mut lhs = self.expression()?;

        if let Some(tok) = self.peek_op_in(&["="]) {
            let (_, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            let ret = Node::new(
                NodeType::Assignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(self.expression()?),
                },
                span,
            );
            self.consume(";", "expected ';'");
            lhs = ret;
        }

        Ok(lhs)
    }

    fn block(&mut self) -> ParseResult {
        // let list: Vec<Node> = Vec::new();
        // while let Some(next) = self.toks.peek() {
        //     match Token {}
        // }
        // Ok(Node::new(NodeType::Block(list), Span::new(0, 0)))
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
                    op: Operator::from_opstr(&opstr),
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
                    op: Operator::from_opstr(&opstr),
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
                    op: Operator::from_opstr(&opstr),
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
                    op: Operator::from_opstr(&opstr),
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
                    op: Operator::from_opstr(&opstr),

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
    fn consume(&mut self, token_str: &str, msg: &str) {
        match self.toks.next_if(|c| c.get_punct() == token_str) {
            Some(_) => {}
            None => {
                println!("Error: {msg}")
            }
        }
    }

    fn next_ident_if(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_ident(tok_str))
    }
}
