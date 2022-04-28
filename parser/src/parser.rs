use std::{iter::Iterator, iter::Peekable, vec::IntoIter};

use crate::lexer::Span;

use super::ast::{Bracket, Node, NodeType, Operator};
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
    // fn global(&mut self) -> ParseResult {
    //     let funs: Vec<NodeType> = Vec::new();
    //     while let Some(tok) = self.toks.peek() {
    //         match tok {
    //             Token::Ident(c, span) if c == "fn" => if let Ok(node) = self.func() {},
    //             _ => todo!(),
    //         }
    //     }
    //     todo!()
    // }

    // fn func(&mut self) -> ParseResult {
    //     let (fn_name, fn_name_span) = self.next_ident_value_if(None)?;
    //     self.eat_punc("(")?;
    //     self.eat_punc(")")?;
    //     Ok(Node::new(
    //         NodeType::Function {
    //             name: fn_name,
    //             params: Vec::new(),
    //         },
    //         fn_name_span,
    //     ))
    // }

    fn func_params(&mut self) -> ParseResult {
        todo!()
    }

    fn assign(&mut self) -> ParseResult {
        let mut lhs = self.expression()?;

        if let Some(tok) = self.next_op_in_if(&["="]) {
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
            self.eat_punc(";")?;
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
        while let Some(tok) = self.next_op_in_if(&["==", "!="]) {
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

        while let Some(tok) = self.next_op_in_if(&["<", ">", "<=", ">="]) {
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

        while let Some(tok) = self.next_op_in_if(&["+", "-"]) {
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

        while let Some(tok) = self.next_op_in_if(&["*", "/"]) {
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
        if let Some(tok) = self.next_op_in_if(&["!", "-"]) {
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

        self.function_call_or_idx()
    }

    fn function_call_or_idx(&mut self) -> ParseResult {
        let mut node = self.primary()?;

        let (ident, span) = match node.get_ident() {
            Some(x) => x,
            None => {
                return Ok(node);
            }
        };
        let mut v: Vec<Node> = Vec::new();
        let bracket = self.peek_op_in(&["(", "["]);
        if bracket.is_none() {
            return Ok(node);
        }
        while let Some(bracket) = self.next_op_in_if(&["(", "["]) {
            let b = bracket.get_inner_string_val();

            let cnode = Node::new(
                NodeType::FunctionCall { params: Vec::new() },
                Span::new(0, 0),
            );
            v.push(cnode);

            self.eat_punc(Bracket::get_closing(b))?;
        }
        node = Node::new(
            NodeType::FnOrIdx {
                name: ident,
                call: v,
            },
            span,
        );

        Ok(node)
    }
    fn primary(&mut self) -> ParseResult {
        if let Some(tok) = self.toks.next() {
            let (nt, span) = match tok {
                Token::Ident(v, s) => (NodeType::Ident(v), s),
                Token::NumLit(v, s) => (NodeType::LitNumber(v), s),
                Token::StrLit(v, s) => (NodeType::LitString(v), s),
                Token::Punct(v, s) if v == "(" => {
                    let ret = Node::new(NodeType::Grouping(Box::new(self.expression()?)), s);
                    self.eat_punc(Bracket::get_closing("("))?;
                    return Ok(ret);
                }
                _ => unreachable!(),
            };
            let ret = Node::new(nt, span);
            return Ok(ret);
        }
        Err("ksd".to_string())
    }
    fn call_idx(&mut self) -> Result<Vec<Node>, String> {
        let calls: Vec<Node> = Vec::new();

        while let Some(tok) = self.next_op_in_if(&["(", "["]) {
            let (opstr, span) = match tok {
                Token::Punct(op, span) => (op, span),
                _ => unreachable!(),
            };
            self.eat_punc(&opstr)?;

            self.eat_punc(Bracket::get_closing(&opstr))?;
        }

        todo!()
    }
    fn function_call_params(&mut self) -> Result<Vec<Node>, String> {
        Ok(Vec::new())
    }

    fn next_op_in_if(&mut self, ops: &[&str]) -> Option<Token> {
        self.toks
            .next_if(|c| ops.iter().any(|o: &&str| o == &c.get_punct()))
    }

    fn peek_op_in(&mut self, ops: &[&str]) -> Option<Token> {
        let tok = self.toks.peek()?;
        ops.iter()
            .map(|s| Token::Punct(s.to_string(), Span::new(0, 0)))
            .find(|t| *t == *tok)
    }

    fn consume(&mut self, token_str: &str, msg: &str) {
        match self.toks.next_if(|c| c.get_punct() == token_str) {
            Some(_) => {}
            None => {
                println!("Error: {msg}")
            }
        }
    }

    fn eat_punc(&mut self, punct: &str) -> Result<(String, Span), String> {
        let tok = self
            .next_punct_if(Some(punct))
            .ok_or_else(|| format!("Fehler {punct} erwartet"))?;

        Ok(tok.get_inner_values())
    }
    fn next_punct_if(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_punct(tok_str))
    }
    fn next_is(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_ident(tok_str))
    }
    fn next_ident_if(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_ident(tok_str))
    }

    fn next_ident_value_if(&mut self, tok_str: Option<&str>) -> Result<(String, Span), String> {
        let tok = self
            .next_ident_if(None)
            .ok_or_else(|| "function name expected".to_string())?;

        Ok(tok.get_inner_values())
    }
}
