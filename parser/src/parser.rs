use std::{iter::Iterator, iter::Peekable, vec::IntoIter};

use crate::ast::{Function, Type};
use crate::lexer::Span;

use super::ast::{Bracket, Globals, Node, NodeType, Operator};
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

    pub fn parse(&mut self) -> Result<Globals, String> {
        self.global()
    }
    /// parse global scope
    /// first step: functions only
    fn global(&mut self) -> Result<Globals, String> {
        let mut glob = Globals::new();
        //let mut funs: Vec<Node> = Vec::new();
        while let Some(tok) = self.toks.peek() {
            match tok {
                Token::Ident(c, _) if c == "fn" => {
                    self.toks.next();
                    let node = self.func()?;

                    let (name, parameters, return_type, body) = if let NodeType::FunctionDecl {
                        name,
                        params,
                        ret,
                        body,
                    } = node.ntype
                    {
                        (name, params, ret, body)
                    } else {
                        unreachable!()
                    };
                    let f = Function::new(parameters, return_type, *body, node.span);
                    glob.functions.insert(name, f);
                }

                _ => return Err("nur fn erlaubt".to_string()),
            }
        }

        Ok(glob)
    }

    fn func(&mut self) -> ParseResult {
        let (fn_name, fn_name_span) = self.next_ident_value_if(None)?;
        self.eat_punc("(")?;

        let params = self.func_params()?;
        self.eat_punc(")")?;
        let mut ret_type = Type("()".to_string());
        if self.next_punct_if(Some("->")).is_some() {
            ret_type = self.ttype()?;
        }

        Ok(Node::new(
            NodeType::FunctionDecl {
                name: fn_name,
                params,
                body: Box::new(self.block(None)?),
                ret: ret_type,
            },
            fn_name_span,
        ))
    }

    fn func_params(&mut self) -> Result<Vec<Node>, String> {
        let mut v = Vec::new();
        loop {
            if self.peek_op_in(&[")"]).is_some() {
                return Ok(v);
            }

            v.push(self.typed_ident()?);
            if self.next_punct_if(Some(",")).is_some() {
                continue;
            } else {
                return Ok(v);
            }
        }
    }

    fn block(&mut self, eaten_span: Option<Span>) -> ParseResult {
        let span = if let Some(s) = eaten_span {
            s
        } else {
            let (_, s) = self.eat_punc("{")?;
            s
        };
        println!("{:?}", span);
        let mut nodes = Vec::new();
        loop {
            if self.next_punct_if(Some("}")).is_some() {
                let node = Node::new(NodeType::Block(nodes), span);
                return Ok(node);
            };
            match self.toks.peek() {
                Some(a) if a.get_inner_string_val() == "let" => {
                    nodes.push(self.p_let()?);
                }
                None => {
                    todo!()
                }
                _ => {
                    nodes.push(self.p_if()?);
                    self.next_punct_if(Some(";"));
                }
            }
            //nodes.push(self.p_if()?);
        }
    }
    /// Todo if ... else if
    fn p_let(&mut self) -> ParseResult {
        let node = self.next_ident_if(Some("let"));
        let (_, span) = node.unwrap().get_inner_values();
        let lhs = Box::new(self.typed_ident()?);
        self.next_punct_if(Some("="));
        let rhs = Box::new(self.p_if()?);
        self.next_punct_if(Some(";"));
        Ok(Node::new(NodeType::VarDecl { lhs, rhs }, span))
    }
    fn p_if(&mut self) -> ParseResult {
        let node = self.next_ident_if(Some("if"));
        if node.is_none() {
            return self.p_while();
        }
        let (_, span) = node.unwrap().get_inner_values();
        let cond = self.expression()?;
        //        println!("{:?}", cond);
        let tblock = self.block(None)?;
        let fblock = if self.next_ident_if(Some("else")).is_none() {
            None
        } else {
            Some(Box::new(self.block(None)?))
        };
        let ret = Node::new(
            NodeType::IfExpression {
                condition: Box::new(cond),
                true_case: Box::new(tblock),
                false_case: fblock,
            },
            span,
        );

        Ok(ret)
    }
    fn p_while(&mut self) -> ParseResult {
        let node = self.next_ident_if(Some("while"));
        if node.is_none() {
            return self.assign();
        }
        let (_, span) = node.unwrap().get_inner_values();
        let cond = self.expression()?;
        //        println!("{:?}", cond);
        let body = self.block(None)?;
        let ret = Node::new(
            NodeType::WhileExpression {
                condition: Box::new(cond),
                body: Box::new(body),
            },
            span,
        );

        Ok(ret)
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
                    rhs: Box::new(self.p_while()?),
                },
                span,
            );
            //println!("{:?}", lhs.span.clone());
            self.eat_punc(";")?;
            lhs = ret;
        }

        Ok(lhs)
    }

    fn expression(&mut self) -> ParseResult {
        let expr = self.equality()?;
        //self.next_punct_if(Some(";"));
        Ok(expr)
    }

    fn tuple(&mut self) -> ParseResult {
        todo!()
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
            let (b, span) = bracket.get_inner_values();
            let cnode = if b == "(" {
                Node::new(
                    NodeType::FunctionCall {
                        params: self.function_call_params()?,
                    },
                    span,
                )
            } else {
                Node::new(
                    NodeType::ArrayIdx {
                        idx: Box::new(self.expression()?),
                    },
                    span,
                )
            };
            v.push(cnode);

            self.eat_punc(Bracket::get_closing(&b))?;
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
                Token::Ident(v, s) => (NodeType::Ident(v, None), s),
                Token::NumLit(v, s) => (NodeType::LitNumber(v), s),
                Token::StrLit(v, s) => (NodeType::LitString(v), s),
                Token::CharLit(v, s) => (NodeType::LitChar(v), s),
                Token::Punct(v, s) if v == "(" => {
                    let ret = Node::new(NodeType::Grouping(Box::new(self.expression()?)), s);
                    self.eat_punc(Bracket::get_closing("("))?;
                    return Ok(ret);
                }
                Token::Punct(v, s) if v == "{" => {
                    let ret = self.block(Some(s))?;

                    return Ok(ret);
                }
                Token::Punct(v, s) => {
                    println!("nicht erwartet: '{}' at {:?}", v, s);
                    return Err("".to_string());
                }
            };
            let ret = Node::new(nt, span);
            return Ok(ret);
        }
        Err("ksd".to_string())
    }

    fn typed_ident(&mut self) -> ParseResult {
        let node = self.primary()?;
        if let Some((ident, span)) = node.get_ident() {
            return Ok(Node::new(
                NodeType::Ident(ident, self.type_qualifier()?),
                span,
            ));
        }
        Err("Type erwartet".to_string())
    }
    fn type_qualifier(&mut self) -> Result<Option<Type>, String> {
        if self.next_punct_if(Some(":")).is_some() {
            let node = self.primary()?;
            if let Some((ident, _span)) = node.get_ident() {
                return Ok(Some(Type(ident)));
            }
            return Err("Type erwartet".to_string());
        };
        Ok(None)
    }

    fn ttype(&mut self) -> Result<Type, String> {
        let node = self.primary()?;
        if let Some((ident, _span)) = node.get_ident() {
            return Ok(Type(ident));
        }
        Err("Type erwartet".to_string())
    }

    fn function_call_params(&mut self) -> Result<Vec<Node>, String> {
        let mut v = Vec::new();
        if let Some(t) = self.toks.peek() {
            if !t.get_punct().is_empty() {
                return Ok(v);
            }
        }
        loop {
            let expr = self.expression()?;
            v.push(expr);

            match self.toks.peek() {
                Some(t) if t.get_inner_string_val() == "," => {
                    self.eat_punc(",")?;
                }
                Some(t) if t.get_inner_string_val() == ")" => {
                    return Ok(v);
                }
                _ => {
                    return Err("fcall: ) erwartet".to_string());
                }
            }
        }
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

    fn eat_punc(&mut self, punct: &str) -> Result<(String, Span), String> {
        let tok = self
            .next_punct_if(Some(punct))
            .ok_or_else(|| format!("Fehler {punct} erwartet"))?;

        Ok(tok.get_inner_values())
    }
    fn next_punct_if(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_punct(tok_str))
    }

    fn next_ident_if(&mut self, tok_str: Option<&str>) -> Option<Token> {
        self.toks.next_if(|x| x.is_ident(tok_str))
    }

    fn next_ident_value_if(&mut self, tok_str: Option<&str>) -> Result<(String, Span), String> {
        let tok = self
            .next_ident_if(tok_str)
            .ok_or_else(|| "function name expected".to_string())?;

        Ok(tok.get_inner_values())
    }
}
