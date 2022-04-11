use std::fmt;

use chumsky::prelude::*;
use chumsky::Stream;

use crate::lex::Group::*;
use crate::lex::Token::{self, *};
use crate::lex::{self, Span};
use crate::sym::{kw, Symbol};

#[derive(Debug)]
pub enum Constness {
    Default,
    Not,
    Yes,
}

#[derive(Debug)]
pub enum Sign {
    Positive,
    Negative,
}

#[derive(Debug)]
pub enum Lit {
    True,
    False,
    Char(char),
    Int(i128),
    Str(String),
}

fn escape_char(mut s: String, span: Span) -> Result<char, Simple<Token>> {
    if let Some(c) = s.pop() {
        if !s.is_empty() {
            todo!();
        }

        Ok(c)
    } else {
        Err(Simple::custom(span, "empty char literal"))
    }
}

fn escape_str(s: String, span: Span) -> Result<String, Simple<Token>> {
    if s.contains('\\') {
        todo!()
    }

    Ok(s)
}

fn lower_lit(lit: lex::Lit, span: Span) -> Result<Lit, Simple<Token>> {
    match lit {
        lex::Lit::Char(c) => escape_char(c, span).map(Lit::Char),
        lex::Lit::Str(s) => escape_str(s, span).map(Lit::Str),
        lex::Lit::Number(s) => {
            let mut s = &*s;
            let mut radix = 10;

            let negative = if let Some(news) = s.strip_prefix('-') {
                s = news;
                true
            } else if let Some(news) = s.strip_prefix('+') {
                s = news;
                false
            } else {
                false
            };

            if let Some(news) = s.strip_prefix("0x") {
                radix = 16;
                s = news;
            } else if let Some(news) = s.strip_prefix("0b") {
                radix = 2;
                s = news;
            } else if let Some(news) = s.strip_prefix("0o") {
                radix = 8;
                s = news;
            }

            let num = match i128::from_str_radix(s, radix) {
                Ok(num) if negative => {
                    if let Some(num) = num.checked_neg() {
                        num
                    } else {
                        return Err(Simple::custom(span, "number cannot be negated"));
                    }
                }
                Ok(num) => num,
                Err(_) => return Err(Simple::custom(span, "failed to parse integer")),
            };

            Ok(Lit::Int(num))
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Lit(Lit),
    Ident(Symbol),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Call { recv: Box<Expr>, rhs: Vec<Expr> },
}

#[derive(Debug)]
pub enum Stmt {
    Semi(Expr),
}

#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub enum Item {
    Fn {
        name: Symbol,
        args: Vec<String>,
        body: Block,
    },
}

#[derive(Debug)]
pub struct File {
    pub items: Vec<Item>,
}

fn expr() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let atom = filter_map::<_, _, _, Simple<Token>>(move |span, x| match x {
            Token::Lit(lit) => lower_lit(lit, span).map(Expr::Lit),
            Token::Group(lex::Group::Paren(tokens)) => expr
                .parse(Stream::from_iter(
                    usize::MAX..usize::MAX,
                    tokens.into_iter(),
                ))
                .map_err(|mut e| {
                    let init = e.pop().unwrap();
                    e.into_iter().fold(init, chumsky::Error::merge)
                }),
            Token::Ident(sym) if sym == kw::True => Ok(Expr::Lit(Lit::True)),
            Token::Ident(sym) if sym == kw::False => Ok(Expr::Lit(Lit::False)),
            Token::Ident(sym) => Ok(Expr::Ident(sym)),
            _ => Err(chumsky::Error::expected_input_found(span, None, Some(x))),
        });

        let unary = just(Token::Punct('-'))
            .repeated()
            .then(atom)
            .foldr(|_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary
            .clone()
            .then(
                just(Token::Punct('*'))
                    .to(Expr::Mul as fn(_, _) -> _)
                    .or(just(Token::Punct('*')).to(Expr::Div as fn(_, _) -> _))
                    .then(unary)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)));

        
        product
            .clone()
            .then(
                just(Token::Punct('+'))
                    .to(Expr::Add as fn(_, _) -> _)
                    .or(just(Token::Punct('-')).to(Expr::Sub as fn(_, _) -> _))
                    .then(product)
                    .repeated(),
            )
            .foldl(|lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)))
    })
    .then_ignore(end())
}

fn block() -> impl Parser<Token, Block, Error = Simple<Token>> + Clone {
    expr()
        .separated_by(just(Token::Punct(';')))
        .map(|mut exprs| {
            let expr = exprs.pop();
            Block {
                stmts: exprs.into_iter().map(Stmt::Semi).collect(),
                expr,
            }
        }).then_ignore(just(Token::Punct(';'))).then(expr().or_not()).map(|(mut b, trailing)| {
            if let Some(trailing) = trailing {
                b.stmts.extend(b.expr.replace(trailing).map(Stmt::Semi));
            }
            b
        })
}

fn item() -> impl Parser<Token, Item, Error = Simple<Token>> + Clone {
    just(Ident(kw::Fn))
        .ignore_then(select! { Ident(x) => x })
        .then(
            select! {
                Group(Brace(tokens)) => tokens,
            }
            .try_map(|tokens, _| {
                block()
                    .parse(Stream::from_iter(
                        usize::MAX..usize::MAX,
                        tokens.into_iter(),
                    ))
                    .map_err(|mut e| {
                        let s = e.pop().unwrap();
                        e.into_iter().fold(s, |e, accum| e.merge(accum))
                    })
            }),
        )
        .map(|(ident, block)| Item::Fn {
            name: ident,
            args: vec![],
            body: block,
        })
}

pub fn parser() -> impl Parser<Token, File, Error = Simple<Token>> + Clone {
    item().repeated().map(|items| File { items }).then_ignore(end())
}
