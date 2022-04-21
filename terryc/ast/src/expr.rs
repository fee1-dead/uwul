use std::fmt::{self, Write};

use terryc_base::errors::ErrorReported;
use terryc_base::sym::{kw, Symbol};
use terryc_base::Span;
use terryc_lex::{Ident, TokenKind as T};

use super::{Block, Parser, TyKind};

pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOpKind {
    fn as_str(self) -> &'static str {
        match self {
            BinOpKind::Equal => "==",
            BinOpKind::NotEqual => "!=",
            BinOpKind::Less => "<",
            BinOpKind::LessEqual => "<=",
            BinOpKind::Greater => ">",
            BinOpKind::GreaterEqual => ">=",
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Minus,
    Bang,
}

pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

impl UnOpKind {
    fn as_char(self) -> char {
        match self {
            UnOpKind::Minus => '-',
            UnOpKind::Bang => '!',
        }
    }
}

pub enum LiteralKind {
    Int(u128),
    String(Symbol),
    Float(f64),
    Bool(bool),
}

impl LiteralKind {
    pub fn ty(&self) -> TyKind {
        match self {
            LiteralKind::Int(_) => TyKind::I32,
            LiteralKind::String(_) => TyKind::String,
            LiteralKind::Float(_) => TyKind::F32,
            LiteralKind::Bool(_) => TyKind::Bool,
        }
    }
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Int(i) => write!(f, "{}", i),
            LiteralKind::String(s) => write!(f, "\"{}\"", s),
            LiteralKind::Float(float) => write!(f, "{}", float),
            LiteralKind::Bool(b) => write!(f, "{}", b),
        }
    }
}

pub struct Literal {
    pub kind: LiteralKind,
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Debug for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

#[derive(Debug)]
pub enum Else {
    ElseIf(Box<ExprIf>, Span),
    Else(Block),
}

#[derive(Debug)]
pub struct ExprIf {
    pub expr: Box<Expr>,
    pub block: Block,
    pub else_: Option<Else>,
}

#[derive(Debug)]
pub struct ExprWhile {
    pub while_: Span,
    pub expr: Box<Expr>,
    pub block: Block,
}

#[derive(Debug)]
pub enum ExprKind {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    Group(Box<Expr>, Span),
    Literal(Literal),
    Ident(Symbol),
    Block(Block),
    Assignment { lhs: Box<Expr>, rhs: Box<Expr> },
    If(ExprIf),
    While(ExprWhile),
    Call { callee: Box<Expr>, args: Vec<Expr> },
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl ExprKind {
    pub fn has_block(&self) -> bool {
        match self {
            ExprKind::BinOp(_, _, _) => false,
            ExprKind::UnOp(_, _) => false,
            ExprKind::Literal(_) => false,
            ExprKind::Ident(_) => false,
            ExprKind::Assignment { .. } => false,
            ExprKind::Call { .. } => false,
            ExprKind::Group(_, _) => false,
            ExprKind::Block(_) => true,
            ExprKind::If(_) => true,
            ExprKind::While { .. } => true,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_expr(&mut self) -> Result<Expr, ErrorReported> {
        self.expression().ok_or(ErrorReported)
    }

    fn expression(&mut self) -> Option<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Option<Expr> {
        let expr = self.equality()?;
        if self.eat(T::Eq) {
            let expr2 = self.expression()?;
            let span = expr.span.to(expr2.span);
            Some(Expr {
                kind: ExprKind::Assignment {
                    lhs: Box::new(expr),
                    rhs: Box::new(expr2),
                },
                span,
            })
        } else {
            Some(expr)
        }
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;
        while self.eat_any(&[T::EqEq, T::NotEq]) {
            let token = &self.prev_token;
            let op = match token.kind {
                T::EqEq => BinOpKind::Equal,
                T::NotEq => BinOpKind::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            let span = expr.span.to(right.span);
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
                span,
            };
        }
        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;
        while self.eat_any(&[T::Greater, T::GreaterEq, T::Less, T::LessEq]) {
            let token = &self.prev_token;
            let op = match token.kind {
                T::Greater => BinOpKind::Greater,
                T::GreaterEq => BinOpKind::GreaterEqual,
                T::Less => BinOpKind::Less,
                T::LessEq => BinOpKind::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            let span = expr.span.to(right.span);
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
                span,
            };
        }
        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while self.eat_any(&[T::Minus, T::Plus]) {
            let token = &self.prev_token;
            let op = match token.kind {
                T::Minus => BinOpKind::Sub,
                T::Plus => BinOpKind::Add,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            let span = expr.span.to(right.span);
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
                span,
            };
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;
        while self.eat_any(&[T::Star, T::Slash]) {
            let token = &self.prev_token;
            let op = match token.kind {
                T::Star => BinOpKind::Mul,
                T::Slash => BinOpKind::Div,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            let span = expr.span.to(right.span);
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
                span,
            };
        }
        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if self.eat_any(&[T::Minus, T::Not]) {
            let op = match self.prev_token.kind {
                T::Minus => UnOpKind::Minus,
                T::Not => UnOpKind::Bang,
                _ => unreachable!(),
            };
            let expr = self.unary()?;
            let span = expr.span;
            Some(Expr {
                kind: ExprKind::UnOp(op, Box::new(expr)),
                span: self.prev_token.span.to(span),
            })
        } else {
            self.call()
        }
    }

    fn finish_call(&mut self, expr: Expr) -> Option<Expr> {
        let mut args = vec![];
        let span = expr.span;
        if self.eat(T::Comma) {
            if self.eat(T::RightParen) {
                let span = span.to(self.prev_token.span);
                return Some(Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                });
            } else {
                self.error("expected `)`");
                return None;
            }
        } else if self.eat(T::RightParen) {
            let span = span.to(self.prev_token.span);
            return Some(Expr {
                kind: ExprKind::Call {
                    callee: Box::new(expr),
                    args,
                },
                span,
            });
        }

        loop {
            args.push(self.expression()?);
            if self.eat(T::Comma) {
                if self.eat(T::RightParen) {
                    let span = span.to(self.prev_token.span);
                    return Some(Expr {
                        kind: ExprKind::Call {
                            callee: Box::new(expr),
                            args,
                        },
                        span,
                    });
                }
            } else if self.eat(T::RightParen) {
                let span = span.to(self.prev_token.span);
                return Some(Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(expr),
                        args,
                    },
                    span,
                });
            } else {
                self.error("expected `)` or `,`");
                return None;
            }
        }
    }
    fn call(&mut self) -> Option<Expr> {
        let mut expr = self.primary()?;
        while self.eat(T::LeftParen) {
            expr = self.finish_call(expr)?;
        }
        Some(expr)
    }

    fn while_(&mut self) -> Option<Expr> {
        if self.eat_kw(kw::While) {
            let span = self.prev_token.span;
            let expr = self.expression()?;
            let block = self.parse_block().ok()?;
            let span = span.to(block.span);
            Some(Expr {
                kind: ExprKind::While(ExprWhile {
                    while_: span,
                    expr: Box::new(expr),
                    block,
                }),
                span,
            })
        } else {
            None
        }
    }

    fn if_(&mut self) -> Option<Expr> {
        self.opt_if().map(|(if_, span)| Expr {
            kind: ExprKind::If(if_),
            span,
        })
    }

    fn opt_if(&mut self) -> Option<(ExprIf, Span)> {
        if self.eat_kw(kw::If) {
            let prev = self.prev_token.span;
            let expr = self.expression()?;
            let block = self.parse_block().ok()?;
            let else_ = self.opt_else();
            let span = prev.to(self.prev_token.span);
            Some((
                ExprIf {
                    expr: Box::new(expr),
                    block,
                    else_,
                },
                span,
            ))
        } else {
            None
        }
    }

    fn opt_else(&mut self) -> Option<Else> {
        if self.eat_kw(kw::Else) {
            if let Some(if_) = self.opt_if() {
                Some(Else::ElseIf(Box::new(if_.0), if_.1))
            } else {
                self.parse_block().ok().map(Else::Else)
            }
        } else {
            None
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        let tok = self.peek();
        let span = tok.span;
        let expr = match tok.kind {
            T::Integer(n) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(n),
                }),
                span,
            },
            T::Keyword(Ident {
                symbol: kw::True,
                span,
            }) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(true),
                }),
                span,
            },
            T::Keyword(Ident {
                symbol: kw::False,
                span,
            }) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(false),
                }),
                span,
            },
            T::String(s) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::String(s),
                }),
                span,
            },
            /*T::Decimal(f) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Float(f),
                }),
                span,
            },*/
            T::LeftParen => {
                self.bump();
                let expr = self.expression()?;
                if self.peek().kind != T::RightParen {
                    self.error("expected ')'");
                }
                expr
            }
            T::Ident(sym) => Expr {
                kind: ExprKind::Ident(sym.symbol),
                span,
            },
            T::LeftBrace => {
                let block = self.parse_block().ok()?;
                let span = block.span;
                return Some(Expr {
                    kind: ExprKind::Block(block),
                    span,
                });
            }
            T::Keyword(Ident {
                symbol: kw::While, ..
            }) => return self.while_(),
            T::Keyword(Ident { symbol: kw::If, .. }) => return self.if_(),
            T::Eof => return None,
            _ => {
                self.error("expected expression");
                return None;
            }
        };
        self.bump();
        Some(expr)
    }
}
