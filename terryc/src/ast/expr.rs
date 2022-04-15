use std::fmt::{self, Write};

use super::{Block, Parser};
use crate::lex::{ErrorReported, TokenKind as T};
use crate::sym::{kw, Symbol};

pub struct Expr {
    pub kind: ExprKind,
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
    Str(String),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Int(i) => write!(f, "{}", i),
            LiteralKind::Str(s) => write!(f, "\"{}\"", s),
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
    ElseIf(Box<IfExpr>),
    Else(Block),
}

#[derive(Debug)]
pub struct IfExpr {
    pub expr: Box<Expr>,
    pub block: Block,
    pub else_: Option<Else>,
}

#[derive(Debug)]
pub enum ExprKind {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    Literal(Literal),
    Ident(Symbol),
    Block(Block),
    Assignment { lhs: Box<Expr>, rhs: Box<Expr> },
    If(IfExpr),
    While {
        expr: Box<Expr>,
        block: Block,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    }
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
        if self.eat(T::Equal) {
            let expr2 = self.expression()?;
            Some(Expr {
                kind: ExprKind::Assignment { lhs: Box::new(expr), rhs: Box::new(expr2) },
            })
        } else {
            Some(expr)
        }
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;
        while let Some(token) = self.eat_any(&[T::EqualEqual, T::BangEqual]) {
            let op = match token.kind {
                T::EqualEqual => BinOpKind::Equal,
                T::BangEqual => BinOpKind::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;
        while let Some(token) = self.eat_any(&[T::Greater, T::GreaterEqual, T::Less, T::LessEqual])
        {
            let op = match token.kind {
                T::Greater => BinOpKind::Greater,
                T::GreaterEqual => BinOpKind::GreaterEqual,
                T::Less => BinOpKind::Less,
                T::LessEqual => BinOpKind::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) = self.eat_any(&[T::Minus, T::Plus]) {
            let op = match token.kind {
                T::Minus => BinOpKind::Sub,
                T::Plus => BinOpKind::Add,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;
        while let Some(token) = self.eat_any(&[T::Star, T::Slash]) {
            let op = match token.kind {
                T::Star => BinOpKind::Mul,
                T::Slash => BinOpKind::Div,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(token) = self.eat_any(&[T::Minus, T::Bang]) {
            let op = match token.kind {
                T::Minus => UnOpKind::Minus,
                T::Bang => UnOpKind::Bang,
                _ => unreachable!(),
            };
            Some(Expr {
                kind: ExprKind::UnOp(op, Box::new(self.unary()?)),
            })
        } else {
            self.call()
        }
    }

    fn finish_call(&mut self, expr: Expr) -> Option<Expr> {
        let mut args = vec![];
        if self.eat(T::Comma) {
            if self.eat(T::RightParen) {
                return Some(Expr { kind: ExprKind::Call { callee: Box::new(expr), args } });
            } else {
                self.error("expected `)`");
                return None;
            }
        } else if self.eat(T::RightParen) {
            return Some(Expr { kind: ExprKind::Call { callee: Box::new(expr), args } });
        }

        loop {
            args.push(self.expression()?);
            if self.eat(T::Comma) {
                if self.eat(T::RightParen) {
                    return Some(Expr { kind: ExprKind::Call { callee: Box::new(expr), args } });
                }
            } else if self.eat(T::RightParen) {
                return Some(Expr { kind: ExprKind::Call { callee: Box::new(expr), args } });
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
        if self.eat(T::Keyword(kw::While)) {
            let expr = self.expression()?;
            let block = self.parse_block().ok()?;
            Some(Expr { kind: ExprKind::While { expr: Box::new(expr), block } })
        } else {
            None
        }
    }

    fn if_(&mut self) -> Option<Expr> {
        self.opt_if().map(|if_| Expr {
            kind: ExprKind::If(if_),
        })
    }

    fn opt_if(&mut self) -> Option<IfExpr> {
        if self.eat(T::Keyword(kw::If)) {
            let expr = self.expression()?;
            let block = self.parse_block().ok()?;
            let else_ = self.opt_else();
            Some(IfExpr { expr: Box::new(expr), block, else_  })
        } else {
            None
        }
    }

    fn opt_else(&mut self) -> Option<Else> {
        if self.eat(T::Keyword(kw::Else)) {
            if let Some(if_) = self.opt_if() {
                Some(Else::ElseIf(Box::new(if_)))
            } else {
                self.parse_block().ok().map(Else::Else)
            }
        } else {
            None
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        let expr = match self.peek().unwrap().kind {
            T::Integer(n) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(n),
                }),
            },
            T::Keyword(kw::True) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(true),
                }),
            },
            T::Keyword(kw::False) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(false),
                }),
            },
            T::String(s) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Str(s.to_owned()),
                }),
            },
            T::Decimal(f) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Float(f),
                }),
            },
            T::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                if !self.peek().map_or(false, |t| t.kind == T::RightParen) {
                    self.error("expected ')'");
                }
                expr
            }
            T::Ident(sym) => Expr {
                kind: ExprKind::Ident(sym),
            },
            T::LeftBrace => {
                let block = self.parse_block().ok()?;
                return Some(Expr {
                    kind: ExprKind::Block(block),
                });
            }
            T::Keyword(kw::While) => return self.while_(),
            T::Keyword(kw::If) => return self.if_(),
            _ => {
                self.error("expected expression");
                return None;
            }
        };
        self.advance();
        Some(expr)
    }
}
