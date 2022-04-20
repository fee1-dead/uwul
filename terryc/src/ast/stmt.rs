use std::fmt;

use super::{DeclId, Expr, Item, Parser};
use crate::lex::{ErrorReported, TokenKind as T, Ident, Span};
use crate::sym::{kw, Symbol};

pub struct Stmt {
    pub kind: StmtKind,
}

pub struct Block {
    pub stmts: Vec<Stmt>,
    /// Optional trailing expression
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

pub struct Function {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Block,
}

pub enum StmtKind {
    Expr(Expr),
    Let {
        decl_id: DeclId,
        name: Ident,
        value: Option<Expr>,
    },
    Item(Item),
}

impl fmt::Debug for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => expr.fmt(f),
            StmtKind::Let {
                name,
                value,
                decl_id: _,
            } => {
                write!(f, "let {name}")?;
                if let Some(value) = value {
                    f.write_str(" = ")?;
                    value.fmt(f)?;
                }
                Ok(())
            }
            StmtKind::Item(_) => todo!(),
        }
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut set = f.debug_set();
        set.entries(&self.stmts);

        if let Some(expr) = &self.expr {
            set.entry(expr);
        }
        set.finish()
    }
}

impl<'a> Parser<'a> {
    pub fn parse_stmts(&mut self) -> Result<Vec<Stmt>, ErrorReported> {
        let mut stmts = vec![];
        while !self.is_end() {
            if let Ok(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                self.has_errors = true;
                self.synchronize();
            }
        }

        if self.has_errors {
            Err(ErrorReported)
        } else {
            Ok(stmts)
        }
    }

    pub fn parse_block(&mut self) -> Result<Block, ErrorReported> {
        if !self.eat(T::LeftBrace) {
            return Err(self.error("expected block"));
        }

        let lbrace = self.prev_token.span;

        let mut stmts = vec![];
        let mut trailing = None;
        while !self.eat(T::RightBrace) && !self.is_end() {
            if let Ok(stmt) = self.stmt() {
                if self.eat(T::RightBrace) && let StmtKind::Expr(e) = stmt.kind {
                    trailing = Some(Box::new(e));
                    break;
                }

                self.stmt_end(&stmt);

                stmts.push(stmt);
            } else {
                self.has_errors = true;
                self.synchronize();
            }
        }

        if self.has_errors {
            Err(ErrorReported)
        } else {
            Ok(Block {
                stmts,
                expr: trailing,
                span: lbrace.to(self.prev_token.span),
            })
        }
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ErrorReported> {
        let stmt = self.stmt()?;

        self.stmt_end(&stmt);

        Ok(stmt)
    }

    fn needs_semicolon(stmt: &Stmt) -> bool {
        match &stmt.kind {
            StmtKind::Expr(e) => !e.kind.has_block(),
            StmtKind::Let { .. } => true,
            StmtKind::Item(_) => false,
        }
    }
    fn stmt_end(&mut self, stmt: &Stmt) {
        if Self::needs_semicolon(stmt) && !self.eat(T::Semicolon) {
            self.error("expected semicolon");
            self.synchronize();
        }
    }

    fn var(&mut self) -> Result<Stmt, ErrorReported> {
        let name = self.expect_ident()?;

        let value = if self.eat(T::Eq) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        let kind = StmtKind::Let {
            decl_id: self.mk_id(),
            name,
            value,
        };
        Ok(Stmt { kind })
    }

    fn stmt(&mut self) -> Result<Stmt, ErrorReported> {
        let kind = if self.eat_kw(kw::Let) {
            return self.var();
        } else if self.eat_kw(kw::Fn) {
            self.parse_item().map(StmtKind::Item)?
        } else {
            StmtKind::Expr(self.parse_expr()?)
        };

        Ok(Stmt { kind })
    }
}
