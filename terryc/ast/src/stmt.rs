use terryc_base::ast::*;
use terryc_base::errors::ErrorReported;
use terryc_base::lex::TokenKind as T;
use terryc_base::sym::kw;

use crate::Parser;

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

        let user_ty = self.eat(T::Colon).then(|| self.parse_ty()).transpose()?;

        let value = self.eat(T::Eq).then(|| self.parse_expr()).transpose()?;

        let kind = StmtKind::Let {
            id: self.mk_id(),
            user_ty,
            name,
            value,
        };
        Ok(Stmt { kind })
    }

    fn stmt(&mut self) -> Result<Stmt, ErrorReported> {
        let kind = if self.eat_kw(kw::Let) {
            return self.var();
        } else if self.check_kw(kw::Fn) {
            self.parse_item().map(StmtKind::Item)?
        } else {
            StmtKind::Expr(self.parse_expr()?)
        };

        Ok(Stmt { kind })
    }
}
