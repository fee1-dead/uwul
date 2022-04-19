mod item;
pub use item::*;

mod expr;
pub use expr::*;
use rustc_hash::FxHashMap;

use crate::ast::{self, DeclId, UnOpKind, TyKind};
use crate::lex::ErrorReported;
use crate::sym::Symbol;

#[derive(Clone)]
pub struct LocalDecl {
    type_: TyKind,
}

pub struct AstLowerer {
    fn_symbols: FxHashMap<Symbol, DeclId>,
    scoped_syms: FxHashMap<Symbol, LocalDecl>,
    all_items: Vec<item::Item>,
    pub had_errors: bool,
}

impl AstLowerer {
    fn error(&mut self, message: &str) -> ErrorReported {
        self.had_errors = true;
        eprintln!("error: {message}");
        ErrorReported
    }

    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Stmt {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => Stmt::Expr(self.lower_expr(expr)),
            ast::StmtKind::Let { decl_id, name, value } => {
                todo!()
            }
            ast::StmtKind::Item(_) => todo!(),
        }
    }

    fn lower_block(&mut self, block: &ast::Block) -> Block {
        let mut statements = vec![];
        let prev_env = self.scoped_syms.clone();
        for stmt in &block.stmts {
            statements.push(self.lower_stmt(stmt));
        }
        self.scoped_syms = prev_env;
        Block { statements, expr: block.expr.as_ref().map(|e| self.lower_expr(e)).map(Box::new) }
    }

    fn typeck(&mut self, e: &ast::Expr) -> Result<TyKind, ErrorReported> {
        macro restricted_typeck($e:expr, $c:literal, $($kinds:pat),*$(,)?) {
            match self.typeck($e)? {
                $(a @ $kinds => a,)*
                other => return Err(self.error(&format!(concat!('`', $c, "` cannot be applied to type {:?}"), other))),
            }
        }

        Ok(match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(UnOpKind::Bang, expr) => restricted_typeck!(expr, '!', TyKind::Bool),
            ast::ExprKind::UnOp(UnOpKind::Minus, expr) => restricted_typeck!(expr, '-', TyKind::I32, TyKind::F32),
            ast::ExprKind::Literal(lit) => lit.kind.ty(),
            ast::ExprKind::Ident(_) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Assignment { lhs, rhs } => todo!(),
            ast::ExprKind::If(_) => todo!(),
            ast::ExprKind::While(_) => TyKind::Unit,
            ast::ExprKind::Call { callee, args } => todo!(),
            ast::ExprKind::Group(e, _) => return self.typeck(e),
        })
    }
    fn lower_expr(&mut self, e: &ast::Expr) -> Expr {
        match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(_, _) => todo!(),
            ast::ExprKind::Literal(_) => todo!(),
            ast::ExprKind::Ident(_) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Assignment { lhs, rhs } => todo!(),
            ast::ExprKind::If(_) => todo!(),
            ast::ExprKind::While(_) => todo!(),
            ast::ExprKind::Call { callee, args } => todo!(),
            ast::ExprKind::Group(_, _) => todo!(),
        }
    }
}
