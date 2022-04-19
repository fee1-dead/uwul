mod item;
pub use item::*;

mod expr;
pub use expr::*;
use rustc_hash::FxHashMap;

use crate::ast::{self, DeclId, UnOpKind, TyKind};
use crate::lex::ErrorReported;
use crate::sym::Symbol;

pub struct AstLowerer {
    fn_symbols: FxHashMap<Symbol, DeclId>,
    pub had_errors: bool,
}

impl AstLowerer {
    fn error(&mut self, message: &str) -> ErrorReported {
        self.had_errors = true;
        eprintln!("error: {message}");
        ErrorReported
    }

    fn typeck(&mut self, e: &ast::Expr) -> Result<TyKind, ErrorReported> {
        Ok(match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(UnOpKind::Bang, expr) => match self.typeck(&**expr)? {
                TyKind::Bool => TyKind::Bool,
                other => {
                    return Err(self.error(&format!(
                        "this operator cannot be applied to type {other:?}"
                    )))
                }
            },
            ast::ExprKind::UnOp(UnOpKind::Minus, expr) => match self.typeck(&**expr)? {
                TyKind::I32 => TyKind::I32,
                TyKind::F32 => TyKind::F32,
                other => {
                    return Err(self.error(&format!(
                        "this operator cannot be applied to type {other:?}"
                    )))
                }
            },
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
    fn lower_expr(e: ast::Expr) -> Expr {
        match e.kind {
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
