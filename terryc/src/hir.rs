mod item;
pub use item::*;

mod expr;
pub use expr::*;
use rustc_hash::FxHashMap;

use crate::ast::{DeclId, self};
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

    fn typeck(e: &ast::Expr) -> Ty {
        match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(_, _) => todo!(),
            ast::ExprKind::Literal(ast::Literal { kind: ast::LiteralKind::Float(_) }) => Ty::F32,
            ast::ExprKind::Literal(ast::Literal { kind: ast::LiteralKind::Int(_) }) => Ty::I32,
            ast::ExprKind::Literal(ast::Literal { kind: ast::LiteralKind::String(_) }) => Ty::String,
            ast::ExprKind::Literal(ast::Literal { kind: ast::LiteralKind::Bool(_) }) => Ty::Bool,
            ast::ExprKind::Ident(_) => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Assignment { lhs, rhs } => todo!(),
            ast::ExprKind::If(_) => todo!(),
            ast::ExprKind::While { expr, block } => todo!(),
            ast::ExprKind::Call { callee, args } => todo!(),
        }
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
            ast::ExprKind::While { expr, block } => todo!(),
            ast::ExprKind::Call { callee, args } => todo!(),
        }
    }
}