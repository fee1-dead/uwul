#![feature(decl_macro)]

mod item;
pub use item::*;

mod expr;
pub use expr::*;
use rustc_hash::FxHashMap;
use terryc_ast::{self as ast, DeclId, TyKind, UnOpKind};
use terryc_base::{IdMaker, Id};
use terryc_base::errors::{ErrorReported, DiagnosticBuilder, DiagnosticSeverity, make_diag};
use terryc_base::sym::Symbol;

#[derive(Clone)]
pub struct LocalDecl {
    type_: TyKind,
}

pub struct AstLowerer {
    fn_symbols: FxHashMap<Symbol, DeclId>,
    scoped_syms: FxHashMap<Symbol, LocalDecl>,
    all_things: FxHashMap<Id, LocalDecl>,
    all_items: Vec<item::Item>,
    def_ids: IdMaker,
    pub had_errors: bool,
}

impl AstLowerer {
    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Result<Stmt, ErrorReported> {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => Ok(Stmt::Expr(self.lower_expr(expr))),
            ast::StmtKind::Let {
                decl_id,
                name,
                value,
            } => {
                let ty = if let Some(val) = value {
                    self.typeck(val)?
                } else {
                    raise::yeet!(make_diag! {
                        Error,
                        name.span,
                        "missing type annotation for `{}`",
                        name.symbol
                    }.emit());
                };
                let value = value.as_ref().map(|v| self.lower_expr(v));
                let sym = &name.symbol;
                if self.fn_symbols.contains_key(sym) {
                    DiagnosticBuilder::new(DiagnosticSeverity::Error, format!("`{sym}` clashes with a previous function declaration"), name.span).emit();
                }
                self.scoped_syms.insert(*sym, LocalDecl { type_: ty  });
                todo!()
            }
            ast::StmtKind::Item(_) => todo!(),
        }
    }

    fn lower_block(&mut self, block: &ast::Block) -> Result<Block, ErrorReported> {
        let mut statements = vec![];
        let prev_env = self.scoped_syms.clone();
        for stmt in &block.stmts {
            statements.push(self.lower_stmt(stmt)?);
        }
        let expr = block
            .expr
            .as_ref()
            .map(|e| self.lower_expr(e))
            .map(Box::new);
        self.scoped_syms = prev_env;
        Ok(Block {
            statements,
            expr,
        })
    }

    fn typeck(&mut self, e: &ast::Expr) -> Result<TyKind, ErrorReported> {
        macro restricted_typeck($e:expr, $c:literal, $($kinds:pat),*$(,)?) {
            match self.typeck($e)? {
                $(a @ $kinds => a,)*
                other => return Err(DiagnosticBuilder::new(DiagnosticSeverity::Error, &format!(concat!('`', $c, "` cannot be applied to type {:?}"), other), e.span).emit()),
            }
        }

        
        Ok(match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(UnOpKind::Bang, expr) => {
                restricted_typeck!(expr, '!', TyKind::Bool)
            }
            ast::ExprKind::UnOp(UnOpKind::Minus, expr) => {
                restricted_typeck!(expr, '-', TyKind::I32, TyKind::F32)
            }
            ast::ExprKind::Literal(lit) => lit.kind.ty(),
            ast::ExprKind::Ident(ident) => {
                if let Some(decl) = self.scoped_syms.get(ident) {
                    decl.type_
                } else {
                    return Err(DiagnosticBuilder::new(DiagnosticSeverity::Error, "unknown identifier", e.span).emit())
                }
            }
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
