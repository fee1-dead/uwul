#![feature(decl_macro, let_chains)]

mod item;
use ast::ExprKind;
pub use item::*;

mod expr;
pub use expr::*;
use rustc_hash::FxHashMap;
use terryc_ast::{self as ast, DeclId, TyKind, UnOpKind};
use terryc_base::errors::{make_diag, DiagnosticBuilder, DiagnosticSeverity, ErrorReported};
use terryc_base::sym::Symbol;
use terryc_base::{sym, Id, IdMaker};

#[derive(Clone)]
pub struct LocalDecl {
    id: Id,
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
            ast::StmtKind::Expr(expr) => Ok(Stmt::Expr(self.lower_expr(expr)?)),
            ast::StmtKind::Let {
                decl_id: _,
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
                    }
                    .emit());
                };
                let value = value.as_ref().map(|v| self.lower_expr(v)).transpose()?;
                let sym = &name.symbol;
                if self.fn_symbols.contains_key(sym) {
                    DiagnosticBuilder::new(
                        DiagnosticSeverity::Error,
                        format!("`{sym}` clashes with a previous function declaration"),
                        name.span,
                    )
                    .emit();
                }
                let id = self.def_ids.make();
                self.scoped_syms.insert(*sym, LocalDecl { type_: ty, id });
                Ok(Stmt::Local(expr::LocalDecl {
                    id,
                    ty,
                    initializer: value,
                }))
            }
            ast::StmtKind::Item(ast::Item {
                kind:
                    ast::ItemKind::Fn(ast::ItemFn {
                        name,
                        id,
                        args,
                        ret,
                        body,
                    }),
            }) => {
                todo!()
            }
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
            .transpose()?
            .map(Box::new);
        self.scoped_syms = prev_env;
        Ok(Block { statements, expr })
    }

    fn typeck_if(&mut self, e: &ast::ExprIf) -> Result<TyKind, ErrorReported> {
        let ty1 = e
            .block
            .expr
            .as_ref()
            .map(|b| self.typeck(b))
            .transpose()?
            .unwrap_or(TyKind::Unit);
        let (ty2, sp2) = match &e.else_ {
            None => (TyKind::Unit, None),
            Some(ast::Else::Else(b)) => (
                b.expr
                    .as_ref()
                    .map(|b| self.typeck(b))
                    .transpose()?
                    .unwrap_or(TyKind::Unit),
                Some(b.span),
            ),
            Some(ast::Else::ElseIf(elif, sp)) => (self.typeck_if(elif)?, Some(*sp)),
        };

        let mut sp = e.block.span;

        if let Some(s) = sp2 {
            sp = sp.to(s);
        }

        if ty1 != ty2 {
            raise::yeet!(make_diag! {
                Error,
                sp,
                "conflicting types",
            }
            .emit());
        }

        Ok(ty1)
    }

    fn typeck(&mut self, e: &ast::Expr) -> Result<TyKind, ErrorReported> {
        macro restricted_typeck($e:expr, $c:expr, $($kinds:pat),*$(,)?) {
            match self.typeck($e)? {
                $(a @ $kinds => a,)*
                other => return Err(DiagnosticBuilder::new(DiagnosticSeverity::Error, &format!("`{}` cannot be applied to type {:?}", $c, other), e.span).emit()),
            }
        }

        Ok(match &e.kind {
            ast::ExprKind::BinOp(op, expr1, expr2) => {
                let ty1 = restricted_typeck!(expr1, op.as_str(), TyKind::I32, TyKind::F32);
                let ty2 = restricted_typeck!(expr2, op.as_str(), TyKind::I32, TyKind::F32);
                if ty1 != ty2 {
                    return Err(make_diag!(
                        Error,
                        expr1.span.to(expr2.span),
                        "cannot compare two values of different types"
                    )
                    .emit());
                }
                ty1
            }
            ast::ExprKind::UnOp(UnOpKind::Not, expr) => {
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
                    return Err(DiagnosticBuilder::new(
                        DiagnosticSeverity::Error,
                        "unknown identifier",
                        e.span,
                    )
                    .emit());
                }
            }
            ast::ExprKind::Block(block) => {
                if let Some(e) = &block.expr {
                    self.typeck(e)?
                } else {
                    TyKind::Unit
                }
            }
            ast::ExprKind::Assignment { .. } => TyKind::Unit,
            ast::ExprKind::If(if_) => self.typeck_if(if_)?,
            ast::ExprKind::While(_) => TyKind::Unit,
            ast::ExprKind::Call { callee, args } => {
                if let ast::ExprKind::Ident(sym::println) = callee.kind {
                    if let [_] = &**args {
                        TyKind::Unit
                    } else {
                        raise::yeet! {
                            make_diag! {
                                Error,
                                e.span,
                                "`println` takes exact one argument",
                            }.emit()
                        }
                    }
                } else {
                    todo!()
                }
            }
            ast::ExprKind::Group(e, _) => return self.typeck(e),
        })
    }
    fn lower_expr(&mut self, e: &ast::Expr) -> Result<Expr, ErrorReported> {
        Ok(match &e.kind {
            ast::ExprKind::BinOp(_, _, _) => todo!(),
            ast::ExprKind::UnOp(_, _) => todo!(),
            ast::ExprKind::Literal(lit) => Expr::Literal(match lit.kind {
                ast::LiteralKind::Bool(x) => Literal::Bool(x),
                ast::LiteralKind::Int(x) => Literal::Int(x),
                ast::LiteralKind::String(x) => Literal::String(x),
                ast::LiteralKind::Float(x) => Literal::Float(x),
            }),
            ast::ExprKind::Ident(symbol) => {
                        if let Some(decl) = self.scoped_syms.get(symbol) {
                            Expr::Use(decl.id)
                        } else {
                            todo!()
                        }
                    }
                    ast::ExprKind::Block(block) => Expr::Block(self.lower_block(block)?),
                    ast::ExprKind::Assignment { lhs, rhs } => {
                        if let ExprKind::Ident(symbol) = lhs.kind &&
                            let Some(decl) = self.scoped_syms.get(&symbol) {
                            Expr::Assign { to: decl.id, rvalue: Box::new(self.lower_expr(rhs)?) }
                        } else {
                            todo!()
                        }
                    }
                    ast::ExprKind::If(_) => todo!(),
                    ast::ExprKind::While(_) => todo!(),
                    ast::ExprKind::Call { callee, args } => {
                        if let ExprKind::Ident(sym::println) = callee.kind && let [arg1] = &**args {
                            Expr::Call { callee: HirId { id: u32::MAX }, args: vec![self.lower_expr(arg1)?] }
                        } else {
                            todo!()
                        }
                    }
                    ast::ExprKind::Group(e, _) => Expr::Group(Box::new(self.lower_expr(e)?)),
        })
    }
}
