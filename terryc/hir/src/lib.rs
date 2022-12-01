#![feature(decl_macro, let_chains, iter_intersperse)]
#![warn(rust_2018_idioms)]

use std::collections::hash_map::Entry;

use ast::{BinOpKind, Ty};
use rustc_hash::{FxHashMap, FxHashSet};
use terryc_ast::{self as ast, TyKind, UnOpKind};
use terryc_base::ast::ExprKind;
use terryc_base::errors::{make_diag, DiagnosticBuilder, DiagnosticSeverity, ErrorReported};
pub use terryc_base::hir::*;
use terryc_base::sym::Symbol;
use terryc_base::{sym, Context, FileId, Id, IdMaker, Providers, Span};

#[derive(Clone)]
pub struct ResolvedDecl {
    id: Id,
    type_: TyKind,
}

#[derive(Clone, Copy)]
pub enum TypeckExpectation<'a> {
    NoExpectation,
    Equals {
        ty: TyKind,
        sp: Span,
    },
    AnyOf {
        tys: &'a FxHashSet<TyKind>,
        sp: Span,
    },
}

impl From<Ty> for TypeckExpectation<'_> {
    fn from(ty: Ty) -> Self {
        Self::Equals {
            ty: ty.kind,
            sp: ty.span,
        }
    }
}

impl TypeckExpectation<'_> {
    pub fn check(&self, result: TyKind, res: Span) -> Result<(), ErrorReported> {
        match self {
            Self::Equals { ty, sp } if result != *ty => Err(make_diag! {
                Error,
                res,
                "mismatched types",
            }
            .note(format_args!("expected `{ty}`, found `{result}`"))
            .span_note(*sp, "expected because of this")
            .emit()),
            Self::AnyOf { tys, sp } if !tys.contains(&result) => Err(make_diag! {
                Error,
                res,
                "mismatched types",
            }
            .note(format_args!(
                "expected one of {}, found `{result}`",
                tys.iter()
                    .map(|x| format!("`{x}`"))
                    .intersperse(", ".to_string())
                    .collect::<String>()
            ))
            .span_note(*sp, "expected because of this")
            .emit()),
            Self::Equals { .. } | Self::AnyOf { .. } | Self::NoExpectation => Ok(()),
        }
    }
}

#[derive(Default)]
pub struct AstLowerer {
    fn_symbols: FxHashMap<Symbol, Id>,
    scoped_syms: FxHashMap<Symbol, ResolvedDecl>,
    functions: FxHashMap<Id, Func>,
    // all_items: Vec<Item>,
    def_ids: IdMaker,
    current_func_ret_ty: Option<Ty>,
    pub had_errors: bool,
}

impl AstLowerer {
    fn lower_ty(&mut self, ty: &Ty) -> TyKind {
        match ty.kind {
            ast::TyKind::I32 => TyKind::I32,
            ast::TyKind::Unit => TyKind::Unit,
            _ => todo!(),
        }
    }
    fn lower_item(&mut self, item: &ast::Item) -> Result<Item, ErrorReported> {
        match &item.kind {
            ast::ItemKind::Mod { name, tree } => {
                Ok(Item::Mod { name: *name, tree: AstLowerer::default().lower_tree(tree)? })
            }
            ast::ItemKind::Fn(ast::ItemFn {
                name,
                id,
                args,
                ret,
                body,
            }) => match self.fn_symbols.entry(name.symbol) {
                Entry::Occupied(_) => {
                    raise::yeet!(
                        make_diag!(Error, name.span, "function clashes with variable").emit()
                    );
                }
                Entry::Vacant(v) => {
                    v.insert(*id);
                    self.functions.insert(
                        *id,
                        Func {
                            name: *name,
                            args: args.iter().map(|(_, t)| *t).collect(),
                            ret: ret.kind,
                        },
                    );
                    let mut lowered_args = Vec::with_capacity(args.len());
                    let prev = self.scoped_syms.clone();
                    self.current_func_ret_ty = Some(*ret);
                    for (ident, ty) in args {
                        let id = self.def_ids.make();
                        let ty = self.lower_ty(ty);
                        self.scoped_syms
                            .insert(ident.symbol, ResolvedDecl { id, type_: ty });
                        lowered_args.push(FnArg {
                            name: *ident,
                            ty,
                            id,
                        })
                    }
                    let block = self.lower_block(body, (*ret).into())?;
                    self.scoped_syms = prev;
                    self.current_func_ret_ty = None;
                    Ok(Item::Fn(ItemFn {
                        id: *id,
                        name: name.symbol,
                        args: lowered_args,
                        ret: self.lower_ty(ret),
                        block,
                    }))
                }
            },
        }
    }
    fn lower_stmt(&mut self, stmt: &ast::Stmt) -> Result<Stmt, ErrorReported> {
        match &stmt.kind {
            ast::StmtKind::Expr(expr) => Ok(Stmt::Expr(
                self.lower_expr(expr, TypeckExpectation::NoExpectation)?,
            )),
            ast::StmtKind::Let {
                id: _,
                name,
                user_ty,
                value,
            } => {
                let expectation = user_ty
                    .map(|x| TypeckExpectation::Equals {
                        ty: x.kind,
                        sp: x.span,
                    })
                    .unwrap_or(TypeckExpectation::NoExpectation);
                let ty = if let Some(val) = value {
                    self.typeck(val, expectation)?
                } else if let Some(user_ty) = user_ty {
                    user_ty.kind
                } else {
                    raise::yeet!(make_diag! {
                        Error,
                        name.span,
                        "missing type annotation for `{}`",
                        name.symbol
                    }
                    .emit());
                };

                let value = value
                    .as_ref()
                    .map(|v| self.lower_expr(v, expectation))
                    .transpose()?;
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
                self.scoped_syms
                    .insert(*sym, ResolvedDecl { type_: ty, id });
                Ok(Stmt::Local(LocalDecl {
                    id,
                    ty,
                    initializer: value,
                }))
            }
            ast::StmtKind::Item(item) => Ok(Stmt::Item(self.lower_item(item)?)),
        }
    }

    fn lower_block(
        &mut self,
        block: &ast::Block,
        expectation: TypeckExpectation<'_>,
    ) -> Result<Block, ErrorReported> {
        let mut statements = vec![];
        let prev_env = self.scoped_syms.clone();
        for stmt in &block.stmts {
            statements.push(self.lower_stmt(stmt)?);
        }
        let expr = block
            .expr
            .as_ref()
            .map(|e| self.lower_expr(e, expectation))
            .transpose()?
            .map(Box::new);
        self.scoped_syms = prev_env;
        Ok(Block { statements, expr })
    }

    fn typeck_if(
        &mut self,
        e: &ast::ExprIf,
        sp: Span,
        expectation: TypeckExpectation<'_>,
    ) -> Result<TyKind, ErrorReported> {
        let new_expectation = if e.else_.is_some() {
            expectation
        } else {
            TypeckExpectation::NoExpectation
        };
        let ty1 = e
            .block
            .expr
            .as_ref()
            .map(|b| self.typeck(b, new_expectation))
            .transpose()?
            .unwrap_or(TyKind::Unit);
        let else_expect = TypeckExpectation::Equals {
            ty: ty1,
            sp: e.block.span,
        };
        let (ty2, sp2) = match &e.else_ {
            None => (TyKind::Unit, None),
            Some(ast::Else::Else(b)) => (
                b.expr
                    .as_ref()
                    .map(|b| self.typeck(b, else_expect))
                    .transpose()?
                    .unwrap_or(TyKind::Unit),
                Some(b.span),
            ),
            Some(ast::Else::ElseIf(elif, sp)) => {
                (self.typeck_if(elif, *sp, else_expect)?, Some(*sp))
            }
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

        expectation.check(ty1, sp)?;

        Ok(ty1)
    }

    fn typeck(
        &mut self,
        e: &ast::Expr,
        expectation: TypeckExpectation<'_>,
    ) -> Result<TyKind, ErrorReported> {
        let ty = match &e.kind {
            ast::ExprKind::BinOp(op, expr1, expr2) => {
                let mut set = FxHashSet::default();
                set.extend([TyKind::I32, TyKind::F32]);
                let ty1 = self.typeck(
                    expr1,
                    TypeckExpectation::AnyOf {
                        tys: &set,
                        sp: e.span,
                    },
                )?;
                let ty2 = self.typeck(
                    expr2,
                    TypeckExpectation::Equals {
                        ty: ty1,
                        sp: expr1.span,
                    },
                )?;
                if ty1 != ty2 {
                    return Err(make_diag!(
                        Error,
                        expr1.span.to(expr2.span),
                        "cannot compare two values of different types"
                    )
                    .emit());
                }
                match op {
                    BinOpKind::Add
                    | BinOpKind::Div
                    | BinOpKind::Mod
                    | BinOpKind::Mul
                    | BinOpKind::Sub => ty1,
                    BinOpKind::Less
                    | BinOpKind::LessEqual
                    | BinOpKind::Greater
                    | BinOpKind::GreaterEqual
                    | BinOpKind::Equal
                    | BinOpKind::NotEqual => TyKind::Bool,
                }
            }
            ast::ExprKind::UnOp(UnOpKind::Not, expr) => self.typeck(
                expr,
                TypeckExpectation::Equals {
                    ty: TyKind::Bool,
                    sp: e.span,
                },
            )?,
            ast::ExprKind::UnOp(UnOpKind::Minus, expr) => {
                let mut set = FxHashSet::default();
                set.extend([TyKind::I32, TyKind::F32]);
                self.typeck(
                    expr,
                    TypeckExpectation::AnyOf {
                        tys: &set,
                        sp: e.span,
                    },
                )?
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
                    self.typeck(e, expectation)?
                } else {
                    TyKind::Unit
                }
            }
            ast::ExprKind::Assignment { .. } => TyKind::Unit,
            ast::ExprKind::If(if_) => self.typeck_if(if_, e.span, expectation)?,
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
                } else if let ast::ExprKind::Ident(i) = callee.kind {
                    if let Some(&f) = self.fn_symbols.get(&i) {
                        let arg_types = self.functions[&f].args.clone();
                        let types = args
                            .iter()
                            .zip(arg_types)
                            .map(|(expr, ty)| self.typeck(expr, ty.into()))
                            .collect::<Result<Vec<_>, _>>()?;
                        self.functions[&f].ret
                    } else {
                        raise::yeet! {
                            make_diag! {
                                Error,
                                callee.span,
                                "unresolved function call",
                            }.emit()
                        }
                    }
                } else {
                    todo!()
                }
            }
            ast::ExprKind::Group(e, _) => return self.typeck(e, expectation),
            ast::ExprKind::Return(e, _) => {
                self.typeck(e, self.current_func_ret_ty.unwrap().into())?;
                TyKind::Unit
            }
        };

        expectation.check(ty, e.span)?;
        Ok(ty)
    }
    fn resolve(&mut self, sym: Symbol) -> Result<Resolution, ErrorReported> {
        Ok(if let Some(decl) = self.scoped_syms.get(&sym) {
            Resolution::Local(decl.id)
        } else if sym == sym::println {
            Resolution::Builtin(sym)
        } else if let Some(decl) = self.fn_symbols.get(&sym) {
            Resolution::Fn(*decl)
        } else {
            todo!("{sym}")
        })
    }
    fn lower_expr(
        &mut self,
        e: &ast::Expr,
        expectation: TypeckExpectation<'_>,
    ) -> Result<Expr, ErrorReported> {
        Ok(match &e.kind {
            ast::ExprKind::BinOp(kind, left, right) => {
                self.typeck(e, expectation)?;

                let lety = self.typeck(left, TypeckExpectation::NoExpectation)?;
                let expect = Ty {
                    kind: lety,
                    span: left.span,
                }
                .into();
                Expr::BinOp(
                    *kind,
                    Box::new(self.lower_expr(left, expect)?),
                    Box::new(self.lower_expr(right, expect)?),
                    lety,
                )
            }
            ast::ExprKind::UnOp(kind, expr) => {
                self.typeck(e, expectation)?;
                let ety = self.typeck(expr, expectation)?;
                Expr::UnOp(*kind, Box::new(self.lower_expr(expr, expectation)?), ety)
            }
            ast::ExprKind::Literal(lit) => Expr::Literal(match lit.kind {
                ast::LiteralKind::Bool(x) => Literal::Bool(x),
                ast::LiteralKind::Int(x) => Literal::Int(x),
                ast::LiteralKind::String(x) => Literal::String(x),
                ast::LiteralKind::Float(x) => Literal::Float(x),
            }),
            ast::ExprKind::Ident(symbol) => self.resolve(*symbol).map(Expr::Resolved)?,
            ast::ExprKind::Block(block) => Expr::Block(self.lower_block(block, expectation)?),
            ast::ExprKind::Assignment { lhs, rhs } => {
                if let ExprKind::Ident(symbol) = lhs.kind {
                    Expr::Assign {
                        to: self.resolve(symbol)?,
                        rvalue: Box::new(self.lower_expr(rhs, expectation)?),
                    }
                } else {
                    todo!()
                }
            }
            ast::ExprKind::If(ast::ExprIf {
                expr,
                block,
                else_: None,
            }) => Expr::If {
                cond: self
                    .lower_expr(
                        expr,
                        TypeckExpectation::Equals {
                            ty: TyKind::Bool,
                            sp: e.span,
                        },
                    )
                    .map(Box::new)?,
                then: self.lower_block(block, TypeckExpectation::NoExpectation)?,
            },
            ast::ExprKind::If(_) => todo!(),
            ast::ExprKind::While(_) => todo!(),
            ast::ExprKind::Call { callee, args } => match (&callee.kind, &**args) {
                (ExprKind::Ident(i), args) => {
                    let re = self.resolve(*i)?;
                    let (ret, arg_expectations) = match re {
                        Resolution::Builtin(sym::println) => (TyKind::Unit, None),
                        Resolution::Builtin(_) | Resolution::Local(_) => todo!(),
                        Resolution::Fn(id) => {
                            (self.functions[&id].ret, Some(&self.functions[&id].args))
                        }
                    };
                    let expectations = arg_expectations
                        .map(|x| x.iter().copied().map(|x| x.into()).collect())
                        .unwrap_or_else(|| vec![TypeckExpectation::NoExpectation]);
                    Expr::Call {
                        callee: re,
                        args: args
                            .iter()
                            .zip(expectations)
                            .map(|(expr, expectation)| {
                                Ok::<_, _>((
                                    self.lower_expr(expr, expectation)?,
                                    self.typeck(expr, expectation)?,
                                ))
                            })
                            .collect::<Result<_, ErrorReported>>()?,
                        ret,
                    }
                }
                _ => todo!(),
            },
            ast::ExprKind::Group(e, _) => Expr::Group(Box::new(self.lower_expr(e, expectation)?)),
            ast::ExprKind::Return(e, _) => {
                let expectation = self.current_func_ret_ty.unwrap().into();
                Expr::Return(
                    Box::new(self.lower_expr(e, expectation)?),
                    self.typeck(e, expectation)?,
                )
            }
        })
    }

    fn lower_tree(mut self, ast: &ast::Tree) -> Result<HirTree, ErrorReported> {
        let items = ast.items.iter().map(|item| self.lower_item(item)).collect::<Result<_, _>>()?;
        Ok(HirTree { items, functions: self.functions })
    }
}

fn hir(cx: &dyn Context, id: FileId) -> Result<HirTree, ErrorReported> {
    AstLowerer::default().lower_tree(&cx.parse(id)?)
}

pub fn provide(p: &mut Providers) {
    *p = Providers { hir, ..*p };
}
