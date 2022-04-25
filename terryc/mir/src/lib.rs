use std::rc::Rc;

use index_vec::IndexVec;
use terryc_base::ast::TyKind;
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Resolution, Literal};
use terryc_base::mir::{Body, Local, LocalData, BasicBlockData, Statement, Rvalue, Operand, Terminator};
use terryc_base::{Context, FileId, Providers, hir, Id, sym};

fn mir(cx: &dyn Context, id: FileId) -> Result<Rc<Body>, ErrorReported> {
    let hir = cx.hir(id)?;

    todo!()
}

#[derive(Default)]
pub struct HirInfo {
    pub id_to_local: FxHashMap<Id, Local>,
}

fn new_bb() -> BasicBlockData {
    BasicBlockData { statements: vec![], terminator: Terminator::ReplacedAfterConstruction }
}

fn expr_to_rvalue(expr: &hir::Expr, b: &mut Body, info: &mut HirInfo) -> Rvalue {
    match expr {
        hir::Expr::Block(block) => {
            collect_into(&block.statements, b, info);
            if let Some(e) = &block.expr {
                expr_to_rvalue(e, b, info)
            } else {
                Rvalue::Use(Operand::Const(Literal::Unit))
            }
        }
        hir::Expr::Call { callee: Resolution::Builtin(sym), args } => {
            if *sym != sym::println {
                todo!()
            }
            let last = b.blocks.last_idx();
            let newbb = b.blocks.next_idx();
            let ret = b.locals.push(LocalData { ty: TyKind::Unit });
            let args = args.iter().map(|e| expr_to_rvalue(e, b, info)).collect();

            let term = Terminator::Call { callee: *sym, args, destination: (ret, newbb) };
            b.blocks[last].terminator = term;
            b.blocks.push(new_bb());
            Rvalue::Use(Operand::Copy(ret))
        }
        hir::Expr::Call { callee: Resolution::Local(_), args } => todo!(),
        hir::Expr::If { cond, then } => todo!(),
        hir::Expr::While { cond, body } => todo!(),
        hir::Expr::Assign { to, rvalue } => {
            let local = match to {
                Resolution::Builtin(_) => todo!(),
                Resolution::Local(id) => info.id_to_local[id]
            };
            let op = expr_to_rvalue(rvalue, b, info);
            b.expect_last_mut().statements.push(Statement::Assign(local, Rvalue::Use(op)));
            Rvalue::Use(Operand::Const(Literal::Unit))
        }
        hir::Expr::Literal(lit) => Rvalue::Use(Operand::Const(*lit)),
        hir::Expr::Group(e) => expr_to_rvalue(e, b, info),
        hir::Expr::Resolved(Resolution::Builtin(_)) => todo!(),
        hir::Expr::Resolved(Resolution::Local(id)) => {
            Rvalue::Use(Operand::Copy(*info.id_to_local.get(&id).unwrap()))
        }
        hir::Expr::BinOp(kind, e, e2) => {
            let left = 
        }
        hir::Expr::UnOp(kind, _) => todo!(),
    }
}

fn collect_into(hir: &[hir::Stmt], b: &mut Body, info: &mut HirInfo) {
    b.blocks.push(new_bb());
    for stmt in hir {
        match stmt {
            hir::Stmt::Local(hir::LocalDecl { id, ty, initializer }) => {
                let local = b.locals.push(LocalData { ty: *ty });
                if let Some(init) = initializer {
                    let op = expr_to_rvalue(init, b, info);
                    b.expect_last_mut().statements.push(Statement::Assign(local, Rvalue::Use(op)));
                }
                info.id_to_local.insert(*id, local);
            }
            hir::Stmt::Expr(e) => {
                let _ = expr_to_rvalue(e, b, info);
            }
            hir::Stmt::Item(_) => {}
        }
    }
}

pub fn provide(p: &mut Providers) {
    *p = Providers {
        mir,
        ..*p
    }
}