use std::rc::Rc;

use terryc_base::ast::TyKind;
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Func, HirTree, ItemFn, Literal, Resolution};
use terryc_base::mir::{
    BasicBlockData, Body, Function, Local, LocalData, MirTree, Operand, Rvalue, Statement, Targets,
    Terminator,
};
use terryc_base::{hir, Context, ContextExt, FileId, Id, Providers};

fn mir(cx: &dyn Context, id: FileId) -> Result<MirTree, ErrorReported> {
    let HirTree { functions, items } = cx.hir(id)?;
    let mut info = HirInfo::new(functions);
    let items = items.iter().map(
        |hir::Item::Fn(ItemFn {
             name,
             id,
             args,
             ret,
             block,
         })| {
            info.id_to_local.clear();
            let mut body = Body::default();
            for arg in args {
                let local = body.locals.push(LocalData { ty: arg.ty });
                info.id_to_local.insert(arg.id, local);
            }
            body.blocks.push(new_bb());
            collect_into(cx, &block.statements, &mut body, &mut info);
            let ret_place = body.locals.push(LocalData { ty: *ret });
            if let Some(e) = &block.expr {
                let rv = expr_to_rvalue(cx, e, &mut body, &mut info);
                if *ret != TyKind::Unit {
                    body.expect_last_mut()
                        .statements
                        .push(Statement::Assign(ret_place, rv));
                }
            }
            body.expect_last_mut().terminator = Terminator::Return(ret_place);
            (
                *id,
                Function {
                    body,
                    name: *name,
                    args: cx.intern_types(args.iter().map(|arg| arg.ty)),
                    ret: *ret,
                },
            )
        },
    );
    let items = Rc::new(items.collect());

    Ok(MirTree { functions: items })
}

pub struct HirInfo {
    pub id_to_local: FxHashMap<Id, Local>,
    pub id_to_func: FxHashMap<Id, Func>,
}

impl HirInfo {
    fn new(id_to_func: FxHashMap<Id, Func>) -> Self {
        Self {
            id_to_local: FxHashMap::default(),
            id_to_func,
        }
    }
}

fn new_bb() -> BasicBlockData {
    BasicBlockData {
        statements: vec![],
        terminator: Terminator::ReplacedAfterConstruction,
    }
}

fn rvalue_to_operand(rvalue: Rvalue, ty: TyKind, b: &mut Body) -> Operand {
    match rvalue {
        Rvalue::Use(operand) => operand,
        Rvalue::BinaryOp(op, lhs, rhs) => {
            let local = b.locals.push(LocalData { ty });
            b.expect_last_mut()
                .statements
                .push(Statement::Assign(local, Rvalue::BinaryOp(op, lhs, rhs)));
            Operand::Copy(local)
        }
        Rvalue::UnaryOp(op, operand) => {
            let local = b.locals.push(LocalData { ty });
            b.expect_last_mut()
                .statements
                .push(Statement::Assign(local, Rvalue::UnaryOp(op, operand)));
            Operand::Copy(local)
        }
    }
}

fn expr_to_rvalue(cx: &dyn Context, expr: &hir::Expr, b: &mut Body, info: &mut HirInfo) -> Rvalue {
    match expr {
        hir::Expr::Block(block) => {
            collect_into(cx, &block.statements, b, info);
            if let Some(e) = &block.expr {
                expr_to_rvalue(cx, e, b, info)
            } else {
                Rvalue::Use(Operand::Const(Literal::Unit))
            }
        }
        hir::Expr::Call { callee, args, ret } => {
            let last = b.blocks.last_idx();
            let newbb = b.blocks.next_idx();
            let ret = b.locals.push(LocalData { ty: *ret });
            let (args, types): (_, Vec<_>) = args
                .iter()
                .map(|(e, ty)| (expr_to_rvalue(cx, e, b, info), *ty))
                .unzip();

            let term = Terminator::Call {
                callee: *callee,
                args,
                destination: (ret, newbb),
                types: cx.intern_types(types),
            };
            b.blocks[last].terminator = term;
            b.blocks.push(new_bb());
            Rvalue::Use(Operand::Copy(ret))
        }
        hir::Expr::If { cond, then } => {
            let newbb = b.blocks.next_idx();
            let oldbb = newbb - 1;
            // write the condition to the current block, performing computations in the statements if necessary.
            let condition = expr_to_rvalue(cx, cond, b, info);
            b.blocks.push(new_bb());
            collect_into(cx, &then.statements, b, info);
            if let Some(e) = &then.expr {
                expr_to_rvalue(cx, e, b, info);
            }

            // N.B. since collection might push new basic blocks we defer setting the `if`
            // terminator until we have figured out the basic blocks for the statements in the `if`.
            b.blocks[oldbb].terminator = Terminator::SwitchInt(
                condition,
                Targets {
                    values: vec![1],
                    targets: vec![newbb, b.blocks.next_idx()],
                },
            );
            b.expect_last_mut().terminator = Terminator::Goto(b.blocks.next_idx());
            b.blocks.push(new_bb());
            Rvalue::Use(Operand::Const(Literal::Unit))
        }
        hir::Expr::While { cond: _, body: _ } => todo!(),
        hir::Expr::Assign { to, rvalue } => {
            let local = match to {
                Resolution::Builtin(_) => todo!(),
                Resolution::Local(id) => info.id_to_local[id],
                Resolution::Fn(_) => todo!(),
            };
            let op = expr_to_rvalue(cx, rvalue, b, info);
            b.expect_last_mut()
                .statements
                .push(Statement::Assign(local, op));
            Rvalue::Use(Operand::Const(Literal::Unit))
        }
        hir::Expr::Literal(lit) => Rvalue::Use(Operand::Const(*lit)),
        hir::Expr::Group(e) => expr_to_rvalue(cx, e, b, info),
        hir::Expr::Resolved(Resolution::Builtin(_)) => todo!(),
        hir::Expr::Resolved(Resolution::Fn(_id)) => todo!(),
        hir::Expr::Resolved(Resolution::Local(id)) => {
            Rvalue::Use(Operand::Copy(*info.id_to_local.get(id).unwrap()))
        }
        hir::Expr::BinOp(kind, e, e2, ety) => {
            let left = expr_to_rvalue(cx, e, b, info);
            let right = expr_to_rvalue(cx, e2, b, info);

            let left = rvalue_to_operand(left, *ety, b);

            let right = rvalue_to_operand(right, *ety, b);

            Rvalue::BinaryOp(*kind, left, right)
        }
        hir::Expr::UnOp(kind, e, ety) => {
            let e = expr_to_rvalue(cx, e, b, info);
            let e = rvalue_to_operand(e, *ety, b);
            Rvalue::UnaryOp(*kind, e)
        }
        hir::Expr::Return(e, ty) => {
            let rv = expr_to_rvalue(cx, e, b, info);
            let local = b.locals.push(LocalData { ty: *ty });
            b.expect_last_mut()
                .statements
                .push(Statement::Assign(local, rv));
            b.expect_last_mut().terminator = Terminator::Return(local);
            b.blocks.push(new_bb());
            Rvalue::Use(Operand::Const(Literal::Unit))
        }
    }
}

fn collect_into(cx: &dyn Context, hir: &[hir::Stmt], b: &mut Body, info: &mut HirInfo) {
    for stmt in hir {
        match stmt {
            hir::Stmt::Local(hir::LocalDecl {
                id,
                ty,
                initializer,
            }) => {
                let local = b.locals.push(LocalData { ty: *ty });
                if let Some(init) = initializer {
                    let rv = expr_to_rvalue(cx, init, b, info);
                    b.expect_last_mut()
                        .statements
                        .push(Statement::Assign(local, rv));
                }
                info.id_to_local.insert(*id, local);
            }
            hir::Stmt::Expr(e) => {
                let _ = expr_to_rvalue(cx, e, b, info);
            }
            hir::Stmt::Item(_) => {}
        }
    }
}

pub fn provide(p: &mut Providers) {
    *p = Providers { mir, ..*p }
}
