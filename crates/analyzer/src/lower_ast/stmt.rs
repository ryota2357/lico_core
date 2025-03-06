use super::*;

pub(super) fn stmt(ctx: &mut Context, node: &ast::Stmt) -> hir::StmtKind {
    match node {
        ast::Stmt::Attr(node) => attr(ctx, node),
        ast::Stmt::Break(node) => break_(ctx, node),
        ast::Stmt::Continue(node) => continue_(ctx, node),
        ast::Stmt::For(node) => for_(ctx, node),
        ast::Stmt::Func(node) => func(ctx, node),
        ast::Stmt::Return(node) => return_(ctx, node),
        ast::Stmt::Var(node) => var(ctx, node),
        ast::Stmt::While(node) => while_(ctx, node),
        ast::Stmt::Expr(expr_node) => fallback(ctx, expr_node),
    }
}

pub(super) fn stmts(
    ctx: &mut Context,
    children: AstChildren<ast::Stmt>,
) -> Vec<(hir::StmtKind, SyntaxNode)> {
    children.map(|e| (stmt(ctx, &e), e.syntax().clone())).collect()
}

fn attr(ctx: &mut Context, node: &ast::AttrStmt) -> hir::StmtKind {
    ctx.errors
        .push(SyntaxError::new("Attributes are not yet supported", node.syntax().text_range()));
    hir::StmtKind::DiscardResult { expr: None, explicit_discard: None } // dummy
}

fn break_(ctx: &mut Context, node: &ast::BreakStmt) -> hir::StmtKind {
    if !ctx.loops.is_in_loop() {
        ctx.errors.push(SyntaxError::new("'break' outside of loop", node.syntax().text_range()));
    }
    hir::StmtKind::BreakLoop
}

fn continue_(ctx: &mut Context, node: &ast::ContinueStmt) -> hir::StmtKind {
    if !ctx.loops.is_in_loop() {
        ctx.errors.push(SyntaxError::new("'continue' outside of loop", node.syntax().text_range()));
    }
    hir::StmtKind::ContinueLoop
}

fn for_(ctx: &mut Context, node: &ast::ForStmt) -> hir::StmtKind {
    let iterable = node.iterable().map(|e| {
        let kind = expr(ctx, &e);
        ctx.storage.add_expr(kind, e.syntax().clone())
    });
    let binding_scope = ctx.bindings.start_scope();
    let ret_kind = hir::StmtKind::ForLoop {
        variable: match node.pat() {
            Some(ast::Pat::Name(name)) => {
                let Some(loop_var) = name.ident() else {
                    unexpected_ast!("For loop variable has no ident")
                };
                Some(hir::Symbol::new(ctx.bindings.add(loop_var.text().into()), loop_var))
            }
            Some(ast::Pat::Wildcard(_)) | None => None,
        },
        iterable,
        body: match node.loop_body() {
            Some(body) => {
                let loop_scope = ctx.loops.start();
                let kinds = stmts(ctx, body.statements());
                loop_scope.finish(&mut ctx.loops);
                ctx.storage.add_stmts(kinds)
            }
            None => ctx.storage.add_stmts([]),
        },
    };
    binding_scope.finish(&mut ctx.bindings);
    ret_kind
}

fn func(ctx: &mut Context, node: &ast::FuncStmt) -> hir::StmtKind {
    let name = node.name().map(|name| {
        let ident = name.ident().unwrap_or_else(|| unexpected_ast!("Function name has no ident"));
        hir::Symbol::new(ctx.bindings.add(ident.text().into()), ident)
    });
    let scope = ctx.bindings.start_scope();
    let func = {
        let params = node
            .param_list()
            .map(|param_list| param_symbols(ctx, param_list))
            .unwrap_or_else(|| Box::new([]));
        let (init, last) = init_stmts_last_expr(ctx, node.statements());
        let body = (
            ctx.storage.add_stmts(init),
            last.map(|(kind, syntax)| ctx.storage.add_expr(kind, syntax)),
        );
        ctx.storage.add_func(params, body, node.syntax().clone())
    };
    scope.finish(&mut ctx.bindings);

    match name {
        Some(name) => hir::StmtKind::MakeFunc { name, func },
        None => hir::StmtKind::DiscardResult {
            expr: Some(ctx.storage.add_expr(hir::ExprKind::Function(func), node.syntax().clone())),
            explicit_discard: None,
        },
    }
}

fn return_(ctx: &mut Context, node: &ast::ReturnStmt) -> hir::StmtKind {
    hir::StmtKind::Return {
        expr: node.expr().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
    }
}

fn var(ctx: &mut Context, node: &ast::VarStmt) -> hir::StmtKind {
    let expr = node.expr().map(|e| {
        let kind = expr(ctx, &e);
        ctx.storage.add_expr(kind, e.syntax().clone())
    });
    match node.pat() {
        Some(ast::Pat::Name(name)) => match name.ident() {
            Some(name) => hir::StmtKind::MakeLocal {
                name: hir::Symbol::new(ctx.bindings.add(name.text().into()), name),
                expr,
            },
            None => unexpected_ast!("Variable name has no ident"),
        },
        Some(ast::Pat::Wildcard(wildcard)) => {
            let explicit_discard = Some(wildcard.syntax().clone());
            hir::StmtKind::DiscardResult { expr, explicit_discard }
        }
        None => hir::StmtKind::DiscardResult { expr, explicit_discard: None },
    }
}

fn while_(ctx: &mut Context, node: &ast::WhileStmt) -> hir::StmtKind {
    let condition = node.condition().map(|c| {
        let kind = expr(ctx, &c);
        ctx.storage.add_expr(kind, c.syntax().clone())
    });
    hir::StmtKind::WhileLoop {
        condition,
        body: match node.loop_body() {
            Some(body) => {
                let binding_scope = ctx.bindings.start_scope();
                let loop_scope = ctx.loops.start();
                let kinds = stmts(ctx, body.statements());
                loop_scope.finish(&mut ctx.loops);
                binding_scope.finish(&mut ctx.bindings);
                ctx.storage.add_stmts(kinds)
            }
            None => ctx.storage.add_stmts([]),
        },
    }
}

fn fallback(ctx: &mut Context, node: &ast::Expr) -> hir::StmtKind {
    match node {
        ast::Expr::Array(_) => {}
        ast::Expr::Binary(node) => {
            if let Some(ast::BinaryOp::Assign) = node.op_kind() {
                return assign(ctx, node);
            }
        }
        ast::Expr::Call(node) => return call(ctx, node),
        ast::Expr::Do(node) => return do_(ctx, node),
        ast::Expr::Field(_) => {}
        ast::Expr::Func(_) => {}
        ast::Expr::If(node) => return if_(ctx, node),
        ast::Expr::Index(_) => {}
        ast::Expr::Literal(_) => {}
        ast::Expr::Local(_) => {}
        ast::Expr::MethodCall(node) => return method_call(ctx, node),
        ast::Expr::Paren(node) => return paren(ctx, node),
        ast::Expr::Prefix(_) => {}
        ast::Expr::Table(_) => {}
    }
    let kind = expr(ctx, node);
    hir::StmtKind::DiscardResult {
        expr: Some(ctx.storage.add_expr(kind, node.syntax().clone())),
        explicit_discard: None,
    }
}

fn assign(ctx: &mut Context, node: &ast::BinaryExpr) -> hir::StmtKind {
    const INVALID_LHS_MSG: &str = "Invalid left-hand side of assignment";
    let rhs = node.rhs().map(|lhs| {
        let kind = expr(ctx, &lhs);
        ctx.storage.add_expr(kind, lhs.syntax().clone())
    });
    let Some(mut lhs) = node.lhs() else {
        return hir::StmtKind::DiscardResult { expr: rhs, explicit_discard: None };
    };
    while let ast::Expr::Paren(paren) = lhs {
        if let Some(inner) = paren.expr() {
            lhs = inner;
        } else {
            ctx.errors.push(SyntaxError::new(INVALID_LHS_MSG, paren.syntax().text_range()));
            return hir::StmtKind::DiscardResult { expr: rhs, explicit_discard: None };
        }
    }
    match lhs {
        ast::Expr::Array(_) => {}
        ast::Expr::Binary(_) => {}
        ast::Expr::Call(_) => {}
        ast::Expr::Do(_) => {}
        ast::Expr::Field(node) => {
            return hir::StmtKind::SetField {
                target: node.expr().map(|t| {
                    let kind = expr(ctx, &t);
                    ctx.storage.add_expr(kind, t.syntax().clone())
                }),
                field: node.name().and_then(|name| {
                    // Desugar `expr.name` to `expr["name"]`.
                    let string = hir::ExprKind::String { value: name.ident()?.text().into() };
                    Some(ctx.storage.add_expr(string, name.syntax().clone()))
                }),
                expr: rhs,
            };
        }
        ast::Expr::Func(_) => {}
        ast::Expr::If(_) => {}
        ast::Expr::Index(node) => {
            return hir::StmtKind::SetField {
                target: node.expr().map(|t| {
                    let kind = expr(ctx, &t);
                    ctx.storage.add_expr(kind, t.syntax().clone())
                }),
                field: node.index().map(|i| {
                    let kind = expr(ctx, &i);
                    ctx.storage.add_expr(kind, i.syntax().clone())
                }),
                expr: rhs,
            };
        }
        ast::Expr::Literal(_) => {}
        ast::Expr::Local(node) => {
            let name = node
                .name()
                .and_then(|name| name.ident())
                .unwrap_or_else(|| unexpected_ast!("Local assignment has no name or ident"));
            return hir::StmtKind::SetLocal {
                name: hir::Symbol::new(ctx.bindings.add(name.text().into()), name),
                expr: rhs,
            };
        }
        ast::Expr::MethodCall(_) => {}
        ast::Expr::Paren(_) => unreachable!("Paren should have been unwrapped"),
        ast::Expr::Prefix(_) => {}
        ast::Expr::Table(_) => {}
    }
    ctx.errors.push(SyntaxError::new(INVALID_LHS_MSG, lhs.syntax().text_range()));
    hir::StmtKind::DiscardResult {
        // fallback to: lhs `Missing` rhs
        expr: {
            let lhs_kind = expr(ctx, &lhs);
            let lhs_id = ctx.storage.add_expr(lhs_kind, lhs.syntax().clone());
            let kind = hir::ExprKind::Binary { op: hir::BinaryOp::Missing, lhs: Some(lhs_id), rhs };
            Some(ctx.storage.add_expr(kind, node.syntax().clone()))
        },
        explicit_discard: None,
    }
}

fn call(ctx: &mut Context, node: &ast::CallExpr) -> hir::StmtKind {
    hir::StmtKind::Call {
        expr: node.expr().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
        args: match node.arg_list().map(|arg_list| exprs(ctx, arg_list.args())) {
            Some(args) => ctx.storage.add_exprs(args),
            None => ctx.storage.add_exprs([]),
        },
    }
}

fn do_(ctx: &mut Context, node: &ast::DoExpr) -> hir::StmtKind {
    hir::StmtKind::Block {
        stmts: {
            let scope = ctx.bindings.start_scope();
            let kinds = stmts(ctx, node.statements());
            scope.finish(&mut ctx.bindings);
            ctx.storage.add_stmts(kinds)
        },
    }
}

fn if_(ctx: &mut Context, node: &ast::IfExpr) -> hir::StmtKind {
    hir::StmtKind::Branch {
        clauses: {
            let mut clauses = Vec::new();
            clauses.push({
                let condition = node.condition().map(|c| {
                    let kind = expr(ctx, &c);
                    ctx.storage.add_expr(kind, c.syntax().clone())
                });
                let then = {
                    let scope = ctx.bindings.start_scope();
                    let kinds = stmts(ctx, node.statements());
                    scope.finish(&mut ctx.bindings);
                    ctx.storage.add_stmts(kinds)
                };
                (condition, then)
            });
            for elif in node.elif_branches() {
                let condition = elif.condition().map(|c| {
                    let kind = expr(ctx, &c);
                    ctx.storage.add_expr(kind, c.syntax().clone())
                });
                let then = {
                    let scope = ctx.bindings.start_scope();
                    let kinds = stmts(ctx, elif.statements());
                    scope.finish(&mut ctx.bindings);
                    ctx.storage.add_stmts(kinds)
                };
                clauses.push((condition, then));
            }
            if let Some(else_) = node.else_branch() {
                let else_ = {
                    let scope = ctx.bindings.start_scope();
                    let kinds = stmts(ctx, else_.statements());
                    scope.finish(&mut ctx.bindings);
                    ctx.storage.add_stmts(kinds)
                };
                clauses.push((None, else_));
            }
            clauses.into_boxed_slice()
        },
    }
}

fn method_call(ctx: &mut Context, node: &ast::MethodCallExpr) -> hir::StmtKind {
    hir::StmtKind::MethodCall {
        receiver: node.expr().map(|r| {
            let kind = expr(ctx, &r);
            ctx.storage.add_expr(kind, r.syntax().clone())
        }),
        name: match node.ident() {
            Some(name) => hir::Symbol::new(ctx.bindings.add(name.text().into()), name),
            None => unexpected_ast!("Method call has no method name"),
        },
        args: match node.arg_list().map(|arg_list| exprs(ctx, arg_list.args())) {
            Some(args) => ctx.storage.add_exprs(args),
            None => ctx.storage.add_exprs([]),
        },
    }
}

fn paren(ctx: &mut Context, node: &ast::ParenExpr) -> hir::StmtKind {
    let Some(inner) = node.expr() else {
        return hir::StmtKind::DiscardResult { expr: None, explicit_discard: None };
    };
    fallback(ctx, &inner)
}
