use super::*;

pub(super) fn expr(ctx: &mut Context, expr: &ast::Expr) -> hir::ExprKind {
    match expr {
        ast::Expr::Array(node) => array(ctx, node),
        ast::Expr::Binary(node) => binary(ctx, node),
        ast::Expr::Call(node) => call(ctx, node),
        ast::Expr::Do(node) => do_(ctx, node),
        ast::Expr::Field(node) => field(ctx, node),
        ast::Expr::Func(node) => func(ctx, node),
        ast::Expr::If(node) => if_(ctx, node),
        ast::Expr::Index(node) => index(ctx, node),
        ast::Expr::Literal(node) => literal(ctx, node),
        ast::Expr::Local(node) => local(ctx, node),
        ast::Expr::MethodCall(node) => method_call(ctx, node),
        ast::Expr::Paren(node) => paren(ctx, node),
        ast::Expr::Prefix(node) => prefix(ctx, node),
        ast::Expr::Table(node) => table(ctx, node),
    }
}

pub(super) fn exprs(
    ctx: &mut Context,
    children: AstChildren<ast::Expr>,
) -> Vec<(hir::ExprKind, SyntaxNode)> {
    children.map(|e| (expr(ctx, &e), e.syntax().clone())).collect()
}

fn array(ctx: &mut Context, node: &ast::ArrayExpr) -> hir::ExprKind {
    hir::ExprKind::Array {
        elements: {
            let elements = exprs(ctx, node.elements());
            ctx.storage.add_exprs(elements)
        },
    }
}

fn binary(ctx: &mut Context, node: &ast::BinaryExpr) -> hir::ExprKind {
    hir::ExprKind::Binary {
        lhs: node.lhs().map(|lhs| {
            let kind = expr(ctx, &lhs);
            ctx.storage.add_expr(kind, lhs.syntax().clone())
        }),
        rhs: node.rhs().map(|rhs| {
            let kind = expr(ctx, &rhs);
            ctx.storage.add_expr(kind, rhs.syntax().clone())
        }),
        op: if let Some((st, op)) = node.op() {
            match op {
                ast::BinaryOp::Assign => hir::BinaryOp::Assign(st),
                ast::BinaryOp::And => hir::BinaryOp::And(st),
                ast::BinaryOp::Or => hir::BinaryOp::Or(st),
                ast::BinaryOp::Eq => hir::BinaryOp::Eq(st),
                ast::BinaryOp::Ne => hir::BinaryOp::Ne(st),
                ast::BinaryOp::Le => hir::BinaryOp::Le(st),
                ast::BinaryOp::Ge => hir::BinaryOp::Ge(st),
                ast::BinaryOp::Lt => hir::BinaryOp::Lt(st),
                ast::BinaryOp::Gt => hir::BinaryOp::Gt(st),
                ast::BinaryOp::Add => hir::BinaryOp::Add(st),
                ast::BinaryOp::Mul => hir::BinaryOp::Mul(st),
                ast::BinaryOp::Sub => hir::BinaryOp::Sub(st),
                ast::BinaryOp::Div => hir::BinaryOp::Div(st),
                ast::BinaryOp::Mod => hir::BinaryOp::Mod(st),
                ast::BinaryOp::Shl => hir::BinaryOp::Shl(st),
                ast::BinaryOp::Shr => hir::BinaryOp::Shr(st),
                ast::BinaryOp::Concat => hir::BinaryOp::Concat(st),
                ast::BinaryOp::BitXor => hir::BinaryOp::BitXor(st),
                ast::BinaryOp::BitOr => hir::BinaryOp::BitOr(st),
                ast::BinaryOp::BitAnd => hir::BinaryOp::BitAnd(st),
            }
        } else {
            hir::BinaryOp::Missing
        },
    }
}

fn call(ctx: &mut Context, node: &ast::CallExpr) -> hir::ExprKind {
    hir::ExprKind::Call {
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

fn do_(ctx: &mut Context, node: &ast::DoExpr) -> hir::ExprKind {
    let scope = ctx.bindings.start_scope();
    let (init, last) = init_stmts_last_expr(ctx, node.statements());
    scope.finish(&mut ctx.bindings);
    if init.is_empty() && last.is_none() {
        // `do end` is equivalent to `nil`
        return hir::ExprKind::Nil;
    }
    hir::ExprKind::Block {
        init: ctx.storage.add_stmts(init),
        last: last.map(|(kind, syntax)| ctx.storage.add_expr(kind, syntax)),
    }
}

fn field(ctx: &mut Context, node: &ast::FieldExpr) -> hir::ExprKind {
    hir::ExprKind::Field {
        expr: node.expr().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
        field: node.name().and_then(|name| {
            // Desugar `expr.name` to `expr["name"]`.
            let string = hir::ExprKind::String { value: name.ident()?.text().into() };
            Some(ctx.storage.add_expr(string, name.syntax().clone()))
        }),
    }
}

fn func(ctx: &mut Context, node: &ast::FuncExpr) -> hir::ExprKind {
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
    hir::ExprKind::Function(func)
}

fn if_(ctx: &mut Context, node: &ast::IfExpr) -> hir::ExprKind {
    hir::ExprKind::Branch {
        clauses: {
            let mut clauses = Vec::new();
            clauses.push({
                let condition = node.condition().map(|c| {
                    let kind = expr(ctx, &c);
                    ctx.storage.add_expr(kind, c.syntax().clone())
                });
                let scope = ctx.bindings.start_scope();
                let (init, last) = init_stmts_last_expr(ctx, node.statements());
                scope.finish(&mut ctx.bindings);
                (
                    condition,
                    ctx.storage.add_stmts(init),
                    last.map(|(kind, syntax)| ctx.storage.add_expr(kind, syntax)),
                )
            });
            for elif in node.elif_branches() {
                let condition = elif.condition().map(|c| {
                    let kind = expr(ctx, &c);
                    ctx.storage.add_expr(kind, c.syntax().clone())
                });
                let scope = ctx.bindings.start_scope();
                let (init, last) = init_stmts_last_expr(ctx, elif.statements());
                scope.finish(&mut ctx.bindings);
                clauses.push((
                    condition,
                    ctx.storage.add_stmts(init),
                    last.map(|(kind, syntax)| ctx.storage.add_expr(kind, syntax)),
                ));
            }
            if let Some(else_) = node.else_branch() {
                let scope = ctx.bindings.start_scope();
                let (init, last) = init_stmts_last_expr(ctx, else_.statements());
                scope.finish(&mut ctx.bindings);
                clauses.push((
                    None,
                    ctx.storage.add_stmts(init),
                    last.map(|(kind, syntax)| ctx.storage.add_expr(kind, syntax)),
                ));
            }
            clauses.into_boxed_slice()
        },
    }
}

fn index(ctx: &mut Context, node: &ast::IndexExpr) -> hir::ExprKind {
    hir::ExprKind::Field {
        expr: node.expr().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
        field: node.index().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
    }
}

fn literal(ctx: &mut Context, node: &ast::Literal) -> hir::ExprKind {
    let Some(token) = node.token() else { unexpected_ast!("Literal expression without a token") };
    let Some(kind) = node.kind() else { unexpected_ast!("Unkown literal kind") };
    let text = token.text();
    match kind {
        ast::LiteralKind::Int => match text.parse() {
            Ok(value) => hir::ExprKind::Int { value },
            Err(err) => {
                ctx.errors.push(SyntaxError::new(
                    format!("Invalid integer literal: {}", err),
                    token.text_range(),
                ));
                hir::ExprKind::InvalidInt
            }
        },
        ast::LiteralKind::Float => match text.parse() {
            Ok(value) => hir::ExprKind::Float { value },
            Err(err) => {
                ctx.errors.push(SyntaxError::new(
                    format!("Invalid float literal: {}", err),
                    token.text_range(),
                ));
                hir::ExprKind::InvalidFloat
            }
        },
        ast::LiteralKind::String => hir::ExprKind::String { value: text.into() },
        ast::LiteralKind::Bool(value) => hir::ExprKind::Bool { value },
        ast::LiteralKind::Nil => hir::ExprKind::Nil,
    }
}

fn local(ctx: &mut Context, node: &ast::Local) -> hir::ExprKind {
    let Some(ident) = node.name().and_then(|name| name.ident()) else {
        unexpected_ast!("Local expression without an identifier")
    };
    match ctx.bindings.resolve(ident.text()) {
        Some(id) => hir::ExprKind::Local { name: hir::Symbol::new(id, ident) },
        None => {
            ctx.errors.push(SyntaxError::new(
                format!("Cannot find variable `{}` in this scope", ident.text()),
                ident.text_range(),
            ));
            hir::ExprKind::UnkownLocal
        }
    }
}

fn method_call(ctx: &mut Context, node: &ast::MethodCallExpr) -> hir::ExprKind {
    hir::ExprKind::MethodCall {
        receiver: node.expr().map(|e| {
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, e.syntax().clone())
        }),
        name: match node.ident() {
            Some(name) => hir::Symbol::new(ctx.bindings.add(name.text().into()), name),
            None => unexpected_ast!("Method name without an identifier"),
        },
        args: match node.arg_list().map(|arg_list| exprs(ctx, arg_list.args())) {
            Some(args) => ctx.storage.add_exprs(args),
            None => ctx.storage.add_exprs([]),
        },
    }
}

fn paren(ctx: &mut Context, node: &ast::ParenExpr) -> hir::ExprKind {
    let Some(inner) = node.expr() else {
        // fallback `()` to `nil` (error is reported by the parser)
        return hir::ExprKind::Nil;
    };
    expr(ctx, &inner)
}

fn prefix(ctx: &mut Context, node: &ast::PrefixExpr) -> hir::ExprKind {
    hir::ExprKind::Prefix {
        expr: node.expr().map(|e| {
            let node = e.syntax().clone();
            let kind = expr(ctx, &e);
            ctx.storage.add_expr(kind, node)
        }),
        op: if let Some((token, op)) = node.op() {
            match op {
                ast::PrefixOp::Plus => hir::PrefixOp::Plus(token),
                ast::PrefixOp::Minus => hir::PrefixOp::Minus(token),
                ast::PrefixOp::BitNot => hir::PrefixOp::BitNot(token),
                ast::PrefixOp::Not => hir::PrefixOp::Not(token),
                ast::PrefixOp::TypeOf => hir::PrefixOp::TypeOf(token),
            }
        } else {
            hir::PrefixOp::Missing
        },
    }
}

fn table(ctx: &mut Context, node: &ast::TableExpr) -> hir::ExprKind {
    hir::ExprKind::Table {
        fields: {
            let fields = node.fields().map(|field| {
                let key = field.name().map(|name| match name {
                    ast::TableFieldName::Name(name) => {
                        let Some(ident) = name.ident() else {
                            unexpected_ast!("Table field name without an identifier")
                        };
                        let kind = hir::ExprKind::String { value: ident.text().into() };
                        ctx.storage.add_expr(kind, name.syntax().clone())
                    }
                    ast::TableFieldName::TableFieldNameExpr(e) => {
                        let Some(e) = e.expr() else {
                            unexpected_ast!("Table field name expression without an expression")
                        };
                        let kind = expr(ctx, &e);
                        ctx.storage.add_expr(kind, e.syntax().clone())
                    }
                });
                let value = field.expr().map(|value| {
                    let kind = expr(ctx, &value);
                    ctx.storage.add_expr(kind, value.syntax().clone())
                });
                (key, value)
            });
            fields.collect()
        },
    }
}
