use super::*;

macro_rules! __unexpected_ast {
    ($msg:literal) => {
        panic!("Unexpected AST structure encountered. This may indicate an unhandled case or a bug in the parser. ({})", $msg)
    };
}
pub(super) use __unexpected_ast as unexpected_ast;

#[allow(clippy::type_complexity)]
pub(super) fn init_stmts_last_expr(
    ctx: &mut Context,
    mut children: AstChildren<ast::Stmt>,
) -> (Vec<(hir::StmtKind, SyntaxNode)>, Option<(hir::ExprKind, SyntaxNode)>) {
    let Some(mut cur_stmt) = children.next() else {
        return (vec![], None);
    };
    let mut init = Vec::new();
    let last = loop {
        match children.next() {
            Some(next_stmt) => {
                let kind = stmt(ctx, &cur_stmt);
                init.push((kind, cur_stmt.syntax().clone()));
                cur_stmt = next_stmt;
            }
            None => match cur_stmt {
                ast::Stmt::Expr(last) => {
                    let kind = expr(ctx, &last);
                    break Some((kind, last.syntax().clone()));
                }
                _ => {
                    let kind = stmt(ctx, &cur_stmt);
                    init.push((kind, cur_stmt.syntax().clone()));
                    break None;
                }
            },
        }
    };
    (init, last)
}

pub(super) fn param_symbols(ctx: &mut Context, param_list: ast::ParamList) -> Box<[hir::Symbol]> {
    param_list
        .params()
        .map(|param| {
            let ident = param
                .name()
                .and_then(|name| name.ident())
                .unwrap_or_else(|| unexpected_ast!("Function parameter has no ident"));
            hir::Symbol::new(ctx.bindings.add(ident.text().into()), ident)
        })
        .collect()
}
