use syntax::{
    SyntaxError, SyntaxNode, TextRange, ast,
    ast::{AstChildren, AstNode},
    hir,
};

mod context;
use context::*;

mod expr;
use expr::{expr, exprs};

mod stmt;
use stmt::{stmt, stmts};

mod utils;
use utils::*;

pub fn lower_ast(source_file: ast::SourceFile) -> (hir::Module, Vec<SyntaxError>) {
    let mut ctx = Context::new();
    let global = hir::GlobalSymbolIds {
        print: ctx.bindings.add("print".into()),
        println: ctx.bindings.add("println".into()),
        require: ctx.bindings.add("require".into()),
    };
    let entry = {
        let stmts = stmts(&mut ctx, source_file.statements());
        let stmts_id = ctx.storage.add_stmts(stmts);
        let syntax = source_file.syntax().clone();
        ctx.storage.add_func(Box::new([]), (stmts_id, None), syntax)
    };
    let module = hir::Module::new(entry, global, ctx.storage);
    let error = ctx.errors;
    (module, error)
}
