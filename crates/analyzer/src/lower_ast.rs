use syntax::{
    SyntaxError, SyntaxNode, ast,
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

pub fn lower_ast(source_file: ast::SourceFile) -> hir::Module {
    let mut ctx = Context::new();
    let stmts = stmts(&mut ctx, source_file.statements());
    hir::Module { top_level: ctx.storage.add_stmts(stmts), storage: ctx.storage }
}
