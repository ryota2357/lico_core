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

fn global_symbols(ctx: &mut Context) -> hir::GlobalSymbolIds {
    hir::GlobalSymbolIds {
        print: ctx.bindings.add("print".into()),
        println: ctx.bindings.add("println".into()),
        require: ctx.bindings.add("require".into()),
    }
}

fn entry_func(ctx: &mut Context, source_file: ast::SourceFile) -> hir::FuncId {
    let stmts = stmts(ctx, source_file.statements());
    let stmts_id = ctx.storage.add_stmts(stmts);
    let syntax = source_file.syntax().clone();
    ctx.storage.add_func(Box::new([]), (stmts_id, None), syntax)
}

pub fn lower_ast(source_file: ast::SourceFile) -> (hir::Module, Vec<SyntaxError>) {
    let mut ctx = Context::new();
    let global = global_symbols(&mut ctx);
    let entry = entry_func(&mut ctx, source_file);
    let module = hir::Module::new(entry, global, ctx.storage);
    let error = ctx.errors;
    (module, error)
}

pub fn lower_ast_with_full_map<S, E, F>(
    source_file: ast::SourceFile,
) -> (hir::Module, Vec<SyntaxError>, impl hir::HirMap<S, E, F>)
where
    S: hir::SymbolInfo,
    E: hir::ExprInfo,
    F: hir::FuncInfo,
{
    let mut ctx = Context::new();
    let global = global_symbols(&mut ctx);
    let entry = entry_func(&mut ctx, source_file);
    let map = FullHirMap {
        symbols: vec![S::default(); ctx.bindings.symbol_count()].into_boxed_slice(),
        exprs: vec![E::default(); ctx.storage.expr_count()].into_boxed_slice(),
        funcs: vec![F::default(); ctx.storage.func_count()].into_boxed_slice(),
    };
    let module = hir::Module::new(entry, global, ctx.storage);
    let error = ctx.errors;
    (module, error, map)
}

// pub fn lower_ast_with_partial_map<S, E, F>(
//     source_file: ast::SourceFile,
// ) -> (hir::Module, Vec<SyntaxError>, impl hir::HirMap<S, E, F>)
// where
//     S: hir::SymbolInfo,
//     E: hir::ExprInfo,
//     F: hir::FuncInfo,
// {
//     todo!()
// }

pub struct FullHirMap<S, E, F> {
    symbols: Box<[S]>,
    exprs: Box<[E]>,
    funcs: Box<[F]>,
}

impl<S, E, F> hir::HirMap<S, E, F> for FullHirMap<S, E, F>
where
    S: hir::SymbolInfo,
    E: hir::ExprInfo,
    F: hir::FuncInfo,
{
    fn symbol(&self, id: hir::SymbolId) -> &S {
        &self.symbols[id.raw() as usize]
    }
    fn symbol_mut(&mut self, id: hir::SymbolId) -> &mut S {
        &mut self.symbols[id.raw() as usize]
    }

    fn expr(&self, id: hir::ExprId) -> &E {
        &self.exprs[id.raw() as usize]
    }
    fn expr_mut(&mut self, id: hir::ExprId) -> &mut E {
        &mut self.exprs[id.raw() as usize]
    }

    fn func(&self, id: hir::FuncId) -> &F {
        &self.funcs[id.raw() as usize]
    }
    fn func_mut(&mut self, id: hir::FuncId) -> &mut F {
        &mut self.funcs[id.raw() as usize]
    }
}
