use crate::{SyntaxNode, SyntaxToken};
use core::{
    hash::{Hash, Hasher},
    num::NonZero,
};
use indexed_arena::{Arena, Idx, IdxRange};
use lean_string::LeanString;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    entry: FuncId,
    global: GlobalSymbolIds,
    storage: Storage,
}

impl Module {
    pub fn new(entry: FuncId, global: GlobalSymbolIds, storage: Storage) -> Self {
        Self { entry, global, storage }
    }

    pub fn entry(&self) -> FuncId {
        self.entry
    }

    pub fn storage(&self) -> &Storage {
        &self.storage
    }

    pub fn global(&self) -> &GlobalSymbolIds {
        &self.global
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct GlobalSymbolIds {
    pub print: SymbolId,
    pub println: SymbolId,
    pub require: SymbolId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    id: SymbolId,
    token: SyntaxToken,
}

impl Symbol {
    pub const fn new(id: SymbolId, token: SyntaxToken) -> Self {
        Symbol { id, token }
    }
    pub const fn token(&self) -> &SyntaxToken {
        &self.token
    }
    pub const fn id(&self) -> SymbolId {
        self.id
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SymbolId(u32);

impl SymbolId {
    pub const fn new(raw: u32) -> Self {
        SymbolId(raw)
    }
    pub const fn raw(&self) -> u32 {
        self.0
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.text().hash(state);
        self.id.hash(state);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Expr {
    kind: ExprKind,
    node: SyntaxNode,
}

impl Expr {
    pub const fn kind(&self) -> &ExprKind {
        &self.kind
    }
    pub const fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExprId(Idx<Expr, NonZero<u32>>);

impl ExprId {
    pub fn raw(&self) -> u32 {
        self.0.into_raw().get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ExprsId(IdxRange<Expr, NonZero<u32>>);

type OptExprId = Option<ExprId>;

#[derive(Clone, Debug, PartialEq)]
pub enum ExprKind {
    Local { name: Symbol },
    Int { value: i64 },
    Float { value: f64 },
    String { value: LeanString },
    Bool { value: bool },
    Nil,
    Function(FuncId),
    Array { elements: ExprsId },
    Table { fields: Box<[(OptExprId, OptExprId)]> },
    Branch { clauses: Box<[(OptExprId, StmtsId, OptExprId)]> },
    Prefix { op: PrefixOp, expr: OptExprId },
    Binary { op: BinaryOp, lhs: OptExprId, rhs: OptExprId },
    Block { init: StmtsId, last: OptExprId },
    Call { expr: OptExprId, args: ExprsId },
    MethodCall { receiver: OptExprId, name: Symbol, args: ExprsId },
    Field { expr: OptExprId, field: OptExprId },
    UnkownLocal,
    InvalidInt,
    InvalidFloat,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    BitNot(SyntaxToken),
    Not(SyntaxToken),
    TypeOf(SyntaxToken),
    Missing,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    And(SyntaxToken),
    Or(SyntaxToken),
    Eq(SyntaxToken),
    Ne(SyntaxToken),
    Le(SyntaxToken),
    Ge(SyntaxToken),
    Lt(SyntaxToken),
    Gt(SyntaxToken),
    Add(SyntaxToken),
    Mul(SyntaxToken),
    Sub(SyntaxToken),
    Div(SyntaxToken),
    Mod(SyntaxToken),
    Shl(SyntaxToken),
    Shr(SyntaxToken),
    Concat(SyntaxToken),
    BitXor(SyntaxToken),
    BitOr(SyntaxToken),
    BitAnd(SyntaxToken),
    Missing,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Stmt {
    kind: StmtKind,
    node: SyntaxNode,
}

impl Stmt {
    pub const fn kind(&self) -> &StmtKind {
        &self.kind
    }
    pub const fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StmtId(Idx<Stmt, u32>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StmtsId(IdxRange<Stmt, u32>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StmtKind {
    MakeLocal { name: Symbol, expr: OptExprId },
    MakeFunc { name: Symbol, func: FuncId },
    SetLocal { name: Symbol, expr: OptExprId },
    SetField { target: OptExprId, field: OptExprId, expr: OptExprId },
    Branch { clauses: Box<[(OptExprId, StmtsId)]> },
    ForLoop { variable: Option<Symbol>, iterable: OptExprId, body: StmtsId },
    WhileLoop { condition: OptExprId, body: StmtsId },
    Block { stmts: StmtsId },
    Call { expr: OptExprId, args: ExprsId },
    MethodCall { receiver: OptExprId, name: Symbol, args: ExprsId },
    Return { expr: OptExprId },
    BreakLoop,
    ContinueLoop,
    DiscardResult { expr: OptExprId, explicit_discard: Option<SyntaxNode> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Func {
    params: Box<[Symbol]>,
    body: (StmtsId, OptExprId),
    node: SyntaxNode,
}

impl Func {
    pub const fn params(&self) -> &[Symbol] {
        &self.params
    }
    pub const fn body(&self) -> &(StmtsId, OptExprId) {
        &self.body
    }
    pub const fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FuncId(Idx<Func, u32>);

impl FuncId {
    pub fn raw(&self) -> u32 {
        self.0.into_raw()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Storage {
    func_arena: Arena<Func, u32>,
    expr_arena: Arena<Expr, NonZero<u32>>,
    stmt_arena: Arena<Stmt, u32>,
}

impl Storage {
    pub const fn new() -> Self {
        Self { func_arena: Arena::new(), expr_arena: Arena::new(), stmt_arena: Arena::new() }
    }

    pub fn add_func(
        &mut self,
        params: Box<[Symbol]>,
        body: (StmtsId, OptExprId),
        node: SyntaxNode,
    ) -> FuncId {
        FuncId(self.func_arena.alloc(Func { params, body, node }))
    }

    pub fn add_expr(&mut self, kind: ExprKind, node: SyntaxNode) -> ExprId {
        ExprId(self.expr_arena.alloc(Expr { kind, node }))
    }

    pub fn add_exprs<I>(&mut self, exprs: I) -> ExprsId
    where
        I: IntoIterator<Item = (ExprKind, SyntaxNode)>,
    {
        let exprs = exprs.into_iter().map(|(kind, node)| Expr { kind, node });
        ExprsId(self.expr_arena.alloc_many(exprs))
    }

    pub fn add_stmts<I>(&mut self, stmts: I) -> StmtsId
    where
        I: IntoIterator<Item = (StmtKind, SyntaxNode)>,
    {
        let stmts = stmts.into_iter().map(|(kind, node)| Stmt { kind, node });
        StmtsId(self.stmt_arena.alloc_many(stmts))
    }

    pub fn get_func(&self, id: FuncId) -> &Func {
        &self.func_arena[id.0]
    }

    pub fn get_expr(&self, id: ExprId) -> &Expr {
        &self.expr_arena[id.0]
    }

    pub fn get_exprs(&self, id: ExprsId) -> &[Expr] {
        &self.expr_arena[id.0]
    }

    pub fn get_stmt(&self, id: StmtId) -> &Stmt {
        &self.stmt_arena[id.0]
    }

    pub fn get_stmts(&self, id: StmtsId) -> &[Stmt] {
        &self.stmt_arena[id.0]
    }

    pub fn func_count(&self) -> usize {
        self.func_arena.len()
    }

    pub fn expr_count(&self) -> usize {
        self.expr_arena.len()
    }

    pub fn stmt_count(&self) -> usize {
        self.stmt_arena.len()
    }

    pub fn iter_func(&self) -> impl Iterator<Item = &Func> {
        self.func_arena.values()
    }

    pub fn iter_func_with_id(&self) -> impl Iterator<Item = (&Func, FuncId)> {
        self.func_arena.iter().map(|(id, func)| (func, FuncId(id)))
    }

    pub fn iter_expr(&self) -> impl Iterator<Item = &Expr> {
        self.expr_arena.values()
    }

    pub fn iter_expr_with_id(&self) -> impl Iterator<Item = (&Expr, ExprId)> {
        self.expr_arena.iter().map(|(id, expr)| (expr, ExprId(id)))
    }

    pub fn iter_stmt(&self) -> impl Iterator<Item = &Stmt> {
        self.stmt_arena.values()
    }

    pub fn iter_stmt_with_id(&self) -> impl Iterator<Item = (&Stmt, StmtId)> {
        self.stmt_arena.iter().map(|(id, stmt)| (stmt, StmtId(id)))
    }
}

impl Default for Storage {
    fn default() -> Self {
        Self::new()
    }
}

pub trait HirMap<S, E, F>
where
    S: SymbolInfo,
    E: ExprInfo,
    F: FuncInfo,
{
    fn symbol(&self, id: SymbolId) -> &S;
    fn symbol_mut(&mut self, id: SymbolId) -> &mut S;

    fn expr(&self, id: ExprId) -> &E;
    fn expr_mut(&mut self, id: ExprId) -> &mut E;

    fn func(&self, id: FuncId) -> &F;
    fn func_mut(&mut self, id: FuncId) -> &mut F;
}

pub trait SymbolInfo: Clone + Default {}

pub trait ExprInfo: Clone + Default {}

pub trait FuncInfo: Clone + Default {}
