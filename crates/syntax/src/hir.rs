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
pub struct ExprIdRange(IdxRange<Expr, NonZero<u32>>);

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
    Array { elements: ExprIdRange },
    Table { fields: Box<[(OptExprId, OptExprId)]> },
    Branch { clauses: Box<[(OptExprId, StmtsId, OptExprId)]> },
    Prefix { op: PrefixOp, expr: OptExprId },
    Binary { op: BinaryOp, lhs: OptExprId, rhs: OptExprId },
    Block { init: StmtsId, last: OptExprId },
    Call { expr: OptExprId, args: ExprIdRange },
    MethodCall { receiver: OptExprId, name: Symbol, args: ExprIdRange },
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
    Call { expr: OptExprId, args: ExprIdRange },
    MethodCall { receiver: OptExprId, name: Symbol, args: ExprIdRange },
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

    pub fn add_exprs<I>(&mut self, exprs: I) -> ExprIdRange
    where
        I: IntoIterator<Item = (ExprKind, SyntaxNode)>,
    {
        let exprs = exprs.into_iter().map(|(kind, node)| Expr { kind, node });
        ExprIdRange(self.expr_arena.alloc_many(exprs))
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

    pub fn get_exprs(&self, id: ExprIdRange) -> &[Expr] {
        &self.expr_arena[id.0]
    }

    pub fn get_stmts(&self, id: StmtsId) -> &[Stmt] {
        &self.stmt_arena[id.0]
    }
}

impl Default for Storage {
    fn default() -> Self {
        Self::new()
    }
}
