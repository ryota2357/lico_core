use crate::{SyntaxNode, SyntaxToken};
use core::{
    hash::{Hash, Hasher},
    num::NonZero,
};
use indexed_arena::{Arena, Idx, IdxRange};
use lean_string::LeanString;

pub struct Module {
    pub storage: Storage,
    pub top_level: StmtIdRange,
}

#[derive(Clone)]
pub struct Symbol {
    token: SyntaxToken,
    generation: u32,
}

impl Symbol {
    pub fn new(token: SyntaxToken, generation: u32) -> Self {
        Self { token, generation }
    }
    pub fn token(&self) -> &SyntaxToken {
        &self.token
    }
    pub fn generation(&self) -> u32 {
        self.generation
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.token.text() == other.token.text() && self.generation == other.generation
    }
}
impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.token.text().hash(state);
        self.generation.hash(state);
    }
}

#[derive(Clone, PartialEq)]
pub struct Expr {
    kind: ExprKind,
    node: SyntaxNode,
}

impl Expr {
    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }
    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, PartialEq)]
pub enum ExprKind {
    Local { name: Symbol },
    Int(i64),
    Float(f64),
    String(LeanString),
    Bool(bool),
    Nil,
    Function(FuncId),
    Array { elements: ExprIdRange },
    Table { fields: Box<[(ExprId, ExprId)]> },
    Branch { condition: ExprId, then: (StmtId, ExprId), else_: (StmtId, ExprId) },
    Prefix { op: PrefixOp, value: ExprId },
    Binary { op: BinaryOp, lhs: ExprId, rhs: ExprId },
    Block { effects: StmtIdRange, tail: ExprId },
    Call { value: ExprId, args: ExprIdRange },
    MethodCall { expr: ExprId, name: Symbol, args: ExprIdRange },
    Field { value: ExprId, field: ExprId },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Plus(SyntaxToken),
    Minus(SyntaxToken),
    Not(SyntaxToken),
    BitNot(SyntaxToken),
    Missing,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add(SyntaxToken),
    Sub(SyntaxToken),
    Mul(SyntaxToken),
    Div(SyntaxToken),
    Mod(SyntaxToken),
    Shl(SyntaxToken),
    Shr(SyntaxToken),
    Concat(SyntaxToken),
    Eq(SyntaxToken),
    Ne(SyntaxToken),
    Lt(SyntaxToken),
    Le(SyntaxToken),
    Gt(SyntaxToken),
    Ge(SyntaxToken),
    And(SyntaxToken),
    Or(SyntaxToken),
    BitAnd(SyntaxToken),
    BitOr(SyntaxToken),
    BitXor(SyntaxToken),
    Assign(SyntaxToken),
    Missing,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Stmt {
    kind: StmtKind,
    node: SyntaxNode,
}

impl Stmt {
    pub fn kind(&self) -> &StmtKind {
        &self.kind
    }
    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum StmtKind {
    MakeLocal { name: Symbol, expr: ExprId },
    MakeFunc { name: Symbol, func: FuncId },
    SetLocal { local: Symbol, expr: ExprId },
    SetField { target: ExprId, field: ExprId, value: ExprId },
    Branch { condition: ExprId, then: StmtId, else_: StmtId },
    ForLoop { variable: Symbol, iterable: ExprId, body: StmtId },
    WhileLoop { condition: ExprId, body: StmtId },
    Block { stmts: StmtIdRange },
    Call { value: ExprId, args: ExprIdRange },
    MethodCall { table: ExprId, name: Symbol, args: ExprIdRange },
    Return { value: ExprId },
    BreakLoop,
    ContinueLoop,
    NoEffectExpr { value: ExprId },
}

#[derive(Clone, PartialEq, Eq)]
pub struct Func {
    args: Box<[Symbol]>,
    body: Box<[StmtId]>,
    node: SyntaxNode,
}

impl Func {
    pub fn args(&self) -> &[Symbol] {
        &self.args
    }
    pub fn body(&self) -> &[StmtId] {
        &self.body
    }
    pub fn node(&self) -> &SyntaxNode {
        &self.node
    }
}

pub type ExprId = Option<Idx<Expr, NonZero<u32>>>;
pub type ExprIdRange = IdxRange<Expr, NonZero<u32>>;

pub type StmtId = Idx<Stmt, u32>;
pub type StmtIdRange = IdxRange<Stmt, u32>;

pub type FuncId = Idx<Func, u32>;

#[derive(Clone, PartialEq)]
pub struct Storage {
    func_arena: Arena<Func, u32>,
    expr_arena: Arena<Expr, NonZero<u32>>,
    stmt_arena: Arena<Stmt, u32>,
}

impl Storage {
    pub fn new() -> Self {
        Self { func_arena: Arena::new(), expr_arena: Arena::new(), stmt_arena: Arena::new() }
    }

    pub fn add_func(
        &mut self,
        args: Box<[Symbol]>,
        body: Box<[StmtId]>,
        node: SyntaxNode,
    ) -> FuncId {
        self.func_arena.alloc(Func { args, body, node })
    }

    pub fn add_expr(&mut self, kind: ExprKind, node: SyntaxNode) -> ExprId {
        Some(self.expr_arena.alloc(Expr { kind, node }))
    }

    pub fn add_stmt(&mut self, kind: StmtKind, node: SyntaxNode) -> StmtId {
        self.stmt_arena.alloc(Stmt { kind, node })
    }

    pub fn get_func(&self, id: FuncId) -> &Func {
        &self.func_arena[id]
    }

    pub fn get_expr(&self, id: ExprId) -> Option<&Expr> {
        id.map(|id| &self.expr_arena[id])
    }

    pub fn get_stmt(&self, id: StmtId) -> &Stmt {
        &self.stmt_arena[id]
    }
}

impl Default for Storage {
    fn default() -> Self {
        Self::new()
    }
}
