use library::Lib;
use melon::{typedef::*, Instruction, IntegerType};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Expr {
    ActualInstr(Instruction),

    PushConstU8(Argument<SmallUInt>),
    PushConstU16(Argument<UInt>),
    PushConstI8(Argument<SmallInt>),
    PushConstI16(Argument<Int>),

    Load(IntegerType, Argument<Address>),
    Store(IntegerType, Argument<Address>),

    Sys(String),
    Call(String),
    Alloc(Argument<UInt>),

    While(While),
    If(If),
}

#[derive(Debug, Clone)]
pub enum Argument<T> {
    Literal(T),
    Constant(String),
}

#[derive(Debug, Clone)]
pub enum Condition {
    Positive,
    Negative,
    Zero,
    NotZero,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Condition,
    pub type_t: IntegerType,
    pub exprs: Vec<Expr>,
    pub else_exprs: Option<Vec<Expr>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Condition,
    pub type_t: IntegerType,
    pub exprs: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub instr: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub name: String,
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub origin_name: String,
    pub alias: String,
    pub module_path: String,
}

#[derive(Debug, Clone)]
pub struct Export {
    pub origin_name: String,
    pub alias: String,
}

#[derive(Debug, Clone)]
pub enum Module {
    Source {
        path: String,
        imports: Vec<Import>,
        exports: Vec<Export>,
        constants: Vec<Const>,
        funcs: Vec<Func>,
    },
    Lib(Lib),
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub modules: BTreeMap<String, Module>,
}
