use library::Lib;
use melon::{typedef::*, Instruction, IntegerType};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Expr {
    ActualInstr(Instruction),

    PushConstU8(Argument<u8>),
    PushConstU16(Argument<u16>),
    PushConstI8(Argument<i8>),
    PushConstI16(Argument<i16>),

    Load(IntegerType, Argument<Address>),
    Store(IntegerType, Argument<Address>),

    Sys(String),
    Call(String),
    Alloc(Argument<u16>),

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
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    NotEqual,
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
    pub expr: Vec<Expr>,
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
