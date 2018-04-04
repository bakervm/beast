use library::Lib;
use melon::{IntegerType, Register, typedef::*};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub enum Instruction {
    Add(IntegerType),
    Sub(IntegerType),
    Mul(IntegerType),
    Div(IntegerType),
    Shr(IntegerType),
    Shl(IntegerType),
    And(IntegerType),
    Or(IntegerType),
    Xor(IntegerType),
    Not(IntegerType),
    Neg(IntegerType),
    Inc(IntegerType),
    Dec(IntegerType),

    U8Promote,
    U16Demote,
    I8Promote,
    I16Demote,

    PushConstU8(Argument<SmallUInt>),
    PushConstU16(Argument<UInt>),
    PushConstI8(Argument<SmallInt>),
    PushConstI16(Argument<Int>),

    LoadReg(Register),

    Load(IntegerType, Argument<Address>),
    LoadIndirect(IntegerType),
    Store(IntegerType, Argument<Address>),
    StoreIndirect(IntegerType),

    Dup(IntegerType),
    Drop(IntegerType),

    Sys(String),

    Call(String),
    Ret,

    Alloc(Argument<UInt>),
    Free,

    While(While),
    If(If),
}

#[derive(Debug, Clone)]
pub enum Argument<T> {
    Literal(T),
    Constant(String),
}

#[derive(Debug, Clone)]
pub enum IfCond {
    Positive,
    Negative,
    Zero,
    NotZero,
}

#[derive(Debug, Clone)]
pub struct If(
    pub IfCond,
    pub IntegerType,
    pub Vec<Instruction>,
    pub Option<Vec<Instruction>>,
);

#[derive(Debug, Clone)]
pub struct While(pub IfCond, pub IntegerType, pub Vec<Instruction>);

#[derive(Debug, Clone)]
pub struct Func {
    pub name: String,
    pub instr: Vec<Instruction>,
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
