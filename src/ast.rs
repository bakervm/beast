use melon::typedef::*;
use melon::{IntegerType, Register};

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Argument<T> {
    Literal(T),
    Constant(String),
}

#[derive(Debug)]
pub enum IfCond {
    Positive,
    Negative,
    Zero,
    NotZero,
}

#[derive(Debug)]
pub struct If(
    pub IfCond,
    pub IntegerType,
    pub Vec<Instruction>,
    pub Option<Vec<Instruction>>,
);

#[derive(Debug)]
pub struct While(pub IfCond, pub IntegerType, pub Vec<Instruction>);

#[derive(Debug)]
pub struct Func {
    pub name: String,
    pub instr: Vec<Instruction>,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Export {
    pub origin_name: String,
    pub alias: String,
}

#[derive(Debug)]
pub struct Module {
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub constants: Vec<Const>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Ast {
    pub modules: Vec<Module>,
}
