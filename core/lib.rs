#[macro_use]
extern crate failure;
extern crate melon;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate flate2;
extern crate rmp_serde as rmps;

mod ast;
mod ast_gen;
pub mod compiler;
mod defaults;
mod parser;
