#[macro_use]
extern crate failure;
extern crate melon;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate toml;

mod ast;
mod compiler;
mod config;
mod parser;

use compiler::Compiler;
use config::Config;

fn main() {
    let config = Config::from_file("Beast.toml").unwrap();

    let res = Compiler::compile("main.beast", config).unwrap_or_else(|e| panic!("{}", e));

    println!("{:#?}", res);
}
