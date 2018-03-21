#[macro_use]
extern crate pest_derive;
extern crate pest;
#[macro_use]
extern crate serde_derive;
extern crate melon;
extern crate serde;
extern crate toml;

mod config;
mod parser;

use std::io::Read;
use std::fs::File;
use pest::Parser;

fn main() {
    let mut file = File::open("main.beast").unwrap();
    let mut buf = String::new();

    file.read_to_string(&mut buf).unwrap();

    let res =
        parser::BeastParser::parse(parser::Rule::file, &buf).unwrap_or_else(|e| panic!("{}", e));

    println!("{}", res);
}
