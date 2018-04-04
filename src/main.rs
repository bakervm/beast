#[macro_use]
extern crate failure;
extern crate melon;
extern crate pest;
#[macro_use]
extern crate pest_derive;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate flate2;
extern crate rmp_serde as rmps;
extern crate toml;

mod ast;
mod ast_gen;
mod compiler;
mod config;
mod library;
mod parser;

use ast_gen::AstGen;
use config::Config;
use melon::typedef::Result;
use std::time::Instant;

fn main() {
    match run() {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{}", e);
            ::std::process::exit(1);
        }
    }
}

fn run() -> Result<()> {
    let config = Config::from_file("Beast.toml").unwrap();

    let entry_point = config
        .clone()
        .compilation
        .unwrap_or_default()
        .entry_point
        .unwrap_or(ast_gen::BEAST_DEFAULT_ENTRY_POINT.into());

    let now = Instant::now();

    let res = AstGen::gen(entry_point, config)?;
    println!("{:#?}", res);

    println!(
        "Compilation finished. Took {} seconds",
        now.elapsed().as_secs()
    );

    Ok(())
}
