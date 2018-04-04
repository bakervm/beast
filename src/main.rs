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
mod library;
mod parser;

use compiler::Compiler;
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
        .unwrap_or(compiler::BEAST_DEFAULT_ENTRY_POINT.into());

    let now = Instant::now();
    let res = Compiler::compile(entry_point, config)?;

    println!(
        "Compilation finished. Took {} seconds",
        now.elapsed().as_secs()
    );

    Ok(())
}
