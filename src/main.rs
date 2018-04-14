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
#[macro_use]
extern crate structopt;

mod ast;
mod ast_gen;
mod compiler;
mod config;
mod library;
mod parser;

use compiler::Compiler;
use config::Config;
use melon::typedef::Result;
use std::{
    fs::{self, File},
    io::Write,
    path::PathBuf,
    time::Instant,
};
use structopt::StructOpt;

const TARGET_DIRECTORY: &str = "target";
const CONFIG_FILE_NAME: &str = "Beast.toml";

#[derive(StructOpt)]
enum Opt {
    #[structopt(name = "init", about = "initialize a new Beast project at the given directory")]
    Init {
        #[structopt(help = "the path to the target project directory", parse(from_os_str))]
        path: PathBuf,
    },
    #[structopt(name = "build", about = "builds the current project")]
    Build,
}

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
    let opt = Opt::from_args();

    match opt {
        Opt::Build => build()?,
        Opt::Init { path } => init(&path)?,
    }

    Ok(())
}

fn build() -> Result<()> {
    let config_file = PathBuf::from(CONFIG_FILE_NAME);

    ensure!(
        config_file.exists(),
        "unable to find {} in current directory",
        CONFIG_FILE_NAME
    );

    let config = Config::from_file(config_file)?;

    let entry_point = config
        .clone()
        .compilation
        .unwrap_or_default()
        .entry_point
        .unwrap_or(ast_gen::BEAST_DEFAULT_ENTRY_POINT_MODULE.into());

    let name = config.program.name.clone();

    let now = Instant::now();

    let program = Compiler::compile(entry_point, config)?;

    println!(
        "Compilation finished. Took {} seconds",
        now.elapsed().as_secs()
    );

    let output_file = PathBuf::from(name).with_extension(melon::typedef::ROM_FILE_EXTENSION);
    let output_path = PathBuf::from(TARGET_DIRECTORY).join(output_file);

    program.save_as(output_path)?;

    Ok(())
}

fn init(path: &PathBuf) -> Result<()> {
    ensure!(!path.exists(), "directory already exists");

    fs::create_dir_all(path.join(TARGET_DIRECTORY))?;
    fs::create_dir_all(path.join(ast_gen::BEAST_DEFAULT_LIB_PATH))?;
    fs::create_dir_all(path.join(ast_gen::BEAST_DEFAULT_INCLUDE_PATH))?;

    let config_data = include_bytes!("templates/Beast.template.toml");
    let main_file_data = include_bytes!("templates/main.template.bst");

    let mut config_file = File::create(path.join(CONFIG_FILE_NAME))?;
    config_file.write_all(&config_data[..])?;

    let mut main_file_file = File::create(
        path.join(ast_gen::BEAST_DEFAULT_INCLUDE_PATH)
            .join("main.bst"),
    )?;
    main_file_file.write_all(&main_file_data[..])?;

    println!("New project successfully initialized at {:?}", path);

    Ok(())
}
