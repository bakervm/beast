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
mod defaults;
mod parser;

use compiler::Compiler;
use config::Config;
use melon::typedef::Result;
use std::{fs::{self, File},
          io::Write,
          path::PathBuf,
          time::Instant};
use structopt::StructOpt;

const TARGET_DIRECTORY: &str = "target";
const CONFIG_FILE_NAME: &str = "Beast.toml";

#[derive(StructOpt)]
enum Opt {
    #[structopt(name = "new", about = "initialize a new Beast project at the given directory")]
    New {
        #[structopt(help = "the path to the target project directory", parse(from_os_str))]
        path: PathBuf,
    },
    #[structopt(name = "build", about = "builds the current project")]
    Build {
        #[structopt(
            long = "emit-func-map",
            help = "emits the corresponding function-map for the current build"
        )]
        emit_func_map: bool,
        #[structopt(long = "emit-ast", help = "emits the corresponding AST for the current build")]
        emit_ast: bool,
    },
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
        Opt::Build {
            emit_func_map,
            emit_ast,
        } => build(emit_func_map, emit_ast)?,
        Opt::New { path } => new(&path)?,
    }

    Ok(())
}

fn build(emit_func_map: bool, emit_ast: bool) -> Result<()> {
    let config_file = PathBuf::from(CONFIG_FILE_NAME);

    ensure!(
        config_file.exists(),
        "unable to find {} in current directory",
        CONFIG_FILE_NAME
    );

    let config = Config::from_file(config_file)?;

    let compilation = config.compilation.clone();

    let entry_point = compilation
        .entry_point
        .unwrap_or(defaults::BIN_ENTRY_POINT_MODULE.into());

    let name = config.program.name.clone();

    let now = Instant::now();

    let program = Compiler::compile(entry_point, config, emit_func_map, emit_ast)?;

    println!(
        "Compilation finished. Took {} seconds",
        now.elapsed().as_secs()
    );

    let output_file = PathBuf::from(name).with_extension(melon::typedef::ROM_FILE_EXTENSION);
    let output_path = PathBuf::from(TARGET_DIRECTORY).join(output_file);

    fs::create_dir_all(TARGET_DIRECTORY)?;

    program.save_as(output_path)?;

    Ok(())
}

fn new(path: &PathBuf) -> Result<()> {
    ensure!(!path.exists(), "directory already exists");

    fs::create_dir_all(path.join(TARGET_DIRECTORY))?;
    fs::create_dir_all(path.join(defaults::LIB_PATH))?;
    fs::create_dir_all(path.join(defaults::INCLUDE_PATH))?;

    let config_data = format!(
        r#"# This is a configuration file template

[program]
name = "program_name"
target_version = "{}" # the version of the melon library used by the target
system_id = "__DEBUG__"

# [signals]
# gurgle = 1
# nuke = 2
# fire = 3"#,
        melon::VERSION
    );

    let mut config_file = File::create(path.join(CONFIG_FILE_NAME))?;
    config_file.write_all(&config_data.into_bytes())?;

    let main_file_data = include_bytes!("templates/main.bst");
    let mut main_file_file = File::create(path.join(defaults::INCLUDE_PATH).join("main.bst"))?;
    main_file_file.write_all(&main_file_data[..])?;

    let gitignore_data = include_bytes!("templates/.gitignore");
    let mut gitignore = File::create(path.join(".gitignore"))?;
    gitignore.write_all(&gitignore_data[..])?;

    println!("New project successfully initialized at {:?}", path);

    Ok(())
}
