#[macro_use]
extern crate failure;
extern crate melon;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate beast;
extern crate structopt;
extern crate toml;

mod config;
mod defaults;

use beast::compiler::{Compiler, SignalPair};
use config::Config;
use melon::typedef::*;
use std::{
    env,
    fs::{self, File},
    io::Write,
    path::PathBuf,
    time::Instant,
};
use structopt::StructOpt;

const TARGET_DIRECTORY: &str = "target";
const CONFIG_FILE_NAME: &str = "Tame.toml";

#[derive(StructOpt)]
#[structopt(
    name = "tame",
    about = "A build tool for the Beast high level assembly language"
)]
enum Opt {
    #[structopt(
        name = "new",
        about = "initialize a new Beast project at the given directory"
    )]
    New {
        #[structopt(
            help = "the path to the target project directory",
            parse(from_os_str)
        )]
        path: PathBuf,
    },
    #[structopt(name = "build", about = "builds the current project")]
    Build,
}

fn main() {
    run().unwrap_or_else(|e| {
        eprintln!("Error: {}", e);
        ::std::process::exit(1);
    });
}

fn run() -> Result<()> {
    let opt = Opt::from_args();

    match opt {
        Opt::Build => build()?,
        Opt::New { path } => new(&path)?,
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

    let compilation = config.compilation.clone();

    let entry_point = compilation
        .entry_point
        .unwrap_or_else(|| defaults::BIN_ENTRY_POINT_MODULE.into());

    let name = config.program.name.clone();

    let now = Instant::now();

    let program = Compiler::compile(
        env::current_dir()?.join(defaults::INCLUDE_PATH),
        entry_point,
        config.program.system_id.clone(),
        config.program.mem_pages,
        config
            .signals
            .iter()
            .map(|(key, value)| SignalPair {
                key: key.to_string(),
                value: *value,
            }).collect(),
        config.compilation.include_dirs.clone(),
    )?;

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
    fs::create_dir_all(path.join(defaults::INCLUDE_PATH))?;

    let config_data = include_bytes!("../templates/Tame.toml");
    let mut config_file = File::create(path.join(CONFIG_FILE_NAME))?;
    config_file.write_all(&config_data[..])?;

    let main_file_data = include_bytes!("../templates/main.bst");
    let mut main_file_file = File::create(path.join(defaults::INCLUDE_PATH).join("main.bst"))?;
    main_file_file.write_all(&main_file_data[..])?;

    let gitignore_data = include_bytes!("../templates/.gitignore");
    let mut gitignore = File::create(path.join(".gitignore"))?;
    gitignore.write_all(&gitignore_data[..])?;

    println!("New project successfully initialized at {:?}", path);

    Ok(())
}

#[cfg(test)]
mod tests {
    extern crate assert_cmd;
    extern crate tempfile;

    use self::assert_cmd::prelude::*;
    use std::process::Command;

    #[test]
    fn init_compilation() {
        let tmp_dir = tempfile::tempdir().expect("unable to create temporary directory");

        Command::cargo_bin("tame")
            .unwrap()
            .current_dir(tmp_dir.path())
            .args(&["new", "test_project"])
            .assert()
            .success();

        Command::cargo_bin("tame")
            .unwrap()
            .current_dir(tmp_dir.path().join("test_project"))
            .arg("build")
            .assert()
            .success();
    }
}
