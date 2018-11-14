extern crate assert_cmd;
extern crate melon;

use assert_cmd::prelude::*;
use melon::Program;
use std::{path::PathBuf, process::Command};

#[test]
fn compile_all_code_examples() {
    let code_projects_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("code");

    let code_dirs: Vec<_> = code_projects_dir
        .read_dir()
        .unwrap()
        .filter(|path| path.is_ok())
        .map(|path| path.unwrap().path())
        .collect();

    for code_dir in code_dirs {
        Command::main_binary()
            .unwrap()
            .current_dir(&code_dir)
            .arg("build")
            .assert()
            .success();

        let target_dir = code_dir.join("target");

        let rom_file = target_dir
            .read_dir()
            .unwrap()
            .filter(|path| path.is_ok())
            .map(|path| path.unwrap().path())
            .find(|path| path.exists() && path.is_file())
            .unwrap_or_else(|| panic!("unable to find compilation artifact in {:?}", target_dir));

        Program::from_file(&rom_file)
            .unwrap_or_else(|_| panic!("unable to load program from file {:?}", rom_file));
    }
}
