extern crate beast;
extern crate melon;
extern crate structopt;

mod defaults;

use beast::compiler::{Compiler, SignalPair};
use melon::typedef::Result;
use std::{env, path::PathBuf, time::Instant};
use structopt::StructOpt;

#[derive(StructOpt)]
#[structopt(
    name = "beastc",
    about = "The compiler for the Beast high level assembly language"
)]
struct Opt {
    module: String,
    system_id: String,
    #[structopt(long, short)]
    signal: Vec<SignalPair>,
    #[structopt(long, short)]
    mem_pages: Option<u8>,
    #[structopt(
        short = "o",
        help = "specify output destination",
        parse(from_os_str)
    )]
    output: Option<PathBuf>,
    #[structopt(
        short = "I",
        help = "specify directories to search in for included files",
        parse(from_os_str)
    )]
    include: Vec<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();

    build(opt).unwrap_or_else(|e| {
        eprintln!("Error: {}", e);
        ::std::process::exit(1);
    });
}

fn build(opt: Opt) -> Result<()> {
    let now = Instant::now();

    let program = Compiler::compile(
        env::current_dir()?,
        opt.module,
        opt.system_id,
        opt.mem_pages,
        opt.signal,
        opt.include,
    )?;

    println!(
        "Compilation finished. Took {} seconds",
        now.elapsed().as_secs()
    );

    let output_file = opt.output.unwrap_or_else(|| {
        PathBuf::from(defaults::ROM_FILE_NAME).with_extension(melon::typedef::ROM_FILE_EXTENSION)
    });

    program.save_as(output_file)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    extern crate assert_cmd;
    extern crate tempfile;

    use self::assert_cmd::prelude::*;
    use std::{path::PathBuf, process::Command};

    fn beastc() -> Command {
        Command::cargo_bin("beastc").unwrap()
    }

    #[test]
    fn init_compilation() {
        let tmp_dir = tempfile::tempdir().expect("unable to create temporary directory");

        beastc()
            .current_dir(PathBuf::from("templates"))
            .args(&["main", "some_system_id"])
            .args(&[
                "-o",
                tmp_dir.path().join("some_output.rom").to_str().unwrap(),
            ]).assert()
            .success();

        // TODO: Write tests for new compiler frontend
    }
}
