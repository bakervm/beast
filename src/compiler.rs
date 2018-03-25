use config::Config;
use melon::typedef::*;
use melon::{Instruction, Program};
use std::path::Path;

pub struct Compiler {
    instructions: Vec<Instruction>,
}

impl Compiler {
    pub fn compile_file<P: AsRef<Path>>(&mut self, path: P, config: Config) -> Result<Program> {
        bail!("compiler not implemented yet");
    }
}
