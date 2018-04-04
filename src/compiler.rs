use config::Config;
use melon::Instruction;

pub struct Compiler {
    instructions: Vec<Instruction>,
    config: Config,
}

impl Compiler {
    fn new(config: Config) -> Compiler {
        Compiler {
            config,
            instructions: Vec::new(),
        }
    }
}
