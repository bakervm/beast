use melon::Instruction;
use std::collections::BTreeMap;

#[derive(Deserialize, Serialize)]
pub struct Lib {
    instructions: Vec<Instruction>,
    exports: BTreeMap<String, usize>,
}
