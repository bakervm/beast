const _GRAMMAR: &str = include_str!("beast.pest");

#[derive(Parser)]
#[grammar = "beast.pest"]
pub struct BeastParser;
