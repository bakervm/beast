const _GRAMMAR: &str = include_str!("../src/beast.pest");

#[derive(Parser)]
#[grammar = "beast.pest"]
pub struct BeastParser;

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_data() {
        let input = include_str!("../templates/main.bst");

        BeastParser::parse(Rule::file, input).unwrap();
    }
}
