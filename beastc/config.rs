use melon::typedef::*;
use std::{collections::BTreeMap, fs::File, io::Read, path::Path};
use toml;

#[derive(Deserialize, Debug, Clone)]
pub struct Program {
    /// The version of the melon library used by the target
    pub name: String,
    pub system_id: String,
    pub mem_pages: Option<u8>,
}

#[derive(Deserialize, Debug, Default, Clone)]
pub struct Compilation {
    pub entry_point: Option<String>,
    /// The paths to look in for source files to include
    #[serde(default, rename = "include")]
    pub include_dirs: Vec<String>,
}

#[derive(Deserialize, Debug, Clone)]
pub struct Config {
    pub program: Program,
    #[serde(default)]
    pub compilation: Compilation,
    #[serde(default)]
    pub signals: BTreeMap<String, u16>,
}

impl Config {
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Config> {
        let mut file = File::open(path)?;

        let mut buf = String::new();
        file.read_to_string(&mut buf)?;

        let config = toml::from_str(&buf)?;

        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_config() {
        const FILE_NAME: &str = "src/templates/Beast.toml";

        let config = Config::from_file(FILE_NAME).unwrap();

        println!("{:#?}", config);
    }
}
