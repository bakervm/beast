use flate2::{read::GzDecoder, write::GzEncoder, Compression};
use melon::typedef::*;
use melon::Instruction;
use rmps::{Deserializer, Serializer};
use serde::{Deserialize, Serialize};
use std::{collections::BTreeMap,
          fs::File,
          io::{Read, Write},
          path::Path};

#[derive(Deserialize, Serialize, Debug, Clone)]
pub struct Lib {
    pub instructions: Vec<Instruction>,
    pub exports: BTreeMap<String, usize>,
}

impl Lib {
    /// Loads a MsgPack encoded and gzipped beast library from the given file
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Lib> {
        let mut file = File::open(path)?;

        let mut gz_buf = Vec::new();
        file.read_to_end(&mut gz_buf)?;

        let mut decoder = GzDecoder::new(&gz_buf[..]);
        let mut msgpack_buf = Vec::new();
        decoder.read_to_end(&mut msgpack_buf)?;

        let mut de = Deserializer::new(&msgpack_buf[..]);

        let res = Deserialize::deserialize(&mut de)?;

        Ok(res)
    }

    /// Saves the library as a MsgPack encoded and gzipped image to the given file
    pub fn save_as<P: AsRef<Path>>(&self, path: P) -> Result<()> {
        let mut msgpack_buf = Vec::new();
        self.serialize(&mut Serializer::new(&mut msgpack_buf))?;

        let mut encoder = GzEncoder::new(Vec::new(), Compression::default());
        encoder.write_all(&msgpack_buf[..])?;
        let gz_buf = encoder.finish()?;

        let mut file = File::create(path)?;
        file.write_all(&gz_buf[..])?;
        file.flush()?;

        Ok(())
    }
}
