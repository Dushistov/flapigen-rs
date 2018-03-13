/// To prevent modification time changing
use std::path::PathBuf;
use std::io;
use std::io::{Read, Write};
use std::fs::File;

/// Implement write cache in memory, and update file only if necessary
pub struct FileWriteCache {
    cnt: Vec<u8>,
    path: PathBuf,
}

impl FileWriteCache {
    pub fn new<P: Into<PathBuf>>(p: P) -> FileWriteCache {
        FileWriteCache {
            cnt: vec![],
            path: p.into(),
        }
    }

    pub fn update_file_if_necessary(self) -> Result<(), io::Error> {
        if let Ok(mut f) = File::open(&self.path) {
            let mut cur_cnt = vec![];
            f.read_to_end(&mut cur_cnt)?;
            if cur_cnt == self.cnt {
                return Ok(());
            }
        }
        let mut f = File::create(&self.path)?;
        f.write_all(&self.cnt)?;
        Ok(())
    }
}

impl io::Write for FileWriteCache {
    fn write(&mut self, data: &[u8]) -> Result<usize, io::Error> {
        self.cnt.extend_from_slice(data);
        Ok(data.len())
    }
    fn flush(&mut self) -> Result<(), io::Error> {
        Ok(())
    }
}
