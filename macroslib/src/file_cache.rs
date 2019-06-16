use std::collections::HashSet;

/// To prevent modification time changing
use std::{
    fs::File,
    io,
    io::{Read, Write},
    path::PathBuf,
};

/// Implement write cache in memory, and update file only if necessary
pub(crate) struct FileWriteCache {
    cnt: Vec<u8>,
    path: PathBuf,
    /// To prevent redifining some items,
    /// also it is possible to scan `cnt`, but it is possible to add
    /// user defined comments into generated file, so this is more robust
    already_defined_items: HashSet<String>,
}

impl FileWriteCache {
    pub(crate) fn new<P: Into<PathBuf>>(p: P) -> FileWriteCache {
        FileWriteCache {
            cnt: vec![],
            path: p.into(),
            already_defined_items: HashSet::default(),
        }
    }

    pub(crate) fn update_file_if_necessary(self) -> Result<(), io::Error> {
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

    pub(crate) fn define_item<S: Into<String>>(&mut self, item: S) {
        self.already_defined_items.insert(item.into());
    }
    pub(crate) fn is_item_defined(&self, item: &str) -> bool {
        self.already_defined_items.contains(item)
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
