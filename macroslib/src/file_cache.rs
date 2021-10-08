/// To prevent modification time changing
use std::{
    collections::HashSet,
    fs::File,
    io,
    io::{Read, Write},
    mem,
    path::{Path, PathBuf},
};

/// Implement write cache in memory, and update file only if necessary
pub struct FileWriteCache {
    cnt: Vec<u8>,
    path: PathBuf,
    /// To prevent redifining some items,
    /// also it is possible to scan `cnt`, but it is possible to add
    /// user defined comments into generated file, so this is more robust
    #[allow(dead_code)]
    already_defined_items: HashSet<String>,
}

pub trait FileOperationsRegistrator {
    fn register(&mut self, p: &Path);
}

pub struct NoNeedFsOpsRegistration;
impl FileOperationsRegistrator for NoNeedFsOpsRegistration {
    fn register(&mut self, _p: &Path) {}
}

impl FileWriteCache {
    pub fn new<P: Into<PathBuf>>(
        p: P,
        fs_reg: &mut dyn FileOperationsRegistrator,
    ) -> FileWriteCache {
        let path = p.into();
        fs_reg.register(&path);
        FileWriteCache {
            cnt: vec![],
            path,
            already_defined_items: HashSet::default(),
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

    #[allow(dead_code)]
    pub fn take_content(&mut self) -> Vec<u8> {
        mem::take(&mut self.cnt)
    }

    #[allow(dead_code)]
    pub fn replace_content(&mut self, bytes: Vec<u8>) {
        self.cnt = bytes;
    }

    #[allow(dead_code)]
    pub fn define_item<S: Into<String>>(&mut self, item: S) {
        self.already_defined_items.insert(item.into());
    }

    #[allow(dead_code)]
    pub fn is_item_defined(&self, item: &str) -> bool {
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
