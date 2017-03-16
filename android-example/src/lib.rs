#[macro_use]
extern crate log;
extern crate android_logger;
extern crate log_panics;

mod java_glue;
mod android_c_headers;

//make interface with java public
pub use java_glue::*;

struct Session {
    a: i32,
}

impl Session {
    pub fn new() -> Session {
        android_logger::init_once(log::LogLevel::Debug);
        log_panics::init();
        info!("init log system - done");
        Session { a: 2 }
    }

    pub fn test(&self, val: i32) -> i32 {
        self.a + val
    }
}
