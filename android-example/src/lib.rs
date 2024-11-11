use log::info;
mod java_glue;
pub use crate::java_glue::*;

// ANCHOR: rust_code
struct Session {
    a: i32,
}

impl Session {
    pub fn new() -> Session {
        #[cfg(target_os = "android")]
        android_logger::init_once(
            android_logger::Config::default()
                .with_max_level(log::LevelFilter::Debug)
                .with_tag("Hello"),
        );
        log_panics::init(); // log panics rather than printing them
        info!("init log system - done");
        Session { a: 2 }
    }

    pub fn add_and1(&self, val: i32) -> i32 {
        self.a + val + 1
    }

    // Greeting with full, no-runtime-cost support for newlines and UTF-8
    pub fn greet(to: &str) -> String {
        format!("Hello {} ✋\nIt's a pleasure to meet you!", to)
    }
}
// ANCHOR_END: rust_code
