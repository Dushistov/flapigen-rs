use log::info;
use std::sync::{Arc, Mutex};
mod java_glue;
pub use crate::java_glue::*;

// ANCHOR: rust_code
struct Session {
    a: i32,
}

impl Session {
    pub fn new() -> Arc<Mutex<Session>> {
        #[cfg(target_os = "android")]
        android_logger::init_once(
            android_logger::Config::default()
                .with_max_level(log::LevelFilter::Debug)
                .with_tag("Hello"),
        );
        log_panics::init(); // log panics rather than printing them
        info!("init log system - done");
        Arc::new(Mutex::new(Session { a: 2 }))
    }

    pub fn add_and1(&self, val: i32) -> i32 {
        self.a + val + 1
    }

    pub fn set_base(&mut self, base: i32) {
        self.a = base;
    }

    // Greeting with full, no-runtime-cost support for newlines and UTF-8
    pub fn greet(to: &str) -> String {
        format!("Hello {} ✋\nIt's a pleasure to meet you!", to)
    }
}
// ANCHOR_END: rust_code

// ANCHOR: smart_ptr_copy_java_rust
struct SessionStore {
    session: Arc<Mutex<Session>>,
}

impl SessionStore {
    fn new(session: Arc<Mutex<Session>>) -> Self {
        Self { session }
    }

    fn saved_base(&self) -> i32 {
        self.session.lock().unwrap().a
    }
}
// ANCHOR_END: smart_ptr_copy_java_rust
