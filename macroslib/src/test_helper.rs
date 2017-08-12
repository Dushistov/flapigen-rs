use env_logger;

pub fn logger_init() {
    env_logger::init().unwrap_or_else(|err| {
        eprintln!("logger init error {}", err);
    });
}
