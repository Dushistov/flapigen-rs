trait AsyncCallbacks {
    fn is_cancelled(&self) -> bool;
    fn on_result_ready(self, result: i32);
}

foreign_callback!(callback Completion {
    self_type AsyncCallbacks + Send;
    isCancelled = AsyncCallbacks::is_cancelled(&self) -> bool;
    onResultReady = AsyncCallbacks::on_result_ready(self, result: i32);
});
