trait LocalEvent {
    fn on_event(&self);
}

trait SendEvent: Send {
    fn on_event(&self);
}

trait SyncEvent: Sync {
    fn on_event(&self);
}

trait BothEvent: Send + Sync {
    fn on_event(&self);
}

foreign_callback!(callback LocalCallback {
    self_type LocalEvent;
    onEvent = LocalEvent::on_event(&self);
});

foreign_callback!(callback SendCallback {
    self_type SendEvent: Send;
    onEvent = SendEvent::on_event(&self);
});

foreign_callback!(callback SyncCallback {
    self_type SyncEvent + Sync;
    onEvent = SyncEvent::on_event(&self);
});

foreign_callback!(callback BothCallback {
    self_type BothEvent: Sync + Send;
    onEvent = BothEvent::on_event(&self);
});
