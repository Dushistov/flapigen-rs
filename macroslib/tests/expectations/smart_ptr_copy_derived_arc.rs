foreigner_class!(
    #[derive(SmartPtrCopy)]
    class Session {
        self_type Session;
        constructor session_init() -> Arc<Mutex<Session>>;
        fn setFoo(&mut self, _: i32);
    }
);

foreigner_class!(class NavigationService {
    self_type NavigationService;

    constructor init() -> Arc<Mutex<NavigationService>>;
    fn subscribeOnUpdates(&mut self, session: Arc<Mutex<Session>>);
});
