foreigner_class!(
    #[derive(SmartPtrCopy)]
    class Session {
        self_type Session;
        constructor session_init() -> Rc<RefCell<Session>>;
    }
);

foreigner_class!(class NavigationService {
    self_type NavigationService;

    constructor init() -> Rc<RefCell<NavigationService>>;
    fn subscribeOnUpdates(&mut self, session: Rc<RefCell<Session>>);
});
