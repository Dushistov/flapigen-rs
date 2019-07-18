foreigner_class!(
    #[derive(Copy)]
    class BtAddr {
    self_type BtAddr;
    private constructor = empty;
    fn BtAddr::to_string(&self) -> String;
    fn BtAddr::clone(&self) -> BtAddr;
});
