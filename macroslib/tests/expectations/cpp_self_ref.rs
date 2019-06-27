foreigner_class!(
    #[derive(Copy)]
    class BtAddr {
    self_type BtAddr;
    private constructor = empty;
    method BtAddr::to_string(&self) -> String;
    method BtAddr::clone(&self) -> BtAddr;
});
