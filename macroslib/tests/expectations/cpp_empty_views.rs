foreign_class!(class EmptyViews {
    fn take_slice(value: &[u32]) -> usize;
    fn take_mut_slice(value: &mut [u32]) -> usize;
    fn take_str(value: &str) -> String;
});
