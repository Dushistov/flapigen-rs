foreigner_class!(class Error {
    self_type Error;
    private constructor = empty;
});

foreign_callback!(
    interface Foo {
        self_type Foo;
        unpack = Foo::unpack(&self, x: &str) -> Result<String, Error>;
        remove = Foo::remove(&self) -> Result<(), Error>;
    }
);
