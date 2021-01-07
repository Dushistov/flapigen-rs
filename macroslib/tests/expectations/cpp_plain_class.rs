foreign_class!(
    #[derive(PlainClass)]
    class Foo {
        self_type Foo;

        constructor Foo::new() -> Foo;
        fn Foo::method(&self);
        fn Foo::static_func();
    }
);

foreign_class!(
    #[derive(Clone, Copy, PlainClass)]
    class AppError {
        self_type AppError;
        private constructor = empty;

        fn AppError::clone(&self) -> AppError;
        fn AppError::err_abbr(&self) -> &str;
        foreign_code r#"
    QString display() const noexcept;
"#;
    }
);
