r#"BooWrapper(Foo a_0) noexcept
    {
        this->self_ = Boo_with_foo(a_0.release());
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#;

"uintptr_t f(Foo a_0) const";

"BooOpaque *Boo_with_foo(FooOpaque * a_0);";
