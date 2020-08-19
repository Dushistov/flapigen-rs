r#"private:

    FooWrapper(int32_t a0) noexcept
    {

        this->self_ = private_Foo_from_int(a0);
        if (this->self_ == nullptr) {
            std::abort();
        }
    }

    static void private_f() noexcept;
public:

    static void public_f() noexcept;
protected:

    static void protected_f() noexcept;"#;
