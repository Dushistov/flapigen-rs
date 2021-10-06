"static void f(std::unique_ptr<MyObserver> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline void TestWrapper<OWN_DATA>::f(std::unique_ptr<MyObserver> a0) noexcept
    {

        C_MyObserver tmp = MyObserver::to_c_interface(std::move(a0));
        const struct C_MyObserver * const a00 = &tmp;

        Test_f(std::move(a00));
    }"#;
"void Test_f(const struct C_MyObserver * const a0);";
