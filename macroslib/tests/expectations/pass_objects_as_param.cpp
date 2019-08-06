r#"void f1(const Foo & a0) const noexcept;

    void f2(Foo a0) const noexcept;

    void f3(Foo & a0) const noexcept;

    void f4(const Foo & a0) const noexcept;

    void f5(Foo & a0) const noexcept;"#;

r#"template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f1(const Foo & a0) const noexcept
    {

        TestPassObjectsAsParams_f1(this->self_, static_cast<const FooOpaque *>(a0));
    }

    template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f2(Foo a0) const noexcept
    {

        TestPassObjectsAsParams_f2(this->self_, a0.release());
    }

    template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f3(Foo & a0) const noexcept
    {

        TestPassObjectsAsParams_f3(this->self_, static_cast<FooOpaque *>(a0));
    }

    template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f4(const Foo & a0) const noexcept
    {

        TestPassObjectsAsParams_f4(this->self_, static_cast<const FooOpaque *>(a0));
    }

    template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f5(Foo & a0) const noexcept
    {

        TestPassObjectsAsParams_f5(this->self_, static_cast<FooOpaque *>(a0));
    }"#;

r#"void TestPassObjectsAsParams_f1(const TestPassObjectsAsParamsOpaque * const self, const FooOpaque * a0);

    void TestPassObjectsAsParams_f2(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);

    void TestPassObjectsAsParams_f3(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);

    void TestPassObjectsAsParams_f4(const TestPassObjectsAsParamsOpaque * const self, const FooOpaque * a0);

    void TestPassObjectsAsParams_f5(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);"#;
