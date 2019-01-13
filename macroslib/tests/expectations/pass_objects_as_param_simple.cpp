r#"inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f1(const Foo & a_0) const  noexcept
    {
        TestPassObjectsAsParams_f1(this->self_, static_cast<const FooOpaque *>(a_0));
    }"#;
    
r#"inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f2(Foo a_0) const  noexcept
    {
        TestPassObjectsAsParams_f2(this->self_, a_0.release());
    }"#;
    
    r#"inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f3(Foo & a_0) const  noexcept
    {
        TestPassObjectsAsParams_f3(this->self_, static_cast<const FooOpaque *>(a_0));
    }"#;
