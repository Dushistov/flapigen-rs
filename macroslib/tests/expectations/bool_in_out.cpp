"bool f1(bool a_0)";
r#"template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f1(bool a_0)  noexcept
    {
        char ret = Foo_f1(this->self_, a_0 ? 1 : 0);
        return ret != 0;
    }"#;
"char Foo_f1(FooOpaque * const self, char a_0);";
