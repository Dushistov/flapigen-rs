"static int32_t use_interface(Interface a, int32_t b) noexcept;";

r#"inline int32_t TestPassInterfaceWrapper<OWN_DATA>::use_interface(Interface a, int32_t b) noexcept
    {

        int32_t ret = TestPassInterface_use_interface(a.release(), b);
        return ret;
    }"#;
