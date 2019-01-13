r#"inline int32_t TestPassInterfaceWrapper<OWN_DATA>::use_interface(Interface a_0, int32_t a_1) noexcept
    {
        int32_t ret = TestPassInterface_use_interface(a_0.release(), a_1);
        return ret;
    }"#;
