"virtual void onStateChanged1(int32_t a0, bool a1) const noexcept = 0;";
r#"static void c_onStateChanged1(int32_t a0, char a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChanged1(a0, (a1 != 0));
    }"#;
"void (*onStateChanged1)(int32_t a0, char a1, void *opaque);";


"virtual bool onStateChanged2(bool a0, double a1) const noexcept = 0;";
r#"static char c_onStateChanged2(char a0, double a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        auto ret = pi->onStateChanged2((a0 != 0), a1);
        return ret ? 1 : 0;
    }"#;
"char (*onStateChanged2)(char a0, double a1, void *opaque);";
