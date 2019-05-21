"virtual void onStateChanged1(int32_t a_0, bool a_1) = 0;";
r#"static void c_onStateChanged1(int32_t a_0, char a_1, void *opaque)
   {
        auto p = static_cast<SomeObserver *>(opaque);
        assert(p != nullptr);
        p->onStateChanged1(a_0, a_1 != 0);
   }"#;
"void (*onStateChanged1)(int32_t a_0, char a_1, void *opaque);";


"virtual bool onStateChanged2(bool a_0, double a_1) = 0;";
r#"static char c_onStateChanged2(char a_0, double a_1, void *opaque)
   {
        auto p = static_cast<SomeObserver *>(opaque);
        assert(p != nullptr);
        auto ret = p->onStateChanged2(a_0 != 0, a_1);
        return ret != 0;
   }"#;
"char (*onStateChanged2)(char a_0, double a_1, void *opaque);";
