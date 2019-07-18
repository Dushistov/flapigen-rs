r#"public static Boo factory_method() {
        long ret = do_factory_method();
        Boo conv_ret = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_factory_method();"#;
"public final Foo get_one_foo() {";
"public final int f(int a0, int a1)";
