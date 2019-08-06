r#"public static @NonNull Boo factory_method() {
        long ret = do_factory_method();
        Boo convRet = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_factory_method();"#;
"public final @NonNull Foo get_one_foo() {";
"public final int f(int a0, int a1)";
