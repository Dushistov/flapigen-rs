r#"public Boo(int a0, long a1) throws Exception {
        mNativeObj = init(a0, a1);
    }
    private static native long init(int a0, long a1) throws Exception;"#;
r#"public static Boo factory_method() throws Exception {
        long ret = do_factory_method();
        Boo conv_ret = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_factory_method() throws Exception;"#;
r#"public final int boo_as_arg(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        a0.mNativeObj = 0;

        return do_boo_as_arg(mNativeObj, a00);
    }
    private static native int do_boo_as_arg(long self, long a0);"#;
r#"public final Foo get_one_foo() {
        long ret = do_get_one_foo(mNativeObj);
        Foo conv_ret = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_get_one_foo(long self);"#;
