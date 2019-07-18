r#"public Foo(int a0) {
        mNativeObj = init(a0);
    }
    private static native long init(int a0);"#;
r#"public final int f(int a0, int a1) {
        return do_f(mNativeObj, a0, a1);
    }
    private static native int do_f(long self, int a0, int a1);"#;
r#"public Boo(int a0, long a1) throws Exception {
        mNativeObj = init(a0, a1);
    }
    private static native long init(int a0, long a1) throws Exception;"#;
r#"public final Foo [] get_foo_arr() {
        return do_get_foo_arr(mNativeObj);
    }
    private static native Foo [] do_get_foo_arr(long self);"#;
r#"public final Foo get_one_foo() throws Exception {
        long ret = do_get_one_foo(mNativeObj);
        Foo conv_ret = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_get_one_foo(long self) throws Exception;"#;
r#"public static @NonNull java.util.Date now() {
        long ret = do_now();
        java.util.Date conv_ret = new java.util.Date(ret);

        return conv_ret;
    }
    private static native long do_now();"#;
"public static native short r_test_u8(short v) throws Exception;";
