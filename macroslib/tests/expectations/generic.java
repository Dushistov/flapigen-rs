r#"public Foo(int a0) {
        mNativeObj = init(a0);
    }
    private static native long init(int a0);"#;
r#"public final int f(int a0, int a1) {
        int ret = do_f(mNativeObj, a0, a1);

        return ret;
    }
    private static native int do_f(long self, int a0, int a1);"#;
r#"public Boo(int a0, long a1) throws Exception {
        mNativeObj = init(a0, a1);
    }
    private static native long init(int a0, long a1) throws Exception;"#;
r#"public final @NonNull Foo [] get_foo_arr() {
        Foo [] ret = do_get_foo_arr(mNativeObj);

        return ret;
    }
    private static native @NonNull Foo [] do_get_foo_arr(long self);"#;
r#"public final @NonNull Foo get_one_foo() throws Exception {
        long ret = do_get_one_foo(mNativeObj);
        Foo convRet = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_get_one_foo(long self) throws Exception;"#;
r#"public static @NonNull java.util.Date now() {
        long ret = do_now();
        java.util.Date convRet = new java.util.Date(ret);

        return convRet;
    }
    private static native long do_now();"#;
"public static native short r_test_u8(short v) throws Exception;";
