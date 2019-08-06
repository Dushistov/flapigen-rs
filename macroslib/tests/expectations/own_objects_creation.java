r#"public Boo(int a0, long a1) throws Exception {
        mNativeObj = init(a0, a1);
    }
    private static native long init(int a0, long a1) throws Exception;"#;
r#"public static @NonNull Boo factory_method() throws Exception {
        long ret = do_factory_method();
        Boo convRet = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_factory_method() throws Exception;"#;
r#"public final int boo_as_arg(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        a0.mNativeObj = 0;

        int ret = do_boo_as_arg(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);

        return ret;
    }
    private static native int do_boo_as_arg(long self, long a0);"#;
r#"public final @NonNull Foo get_one_foo() {
        long ret = do_get_one_foo(mNativeObj);
        Foo convRet = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_get_one_foo(long self);"#;
