r#"public final void f1(@NonNull Foo a0) {
        long a00 = a0.mNativeObj;
        do_f1(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
    private static native void do_f1(long self, long a0);"#;

r#"public final void f2(@NonNull Foo a0) {
        long a00 = a0.mNativeObj;
        a0.mNativeObj = 0;

        do_f2(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
    private static native void do_f2(long self, long a0);"#;

r#"public final void f3(@NonNull Foo a0) {
        long a00 = a0.mNativeObj;
        do_f3(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
    private static native void do_f3(long self, long a0);"#;

r#"public final void f4(@NonNull Foo a0) {
        long a00 = a0.mNativeObj;
        do_f4(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
    private static native void do_f4(long self, long a0);"#;

r#"public final void f5(@NonNull Foo a0) {
        long a00 = a0.mNativeObj;
        do_f5(mNativeObj, a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
    private static native void do_f5(long self, long a0);"#;
