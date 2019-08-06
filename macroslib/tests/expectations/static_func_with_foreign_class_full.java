r#"public static void f1(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_f1(a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }"#;

r#"public static void f2(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_f2(a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }"#;


