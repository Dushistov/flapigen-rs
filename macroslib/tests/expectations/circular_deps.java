r#"public static void a(@NonNull B b) {
        long a0 = b.mNativeObj;
        do_a(a0);
    }
    private static native void do_a(long b);"#;
r#"public static void b(@NonNull A a) {
        long a0 = a.mNativeObj;
        do_b(a0);
    }
    private static native void do_b(long a);"#;
