r#"public static void static_foo(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_static_foo(a00);
    }"#;
"private static native void do_f1(long self)";
