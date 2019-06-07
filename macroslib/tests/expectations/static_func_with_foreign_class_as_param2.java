r#"public static void static_foo(@NonNull Boo a0)  {
        long a0C0 = a0.mNativeObj;
         do_static_foo(a0C0);
    }"#;
"private static native void do_f1(long me)";
