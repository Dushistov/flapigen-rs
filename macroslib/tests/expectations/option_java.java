"public final java.util.OptionalDouble f1(@Nullable Double a0)";
"public final java.util.OptionalLong f2(@Nullable Long a0)";
"public final java.util.Optional<Boo> f3()";
r#"public final void f4(@Nullable Boo foo) {

        long a0 = 0;//TODO: use ptr::null() for corresponding constant
        if (foo != null) {
            a0 = foo.mNativeObj;
            foo.mNativeObj = 0;
        }

        do_f4(mNativeObj, a0);
    }
    private static native void do_f4(long self, long foo);
"#;
"public final java.util.Optional<String> f5()";
