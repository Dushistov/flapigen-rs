"public final @NonNull java.util.OptionalDouble f1(@Nullable Double a0)";
"public final java.util.OptionalLong f2(@Nullable Long a0)";
r#"public final @NonNull java.util.Optional<Boo> f3() {
        long ret = do_f3(mNativeObj);
        java.util.Optional<Boo> convRet = (ret != 0) ? java.util.Optional.of(new Boo(InternalPointerMarker.RAW_PTR, ret)) :
                           java.util.Optional.empty();

        return convRet;
    }
    private static native long do_f3(long self);"#;
r#"public final void f4(@Nullable Boo boo) {
        long a0 = 0;//TODO: use ptr::null() for corresponding constant
        if (boo != null) {
            a0 = boo.mNativeObj;
            boo.mNativeObj = 0;
        }

        do_f4(mNativeObj, a0);
    }
    private static native void do_f4(long self, long boo);
"#;

r#"public final @NonNull java.util.Optional<String> f5() {
        String ret = do_f5(mNativeObj);
        java.util.Optional<String> convRet = java.util.Optional.ofNullable(ret);

        return convRet;
    }
    private static native String do_f5(long self);"#;

r#"public final void f6(@Nullable Boo boo) {
        long a0 = 0;//TODO: use ptr::null() for corresponding constant
        if (boo != null) {
            a0 = boo.mNativeObj;
        }

        do_f6(mNativeObj, a0);
    }
    private static native void do_f6(long self, long boo);"#;

r#"public final void f7(@Nullable String a0) {
        do_f7(mNativeObj, a0);
    }
    private static native void do_f7(long self, @Nullable String a0);"#;

r#"public final @NonNull java.util.OptionalInt f8(@Nullable Integer a0) {
        return do_f8(mNativeObj, a0);
    }
    private static native @NonNull java.util.OptionalInt do_f8(long self, @Nullable Integer a0);"#;
