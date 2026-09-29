@@expect {"after":" {\n        j","before":"private static native long init();\n\n    ","file":"Foo.java","kind":"between"}
public final @NonNull java.util.OptionalDouble f1(@Nullable Double a0)
@@end

@@expect {"after":" {\n        j","before":"private static native @NonNull java.util.OptionalDouble do_f1(long self, @Nullable Double a0);\n\n    ","file":"Foo.java","kind":"between"}
public final @NonNull java.util.OptionalLong f2(@Nullable Long a0)
@@end

@@expect {"after":"\n\n    public final void ","before":"private static native @NonNull java.util.OptionalLong do_f2(long self, @Nullable Long a0);\n\n    ","file":"Foo.java","kind":"between"}
public final @NonNull java.util.Optional<Boo> f3() {
        long ret = do_f3(mNativeObj);
        java.util.Optional<Boo> convRet;
        if (ret != 0) {
            convRet = java.util.Optional.of(new Boo(InternalPointerMarker.RAW_PTR, ret));
        } else {
            convRet = java.util.Optional.empty();
        }

        return convRet;
    }
    private static native long do_f3(long self);
@@end

@@expect {"after":"\n    public final @NonNu","before":"private static native long do_f3(long self);\n\n    ","file":"Foo.java","kind":"between"}
public final void f4(@Nullable Boo boo) {
        long a0 = 0;//TODO: use ptr::null() for corresponding constant
        if (boo != null) {
            a0 = boo.mNativeObj;
            boo.mNativeObj = 0;
        }

        do_f4(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(boo);
    }
    private static native void do_f4(long self, long boo);

@@end

@@expect {"after":"\n\n    public final void ","before":"private static native void do_f4(long self, long boo);\n\n    ","file":"Foo.java","kind":"between"}
public final @NonNull java.util.Optional<String> f5() {
        String ret = do_f5(mNativeObj);
        java.util.Optional<String> convRet = java.util.Optional.ofNullable(ret);

        return convRet;
    }
    private static native @Nullable String do_f5(long self);
@@end

@@expect {"after":"\n\n    public final void ","before":"private static native @Nullable String do_f5(long self);\n\n    ","file":"Foo.java","kind":"between"}
public final void f6(@Nullable Boo boo) {
        long a0 = 0;//TODO: use ptr::null() for corresponding constant
        if (boo != null) {
            a0 = boo.mNativeObj;
        }

        do_f6(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(boo);
    }
    private static native void do_f6(long self, long boo);
@@end

@@expect {"after":"\n\n    public final @NonNull java.util.Op","before":"private static native void do_f6(long self, long boo);\n\n    ","file":"Foo.java","kind":"between"}
public final void f7(@Nullable String a0) {
        do_f7(mNativeObj, a0);
    }
    private static native void do_f7(long self, @Nullable String a0);
@@end

@@expect {"after":"\n\n    public synchronize","before":"private static native void do_f7(long self, @Nullable String a0);\n\n    ","file":"Foo.java","kind":"between"}
public final @NonNull java.util.OptionalInt f8(@Nullable Integer a0) {
        java.util.OptionalInt ret = do_f8(mNativeObj, a0);

        return ret;
    }
    private static native @NonNull java.util.OptionalInt do_f8(long self, @Nullable Integer a0);
@@end
