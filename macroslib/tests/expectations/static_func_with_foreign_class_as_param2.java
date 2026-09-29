@@expect {"after":"\n    private","before":"public final class Foo {\n\n    ","file":"Foo.java","kind":"between"}
public static void static_foo(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_static_foo(a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
@@end

@@expect {"after":";\n\n    public synchroniz","before":"do_f1(mNativeObj);\n    }\n    ","file":"Boo.java","kind":"between"}
private static native void do_f1(long self)
@@end
