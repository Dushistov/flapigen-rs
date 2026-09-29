@@expect {"after":"\n    private","before":"public final class Foo {\n\n    ","file":"Foo.java","kind":"between"}
public static void f1(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_f1(a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
@@end

@@expect {"after":"\n    private","before":"private static native void do_f1(long a0);\n\n    ","file":"Foo.java","kind":"between"}
public static void f2(@NonNull Boo a0) {
        long a00 = a0.mNativeObj;
        do_f2(a00);

        JNIReachabilityFence.reachabilityFence1(a0);
    }
@@end
