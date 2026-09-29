@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"A.java","kind":"between"}
public static void a(@NonNull B b) {
        long a0 = b.mNativeObj;
        do_a(a0);

        JNIReachabilityFence.reachabilityFence1(b);
    }
    private static native void do_a(long b);
@@end

@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"B.java","kind":"between"}
public static void b(@NonNull A a) {
        long a0 = a.mNativeObj;
        do_b(a0);

        JNIReachabilityFence.reachabilityFence1(a);
    }
    private static native void do_b(long a);
@@end
