@@expect {"after":" {\n        m","before":"public final class Foo {\n\n    ","file":"Foo.java","kind":"between"}
public Foo(int a0)
@@end

@@expect {"after":" {\n        i","before":"private static native long init(int a0);\n\n    ","file":"Foo.java","kind":"between"}
public final int f(int a0, int a1)
@@end

@@expect {"after":" {\n        m","before":"public final class Boo {\n\n    ","file":"Boo.java","kind":"between"}
public Boo(int a0, long a1)
@@end

@@expect {"after":"\n\n    public","before":"private static native long init(int a0, long a1);\n\n    ","file":"Boo.java","kind":"between"}
public Boo(@NonNull Foo f) {
        long a0 = f.mNativeObj;
        f.mNativeObj = 0;

        mNativeObj = init(a0);
        JNIReachabilityFence.reachabilityFence1(f);
    }
    private static native long init(long f);
@@end

@@expect {"after":"\n    private","before":"private static native long init(long f);\n\n    ","file":"Boo.java","kind":"between"}
public final long f(@NonNull Foo foo) {
        long a0 = foo.mNativeObj;
        foo.mNativeObj = 0;

        long ret = do_f(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(foo);

        return ret;
    }
@@end

@@expect {"after":"\n\n    public","before":"private static native long do_f(long self, long foo);\n\n    ","file":"Boo.java","kind":"between"}
public static int f2(double a0, @NonNull Foo foo) {
        long a1 = foo.mNativeObj;
        foo.mNativeObj = 0;

        int ret = do_f2(a0, a1);

        JNIReachabilityFence.reachabilityFence1(foo);

        return ret;
    }
    private static native int do_f2(double a0, long foo);
@@end
