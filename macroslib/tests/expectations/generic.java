@@expect {"after":"\n\n    public","before":"public final class Foo {\n\n    ","file":"Foo.java","kind":"between"}
public Foo(int a0) {
        mNativeObj = init(a0);
    }
    private static native long init(int a0);
@@end

@@expect {"after":"\n\n    public","before":"private static native long init(int a0);\n\n    ","file":"Foo.java","kind":"between"}
public final int f(int a0, int a1) {
        int ret = do_f(mNativeObj, a0, a1);

        return ret;
    }
    private static native int do_f(long self, int a0, int a1);
@@end

@@expect {"after":"\n\n    public","before":"public final class Boo {\n\n    ","file":"Boo.java","kind":"between"}
public Boo(int a0, long a1) throws Exception {
        mNativeObj = init(a0, a1);
    }
    private static native long init(int a0, long a1) throws Exception;
@@end

@@expect {"after":"\n\n    public final @NonN","before":"private static native long init(int a0, long a1) throws Exception;\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo [] get_foo_arr() {
        Foo [] ret = do_get_foo_arr(mNativeObj);

        return ret;
    }
    private static native @NonNull Foo [] do_get_foo_arr(long self);
@@end

@@expect {"after":"\n\n    public","before":"private static native @NonNull Foo [] do_get_foo_arr(long self);\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo get_one_foo() throws Exception {
        long ret = do_get_one_foo(mNativeObj);
        Foo convRet = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_get_one_foo(long self) throws Exception;
@@end

@@expect {"after":"\n\n    public static @NonNull java.util.D","before":"private static native long do_get_one_foo(long self) throws Exception;\n\n    ","file":"Boo.java","kind":"between"}
public static @NonNull java.util.Date now() {
        long ret = do_now();
        java.util.Date convRet = new java.util.Date(ret);

        return convRet;
    }
    private static native long do_now();
@@end

@@expect {"after":"\n\n    public synchronized void delete() ","before":"private static native long do_now2() throws Exception;\n\n    ","file":"Boo.java","kind":"between"}
public static native short r_test_u8(short v) throws Exception;
@@end
