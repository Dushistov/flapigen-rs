r#"public final class LongOperation {

    public LongOperation(@NonNull DropCounter drop_counter) {
        long a0 = drop_counter.mNativeObj;
        mNativeObj = init(a0);
        JNIReachabilityFence.reachabilityFence1(drop_counter);
    }
    private static native long init(long drop_counter);

    public final void mf(@NonNull DropCounter drop_counter) {
        long a0 = drop_counter.mNativeObj;
        do_mf(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(drop_counter);
    }
    private static native void do_mf(long self, long drop_counter);

    public static void f(@NonNull DropCounter drop_counter) {
        long a0 = drop_counter.mNativeObj;
        do_f(a0);

        JNIReachabilityFence.reachabilityFence1(drop_counter);
    }
    private static native void do_f(long drop_counter);

    public final int mf2(@NonNull DropCounter drop_counter) {
        long a0 = drop_counter.mNativeObj;
        int ret = do_mf2(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(drop_counter);

        return ret;
    }
    private static native int do_mf2(long self, long drop_counter);

    public static int f2(@NonNull DropCounter drop_counter) {
        long a0 = drop_counter.mNativeObj;
        int ret = do_f2(a0);

        JNIReachabilityFence.reachabilityFence1(drop_counter);

        return ret;
    }
    private static native int do_f2(long drop_counter);"#;

r#"/*package*/ final class JNIReachabilityFence {
    private JNIReachabilityFence() {}
    /*package*/ static native void reachabilityFence1(Object ref1);
    /*package*/ static native void reachabilityFence2(Object ref1, Object ref2);
    /*package*/ static native void reachabilityFence3(Object ref1, Object ref2, Object ref3);
    /*package*/ static native void reachabilityFence4(Object ref1, Object ref2, Object ref3, Object ref4);"#;


