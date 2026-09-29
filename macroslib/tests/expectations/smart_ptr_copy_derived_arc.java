@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"NavigationService.java","kind":"between"}
public final void subscribeOnUpdates(@NonNull Session session) {
        long a0 = session.mNativeObj;

        do_subscribeOnUpdates(mNativeObj, a0);

        JNIReachabilityFence.reachabilityFence1(session);
    }
    private static native void do_subscribeOnUpdates(long self, long session);
@@end
