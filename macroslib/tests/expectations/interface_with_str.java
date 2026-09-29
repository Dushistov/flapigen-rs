@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"ClassWithCallbacks.java","kind":"between"}
public final void f1(@NonNull SomeObserver cb) {
        do_f1(mNativeObj, cb);
    }
    private static native void do_f1(long self, SomeObserver cb);
@@end

@@expect {"after":"\n","before":"import android.support.annotation.NonNull;\n\n","file":"SomeObserver.java","greedy_match":true,"kind":"between"}
public interface SomeObserver {


    void onStateChanged(@NonNull String a0);

}
@@end
