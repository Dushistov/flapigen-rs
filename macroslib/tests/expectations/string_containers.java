r#"package org.example;
import android.support.annotation.NonNull;

public final class Foo {

    public Foo() {
        mNativeObj = init();
    }
    private static native long init();

    public final @NonNull java.lang.String [] list() {
        java.lang.String [] ret = do_list(mNativeObj);

        return ret;
    }
    private static native @NonNull java.lang.String [] do_list(long self);

    public synchronized void delete() {
        if (mNativeObj != 0) {
            do_delete(mNativeObj);
            mNativeObj = 0;
       }
    }
    @Override
    protected void finalize() throws Throwable {
        try {
            delete();
        }
        finally {
             super.finalize();
        }
    }
    private static native void do_delete(long me);
    /*package*/ Foo(InternalPointerMarker marker, long ptr) {
        assert marker == InternalPointerMarker.RAW_PTR;
        this.mNativeObj = ptr;
    }
    /*package*/ long mNativeObj;
}"#;

