r##"package org.example;
import android.support.annotation.NonNull;

public final class TrackInfo {

    private TrackInfo() {}

    public final @NonNull java.util.Date start_time() {
        long ret = do_start_time(mNativeObj);
        java.util.Date convRet = new java.util.Date(ret);

        return convRet;
    }
    private static native long do_start_time(long self);

    public final @NonNull java.util.OptionalLong end_time() {
        return do_end_time(mNativeObj);
    }
    private static native @NonNull java.util.OptionalLong do_end_time(long self);"##;

r#"package org.example;
import android.support.annotation.NonNull;

public final class Boo {

    public Boo() {
        mNativeObj = init();
    }
    private static native long init();

    public final @NonNull String latDirection() {
        return do_latDirection(mNativeObj);
    }
    private static native @NonNull String do_latDirection(long self);"#;
