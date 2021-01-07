r#"package org.example;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

public final class ReturnNullableValue {

    public static @NonNull java.util.Optional<String> getOpt() {
        String ret = do_getOpt();
        java.util.Optional<String> convRet = java.util.Optional.ofNullable(ret);

        return convRet;
    }
    private static native @Nullable String do_getOpt();

    private ReturnNullableValue() {}
}"#;
