r#"public final class BLAUtils {

    public static native @NonNull String latitude_to_str(@Nullable Double lat, @NonNull String plus_sym, @NonNull String minus_sym);

    public static native @NonNull String longitude_to_str(@Nullable Double lon, @NonNull String plus_sym, @NonNull String minus_sym);

    private BLAUtils() {}
}"#;
