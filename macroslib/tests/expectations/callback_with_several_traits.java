r#"public final class Test {

    public static native void f(@NonNull MyObserver a0);

    private Test() {}
}"#;

r#"public interface MyObserver {


    void onStateChanged(int x, @NonNull String s);

}"#;
