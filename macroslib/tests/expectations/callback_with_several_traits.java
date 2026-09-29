@@expect {"before":"import android.support.annotation.NonNull;\n\n","file":"Test.java","kind":"between"}
public final class Test {

    public static native void f(@NonNull MyObserver a0);

    private Test() {}
}
@@end

@@expect {"after":"\n","before":"import android.support.annotation.NonNull;\n\n","file":"MyObserver.java","greedy_match":true,"kind":"between"}
public interface MyObserver {


    void onStateChanged(int x, @NonNull String s);

}
@@end
