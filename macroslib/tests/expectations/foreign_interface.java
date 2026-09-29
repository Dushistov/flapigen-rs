@@expect {"after":"\n","before":"import android.support.annotation.NonNull;\n\n","file":"SomeObserver.java","greedy_match":true,"kind":"between"}
public interface SomeObserver {


    void onStateChanged(int a0, boolean a1);


    void onStateChangedWithoutArgs();


    void onStateChangedFoo(@NonNull Foo foo);


    float getTextSize();

}
@@end
