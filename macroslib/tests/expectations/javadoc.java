@@expect {"after":" {\n        m","before":"import android.support.annotation.NonNull;\n","file":"Foo.java","kind":"between"}
/**
 * Class comment description for Foo.
 */
public final class Foo {
    /**
     * some text about the new function
     * 
     * ```
     * some markdown example in the text
     * ```
     * 
     * @param id - some number
     * @param desc - more information
     */
    public Foo(int id, @NonNull String desc)
@@end
