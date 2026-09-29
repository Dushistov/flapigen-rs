@@expect {"after":"\n\n    privat","before":"package org.example;\n\n\n","file":"ControlItem.java","kind":"between"}
public enum ControlItem {
    GNSS(0),
    GPS_PROVIDER(1);
@@end

@@expect {"after":"\n","before":"import android.support.annotation.NonNull;\n\n","file":"ControlStateObserver.java","greedy_match":true,"kind":"between"}
public interface ControlStateObserver {


    void onSessionUpdate(@NonNull ControlItem item, boolean is_ok);

}
@@end
