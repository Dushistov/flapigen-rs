r#"public enum ControlItem {
GNSS(0),
GPS_PROVIDER(1);"#;

r#"public interface ControlStateObserver {


    void onSessionUpdate(ControlItem a0, boolean a1);

}"#;
