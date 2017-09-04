mod swig_foreign_types_map {}

#[swig_to_foreigner_hint = "java.util.Date"]
impl SwigFrom<DateTime<Utc>> for jobject {
    fn swig_from(x: DateTime<Utc>, env: *mut JNIEnv) -> Self {
        let unix_secs = x.timestamp();
        let mills = x.timestamp_subsec_millis();
        let mills = (unix_secs * 1_000 + mills as i64) as jlong;
        let date_class: jclass =
            unsafe { (**env).FindClass.unwrap()(env, swig_c_str!("java/util/Date")) };
        assert!(
            !date_class.is_null(),
            "FindClass for `java/util/Date` failed"
        );
        let init: jmethodID = unsafe {
            (**env).GetMethodID.unwrap()(
                env,
                date_class,
                swig_c_str!("<init>"),
                swig_c_str!("(J)V"),
            )
        };
        assert!(
            !init.is_null(),
            "java/util/Date GetMethodID for init failed"
        );
        let x = unsafe { (**env).NewObject.unwrap()(env, date_class, init, mills) };
        assert!(!x.is_null());
        x
    }
}
