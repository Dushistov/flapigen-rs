foreign_typemap!(
    ($p:r_type) DateTime<Utc> => jlong {
        $out = $p.timestamp_millis()
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*chrono*/") => "/*chrono*/java.util.Date" "$out = new java.util.Date($p);";
    ($p:f_type, option = "NullAnnotations", unique_prefix = "/*chrono*/") => "/*chrono*/@NonNull java.util.Date" "$out = new java.util.Date($p);";
);

foreigner_class!(
    class TrackInfo {
        self_type TrackInfo;
        private constructor = empty;
        fn TrackInfo::start_time(&self) -> DateTime<Utc>;
        fn end_time(&self) -> Option<i64>;
    }
);

foreigner_class!(class Boo {
    self_type Boo;
    constructor Boo::default() -> Boo;
    fn latDirection(&self) -> &str;
});
