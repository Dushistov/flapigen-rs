foreign_typemap!(
    ($p:r_type) Date<Utc> => i64 {
        $out = $p.add_hms(0, 0, 0).timestamp_millis();
    };
    ($p:f_type) => "QDate"
        "QDateTime::fromMSecsSinceEpoch($p, Qt::UTC, 0).date()";
);

foreigner_class!(class Foo {
    static_method f() -> Date<Utc>;
    static_method f2() -> Option<Date<Utc>>;
});
