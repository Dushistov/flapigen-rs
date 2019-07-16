mod swig_foreign_types_map {}

foreign_typemap!(
    ($p:r_type) DateTime<Utc> => jlong {
        $out = $p.timestamp_millis()
    };
    ($p:f_type) => "/*chrono*/java.util.Date" "$out = new java.util.Date($p);";
);
