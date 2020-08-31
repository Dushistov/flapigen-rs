//ANCHOR: foreign_typemap_chrono_example
foreign_typemap!(
    ($p:r_type) DateTime<Utc> => jlong {
        $out = $p.timestamp_millis();
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*chrono*/")
        => "/*chrono*/java.util.Date" "$out = new java.util.Date($p);";
    ($p:f_type, option = "NullAnnotations", unique_prefix = "/*chrono*/")
        => "/*chrono*/@NonNull java.util.Date" "$out = new java.util.Date($p);";
);
//ANCHOR_END: foreign_typemap_chrono_example

foreign_typemap!(
    ($p:r_type) Option<DateTime<Utc>> => internal_aliases::JOptionalLong {
        let tmp: Option<i64> = $p.map(|x| x.timestamp_millis());
        $out = to_java_util_optional_long(env, tmp);
    };
    ($p:f_type, unique_prefix = "/*chrono*/") => "/*chrono*/java.util.Optional<java.util.Date>"
        r#"
        $out;
        if ($p.isPresent()) {
            $out = java.util.Optional.of(new java.util.Date($p.getAsLong()));
        } else {
            $out = java.util.Optional.empty();
        }
"#;
);
