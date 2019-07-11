foreigner_class!(class BLAUtils {
    fn latitude_to_str(lat: Option<f64>, plus_sym: &str, minus_sym: &str)
                       -> String {
        format!(
            "{}",
            Latitude::new_with_symbols(lat, plus_sym.into(), minus_sym.into())
        )
    }
    fn longitude_to_str(lon: Option<f64>, plus_sym: &str, minus_sym: &str)
                        -> String {
        format!(
            "{}",
            Longitude::new_with_symbols(lon, plus_sym.into(), minus_sym.into())
        )
    }
});
