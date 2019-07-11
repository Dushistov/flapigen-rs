foreign_typemap!(
    define_c_type!(
        module = "LatLon.h";
         #[repr(C)]
         pub struct CLatLon {
             lat: f64,
             lon: f64,
         }
    );
    ($p:r_type) LatLon => CLatLon {
        $out = CLatLon { lat: $p.0, lon: $p.1 }
    };
    ($p:r_type) LatLon <= CLatLon {
        $out = ($p.lat, $p.lon)
    };
);

foreigner_class!(class WindVelocity {
    self_type WindVelocity;
    private constructor = empty;
});

foreigner_class!(class RemoteApiError {
    self_type RemoteApiError;
    constructor RemoteApiError::default() -> RemoteApiError;
});

foreigner_class!(class Weather {
    static_method get_wind_for(pos: LatLon) -> Result<Vec<WindVelocity>, RemoteApiError>;
});
