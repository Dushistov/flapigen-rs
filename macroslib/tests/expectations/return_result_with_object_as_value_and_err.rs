foreigner_class!(class Position {
    self_type GnssInfo;
    private constructor create_position() -> GnssInfo;
    method Position::getLatitude(&self) -> f64;
});

foreigner_class!(class PosErr {
    self_type PosErr;
    constructor PosErr::new() -> PosErr;
});

foreigner_class!(class LocationService {
    self_type LocationService;

    constructor LocationService::new() -> LocationService;
    static_method LocationService::f1() -> Result<GnssInfo, String>;
    static_method LocationService::f2() -> Result<(), String>;

    method LocationService::f3(&self) -> Result<GnssInfo, PosErr>;
    static_method LocationService::f4() -> Result<(), PosErr>;
    static_method LocationService::f5() -> Result<Vec<GnssInfo>, PosErr>;
    static_method LocationService::create() -> Result<LocationService, String>;
});

foreigner_class!(class Foo {
    self_type Foo<'a>;
    constructor Foo::create() -> Foo<'a>;
    static_method Foo::from_string<'a>(_: &str) -> Result<Foo<'a>, String>;
});
