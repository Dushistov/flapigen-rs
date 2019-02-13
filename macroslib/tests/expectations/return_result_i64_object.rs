foreigner_class!(class PosErr {
    self_type PosErr;
    constructor PosErr::new() -> PosErr;
});

foreigner_class!(class LocationService {
    self_type LocationService;

    constructor LocationService::new() -> LocationService;
    method f1(&self) -> Result<i64, PosErr>;
});
