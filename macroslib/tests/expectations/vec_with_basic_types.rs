foreigner_class!(class PosErr {
    self_type PosErr;
    constructor PosErr::new() -> PosErr;
});

foreigner_class!(class LocationService {
    self_type LocationService;

    constructor LocationService::new() -> LocationService;
    method f1(&self) -> Result<Vec<u8>, PosErr>;
    fn f2(&self, p: Vec<u8>);
});

foreign_callback!(callback IXyz {
    self_type IXyz;
    on_have_data = IXyz::on_have_data(&mut self, data: Vec<u8>);
});
