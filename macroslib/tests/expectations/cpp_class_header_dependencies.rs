foreign_class!(class Y {
    self_type Y;
    constructor Y::default() -> Y;
});

foreign_class!(class XDirect {
    self_type XDirect;
    constructor XDirect::default() -> XDirect;
    fn XDirect::take_y(y: Y);
    fn XDirect::get_y(&self) -> Y;
});

foreign_class!(class XOptional {
    self_type XOptional;
    constructor XOptional::default() -> XOptional;
    fn XOptional::take_y(y: Option<Y>);
    fn XOptional::get_y(&self) -> Option<Y>;
});

foreign_class!(class XVec {
    self_type XVec;
    constructor XVec::default() -> XVec;
    fn XVec::take_y(y: Vec<Y>);
    fn XVec::get_y(&self) -> Vec<Y>;
});

foreign_class!(class XSlice {
    self_type XSlice;
    constructor XSlice::default() -> XSlice;
    fn XSlice::take_y(y: &[Y]);
    fn XSlice::get_y(&self) -> &[Y];
});
