foreigner_class!(class Boo {
    self_type Boo;
    constructor create_boo() -> Rc<RefCell<Boo>>;
    method Boo::test(&self, _: bool) -> f32;
    method Boo::set_a(&mut self, _: i32);
});
foreigner_class!(class Moo {
    self_type Moo;
    constructor Moo::empty() -> Moo;
    method Moo::get_boo(&self) -> Rc<RefCell<Boo>>; alias getBoo;
});
