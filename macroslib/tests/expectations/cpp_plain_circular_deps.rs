foreign_class!(
    #[derive(PlainClass)]
    class PlainA {
        self_type PlainA;
        constructor PlainA::default() -> PlainA;
        fn PlainA::take_b(b: &PlainB);
    }
);

foreign_class!(
    #[derive(PlainClass)]
    class PlainB {
        self_type PlainB;
        constructor PlainB::default() -> PlainB;
        fn PlainB::take_a(a: &PlainA);
    }
);
