
// With self_type, C header must contain 'typedef WithSelfTypeOpaque'.
foreigner_class!(
    class WithSelfType {
        self_type WithSelfType;
        constructor WithSelfType::new() -> WithSelfType;
        fn WithSelfType::do_something(&self);
    }
);

// With no self_type, C header should not contain 'typedef WithoutSelfTypeOpaque'.
foreign_class!(
    class WithoutSelfType {
        fn WithoutSelfType::do_nothing();
    }
);

