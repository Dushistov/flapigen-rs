foreign_enum!(
    /// Multi-line comment
    /// for `enum SomeEnum`.
    enum SomeEnum {
        /// `Val1` has a one-line comment.
        Val1 = SomeEnum::Val1,

        Val2 = SomeEnum::Val2,

        /// `Val3` has a one-line comment.
        /// `Val2` had no comment at all.
        Val3 = SomeEnum::Val3,
    }
);

foreign_enum!(
    enum OtherEnum {
        Val4,
        Val5,
    }
);

foreign_class!(class Boo {
    fn Boo::return_is_enum() -> SomeEnum;
    fn Boo::param_is_enum(e: SomeEnum);
    fn Boo::param_and_return_are_enum(e: SomeEnum) -> SomeEnum;
});
