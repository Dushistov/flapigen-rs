r#"//This is class Foo
template<bool OWN_DATA>
class FooWrapper {"#;

r#"//Some documentation comment
    FooWrapper(int32_t a_0, std::string_view a_1) noexcept"#;

r#"//1 Some documentation comment
    //2 Some documentation comment
    int32_t f(int32_t a_0, int32_t a_1) const  noexcept;"#;
