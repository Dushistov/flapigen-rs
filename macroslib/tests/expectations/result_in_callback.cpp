r##"class Foo {
public:
    virtual ~Foo() noexcept {}

    virtual std::variant<std::string_view, Error> unpack(std::string_view x) const noexcept = 0;

    virtual std::optional<Error> remove() const noexcept = 0;"##;

r#"union CRustVoidOkResultUnion4232mut3232c_void {
    uint8_t ok;
    void * err;
};"#;
