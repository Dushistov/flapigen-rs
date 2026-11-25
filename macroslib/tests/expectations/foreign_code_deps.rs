foreign_class!(class A {
    self_type A;
    constructor A::default() -> A;
    protected fn A::do_f(&self, a: i32);
    foreign_code req_modules = ["<cstdio>", "<cassert>"] r#"
public:
    void f(int32_t a) noexcept {
        assert(a > 10);
        printf("you call f\n");
        do_f(a);
    }
    "#;
});
