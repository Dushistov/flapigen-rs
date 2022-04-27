r#"class Foo {
public:
    virtual ~Foo() noexcept {}

    virtual void f() const noexcept = 0;


    static C_Foo to_c_interface(std::unique_ptr<Foo> p) noexcept
    {
        assert(p != nullptr);
        C_Foo ret;
        ret.opaque = p.release();

        ret.C_Foo_deref = c_Foo_deref;
        ret.f = c_f;

        return ret;
    }
    static C_Foo reference_to_c_interface(Foo &cpp_interface) noexcept
    {
        C_Foo ret;
        ret.opaque = &cpp_interface;
        ret.f = c_f;

        ret.C_Foo_deref = [](void *) {};
        return ret;
    }
protected:

    static void c_Foo_deref(void *opaque)
    {
        auto p = static_cast<Foo *>(opaque);
        delete p;
    }

    static void c_f(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const Foo *>(opaque);

        pi->f();
    }

};"#;

r#"class Boo {
public:
    virtual ~Boo() noexcept {}

    virtual void g(const C_Foo & x) const noexcept = 0;

    virtual void h(C_Foo & x) const noexcept = 0;


    static C_Boo to_c_interface(std::unique_ptr<Boo> p) noexcept
    {
        assert(p != nullptr);
        C_Boo ret;
        ret.opaque = p.release();

        ret.C_Boo_deref = c_Boo_deref;
        ret.g = c_g;
        ret.h = c_h;

        return ret;
    }
    static C_Boo reference_to_c_interface(Boo &cpp_interface) noexcept
    {
        C_Boo ret;
        ret.opaque = &cpp_interface;
        ret.g = c_g;
        ret.h = c_h;

        ret.C_Boo_deref = [](void *) {};
        return ret;
    }
protected:

    static void c_Boo_deref(void *opaque)
    {
        auto p = static_cast<Boo *>(opaque);
        delete p;
    }

    static void c_g(const struct C_Foo * const x, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const Boo *>(opaque);

        pi->g(*x);
    }

    static void c_h(C_Foo * x, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const Boo *>(opaque);

        pi->h(*x);
    }

};"#;

"static void static_member(C_Boo * x) noexcept;";
r#"inline void Class::static_member(C_Boo * x) noexcept
    {

        Class_static_member(x);
    }"#;
"void Class_static_member(C_Boo * x);";
