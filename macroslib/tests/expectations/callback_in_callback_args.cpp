r#"class Foo {
public:
    virtual ~Foo() noexcept {}

    virtual void f() noexcept = 0;


    static C_Foo to_c_interface(std::unique_ptr<Foo> p)
    {
        assert(p != nullptr);
        C_Foo ret;
        ret.opaque = p.release();

        ret.C_Foo_deref = c_Foo_deref;
        ret.f = c_f;

        return ret;
    }
private:

    static void c_Foo_deref(void *opaque)
    {
        auto p = static_cast<Foo *>(opaque);
        delete p;
    }

    static void c_f(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<Foo *>(opaque);

        pi->f();
    }

};"#;

r#"class Boo {
public:
    virtual ~Boo() noexcept {}

    virtual void g(const C_Foo & x) noexcept = 0;

    virtual void h(C_Foo & x) noexcept = 0;


    static C_Boo to_c_interface(std::unique_ptr<Boo> p)
    {
        assert(p != nullptr);
        C_Boo ret;
        ret.opaque = p.release();

        ret.C_Boo_deref = c_Boo_deref;
        ret.g = c_g;
        ret.h = c_h;

        return ret;
    }
private:

    static void c_Boo_deref(void *opaque)
    {
        auto p = static_cast<Boo *>(opaque);
        delete p;
    }

    static void c_g(const struct C_Foo * const x, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<Boo *>(opaque);

        pi->g(*x);
    }

    static void c_h(C_Foo * x, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<Boo *>(opaque);

        pi->h(*x);
    }

};"#;

"static void static_member(C_Boo * x) noexcept;";
r#"inline void Class::static_member(C_Boo * x) noexcept
    {

        Class_static_member(x);
    }"#;
"void Class_static_member(C_Boo * x);";
