@@expect {"after":"\n\n    static","before":"friend class FooWrapper<false>;\n\n    ","file":"Foo.hpp","kind":"between"}
static QDate f() noexcept;
@@end

@@expect {"after":"\n\n    struct","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Foo.h","kind":"between"}
int64_t Foo_f();
@@end

@@expect {"after":"\n\n    templa","before":"static std::optional<QDate> f2() noexcept;\n\n};\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline QDate FooWrapper<OWN_DATA>::f() noexcept
    {

        int64_t ret = Foo_f();
        return QDateTime::fromMSecsSinceEpoch(ret, Qt::UTC, 0).date();
    }
@@end

@@expect {"after":"\n\n};\n\n\n    t","before":"static QDate f() noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static std::optional<QDate> f2() noexcept;
@@end

@@expect {"after":"\n\n#ifdef __c","before":"int64_t Foo_f();\n\n    ","file":"c_Foo.h","kind":"between"}
struct CRustOptioni64 Foo_f2();
@@end

@@expect {"after":"\n\n} // names","before":"return QDateTime::fromMSecsSinceEpoch(ret, Qt::UTC, 0).date();\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::optional<QDate> FooWrapper<OWN_DATA>::f2() noexcept
    {

        struct CRustOptioni64 ret = Foo_f2();
        return (ret.is_some != 0) ? std::optional<QDate>(QDateTime::fromMSecsSinceEpoch(ret.val.data, Qt::UTC, 0).date()) : std::optional<QDate>();
    }
@@end
