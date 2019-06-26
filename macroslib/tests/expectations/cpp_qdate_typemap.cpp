"static QDate f() noexcept;";
"int64_t Foo_f();";

r#"template<bool OWN_DATA>
    inline QDate FooWrapper<OWN_DATA>::f() noexcept
    {

        int64_t ret = Foo_f();
        return QDateTime::fromMSecsSinceEpoch(ret, Qt::UTC, 0).date();
    }"#;

"static std::optional<QDate> f2() noexcept;";
"struct CRustOptioni64 Foo_f2();";
r#"template<bool OWN_DATA>
    inline std::optional<QDate> FooWrapper<OWN_DATA>::f2() noexcept
    {

        struct CRustOptioni64 ret = Foo_f2();
        return (ret.is_some != 0) ? std::optional<QDate>(QDateTime::fromMSecsSinceEpoch(ret.val.data, Qt::UTC, 0).date()) : std::optional<QDate>();
    }"#;
