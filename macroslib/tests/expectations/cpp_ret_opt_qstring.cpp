r#"inline std::optional<QString> RelativePath::toString() const noexcept
    {

        struct CRustOptionCRustString ret = RelativePath_toString(this->self_);
        return (ret.is_some != 0) ? std::optional<QString>(
          [](auto v) -> QString {
             const auto qs =  QString::fromUtf8(v.data, static_cast<int>(v.len));
             crust_string_free(v);
             return qs;
          }(ret.val.data)
) : std::optional<QString>();
    }"#;

r#"inline std::optional</*&str*/QString> RelativePath::getOtherInfo() const noexcept
    {

        struct CRustOptionCRustStrView ret = RelativePath_getOtherInfo(this->self_);
        return (ret.is_some != 0) ? std::optional</*&str*/QString>(
          QString::fromUtf8(ret.val.data.data, static_cast<int>(ret.val.data.len))
) : std::optional</*&str*/QString>();
    }"#;
