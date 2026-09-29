@@expect {"after":"\n\n    static","before":"friend class BLAUtilsWrapper<false>;\n\n    ","file":"BLAUtils.hpp","kind":"between"}
static RustString latitude_to_str(std::optional<double> lat, std::string_view plus_sym, std::string_view minus_sym) noexcept;
@@end

@@expect {"after":"\n\n};\n\n\n    t","before":"static RustString latitude_to_str(std::optional<double> lat, std::string_view plus_sym, std::string_view minus_sym) noexcept;\n\n    ","file":"BLAUtils.hpp","kind":"between"}
static RustString longitude_to_str(std::optional<double> lon, std::string_view plus_sym, std::string_view minus_sym) noexcept;
@@end
