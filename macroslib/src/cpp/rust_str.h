#pragma once

#include <stdint.h>

#ifdef __cplusplus
#include <string>

extern "C" {
#endif

struct RustStrView {
    const char *const data;
    uintptr_t len;
#ifdef __cplusplus
    size_t size() const noexcept
    {
        return this->len;
    }
    bool empty() const noexcept
    {
        return this->len == 0;
    }
    std::string to_std_string() const
    {
        return std::string{ data, len };
    }
#endif
#if __cplusplus > 201402L
    std::string_view to_string_view() const
    {
        return std::string_view{ data, len };
    }
#endif
#ifdef BOOST_STRING_VIEW_HPP
    boost::string_view to_boost_string_view() const
    {
        return boost::string_view{ data, len };
    }
#endif
#if QT_VERSION >= 0x050000 && defined(QSTRING_H)
    QString to_qstring() const
    {
        return QString::fromUtf8(data, len);
    }
#endif
};

struct CRustString {
    const char *data;
    uintptr_t len;
    uintptr_t capacity;
};

void crust_string_free(struct CRustString str);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

static_assert(sizeof(uint8_t) == sizeof(char), "for simplicity assume so");

namespace RUST_SWIG_USER_NAMESPACE {
class RustString final : private CRustString {
public:
    explicit RustString(const CRustString &o) noexcept
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;
    }
    RustString() = delete;
    RustString(const RustString &) = delete;
    RustString &operator=(const RustString &) = delete;
    RustString(RustString &&o) noexcept
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
    }
    RustString &operator=(RustString &&o) noexcept
    {
        free_mem();
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustString() noexcept
    {
        free_mem();
    }
    std::string to_std_string() const
    {
        return std::string(data, len);
    }
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
#if __cplusplus > 201402L
    std::string_view to_string_view() const
    {
        return std::string_view(data, len);
    }
#endif

#ifdef BOOST_STRING_VIEW_HPP
    boost::string_view to_boost_string_view() const
    {
        return boost::string_view{ data, len };
    }
#endif
#if QT_VERSION >= 0x050000 && defined(QSTRING_H)
    QString to_qstring() const
    {
        return QString::fromUtf8(data, len);
    }
#endif
private:
    void free_mem() noexcept
    {
        if (data != nullptr) {
            crust_string_free(*this);
            reset(*this);
        }
    }
    static void reset(RustString &o) noexcept
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};

} // namespace RUST_SWIG_USER_NAMESPACE
#endif //__cplusplus
