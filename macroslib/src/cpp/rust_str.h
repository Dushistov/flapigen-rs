#pragma once

#include <stdint.h>

#ifdef __cplusplus
#include <string>

extern "C" {
#endif

struct RustStrView {
    const char *const data;
    uint32_t len;
#ifdef __cplusplus
    std::string to_std_string() const
    {
        return std::string(data, len);
    }
#endif
};

struct CRustString {
    const char *data;
    uint32_t len;
    uint32_t capacity;
};

void crust_string_free(struct CRustString str);
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

static_assert(sizeof(uint8_t) == sizeof(char), "for simplicity assume so");

namespace {namespace_name} {
class RustString final : public CRustString {
public:
    explicit RustString(const CRustString &o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;
    }
    RustString() = delete;
    RustString(const RustString &) = delete;
    RustString &operator=(const RustString &) = delete;
    RustString(RustString &&o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
    }
    RustString &operator=(RustString &&o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustString()
    {
        if (data != nullptr) {
            crust_string_free(*this);
            reset(*this);
        }
    }
    std::string to_std_string() const
    {
        return std::string(data, len);
    }

#if __cplusplus > 201402L
    std::string_view to_string_view() const
    {
        return std::string_view(data, len);
    }
#endif
private:
    static void reset(RustString &o)
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};

} // namespace {namespace_name}
#endif //__cplusplus
