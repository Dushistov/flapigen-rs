#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus

extern "C" {
#endif

struct RustVecBytes {
    const uint8_t *data;
    uint32_t len;
    uint32_t capacity;
};

void rust_vec_bytes_free(struct RustVecBytes vec);

struct CRustSliceU32 {
    const uint32_t *data;
    uintptr_t len;
};

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include <stddef.h>

namespace {namespace_name} {
class RustVec final : public RustVecBytes {
public:
    RustVec(const RustVecBytes &o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;
    }
    RustVec() = delete;
    RustVec(const RustVec &) = delete;
    RustVec &operator=(const RustVec &) = delete;
    RustVec(RustVec &&o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
    }
    RustVec &operator=(RustVec &&o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustVec()
    {
        if (data != nullptr) {
            rust_vec_bytes_free(*this);
            reset(*this);
        }
    }
    size_t size() const { return len; }
    const uint8_t &operator[](size_t i) const { return data[i]; }

private:
    static void reset(RustVec &o)
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};
} // namespace {namespace_name}
#endif
