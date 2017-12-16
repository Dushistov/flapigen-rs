#pragma once

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
#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include <stddef.h>

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
        o.data = nullptr;
    }
    RustVec &operator=(RustVec &&o)
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        o.data = nullptr;
        return *this;
    }
    ~RustVec()
    {
        if (data != nullptr) {
            rust_vec_bytes_free(std::move(*this));
        }
    }
    size_t size() const { return len; }
    const uint8_t &operator[](size_t i) const { return data[i]; }
};
#endif
