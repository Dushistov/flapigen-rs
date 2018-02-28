#pragma once

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus

extern "C" {
#endif

struct CRustVecU8 {
    const uint8_t *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecU8_free(struct CRustVecU8 vec);

struct CRustVecU32 {
    const uint32_t *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecU32_free(struct CRustVecU32 vec);

struct CRustVecF32 {
    const float *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecF32_free(struct CRustVecF32 vec);

struct CRustVecF64 {
    const double *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecF64_free(struct CRustVecF64 vec);

struct CRustSliceU32 {
    const uint32_t *data;
    uintptr_t len;
};

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

#include <type_traits>

namespace RUST_SWIG_USER_NAMESPACE {

namespace internal {
    template <typename T, typename E>
    E field_type(E T::*);
}

template <typename CContainerType, void (*FreeFunc)(CContainerType)>
class RustVec final : public CContainerType {
public:
    using value_type = typename std::remove_const<
        typename std::remove_reference<decltype(*internal::field_type(&CContainerType::data))>::type>::type;

    explicit RustVec(const CContainerType &o)
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;
    }
    RustVec() = delete;
    RustVec(const RustVec &) = delete;
    RustVec &operator=(const RustVec &) = delete;
    RustVec(RustVec &&o)
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;

        reset(o);
    }
    RustVec &operator=(RustVec &&o)
    {
        free_mem();
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustVec()
    {
        free_mem();
    }
    size_t size() const { return this->len; }
    const value_type &operator[](size_t i) const { return this->data[i]; }

private:
    void free_mem()
    {
        if (this->data != nullptr) {
            FreeFunc(*this);
            reset(*this);
        }
    }
    static void reset(RustVec &o)
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};

using RustVecU8 = RustVec<CRustVecU8, CRustVecU8_free>;
using RustVecU32 = RustVec<CRustVecU32, CRustVecU32_free>;
using RustVecF32 = RustVec<CRustVecF32, CRustVecF32_free>;
using RustVecF64 = RustVec<CRustVecF64, CRustVecF64_free>;
} // namespace RUST_SWIG_USER_NAMESPACE
#endif
