#pragma once

#include <stdint.h>

#ifdef __cplusplus
#include "rust_foreign_slice_iter.hpp"

extern "C" {
#endif
struct CRustSliceU8 {
    const uint8_t *data;
    uintptr_t len;
};

struct CRustSliceI32 {
    const int32_t *data;
    uintptr_t len;
};

struct CRustSliceU32 {
    const uint32_t *data;
    uintptr_t len;
};

struct CRustSliceUsize {
    const uintptr_t *data;
    uintptr_t len;
};

struct CRustObjectSlice {
    const void *data;
    uintptr_t len;
    uintptr_t step;
};

#ifdef __cplusplus
} // extern "C"

namespace RUST_SWIG_USER_NAMESPACE {

namespace internal {
    template <typename T, typename E> E field_type(E T::*);
}

template <typename CContainerType> class RustSlice final : private CContainerType {
public:
    using value_type = typename std::remove_const<typename std::remove_reference<decltype(
        *internal::field_type(&CContainerType::data))>::type>::type;
    using iterator = value_type *;
    using const_iterator = const value_type *;
    explicit RustSlice(const CContainerType &o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
    }
    RustSlice(const RustSlice &) = delete;
    RustSlice &operator=(const RustSlice &) = delete;
    RustSlice(RustSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;

        reset(o);
    }
    RustSlice &operator=(RustSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;

        reset(o);
        return *this;
    }
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
    const value_type &operator[](size_t i) const noexcept { return this->data[i]; }
    iterator begin() noexcept { return this->data; }
    const_iterator begin() const noexcept { return this->data; }
    iterator end() noexcept { return this->data + this->len; }
    const_iterator end() const noexcept { return this->data + this->len; }

private:
    static void reset(RustSlice &o) noexcept
    {
        o.data = nullptr;
        o.len = 0;
    }
};

template <class ForeignClassRef> class RustForeignSlice final : public CRustObjectSlice {
public:
    using const_reference = ForeignClassRef;
    using CForeignType = typename ForeignClassRef::CForeignType;
    using value_type = typename ForeignClassRef::value_type;
    using iterator = RustForeignSliceIterator<ForeignClassRef>;
    using const_iterator = RustForeignSliceIterator<ForeignClassRef>;

    RustForeignSlice() noexcept { reset(); }
    explicit RustForeignSlice(const CRustObjectSlice &o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->step = o.step;
    }
    RustForeignSlice(const RustForeignSlice &) = delete;
    RustForeignSlice &operator=(const RustForeignSlice &) = delete;
    RustForeignSlice(RustForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->step = o.step;
        o.reset();
    }
    RustForeignSlice &operator=(RustForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->step = o.step;
        o.reset();
        return *this;
    }
    ~RustForeignSlice() noexcept {}
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
    ForeignClassRef operator[](size_t i) const noexcept
    {
        assert(i < this->len);
        auto p = static_cast<const uint8_t *>(this->data);
        p += this->step * i;
        auto elem_ptr = static_cast<const CForeignType *>(static_cast<const void *>(p));
        return ForeignClassRef{ elem_ptr };
    }
    iterator begin() noexcept { return iterator{ this->data, this->step }; }

    const_iterator begin() const noexcept { return const_iterator{ this->data, this->step }; }

    iterator end() noexcept
    {
        auto p = static_cast<const uint8_t *>(this->data);
        p += this->step * this->len;
        return iterator{ p, this->step };
    }

    const_iterator end() const noexcept
    {
        auto p = static_cast<const uint8_t *>(this->data);
        p += this->step * this->len;
        return const_iterator{ p, this->step };
    }

private:
    void reset()
    {
        this->step = 0;
        this->len = 0;
        this->data = nullptr;
    }
};

} // namespace RUST_SWIG_USER_NAMESPACE

#endif // __cplusplus
