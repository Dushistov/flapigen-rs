#pragma once

#include "rust_foreign_slice_iter.hpp"

namespace RUST_SWIG_USER_NAMESPACE {
template <class ForeignClassRef, typename CContainerType>
class RustForeignSlice final : public CContainerType {
public:
    using const_reference = ForeignClassRef;
    using CForeignType = typename ForeignClassRef::CForeignType;
    using value_type = typename ForeignClassRef::value_type;
    using iterator = RustForeignSliceIterator<ForeignClassRef>;
    using const_iterator = RustForeignSliceIterator<ForeignClassRef>;

    RustForeignSlice() noexcept { reset(); }
    explicit RustForeignSlice(const CContainerType &o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
    }
    RustForeignSlice(const RustForeignSlice &) = delete;
    RustForeignSlice &operator=(const RustForeignSlice &) = delete;
    RustForeignSlice(RustForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        o.reset();
    }
    RustForeignSlice &operator=(RustForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
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
        p += ForeignClassRef::rust_elem_size * i;
        auto elem_ptr = static_cast<const CForeignType *>(static_cast<const void *>(p));
        return ForeignClassRef{ elem_ptr };
    }
    iterator begin() noexcept { return iterator{ this->data, ForeignClassRef::rust_elem_size }; }

    const_iterator begin() const noexcept { return const_iterator{ this->data, ForeignClassRef::rust_elem_size }; }

    iterator end() noexcept
    {
        auto p = static_cast<const uint8_t *>(this->data);
        p += ForeignClassRef::rust_elem_size * this->len;
        return iterator{ p, ForeignClassRef::rust_elem_size };
    }

    const_iterator end() const noexcept
    {
        auto p = static_cast<const uint8_t *>(this->data);
        p += ForeignClassRef::rust_elem_size * this->len;
        return const_iterator{ p, ForeignClassRef::rust_elem_size };
    }

private:
    void reset() noexcept
    {
        this->len = 0;
        this->data = nullptr;
    }
};

} // namespace RUST_SWIG_USER_NAMESPACE
