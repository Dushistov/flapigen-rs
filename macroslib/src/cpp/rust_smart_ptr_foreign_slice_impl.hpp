#pragma once

#include <cstdint>
#include <cstddef>

namespace RUST_SWIG_USER_NAMESPACE {
template <typename ForeignClassRef, typename CContainerType,
          void *(*IndexAccess)(CContainerType, uintptr_t)>
class RustSmartPtrForeignSlice final : public CContainerType {
public:
    using CForeignType = typename ForeignClassRef::CForeignType;

    RustSmartPtrForeignSlice() noexcept { reset(); }
    explicit RustSmartPtrForeignSlice(CContainerType o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
    }
    RustSmartPtrForeignSlice(const RustSmartPtrForeignSlice &) = delete;
    RustSmartPtrForeignSlice &operator=(const RustSmartPtrForeignSlice &) = delete;
    RustSmartPtrForeignSlice(RustSmartPtrForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        o.reset();
    }
    RustSmartPtrForeignSlice &operator=(RustSmartPtrForeignSlice &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        o.reset();
        return *this;
    }
    ~RustSmartPtrForeignSlice() noexcept {}
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
    ForeignClassRef operator[](size_t i) const noexcept
    {
        assert(i < this->len);
        auto p = IndexAccess(*this, i);
        auto elem_ptr = static_cast<const CForeignType *>(p);
        return ForeignClassRef{ elem_ptr };
    }

private:
    void reset() noexcept
    {
        this->data = nullptr;
        this->len = 0;
    }
};
} // namespace RUST_SWIG_USER_NAMESPACE
