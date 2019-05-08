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

struct CRustVecI32 {
    const int32_t *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecI32_free(struct CRustVecI32 vec);

struct CRustVecU32 {
    const uint32_t *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecU32_free(struct CRustVecU32 vec);

struct CRustVecUsize {
    const uintptr_t *data;
    uintptr_t len;
    uintptr_t capacity;
};

void CRustVecUsize_free(struct CRustVecUsize vec);

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

struct CRustForeignVec {
    const void *data;
    uintptr_t len;
    uintptr_t capacity;
    uintptr_t step;
};

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
}
#endif

#ifdef __cplusplus

#include <cassert>
#include <type_traits>
#include <iterator>

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

template <typename CContainerType, void (*FreeFunc)(CContainerType)>
class RustVec final : private CContainerType {
public:
    using value_type = typename std::remove_const<typename std::remove_reference<decltype(
        *internal::field_type(&CContainerType::data))>::type>::type;
    using iterator = value_type *;
    using const_iterator = const value_type *;

    explicit RustVec(const CContainerType &o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;
    }
    RustVec() noexcept { reset(*this); }
    RustVec(const RustVec &) = delete;
    RustVec &operator=(const RustVec &) = delete;
    RustVec(RustVec &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;

        reset(o);
    }
    RustVec &operator=(RustVec &&o) noexcept
    {
        free_mem();
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustVec() noexcept { free_mem(); }
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
    const value_type &operator[](size_t i) const noexcept { return this->data[i]; }
    iterator begin() noexcept { return this->data; }
    const_iterator begin() const noexcept { return this->data; }
    iterator end() noexcept { return this->data + this->len; }
    const_iterator end() const noexcept { return this->data + this->len; }
    void clear() noexcept { free_mem(); }
    CContainerType release() noexcept
    {
        CContainerType ret{ this->data, this->len, this->capacity, this->step };
        reset(*this);
        return ret;
    }

private:
    void free_mem() noexcept
    {
        if (this->data != nullptr) {
            FreeFunc(*this);
            reset(*this);
        }
    }
    static void reset(RustVec &o) noexcept
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};

using RustVecU8 = RustVec<CRustVecU8, CRustVecU8_free>;
using RustVecI32 = RustVec<CRustVecI32, CRustVecI32_free>;
using RustVecU32 = RustVec<CRustVecU32, CRustVecU32_free>;
using RustVecUsize = RustVec<CRustVecUsize, CRustVecUsize_free>;
using RustVecF32 = RustVec<CRustVecF32, CRustVecF32_free>;
using RustVecF64 = RustVec<CRustVecF64, CRustVecF64_free>;

template <typename T>
class RustForeignVecIterator final
    : public std::iterator<std::bidirectional_iterator_tag, // iterator_category
                           T, // value_type
                           ptrdiff_t, // difference_type
                           const T *, // pointer
                           T // reference
                           > {
public:
    using CForeignType = typename T::CForeignType;

    RustForeignVecIterator() noexcept
        : ptr(nullptr)
        , step(0)
    {
    }
    RustForeignVecIterator(const void *p, size_t s) noexcept
        : ptr(p)
        , step(s)
    {
    }

    /**
     * \defgroup Forward iterator requirements
     */
    /*@{*/
    T operator*() const noexcept
    {
        auto elem_ptr = static_cast<const CForeignType *>(this->ptr);
        return T{ elem_ptr };
    }

    RustForeignVecIterator &operator++() noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) + this->step;
        return *this;
    }
    /*@}*/

    /**
     * \defgroup Bidirectional iterator requirements
     */
    /*@{*/
    RustForeignVecIterator &operator--() noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) - this->step;
        return *this;
    }

    bool operator==(const RustForeignVecIterator<T> &o) const noexcept
    {
        assert(this->step == o.step);
        return this->ptr == o.ptr;
    }
    bool operator!=(const RustForeignVecIterator<T> &o) const noexcept { return !operator==(o); }

private:
    const void *ptr;
    uintptr_t step;
};

template <class ForeignClassRef> class RustForeignSlice final : public CRustObjectSlice {
public:
    using const_reference = ForeignClassRef;
    using CForeignType = typename ForeignClassRef::CForeignType;
    using value_type = typename ForeignClassRef::value_type;
    using iterator = RustForeignVecIterator<ForeignClassRef>;
    using const_iterator = RustForeignVecIterator<ForeignClassRef>;

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

template <class ForeignClassRef, typename CContainerType, void (*FreeFunc)(CContainerType),
          void (*PushFunc)(CContainerType *, void *),
          void *(*RemoveFunc)(CContainerType *, uintptr_t)>
class RustForeignVec final : private CContainerType {
public:
    using const_reference = ForeignClassRef;
    using CForeignType = typename ForeignClassRef::CForeignType;
    using value_type = typename ForeignClassRef::value_type;
    using iterator = RustForeignVecIterator<ForeignClassRef>;
    using const_iterator = RustForeignVecIterator<ForeignClassRef>;

    RustForeignVec() noexcept
    {
        this->step = 0;
        reset(*this);
    }
    explicit RustForeignVec(const CContainerType &o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;
        this->step = o.step;
    }
    RustForeignVec(const RustForeignVec &) = delete;
    RustForeignVec &operator=(const RustForeignVec &) = delete;
    RustForeignVec(RustForeignVec &&o) noexcept
    {
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;
        this->step = o.step;
        reset(o);
    }
    RustForeignVec &operator=(RustForeignVec &&o) noexcept
    {
        free_mem();
        this->data = o.data;
        this->len = o.len;
        this->capacity = o.capacity;
        assert(this->step == o.step || this->step == 0 || o.step == 0);
        this->step = o.step;
        reset(o);
        return *this;
    }
    ~RustForeignVec() noexcept { free_mem(); }

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

    void push(value_type o) noexcept { PushFunc(this, o.release()); }

    value_type remove(size_t idx) noexcept
    {
        auto p = static_cast<CForeignType *>(RemoveFunc(this, idx));
        return value_type{ p };
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

    RustForeignSlice<ForeignClassRef> as_slice()
    {
        return RustForeignSlice<ForeignClassRef>{ CRustObjectSlice{ this->data, this->len,
                                                                    this->step } };
    }

    void clear() noexcept { free_mem(); }

    CContainerType release() noexcept
    {
        CContainerType ret{ this->data, this->len, this->capacity, this->step };
        reset(*this);
        return ret;
    }

private:
    void free_mem() noexcept
    {
        if (this->data != nullptr) {
            FreeFunc(*this);
            reset(*this);
        }
    }
    static void reset(RustForeignVec &o) noexcept
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};
} // namespace RUST_SWIG_USER_NAMESPACE
#endif
