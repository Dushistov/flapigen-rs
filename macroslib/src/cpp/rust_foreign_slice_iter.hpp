#pragma once

#include <cstddef> //ptrdiff_t
#include <cassert>
#include <iterator>

template <typename T>
class RustForeignSliceIterator final
    : public std::iterator<std::random_access_iterator_tag, // iterator_category
                           T, // value_type
                           ptrdiff_t, // difference_type
                           const T *, // pointer
                           T // reference
                           > {
public:
    using CForeignType = typename T::CForeignType;

    RustForeignSliceIterator() noexcept
        : ptr(nullptr)
        , step(0)
    {
    }
    RustForeignSliceIterator(const void *p, size_t s) noexcept
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

    RustForeignSliceIterator &operator++() noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) + this->step;
        return *this;
    }
    /*@}*/

    /**
     * \defgroup Bidirectional iterator requirements
     */
    /*@{*/
    RustForeignSliceIterator &operator--() noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) - this->step;
        return *this;
    }
    /*@}*/
    /**
     * \defgroup Random access iterator requirements
     */
    /*@{*/
    T operator[](ptrdiff_t n) const noexcept
    {
        auto p = static_cast<const uint8_t *>(this->ptr) + n * this->step;
        auto elem_ptr = static_cast<const CForeignType *>(p);
        return T{ elem_ptr };
    }

    RustForeignSliceIterator &operator+=(ptrdiff_t n) noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) + n * this->step;
        return *this;
    }

    RustForeignSliceIterator operator+(ptrdiff_t n) const noexcept
    {
        const void *p = static_cast<const uint8_t *>(this->ptr) + n * this->step;
        return RustForeignSliceIterator(p, this->step);
    }

    RustForeignSliceIterator &operator-=(ptrdiff_t n) noexcept
    {
        this->ptr = static_cast<const uint8_t *>(this->ptr) - n * this->step;
        return *this;
    }

    RustForeignSliceIterator operator-(ptrdiff_t n) const noexcept
    {
        const void *p = static_cast<const uint8_t *>(this->ptr) - n * this->step;
        return RustForeignSliceIterator(p, this->step);
    }

    ptrdiff_t operator-(const RustForeignSliceIterator &o) const noexcept
    {
        assert(this->step == o.step);
        ptrdiff_t diff
            = static_cast<const uint8_t *>(this->ptr) - static_cast<const uint8_t *>(o.ptr);
        return diff / this->step;
    }

    bool operator<(const RustForeignSliceIterator &o) const noexcept
    {
        assert(this->step == o.step);
        return this->ptr < o.ptr;
    }

    bool operator>(const RustForeignSliceIterator &o) const noexcept
    {
        assert(this->step == o.step);
        return this->ptr > o.ptr;
    }

    bool operator<=(const RustForeignSliceIterator &o) const noexcept { return !operator>(o); }

    bool operator>=(const RustForeignSliceIterator &o) const noexcept { return !operator<(o); }
    /*@}*/

    bool operator==(const RustForeignSliceIterator<T> &o) const noexcept
    {
        assert(this->step == o.step);
        return this->ptr == o.ptr;
    }
    bool operator!=(const RustForeignSliceIterator<T> &o) const noexcept { return !operator==(o); }

private:
    const void *ptr;
    uintptr_t step;
};
