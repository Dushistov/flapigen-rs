#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <stdexcept>
#include <type_traits>
#include <utility>

#include "rust_slice_tmpl.hpp"

namespace RUST_SWIG_USER_NAMESPACE {
namespace internal {

    template <typename T, typename E> E vec_field_type(E T::*);

    template <typename Descriptor, void (*Free)(Descriptor)>
    struct NativeVecPolicy {
        using value_type = typename std::remove_const<typename std::remove_reference<
            decltype(*vec_field_type(&Descriptor::data))>::type>::type;
        using reference = const value_type &;
        using iterator = value_type *;
        using const_iterator = const value_type *;

        static Descriptor empty() noexcept
        {
            return Descriptor{ reinterpret_cast<value_type *>(alignof(value_type)), 0, 0 };
        }
        static void free(Descriptor vec) noexcept { Free(vec); }
        static reference index(Descriptor vec, size_t i) noexcept { return vec.data[i]; }
        static iterator begin(Descriptor vec) noexcept { return vec.data; }
        static const_iterator cbegin(Descriptor vec) noexcept { return vec.data; }
        static iterator end(Descriptor vec) noexcept
        {
            return vec.len == 0 ? vec.data : vec.data + vec.len;
        }
        static const_iterator cend(Descriptor vec) noexcept { return end(vec); }
    };

    template <typename ForeignClassRef, typename Descriptor,
              Descriptor (*New)(), void (*Free)(Descriptor),
              void (*Push)(Descriptor *, void *), void *(*Remove)(Descriptor *, uintptr_t)>
    struct ForeignVecPolicy {
        using value_type = typename ForeignClassRef::value_type;
        using reference = ForeignClassRef;
        using iterator = SliceIterator<Descriptor, ForeignVecPolicy>;
        using const_iterator = iterator;
        using CForeignType = typename ForeignClassRef::CForeignType;

        static Descriptor empty() noexcept { return New(); }
        static void free(Descriptor vec) noexcept { Free(vec); }
        static reference index(Descriptor vec, size_t i) noexcept
        {
            return ForeignSliceAccess<value_type>::index(
                SliceStorage<const CForeignType *>{ static_cast<const CForeignType *>(vec.data), vec.len }, i);
        }
        static iterator begin(Descriptor vec) noexcept { return iterator{ vec, 0 }; }
        static const_iterator cbegin(Descriptor vec) noexcept { return begin(vec); }
        static iterator end(Descriptor vec) noexcept { return iterator{ vec, vec.len }; }
        static const_iterator cend(Descriptor vec) noexcept { return end(vec); }
        static RustSlice<const value_type> as_slice(Descriptor vec) noexcept
        {
            return RustSlice<const value_type>{ static_cast<const CForeignType *>(vec.data), vec.len };
        }
        static RustSlice<value_type> as_slice_mut(Descriptor vec) noexcept
        {
            return RustSlice<value_type>{ static_cast<CForeignType *>(vec.data), vec.len };
        }
        static void push(Descriptor &vec, value_type value) noexcept
        {
            Push(&vec, value.release());
        }
        static value_type remove(Descriptor &vec, size_t i) noexcept
        {
            assert(i < vec.len);
            return value_type{ static_cast<CForeignType *>(Remove(&vec, i)) };
        }
    };

} // namespace internal

template <typename Descriptor, typename Policy>
class RustVec final {
public:
    using value_type = typename Policy::value_type;
    using const_reference = typename Policy::reference;
    using iterator = typename Policy::iterator;
    using const_iterator = typename Policy::const_iterator;

    RustVec() noexcept : vec_(Policy::empty()) {}
    explicit RustVec(Descriptor vec) noexcept : vec_(vec) {}
    RustVec(const RustVec &) = delete;
    RustVec &operator=(const RustVec &) = delete;
    RustVec(RustVec &&other) noexcept : vec_(other.release()) {}
    RustVec &operator=(RustVec &&other) noexcept
    {
        if (this != &other) {
            Policy::free(vec_);
            vec_ = other.release();
        }
        return *this;
    }
    ~RustVec() noexcept { Policy::free(vec_); }

    size_t size() const noexcept { return vec_.len; }
    bool empty() const noexcept { return size() == 0; }
    const_reference operator[](size_t i) const noexcept
    {
        assert(i < size());
        return Policy::index(vec_, i);
    }
    const_reference at(size_t i) const
    {
        if (i >= size()) {
            throw std::out_of_range("RustVec::at");
        }
        return Policy::index(vec_, i);
    }
    iterator begin() noexcept { return Policy::begin(vec_); }
    const_iterator begin() const noexcept { return Policy::cbegin(vec_); }
    iterator end() noexcept { return Policy::end(vec_); }
    const_iterator end() const noexcept { return Policy::cend(vec_); }

    template <typename P = Policy>
    auto as_slice() const noexcept -> decltype(P::as_slice(std::declval<Descriptor>()))
    {
        return P::as_slice(vec_);
    }
    template <typename P = Policy>
    auto as_slice_mut() noexcept -> decltype(P::as_slice_mut(std::declval<Descriptor>()))
    {
        return P::as_slice_mut(vec_);
    }
    template <typename P = Policy>
    auto push(value_type value) noexcept
        -> decltype(P::push(std::declval<Descriptor &>(), std::move(value)), void())
    {
        P::push(vec_, std::move(value));
    }
    template <typename P = Policy>
    auto remove(size_t i) noexcept -> decltype(P::remove(std::declval<Descriptor &>(), i))
    {
        return P::remove(vec_, i);
    }

    void clear() noexcept
    {
        Policy::free(vec_);
        vec_ = Policy::empty();
    }
    Descriptor release() noexcept
    {
        Descriptor old = vec_;
        vec_ = Policy::empty();
        return old;
    }

private:
    Descriptor vec_;
};

} // namespace RUST_SWIG_USER_NAMESPACE
