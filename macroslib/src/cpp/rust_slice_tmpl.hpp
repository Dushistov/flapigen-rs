#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iterator>
#include <stdexcept>
#include <type_traits>
#include <utility>

namespace RUST_SWIG_USER_NAMESPACE {
namespace internal {

    template <typename Pointer> struct SliceStorage {
        Pointer data;
        size_t len;
    };

    template <typename T> struct NativeSliceAccess {
        using storage_type = T;

        static T &index(SliceStorage<T *> slice, size_t i) noexcept
        {
            return slice.data[i];
        }
        static const T &index(SliceStorage<const T *> slice, size_t i) noexcept
        {
            return slice.data[i];
        }
    };

    template <typename T> struct ForeignSliceAccess {
        using reference = typename T::ref_type;
        using storage_type = typename T::CForeignType;

        static reference index(SliceStorage<const storage_type *> slice, size_t i) noexcept
        {
            auto p = reinterpret_cast<const uint8_t *>(slice.data);
            p += T::rust_elem_size * i;
            return reference{ reinterpret_cast<const storage_type *>(p) };
        }
        static reference index(SliceStorage<storage_type *> slice, size_t i) noexcept
        {
            return index(SliceStorage<const storage_type *>{ slice.data, slice.len }, i);
        }
    };

    template <typename...> using VoidT = void;

    template <typename T, typename Enable = void> struct SliceAccess : NativeSliceAccess<T> {};

    template <typename T>
    struct SliceAccess<T, VoidT<typename T::ref_type>> : ForeignSliceAccess<T> {};

    template <typename Descriptor, typename Access> class SliceIterator {
    public:
        using iterator_category = std::random_access_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using reference = decltype(Access::index(std::declval<Descriptor>(), size_t{}));
        using value_type = typename std::decay<reference>::type;
        using pointer = void;

        SliceIterator(Descriptor slice, size_t index) noexcept
            : slice_(slice)
            , index_(index)
        {
        }
        reference operator*() const noexcept { return Access::index(slice_, index_); }
        reference operator[](difference_type n) const noexcept
        {
            return Access::index(slice_, index_ + n);
        }
        SliceIterator &operator++() noexcept
        {
            ++index_;
            return *this;
        }
        SliceIterator operator++(int) noexcept
        {
            auto old = *this;
            ++*this;
            return old;
        }
        SliceIterator &operator--() noexcept
        {
            --index_;
            return *this;
        }
        SliceIterator operator--(int) noexcept
        {
            auto old = *this;
            --*this;
            return old;
        }
        SliceIterator &operator+=(difference_type n) noexcept
        {
            index_ += n;
            return *this;
        }
        SliceIterator &operator-=(difference_type n) noexcept
        {
            index_ -= n;
            return *this;
        }
        SliceIterator operator+(difference_type n) const noexcept
        {
            auto out = *this;
            out += n;
            return out;
        }
        SliceIterator operator-(difference_type n) const noexcept
        {
            auto out = *this;
            out -= n;
            return out;
        }
        friend SliceIterator operator+(difference_type n, SliceIterator it) noexcept
        {
            return it + n;
        }
        difference_type operator-(const SliceIterator &o) const noexcept
        {
            assert(same_slice(o));
            return static_cast<difference_type>(index_) - static_cast<difference_type>(o.index_);
        }
        bool operator==(const SliceIterator &o) const noexcept
        {
            return same_slice(o) && index_ == o.index_;
        }
        bool operator!=(const SliceIterator &o) const noexcept { return !(*this == o); }
        bool operator<(const SliceIterator &o) const noexcept
        {
            assert(same_slice(o));
            return index_ < o.index_;
        }
        bool operator>(const SliceIterator &o) const noexcept { return o < *this; }
        bool operator<=(const SliceIterator &o) const noexcept { return !(*this > o); }
        bool operator>=(const SliceIterator &o) const noexcept { return !(*this < o); }

    private:
        bool same_slice(const SliceIterator &o) const noexcept
        {
            return slice_.data == o.slice_.data && slice_.len == o.slice_.len;
        }
        Descriptor slice_;
        size_t index_;
    };

    template <typename T, typename Descriptor, typename Access, bool Native>
    struct SliceIteratorFactory {
        using type = SliceIterator<Descriptor, Access>;
        static type at(Descriptor slice, size_t i) noexcept { return type{ slice, i }; }
    };

    template <typename T, typename Descriptor, typename Access>
    struct SliceIteratorFactory<T, Descriptor, Access, true> {
        using type = T *;
        static type at(Descriptor slice, size_t i) noexcept
        {
            auto p = slice.data;
            return i == 0 ? p : p + i;
        }
    };

} // namespace internal

template <typename T, typename Access = internal::SliceAccess<typename std::remove_const<T>::type>>
class RustSlice final {
    using Element = typename std::remove_const<T>::type;
    using StorageElement = typename Access::storage_type;
    using Pointer = typename std::conditional<std::is_const<T>::value, const StorageElement *,
                                              StorageElement *>::type;
    using Storage = internal::SliceStorage<Pointer>;
    using ConstStorage = internal::SliceStorage<const StorageElement *>;
    static constexpr bool native
        = std::is_base_of<internal::NativeSliceAccess<Element>, Access>::value;
    using MutableIterator = internal::SliceIteratorFactory<T, Storage, Access, native>;
    using ReadIterator = internal::SliceIteratorFactory<const Element, ConstStorage, Access, native>;

public:
    using value_type = Element;
    using storage_type = StorageElement;
    using reference = decltype(Access::index(std::declval<Storage>(), size_t{}));
    using const_reference = decltype(Access::index(std::declval<ConstStorage>(), size_t{}));
    using iterator = typename MutableIterator::type;
    using const_iterator = typename ReadIterator::type;

    RustSlice() noexcept
        : slice_{ nullptr, 0 }
    {
    }
    template <typename Descriptor,
              typename std::enable_if<
                  std::is_same<decltype(std::declval<Descriptor>().data), Pointer>::value,
                  int>::type = 0>
    explicit RustSlice(Descriptor slice) noexcept
        : slice_{ slice.data, slice.len }
    {
    }
    RustSlice(Pointer data, size_t len) noexcept
        : slice_{ data, len }
    {
    }
    RustSlice(const RustSlice &) = delete;
    RustSlice &operator=(const RustSlice &) = delete;
    RustSlice(RustSlice &&o) noexcept
        : slice_(o.slice_)
    {
        o.reset();
    }
    RustSlice &operator=(RustSlice &&o) noexcept
    {
        if (this != &o) {
            slice_ = o.slice_;
            o.reset();
        }
        return *this;
    }

    size_t size() const noexcept { return slice_.len; }
    bool empty() const noexcept { return slice_.len == 0; }
    reference operator[](size_t i) noexcept
    {
        assert(i < size());
        return Access::index(slice_, i);
    }
    const_reference operator[](size_t i) const noexcept
    {
        assert(i < size());
        return Access::index(as_const_storage(), i);
    }
    reference at(size_t i)
    {
        if (i >= size()) {
            throw std::out_of_range("RustSlice::at");
        }
        return Access::index(slice_, i);
    }
    const_reference at(size_t i) const
    {
        if (i >= size()) {
            throw std::out_of_range("RustSlice::at");
        }
        return Access::index(as_const_storage(), i);
    }
    iterator begin() noexcept { return MutableIterator::at(slice_, 0); }
    const_iterator begin() const noexcept { return ReadIterator::at(as_const_storage(), 0); }
    iterator end() noexcept { return MutableIterator::at(slice_, size()); }
    const_iterator end() const noexcept { return ReadIterator::at(as_const_storage(), size()); }
    // The descriptor must be explicit: uintptr_t may alias uint32_t on 32-bit targets.
    template <typename Descriptor> Descriptor as_c() const noexcept
    {
        static_assert(std::is_same<decltype(std::declval<Descriptor>().data), Pointer>::value,
                      "slice descriptor data pointer must match the slice element type");
        return Descriptor{ slice_.data, slice_.len };
    }

private:
    ConstStorage as_const_storage() const noexcept { return ConstStorage{ slice_.data, slice_.len }; }
    void reset() noexcept { slice_ = Storage{ nullptr, 0 }; }
    Storage slice_;
};

} // namespace RUST_SWIG_USER_NAMESPACE
