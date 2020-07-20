r##"#pragma once

//for assert
#include <cassert>
//for std::abort
#include <cstdlib>
//for std::move
#include <utility>
//for std::conditional
#include <type_traits>

#include <QString>
#include "rust_str.h"

#include "c_BtAddr.h"

namespace org_examples {

template<bool>
class BtAddrWrapper;
using BtAddr = BtAddrWrapper<true>;
using BtAddrRef = BtAddrWrapper<false>;


template<bool OWN_DATA>
class BtAddrWrapper {
public:
    using value_type = BtAddrWrapper<true>;
    friend class BtAddrWrapper<true>;
    friend class BtAddrWrapper<false>;

    using SelfType = typename std::conditional<OWN_DATA, BtAddrOpaque *, const BtAddrOpaque *>::type;
    using CForeignType = BtAddrOpaque;

    BtAddrWrapper(BtAddrWrapper &&o) noexcept: self_(o.self_)
    {
        o.self_ = nullptr;
    }
    BtAddrWrapper &operator=(BtAddrWrapper &&o) noexcept
    {
        assert(this != &o);
        free_mem(this->self_);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }
    explicit BtAddrWrapper(SelfType o) noexcept: self_(o) {}
    BtAddrOpaque *release() noexcept
    {
        BtAddrOpaque *ret = self_;
        self_ = nullptr;
        return ret;
    }
    explicit operator SelfType() const noexcept { return self_; }
    BtAddrWrapper<false> as_rref() const noexcept { return BtAddrWrapper<false>{ self_ }; }
    const BtAddrWrapper<true> &as_cref() const noexcept { return reinterpret_cast<const BtAddrWrapper<true> &>(*this); }

    BtAddrWrapper(const BtAddrWrapper& o) noexcept {
         static_assert(OWN_DATA, "copy possible only if class own data");
         if (o.self_ != nullptr) {
             self_ = BtAddr_clone(o.self_);
         } else {
             self_ = nullptr;
         }
    }
    BtAddrWrapper &operator=(const BtAddrWrapper& o) noexcept {
        static_assert(OWN_DATA, "copy possible only if class own data");
        if (this != &o) {
            free_mem(this->self_);
            if (o.self_ != nullptr) {
                self_ = BtAddr_clone(o.self_);
            } else {
                self_ = nullptr;
            }
        }
        return *this;
    }
private:

    BtAddrWrapper() noexcept {}
public:

    QString to_string() const noexcept;

    BtAddr clone() const noexcept;

private:
   static void free_mem(SelfType &p) noexcept
   {
        if (OWN_DATA && p != nullptr) {
            BtAddr_delete(p);
        }
        p = nullptr;
   }
public:
    ~BtAddrWrapper() noexcept
    {
        free_mem(this->self_);
    }

private:
    SelfType self_;
};
"##;
