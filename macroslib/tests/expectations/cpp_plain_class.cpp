r##"#pragma once

//for assert
#include <cassert>
//for std::abort
#include <cstdlib>
//for std::move
#include <utility>


#include "c_Foo.h"

namespace org_examples {


class Foo {
public:
    using SelfType = FooOpaque *;
    using CForeignType = FooOpaque;

    Foo(Foo &&o) noexcept: self_(o.self_)
    {
        o.self_ = nullptr;
    }
    Foo &operator=(Foo &&o) noexcept
    {
        assert(this != &o);
        free_mem(this->self_);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }
    explicit Foo(SelfType o) noexcept: self_(o) {}
    FooOpaque *release() noexcept
    {
        FooOpaque *ret = self_;
        self_ = nullptr;
        return ret;
    }
    explicit operator SelfType() const noexcept { return self_; }

    Foo(const Foo&) = delete;
    Foo &operator=(const Foo&) = delete;

    Foo() noexcept
    {

        this->self_ = Foo_new();
        if (this->self_ == nullptr) {
            std::abort();
        }
    }

    void method() const noexcept;

    static void static_func() noexcept;

private:
   static void free_mem(SelfType &p) noexcept
   {
        if (p != nullptr) {
            Foo_delete(p);
        }
        p = nullptr;
   }
public:
    ~Foo() noexcept
    {
        free_mem(this->self_);
    }

private:
    SelfType self_;
};


    inline void Foo::method() const noexcept
    {

        Foo_method(this->self_);
    }

    inline void Foo::static_func() noexcept
    {

        Foo_static_func();
    }

} // namespace org_examples"##;

r##"#pragma once

namespace org_examples {
class Foo;
} // namespace org_examples"##;

r##"class AppError {
public:
    using SelfType = AppErrorOpaque *;
    using CForeignType = AppErrorOpaque;

    AppError(AppError &&o) noexcept: self_(o.self_)
    {
        o.self_ = nullptr;
    }
    AppError &operator=(AppError &&o) noexcept
    {
        assert(this != &o);
        free_mem(this->self_);
        self_ = o.self_;
        o.self_ = nullptr;
        return *this;
    }
    explicit AppError(SelfType o) noexcept: self_(o) {}
    AppErrorOpaque *release() noexcept
    {
        AppErrorOpaque *ret = self_;
        self_ = nullptr;
        return ret;
    }
    explicit operator SelfType() const noexcept { return self_; }

    AppError(const AppError& o) noexcept {
         
         if (o.self_ != nullptr) {
             self_ = AppError_clone(o.self_);
         } else {
             self_ = nullptr;
         }
    }
    AppError &operator=(const AppError& o) noexcept {
        
        if (this != &o) {
            free_mem(this->self_);
            if (o.self_ != nullptr) {
                self_ = AppError_clone(o.self_);
            } else {
                self_ = nullptr;
            }
        }
        return *this;
    }
private:

    AppError() noexcept {}
public:

    AppError clone() const noexcept;

    std::string_view err_abbr() const noexcept;

private:
   static void free_mem(SelfType &p) noexcept
   {
        if (p != nullptr) {
            AppError_delete(p);
        }
        p = nullptr;
   }
public:
    ~AppError() noexcept
    {
        free_mem(this->self_);
    }


    QString display() const noexcept;


private:
    SelfType self_;
};"##;
