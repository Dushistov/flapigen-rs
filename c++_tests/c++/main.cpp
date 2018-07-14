#define _USE_MATH_DEFINES // for C++
#include <atomic>
#include <cassert>
#include <cmath>
#include <cstdbool>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <functional>
#include <limits>
#include <string>
#include <iostream>
#include <sstream>
#ifdef HAS_STDCXX_17
#include <optional>
#include <variant>
#endif
#ifdef USE_BOOST
#include <boost/optional.hpp>
#include <boost/variant.hpp>
#ifdef HAS_BOOST_STRING_VIEW_HPP
#include <boost/utility/string_view.hpp>
#endif
#endif
#include <gtest/gtest.h>

#include "rust_interface/rust_str.h"
#include "rust_interface/rust_vec.h"
#include "rust_interface/rust_result.h"
#include "rust_interface/rust_option.h"
#include "rust_interface/CheckPrimitiveTypesClass.hpp"
#include "rust_interface/Foo.hpp"
#include "rust_interface/c_SomeObserver.h"
#include "rust_interface/SomeObserver.hpp"
#include "rust_interface/ClassCooperationTest.hpp"
#include "rust_interface/TestObjectLifetime.hpp"
#include "rust_interface/RustForeignVecFoo.h"
#include "rust_interface/TestWorkWithVec.hpp"
#include "rust_interface/c_MyEnum.h"
#include "rust_interface/TestEnumClass.hpp"
#include "rust_interface/TestPassPathAsParam.hpp"
#if defined(HAS_STDCXX_17) || defined(USE_BOOST)
#include "rust_interface/TestOptional.hpp"
#include "rust_interface/TestError.hpp"
#include "rust_interface/TestResult.hpp"
#include "rust_interface/Position.hpp"
#include "rust_interface/LocationService.hpp"
#endif
#include "rust_interface/TestReferences.hpp"
#include "rust_interface/TestOnlyStaticMethods.hpp"
#include "rust_interface/Interface.hpp"
#include "rust_interface/TestPassInterface.hpp"

using namespace rust;

static std::atomic<uint32_t> c_simple_cb_counter{ 0 };
static std::atomic<uint32_t> c_simple_cb_counter_without_args{ 0 };

static void c_delete_int(void *opaque)
{
    printf("clear\n");
    auto self = static_cast<int *>(opaque);
    ASSERT_EQ(17, *self);
    delete self;
}

static void c_simple_cb(int32_t a, char b, void *opaque)
{
    assert(opaque != nullptr);
    const int tag = *static_cast<int *>(opaque);
    ASSERT_EQ(17, tag);
    printf("!!! a %d, b: %d, tag %d\n", static_cast<int>(a), b, tag);
    ++c_simple_cb_counter;
}

static void c_simple_cb_without_args(void *opaque)
{
    assert(opaque != nullptr);
    const int tag = *static_cast<int *>(opaque);
    ASSERT_EQ(17, tag);
    ++c_simple_cb_counter_without_args;
}

TEST(c_Foo, Simple)
{
    auto foo = Foo_new(1, "a");
    ASSERT_NE(foo, nullptr);

    EXPECT_EQ(3, Foo_f(foo, 1, 1));
    auto name = Foo_getName(foo);
    EXPECT_EQ(std::string("a"), std::string(name.data, name.len));

    Foo_set_field(foo, 5);
    EXPECT_EQ(7, Foo_f(foo, 1, 1));
    const C_SomeObserver obs = {
        new int(17),
        c_delete_int,
        c_simple_cb,
        c_simple_cb_without_args,
    };
    c_simple_cb_counter = 0;
    c_simple_cb_counter_without_args = 0;
    Foo_call_me(&obs);
    EXPECT_EQ(1u, c_simple_cb_counter.load());
    EXPECT_EQ(1u, c_simple_cb_counter_without_args.load());
    Foo_delete(foo);
}

TEST(Foo, Simple)
{
    Foo foo(1, "b");
    EXPECT_EQ(3, foo.f(1, 1));
    {
        RustStrView name = foo.getName();
        EXPECT_EQ(std::string("b"), std::string(name.data, name.len));
    }
    EXPECT_NEAR(std::hypot(1., 1.) + 1., foo.f_double(1., 1.), 1e-10);
    EXPECT_NEAR(std::hypot(1.0, 1.0), Foo::fHypot(1.0, 1.0), 1e-10);
    foo.set_field(5);
    EXPECT_EQ(7, foo.f(1, 1));
    const C_SomeObserver obs = {
        new int(17),
        c_delete_int,
        c_simple_cb,
        c_simple_cb_without_args,
    };
    c_simple_cb_counter = 0;
    c_simple_cb_counter_without_args = 0;
    Foo::call_me(&obs);
    EXPECT_EQ(1u, c_simple_cb_counter.load());
    EXPECT_EQ(1u, c_simple_cb_counter_without_args.load());

    EXPECT_NEAR(7.5, foo.one_and_half(), 1e-16);
    {
        Foo f2(17, "");
        EXPECT_EQ(19, f2.f(1, 1));
        auto name = f2.getName();
        EXPECT_EQ(std::string(""), std::string(name.data, name.len));
    }

    EXPECT_EQ(4u, foo.cpp_func(std::string("abcd")));
}

namespace {
struct MySomeObserver final : public SomeObserver {
    static size_t f1_call;
    static size_t f2_call;
    static size_t deleted;

    MySomeObserver()
    {
        f1_call = 0;
        f2_call = 0;
        deleted = 0;
    }
    ~MySomeObserver()
    {
        ++deleted;
    }
    void onStateChanged(int32_t a, bool b) override
    {
        std::cout << "onStateChanged: a: " << a << ", b: " << b << "\n";
        ASSERT_EQ(2, a);
        ASSERT_FALSE(!!b);
        ++f1_call;
    }
    void onStateChangedWithoutArgs(void) override
    {
        std::cout << "onStateChangedWithoutArgs\n";
        ++f2_call;
    }
};

size_t MySomeObserver::f1_call = 0;
size_t MySomeObserver::f2_call = 0;
size_t MySomeObserver::deleted = 0;
} // namespace

TEST(Foo, CppSomeObserver)
{
    Foo foo(17, "CppSomeObserver");
    auto obs = new MySomeObserver;
    ASSERT_EQ(0u, obs->f1_call);
    ASSERT_EQ(0u, obs->f2_call);
    auto c_class = SomeObserver::to_c_interface(obs);
    Foo::call_me(&c_class);
    EXPECT_EQ(1u, MySomeObserver::f1_call);
    EXPECT_EQ(1u, MySomeObserver::f2_call);
    EXPECT_EQ(1u, MySomeObserver::deleted);
}

TEST(CheckPrimitiveTypesClass, smokeTest)
{
    CheckPrimitiveTypesClass x;

    EXPECT_TRUE(CheckPrimitiveTypesClass::invert(false));
    EXPECT_FALSE(CheckPrimitiveTypesClass::invert(true));

    EXPECT_TRUE(CheckPrimitiveTypesClass::Xor(true, false));
    EXPECT_TRUE(CheckPrimitiveTypesClass::Xor(false, true));
    EXPECT_FALSE(CheckPrimitiveTypesClass::Xor(true, true));
    EXPECT_FALSE(CheckPrimitiveTypesClass::Xor(false, false));

    EXPECT_NEAR(static_cast<float>(M_E), x.test(true), std::numeric_limits<float>::epsilon());
    EXPECT_NEAR(static_cast<float>(M_PI), x.test(false), std::numeric_limits<float>::epsilon());
    EXPECT_EQ(255, CheckPrimitiveTypesClass::test_u8(254u));
    EXPECT_EQ(0, CheckPrimitiveTypesClass::test_i8(-1));
    EXPECT_EQ(65535u, CheckPrimitiveTypesClass::test_u16(65534u));
    EXPECT_EQ(0, CheckPrimitiveTypesClass::test_i16(-1));
    EXPECT_EQ(4294967295u, CheckPrimitiveTypesClass::test_u32(4294967295u - 1));
    EXPECT_EQ(0, CheckPrimitiveTypesClass::test_i32(-1));
    EXPECT_EQ(18446744073709551615ull,
              CheckPrimitiveTypesClass::test_u64(18446744073709551615ull - 1));
    EXPECT_EQ(0, CheckPrimitiveTypesClass::test_i64(-1));
    EXPECT_NEAR(2.1f, CheckPrimitiveTypesClass::test_f32(1.1f), 1e-12f);
    EXPECT_NEAR(0., CheckPrimitiveTypesClass::test_f64(-1.0), 1e-12);
}

TEST(ClassCooperationTest, smokeTest)
{
    ClassCooperationTest x;
    auto f1 = x.get(0);
    EXPECT_EQ(std::string("5"), f1.getName().to_std_string());
    EXPECT_EQ(5, f1.f(0, 0));
    auto f2 = x.get(1);
    EXPECT_EQ(std::string("7"), f2.getName().to_std_string());
    EXPECT_EQ(6, f2.f(0, 0));

    Foo new_f2{ 437, "437" };
    x.set(1, std::move(new_f2));
    f2 = x.get(1);
    EXPECT_EQ(std::string("437"), f2.getName().to_std_string());
    EXPECT_EQ(437, f2.f(0, 0));
}

TEST(TestObjectLifetime, smokeTest)
{
    TestObjectLifetime x;
    EXPECT_EQ(5, x.get_data());
    x.set_data(1, 2, 3, 4., 5.);
    EXPECT_EQ(15, x.get_data());
}

TEST(TestWorkWithVec, smokeTest)
{
    const char tag[] = "Test data";
    const size_t tag_len = std::strlen(tag);
    TestWorkWithVec t{ tag };
    for (uint32_t n : { 0, 1, 2, 3, 5, 10, 100, 1000 }) {
        RustVecU8 vec{ t.get_bytes(n) };
        EXPECT_TRUE(n == 0 || !vec.empty());
        EXPECT_EQ(tag_len * n, vec.size());
        for (size_t i = 0; i < vec.size(); i += std::strlen(tag)) {
            EXPECT_TRUE(i + tag_len <= vec.size());
            EXPECT_EQ(std::string(tag), std::string(reinterpret_cast<const char *>(&vec[i]), tag_len));
        }
        vec = RustVecU8{ t.get_bytes(n) };
        EXPECT_EQ(tag_len * n, vec.size());
        for (size_t i = 0; i < vec.size(); i += std::strlen(tag)) {
            EXPECT_TRUE(i + tag_len <= vec.size());
            EXPECT_EQ(std::string(tag), std::string(reinterpret_cast<const char *>(&vec[i]), tag_len));
        }
    }

    auto sp = t.get_u32_slice();
    ASSERT_EQ(tag_len + 1, sp.len);
    for (size_t i = 0; i < tag_len; ++i) {
        EXPECT_EQ(i, sp.data[i]);
    }
    EXPECT_EQ(uint32_t(1) << 30, sp.data[tag_len]);

    static_assert(std::is_same<RustVecU32::value_type, uint32_t>::value,
                  "RustVecU32::value_type should be uint32_t");
    RustVecU32 vec_u32{ t.get_vec_u32() };
    ASSERT_EQ(tag_len + 1, vec_u32.size());
    for (size_t i = 0; i < tag_len; ++i) {
        EXPECT_EQ(i, vec_u32[i]);
    }
    EXPECT_EQ(uint32_t(1) << 30, vec_u32[tag_len]);

    RustVecF32 vec_f32{ t.get_vec_f32() };
    ASSERT_EQ(2u, vec_f32.size());
    EXPECT_NEAR(static_cast<float>(M_E), vec_f32[0], std::numeric_limits<float>::epsilon());
    EXPECT_NEAR(static_cast<float>(M_PI), vec_f32[1], std::numeric_limits<float>::epsilon());

    RustVecF64 vec_f64{ t.get_vec_f64() };
    ASSERT_EQ(2u, vec_f64.size());
    EXPECT_NEAR(static_cast<double>(M_E), vec_f64[0], std::numeric_limits<double>::epsilon());
    EXPECT_NEAR(static_cast<double>(M_PI), vec_f64[1], std::numeric_limits<double>::epsilon());

    RustForeignVecFoo vec_foo = t.get_vec_foo();
    ASSERT_EQ(tag_len, vec_foo.size());
    for (size_t i = 0; i < vec_foo.size(); ++i) {
        EXPECT_EQ(std::string(tag), vec_foo[i].getName().to_std_string());
        EXPECT_TRUE(vec_foo[i].f(0, 0) >= 0);
        EXPECT_EQ(i, size_t(vec_foo[i].f(0, 0)));
    }
    vec_foo.push(Foo{ 57, "boo" });
    ASSERT_EQ(tag_len + 1, vec_foo.size());
    EXPECT_EQ(57, vec_foo[vec_foo.size() - 1].f(0, 0));
    EXPECT_EQ(std::string("boo"), vec_foo[vec_foo.size() - 1].getName().to_std_string());
    {
        auto elem = vec_foo.remove(tag_len);
        EXPECT_EQ(tag_len, vec_foo.size());
        EXPECT_EQ(57, elem.f(0, 0));
        EXPECT_EQ(std::string("boo"), elem.getName().to_std_string());
        for (size_t i = 0; i < vec_foo.size(); ++i) {
            EXPECT_EQ(std::string(tag), vec_foo[i].getName().to_std_string());
            EXPECT_TRUE(vec_foo[i].f(0, 0) >= 0);
            EXPECT_EQ(i, size_t(vec_foo[i].f(0, 0)));
        }
    }
    {
        auto slice_foo = t.get_slice_foo();
        ASSERT_EQ(tag_len, slice_foo.size());
        for (size_t i = 0; i < slice_foo.size(); ++i) {
            EXPECT_EQ(std::string(tag), slice_foo[i].getName().to_std_string());
            EXPECT_TRUE(slice_foo[i].f(0, 0) >= 0);
            EXPECT_EQ(i, size_t(slice_foo[i].f(0, 0)));
        }
        size_t i = 0;
        for (auto foo : slice_foo) {
            EXPECT_EQ(std::string(tag), foo.getName().to_std_string());
            EXPECT_TRUE(foo.f(0, 0) >= 0);
            EXPECT_EQ(i, size_t(foo.f(0, 0)));
            ++i;
        }
    }
    {
        auto v = TestWorkWithVec::create_foo_vec(30);
        auto v1 = TestWorkWithVec::create_foo_vec(0);
        while (!v.empty()) {
            v1.push(v.remove(v.size() - 1));
        }
        size_t i = v1.size() - 1;
        for (const auto &elem : v1) {
            EXPECT_EQ(i, static_cast<size_t>(elem.f(0, 0)));
            --i;
        }
        TestWorkWithVec::sort_foo_slice(v1.as_slice());
        i = 0;
        for (const auto &elem : v1) {
            EXPECT_EQ(i, static_cast<size_t>(elem.f(0, 0)));
            ++i;
        }
    }

    {
        auto sl = RustSlice<CRustSliceUsize>{ t.return_usize_slice() };
        ASSERT_EQ(2u, sl.size());
        EXPECT_EQ(17u, sl[0]);
        EXPECT_EQ(18u, sl[1]);

        auto v = t.return_usize_vec();
        ASSERT_EQ(2u, v.size());
        EXPECT_EQ(17u, v[0]);
        EXPECT_EQ(18u, v[1]);
    }
}

TEST(TestWorkWithVec, rangeLoop)
{
    const size_t N = 2000;
    auto vec = TestWorkWithVec::create_foo_vec(N);
    ASSERT_EQ(N, vec.size());
    size_t i = 0;
    std::stringstream fmt;
    for (auto &&elem : vec) {
        ASSERT_EQ(static_cast<int32_t>(i), elem.f(0, 0));
        fmt << i;
        ASSERT_EQ(fmt.str(), elem.getName().to_std_string());
        fmt.str(std::string());
        fmt.clear();
        ++i;
    }
    ASSERT_EQ(N, i);
}

TEST(TestEnumClass, smokeTest)
{
    TestEnumClass x;
    ASSERT_EQ(-5, x.f1(ITEM1));
    ASSERT_EQ(-5, x.f1(ITEM3));
    ASSERT_EQ(17, x.f1(ITEM2));
    ASSERT_EQ(ITEM2, TestEnumClass::next_enum(ITEM1));
    ASSERT_EQ(ITEM3, TestEnumClass::next_enum(ITEM2));
    ASSERT_EQ(ITEM1, TestEnumClass::next_enum(ITEM3));
}

TEST(TestPassPathAsParam, smokeTest)
{
    TestPassPathAsParam x;
    x.set_path("/tmp/a.txt");
    ASSERT_EQ("\"/tmp/a.txt\"", x.path().to_std_string());
}

TEST(TestRustStringReturn, smokeTest)
{
    auto try_ret = [](const char *word) {
        Foo foo(1, word);
        EXPECT_EQ(std::string(word), foo.getName().to_std_string());
        EXPECT_EQ(std::string(word), foo.ret_string().to_std_string());
    };
    try_ret("Word");
    try_ret("");
    for (size_t i = 0; i < 100; ++i) {
        std::string expect(i, 'A');
        try_ret(expect.c_str());
    }
}

#if defined(HAS_STDCXX_17) || defined(USE_BOOST)
TEST(TestOptional, smokeTest)
{
    TestOptional x;
    {
        auto foo = x.f1(true);
        ASSERT_TRUE(!!foo);
        EXPECT_EQ(17, foo->f(0, 0));
        EXPECT_EQ(std::string("17"), foo->getName().to_std_string());
    }
    {
        auto foo = x.f1(false);
        EXPECT_FALSE(!!foo);
    }
    {
        auto val = x.f2(true);
        ASSERT_TRUE(!!val);
        EXPECT_NEAR(static_cast<double>(M_E), *val, std::numeric_limits<double>::epsilon());
        auto val2 = x.f2(false);
        ASSERT_FALSE(!!val2);
    }
    {
        auto val = x.f3(true);
        ASSERT_TRUE(!!val);
        EXPECT_EQ(17u, *val);
        auto val2 = x.f3(false);
        ASSERT_FALSE(!!val2);
    }

    EXPECT_NEAR(10., x.f4({ 5. }), std::numeric_limits<double>::epsilon());
    EXPECT_NEAR(-1., x.f4({}), std::numeric_limits<double>::epsilon());

    {
        auto val = x.f5(true);
        ASSERT_TRUE(!!val);
        FooRef foo = std::move(*val);
        EXPECT_EQ(5, foo.f(0, 0));
        EXPECT_EQ(std::string("aaa"), foo.getName().to_std_string());
    }
    {
        auto foo = x.f5(false);
        EXPECT_FALSE(!!foo);
    }

    {
        Foo foo(17, "17");
        x.f6({ std::move(foo) });
        auto val = x.f5(true);
        ASSERT_TRUE(!!val);
        FooRef foor = std::move(*val);
        EXPECT_EQ(17, foor.f(0, 0));
        EXPECT_EQ(std::string("17"), foor.getName().to_std_string());
        x.f6({});
    }

    {
        auto val = x.f7();
        ASSERT_TRUE(!!val);
        EXPECT_EQ(ITEM1, *val);
    }
    {
        auto val = x.f9({ "aaa" });
        EXPECT_EQ(std::string("your name is aaa"), val.to_std_string());
        auto val2 = x.f9({});
        EXPECT_EQ(std::string("None"), val2.to_std_string());
    }
}

TEST(TestResult, smokeTest)
{
#ifdef HAS_STDCXX_17
    std::variant<TestResult, RustString> res = TestResult::new_with_err();
    EXPECT_EQ(nullptr, std::get_if<TestResult>(&res));
    EXPECT_NE(nullptr, std::get_if<RustString>(&res));
    EXPECT_EQ(std::string_view("this is error"), std::get<RustString>(res).to_string_view());
    auto res2 = TestResult::f(true);
    EXPECT_EQ(nullptr, std::get_if<RustString>(&res2));
    EXPECT_NE(nullptr, std::get_if<void *>(&res2));
    auto res3 = TestResult::f(false);
    EXPECT_NE(nullptr, std::get_if<RustString>(&res3));
    EXPECT_EQ(nullptr, std::get_if<void *>(&res3));
    EXPECT_EQ(std::string_view("Not ok"), std::get<RustString>(res3).to_string_view());

    auto res_vec = TestResult::f_vec(true);
    EXPECT_EQ(nullptr, std::get_if<RustString>(&res_vec));
    EXPECT_NE(nullptr, std::get_if<RustForeignVecFoo>(&res_vec));
    auto vec = std::get<RustForeignVecFoo>(std::move(res_vec));
    ASSERT_EQ(2u, vec.size());
    EXPECT_EQ(std::string_view("15"), vec[0].getName().to_string_view());
    EXPECT_EQ(15, vec[0].f(0, 0));
    EXPECT_EQ(std::string_view("13"), vec[1].getName().to_string_view());
    EXPECT_EQ(13, vec[1].f(0, 0));
    res_vec = TestResult::f_vec(false);
    EXPECT_NE(nullptr, std::get_if<RustString>(&res_vec));
    EXPECT_EQ(nullptr, std::get_if<RustForeignVecFoo>(&res_vec));
    EXPECT_EQ(std::string_view("Not ok"), std::get<RustString>(res_vec).to_string_view());

    auto f2_ok = TestResult::f2(true);
    EXPECT_NE(nullptr, std::get_if<Foo>(&f2_ok));
    EXPECT_EQ(nullptr, std::get_if<TestError>(&f2_ok));
    Foo f2_ok_ret = std::get<Foo>(std::move(f2_ok));
    ASSERT_EQ(17, f2_ok_ret.f(0, 0));
    ASSERT_EQ(std::string_view("ok"), f2_ok_ret.getName().to_string_view());

    auto f2_err = TestResult::f2(false);
    EXPECT_EQ(nullptr, std::get_if<Foo>(&f2_err));
    EXPECT_NE(nullptr, std::get_if<TestError>(&f2_err));
    TestError f2_err_ret = std::get<TestError>(std::move(f2_err));
    ASSERT_EQ(std::string_view("Not ok"), RustString{ f2_err_ret.to_string() }.to_string_view());

    auto f3_ok = TestResult::f3(true);
    EXPECT_NE(nullptr, std::get_if<RustForeignVecFoo>(&f3_ok));
    EXPECT_EQ(nullptr, std::get_if<TestError>(&f3_ok));
    auto f3_vec = std::get<RustForeignVecFoo>(std::move(f3_ok));

    ASSERT_EQ(2u, f3_vec.size());
    EXPECT_EQ(std::string_view("40"), f3_vec[0].getName().to_string_view());
    EXPECT_EQ(40, f3_vec[0].f(0, 0));
    EXPECT_EQ(std::string_view(""), f3_vec[1].getName().to_string_view());
    EXPECT_EQ(60, f3_vec[1].f(0, 0));

    auto f3_err = TestResult::f3(false);
    EXPECT_EQ(nullptr, std::get_if<RustForeignVecFoo>(&f3_err));
    EXPECT_NE(nullptr, std::get_if<TestError>(&f3_err));
    TestError f3_err_ret = std::get<TestError>(std::move(f3_err));
    ASSERT_EQ(std::string_view("Not ok"), RustString{ f3_err_ret.to_string() }.to_string_view());
#endif //HAS_STDCXX_17
#ifdef USE_BOOST
    boost::variant<TestResult, RustString> res = TestResult::new_with_err();
    EXPECT_EQ(nullptr, boost::get<TestResult>(&res));
    EXPECT_NE(nullptr, boost::get<RustString>(&res));
#ifdef HAS_BOOST_STRING_VIEW_HPP
    EXPECT_EQ(boost::string_view("this is error"), boost::get<RustString>(std::move(res)).to_boost_string_view());
#else
    EXPECT_EQ(std::string("this is error"), boost::get<RustString>(std::move(res)).to_std_string());
#endif
    auto res2 = TestResult::f(true);
    EXPECT_EQ(nullptr, boost::get<RustString>(&res2));
    EXPECT_NE(nullptr, boost::get<void *>(&res2));
    auto res3 = TestResult::f(false);
    EXPECT_NE(nullptr, boost::get<RustString>(&res3));
    EXPECT_EQ(nullptr, boost::get<void *>(&res3));
#ifdef HAS_BOOST_STRING_VIEW_HPP
    EXPECT_EQ(boost::string_view("Not ok"), boost::get<RustString>(std::move(res3)).to_boost_string_view());
#else
    EXPECT_EQ(std::string("Not ok"), boost::get<RustString>(std::move(res3)).to_std_string());
#endif
    {
        auto res_vec = TestResult::f_vec(true);
        EXPECT_EQ(nullptr, boost::get<RustString>(&res_vec));
        EXPECT_NE(nullptr, boost::get<RustForeignVecFoo>(&res_vec));
        auto vec = std::move(boost::get<RustForeignVecFoo>(std::move(res_vec)));
        ASSERT_EQ(2u, vec.size());
#ifdef HAS_BOOST_STRING_VIEW_HPP
        EXPECT_EQ(boost::string_view("15"), vec[0].getName().to_boost_string_view());
        EXPECT_EQ(boost::string_view("13"), vec[1].getName().to_boost_string_view());
#else
        EXPECT_EQ(std::string("15"), vec[0].getName().to_std_string());
        EXPECT_EQ(std::string("13"), vec[1].getName().to_std_string());
#endif
        EXPECT_EQ(15, vec[0].f(0, 0));
        EXPECT_EQ(13, vec[1].f(0, 0));
    }
    auto res_vec = TestResult::f_vec(false);
    EXPECT_NE(nullptr, boost::get<RustString>(&res_vec));
    EXPECT_EQ(nullptr, boost::get<RustForeignVecFoo>(&res_vec));
#ifdef HAS_BOOST_STRING_VIEW_HPP
    EXPECT_EQ(boost::string_view("Not ok"), boost::get<RustString>(res_vec).to_boost_string_view());
#else
    EXPECT_EQ(std::string("Not ok"), boost::get<RustString>(res_vec).to_std_string());
#endif
#endif //USE_BOOST
}
#endif

TEST(TestReferences, smokeTest)
{
    TestReferences tr(500, "bugaga");
    auto foo = tr.get_foo_ref();
    EXPECT_EQ(502, foo.f(1, 1));
    EXPECT_EQ(std::string("bugaga"), foo.getName().to_std_string());

    Foo new_foo(100, "100");
    tr.update_foo(new_foo);
    foo = tr.get_foo_ref();
    EXPECT_EQ(102, foo.f(1, 1));
    EXPECT_EQ(std::string("100"), foo.getName().to_std_string());

    Foo foo2(200, "200");
    tr.update_mut_foo(foo2);
    foo = tr.get_foo_ref();
    EXPECT_EQ(202, foo.f(1, 1));
    EXPECT_EQ(std::string("200A"), foo2.getName().to_std_string());
}

TEST(TestOnlyStaticMethods, smokeTest)
{
    EXPECT_EQ(4, TestOnlyStaticMethods::add_func(2, 2));
}

#if defined(HAS_STDCXX_17) || defined(USE_BOOST)
TEST(TestDummyConstructor, smokeTest)
{
    auto res = LocationService::position();
#ifdef HAS_STDCXX_17
    ASSERT_NE(nullptr, std::get_if<Position>(&res));
    auto pos = std::get<Position>(std::move(res));
#endif //HAS_STDCXX_17
#ifdef USE_BOOST
    ASSERT_NE(nullptr, boost::get<Position>(&res));
    auto pos = boost::get<Position>(std::move(res));
#endif //USE_BOOST
    EXPECT_NEAR(0.1, pos.latitude(), 1e-10);
}
#endif

TEST(Interface, smokeTest)
{
    Interface x;
    EXPECT_EQ(18, x.f(1));
    x.set(0);
    EXPECT_EQ(1, x.f(1));
    EXPECT_EQ(17, TestPassInterface::use_interface(std::move(x), 17));
}

int main(int argc, char *argv[])
{
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::GTEST_FLAG(throw_on_failure) = true;
    return RUN_ALL_TESTS();
}
