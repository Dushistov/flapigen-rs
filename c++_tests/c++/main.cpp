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
#ifdef HAS_STDCXX_17
#include <optional>
#include <variant>
#endif
#ifdef USE_BOOST
#include <boost/optional.hpp>
#include <boost/variant.hpp>
#endif
#include <gtest/gtest.h>

#include "rust_interface/rust_str.h"
#include "rust_interface/rust_vec.h"
#include "rust_interface/rust_result.h"
#include "rust_interface/CheckPrimitiveTypesClass.hpp"
#include "rust_interface/Foo.hpp"
#include "rust_interface/c_SomeObserver.h"
#include "rust_interface/ClassCooperationTest.hpp"
#include "rust_interface/TestObjectLifetime.hpp"
#include "rust_interface/TestWorkWithVec.hpp"
#include "rust_interface/c_MyEnum.h"
#include "rust_interface/TestEnumClass.hpp"
#include "rust_interface/TestPassPathAsParam.hpp"
#if defined(HAS_STDCXX_17) || defined(USE_BOOST)
#include "rust_interface/TestOptional.hpp"
#include "rust_interface/TestResult.hpp"
#endif
#include "rust_interface/TestReferences.hpp"
#include "rust_interface/TestOnlyStaticMethods.hpp"

using namespace rust;

static std::atomic<uint32_t> c_simple_cb_counter{ 0 };

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
    };
    c_simple_cb_counter = 0;
    Foo_call_me(&obs);
    EXPECT_EQ(1, c_simple_cb_counter.load());
    Foo_delete(foo);
}

TEST(Foo, Simple)
{
    Foo foo(1, "b");
    EXPECT_EQ(3, foo.f(1, 1));
    RustStrView name = foo.getName();
    EXPECT_EQ(std::string("b"), std::string(name.data, name.len));
    EXPECT_NEAR(std::hypot(1., 1.) + 1., foo.f_double(1., 1.), 1e-10);
    EXPECT_NEAR(std::hypot(1.0, 1.0), Foo::fHypot(1.0, 1.0), 1e-10);
    foo.set_field(5);
    EXPECT_EQ(7, foo.f(1, 1));
    const C_SomeObserver obs = {
        new int(17),
        c_delete_int,
        c_simple_cb,
    };
    c_simple_cb_counter = 0;
    Foo::call_me(&obs);
    EXPECT_EQ(1, c_simple_cb_counter.load());

    EXPECT_NEAR(7.5, foo.one_and_half(), 1e-16);
    {
        Foo f2(17, "");
        EXPECT_EQ(19, f2.f(1, 1));
        auto name = f2.getName();
        EXPECT_EQ(std::string(""), std::string(name.data, name.len));
    }

    EXPECT_EQ(4u, foo.cpp_func(std::string("abcd")));
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
    TestWorkWithVec t(tag);
    for (uint32_t n : { 0, 1, 2, 3, 5, 10, 100, 1000 }) {
        RustVec vec{ t.get_bytes(n) };
        EXPECT_EQ(tag_len * n, vec.size());
        for (size_t i = 0; i < vec.size(); i += std::strlen(tag)) {
            EXPECT_TRUE(i + tag_len <= vec.size());
            EXPECT_EQ(std::string(tag), std::string(reinterpret_cast<const char *>(&vec[i]), tag_len));
        }
    }

    auto sp = t.get_u32_slice();
    ASSERT_EQ(tag_len, sp.len);
    for (size_t i = 0; i < sp.len; ++i) {
        EXPECT_EQ(i, sp.data[i]);
    }
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
        RustString s{ foo.ret_string() };
        EXPECT_EQ(std::string(word), s.to_std_string());
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
}

TEST(TestResult, smokeTest)
{
#ifdef HAS_STDCXX_17
    std::variant<TestResult, RustString> res = TestResult::new_with_err();
    EXPECT_EQ(nullptr, std::get_if<TestResult>(&res));
    EXPECT_NE(nullptr, std::get_if<RustString>(&res));
    EXPECT_EQ(std::string_view("this is error"), std::get<RustString>(res).to_string_view());
#endif //HAS_STDCXX_17
#ifdef USE_BOOST
    boost::variant<TestResult, RustString> res = TestResult::new_with_err();
    EXPECT_EQ(nullptr, boost::get<TestResult>(&res));
    EXPECT_NE(nullptr, boost::get<RustString>(&res));
    EXPECT_EQ(std::string("this is error"), boost::get<RustString>(std::move(res)).to_std_string());
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
}

TEST(TestOnlyStaticMethods, smokeTest)
{
	EXPECT_EQ(4, TestOnlyStaticMethods::add_func(2, 2));
}

int main(int argc, char *argv[])
{
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::GTEST_FLAG(throw_on_failure) = true;
    return RUN_ALL_TESTS();
}
