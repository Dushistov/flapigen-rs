#define _USE_MATH_DEFINES // for C++
#include <atomic>
#include <cassert>
#include <cmath>
#include <cstdbool>
#include <cstdint>
#include <cstdio>
#include <functional>
#include <limits>

#include "rust_interface/RustStrView.h"
#include "rust_interface/CheckPrimitiveTypesClass.hpp"
#include "rust_interface/Foo.hpp"
#include "rust_interface/c_CheckPrimitiveTypesClass.h"
#include "rust_interface/c_Foo.h"
#include "rust_interface/c_SomeObserver.h"
#include "rust_interface/c_ClassCooperationTest.h"
#include "rust_interface/ClassCooperationTest.hpp"
#include "rust_interface/c_TestObjectLifetime.h"
#include "rust_interface/TestObjectLifetime.hpp"
#include <gtest/gtest.h>

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
}

TEST(CheckPrimitiveTypesClass, smokeTest)
{
    CheckPrimitiveTypesClass x;
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
    EXPECT_NEAR(2.1f, CheckPrimitiveTypesClass::test_f32(1.1), 1e-12);
    EXPECT_NEAR(0., CheckPrimitiveTypesClass::test_f64(-1.0), 1e-12);
}

TEST(ClassCooperationTest, smokeTest)
{
    ClassCooperationTest x;
    auto f1 = x.get(0);
    EXPECT_EQ(std::string("5"), f1.getName().as_str());
    EXPECT_EQ(5, f1.f(0, 0));
    auto f2 = x.get(1);
    EXPECT_EQ(std::string("7"), f2.getName().as_str());
    EXPECT_EQ(6, f2.f(0, 0));

    Foo new_f2{ 437, "437" };
    x.set(1, new_f2.release());
    f2 = x.get(1);
    EXPECT_EQ(std::string("437"), f2.getName().as_str());
    EXPECT_EQ(437, f2.f(0, 0));
}

TEST(TestObjectLifetime, smokeTest)
{
    TestObjectLifetime x;
    EXPECT_EQ(5, x.get_data());
    x.set_data(1, 2, 3, 4., 5.);
    EXPECT_EQ(15, x.get_data());
}

int main(int argc, char *argv[])
{
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::GTEST_FLAG(throw_on_failure) = true;
    return RUN_ALL_TESTS();
}
