#include <cstdint>
#include <cstdio>
#include <cassert>
#include <cstdbool>
#include <functional>

#include <gtest/gtest.h>
#include "rust_interface/c_SomeObserver.h"
#include "rust_interface/c_Foo.h"

static void c_delete_int(void *opaque)
{
	printf("clear\n");
	auto self = static_cast<int *>(opaque);
	delete self;
}

static void c_simple_cb(int32_t a, char b, void *opaque)
{
	assert(opaque != nullptr);
	const int tag = *static_cast<int *>(opaque);
	printf("!!! a %d, b: %d, tag %d\n", static_cast<int>(a), b, tag);
}

TEST(Foo, Simple)
{
	auto foo = Foo_new(1);
	ASSERT_NE(foo, nullptr);

	EXPECT_EQ(3, Foo_f(foo, 1, 1));
	Foo_set_field(foo, 5);
	EXPECT_EQ(7, Foo_f(foo, 1, 1));
	const C_SomeObserver obs = {
		new int(17),
		c_delete_int,
		c_simple_cb,
	};
	Foo_call_me(&obs);
	Foo_delete(foo);
}

int main(int argc, char *argv[])
{
    ::testing::InitGoogleTest(&argc, argv);
    ::testing::GTEST_FLAG(throw_on_failure) = true;
    return RUN_ALL_TESTS();
}
