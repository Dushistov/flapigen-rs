#include <cstdint>
#include <cstdio>

extern "C" {
uint32_t add(uint32_t a, uint32_t b);
}

int main()
{
	printf("2 + 2 = %u\n", static_cast<unsigned>(add(2, 2)));
}
