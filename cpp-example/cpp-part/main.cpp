#include <cstdlib>
#include <iostream>

#include "Foo.hpp"
using namespace rust;

int main()
{
    Foo foo(5);
    int res = foo.f(1, 2);
    if (res == 8) {
        std::cout << "All works fine\n";
    }
    else {
        std::cout << "Something really BAD!!!\n";
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
