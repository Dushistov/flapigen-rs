#include <cstdlib>
#include <iostream>

#include "score_report.hpp"
#include "Foo.hpp"
#include "ScoreAdjustment.hpp"
#include "SharedCounter.hpp"
using namespace rust;

// ANCHOR: plain_class_cpp_implementation
int adjusted_score(const ScoreAdjustment& adjustment, int base)
{
    return adjustment.apply(base);
}
// ANCHOR_END: plain_class_cpp_implementation

int main()
{
    // ANCHOR: call_rust
    Foo foo(5);
    int res = foo.f(1, 2);
    // ANCHOR_END: call_rust
    if (res == 8) {
        std::cout << "All works fine\n";
    }
    else {
        std::cout << "Something really BAD!!!\n";
        return EXIT_FAILURE;
    }

    // ANCHOR: smart_ptr_copy_cpp_use
    SharedCounter counter;
    SharedCounter second_handle = counter; // clones Rc, not SharedCounter
    second_handle.increment();
    SharedCounter assigned;
    assigned = counter;
    assigned.increment();
    if (counter.value() != 2 || second_handle.value() != 2) {
        return EXIT_FAILURE;
    }
    // ANCHOR_END: smart_ptr_copy_cpp_use

    // ANCHOR: plain_class_cpp_use
    ScoreAdjustment adjustment(3);
    if (adjusted_score(adjustment, 5) != 8) {
        return EXIT_FAILURE;
    }
    // ANCHOR_END: plain_class_cpp_use
    return EXIT_SUCCESS;
}
