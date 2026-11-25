@@expect {"after":"\n\nnamespace org_examples {","before":"#pragma once\n\n","file":"A.hpp","kind":"between"}
#include <cassert>
#include <cstdlib>
#include <utility>
#include <type_traits>
#include <stdint.h>
#include <cstdio>

#include "c_A.h"
@@end

@@expect {"after":"\n    \n\nprivate:\n    SelfType self_;","before":"~AWrapper() noexcept\n    {\n        free_mem(this->self_);\n    }\n\n\npublic:","file":"A.hpp","kind":"between"}

    void f(int32_t a) noexcept {
        assert(a > 10);
        printf("you call f\n");
        do_f(a);
    }
@@end
