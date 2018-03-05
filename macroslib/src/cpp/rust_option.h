#pragma once

#include <stdint.h>

struct CRustOptionF64 {
    double val;
    uint8_t is_some;
};

struct CRustOptionU32 {
    uint32_t val;
    uint8_t is_some;
};
