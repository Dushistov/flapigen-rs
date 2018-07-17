#pragma once

#include <stdint.h>

#include "rust_str.h"

struct CRustOptionF64 {
    double val;
    uint8_t is_some;
};

struct CRustOptionU32 {
    uint32_t val;
    uint8_t is_some;
};

struct CRustOptionI64 {
    int64_t val;
    uint8_t is_some;
};

struct CRustOptionU64 {
    uint64_t val;
    uint8_t is_some;
};

struct CRustOptionUSize {
    uintptr_t val;
    uint8_t is_some;
};

struct CRustOptionStr {
    RustStrView val;
    uint8_t is_some;
};

#ifdef __cplusplus
#include <cstring>

template <typename T>
inline T c_option_empty()
{
    T a;
    std::memset(&a.val, 0, sizeof(a.val));
    a.is_some = 0;
    return a;
}
#endif
