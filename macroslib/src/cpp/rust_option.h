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

struct CRustOptionUSize {
    uintptr_t val;
    uint8_t is_some;
};

#ifdef __cplusplus
template<typename T> 
inline T c_option_empty()
{
    T a;
    a.is_some = 0;
    return a;
}
#endif
