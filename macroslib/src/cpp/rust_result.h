#pragma once

#include <stdint.h>
#include "rust_str.h"
#include "rust_vec.h"

struct CResultObjectString {
    uint8_t is_ok;
    union {
        void *ok;
        struct CRustString err;
    } data;
};

struct CResultCRustForeignVecString {
    uint8_t is_ok;
    union {
        struct CRustForeignVec ok;
        struct CRustString err;
    } data;
};
