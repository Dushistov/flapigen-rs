#pragma once

#include <stdint.h>
#include "rust_str.h"

struct CResultObjectString {
    uint8_t is_ok;
    union {
        void *ok;
        struct CRustString err;
    } data;
};
