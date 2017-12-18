#pragma once

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

struct RustStrView {
    const char *const data;
    uint32_t len;
#ifdef __cplusplus
    std::string as_str() const
    {
        return std::string(data, len);
    }
#endif
};

#ifdef __cplusplus
}
#endif
