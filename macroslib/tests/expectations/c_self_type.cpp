r##"#pragma once

//for (u)intX_t types
#include <stdint.h>

#ifdef __cplusplus
static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * 8,
   "our conversion usize <-> uintptr_t is wrong");
extern "C" {
#endif


    typedef struct WithSelfTypeOpaque WithSelfTypeOpaque;

    WithSelfTypeOpaque *WithSelfType_new();

    void WithSelfType_do_something(const WithSelfTypeOpaque * const self);

    void WithSelfType_delete(const WithSelfTypeOpaque *self);

#ifdef __cplusplus
}
#endif
"##;


r##"#pragma once

//for (u)intX_t types
#include <stdint.h>

#ifdef __cplusplus
static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * 8,
   "our conversion usize <-> uintptr_t is wrong");
extern "C" {
#endif

    void WithoutSelfType_do_nothing();

#ifdef __cplusplus
}
#endif
"##;
