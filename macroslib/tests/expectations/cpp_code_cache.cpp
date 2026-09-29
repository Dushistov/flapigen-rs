@@expect {"before":"    uintptr_t capacity;\n};\n\n#ifdef __cplusplus\n} // extern \"C\" {\n#endif\n","file":"rust_vec_u8.h","kind":"between"}

#ifdef __cplusplus
extern "C" {
#endif
void CRustVecu8_free(struct CRustVecu8 v);
#ifdef __cplusplus
} // extern "C" {
#endif

#ifdef __cplusplus

#include "rust_vec_impl.hpp"

namespace org_examples {
using RustVecu8 = RustVec<CRustVecu8, CRustVecu8_free>;
}

#endif

@@end
