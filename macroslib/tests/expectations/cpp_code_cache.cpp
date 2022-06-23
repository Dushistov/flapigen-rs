r#"
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

>>> end of file: "rust_vec_u8.h" <<<
"#;
