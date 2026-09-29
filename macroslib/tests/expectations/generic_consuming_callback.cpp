@@expect {"after":"\n\n\n    stati","before":"namespace org_examples {\n\n","file":"Completioni32.hpp","kind":"between"}
class Completioni32 {
public:
    virtual ~Completioni32() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(int32_t result) noexcept = 0;
@@end

@@expect {"after":"\n\n\n    stati","before":"namespace org_examples {\n\n","file":"CompletionCRustString.hpp","kind":"between"}
class CompletionCRustString {
public:
    virtual ~CompletionCRustString() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(RustString result) noexcept = 0;
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_Completioni32.h","greedy_match":true,"kind":"between"}
struct C_Completioni32 {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_Completioni32_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(int32_t result, void *opaque);

};
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_CompletionCRustString.h","greedy_match":true,"kind":"between"}
struct C_CompletionCRustString {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_CompletionCRustString_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(struct CRustString result, void *opaque);

};
@@end
