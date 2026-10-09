@@expect {"after":"\n\n\n    stati","before":"namespace org_examples {\n\n","file":"Completion.hpp","kind":"between"}
class Completion {
public:
    virtual ~Completion() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(int32_t result) noexcept = 0;
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_Completion.h","greedy_match":true,"kind":"between"}
#include <stdint.h>

struct C_Completion {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_Completion_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(int32_t result, void *opaque);

};
@@end

@@expect {"after":"\n\n    static","before":"delete p;\n    }\n\n    ","file":"Completion.hpp","kind":"between"}
static char c_isCancelled(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const Completion *>(opaque);

        auto ret = pi->isCancelled();
        return static_cast<char>(ret ? 1 : 0);
    }
@@end

@@expect {"after":"\n\n};\n} // namespace org_","before":"return static_cast<char>(ret ? 1 : 0);\n    }\n\n    ","file":"Completion.hpp","kind":"between"}
static void c_onResultReady(int32_t result, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<Completion *>(opaque);

        pi->onResultReady(result);
        delete pi;
    }
@@end

@@expect {"after":"\n\n    static","before":"protected:\n\n    ","file":"Completion.hpp","kind":"between"}
static void c_Completion_deref(void *opaque)
    {
        auto p = static_cast<Completion *>(opaque);
        delete p;
    }
@@end
