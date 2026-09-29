@@expect {"after":"\n\nprivate:\n ","before":"            std::abort();\n        }\n    }\n\n    ","file":"ClassWithCallbacks.hpp","kind":"between"}
void f1(std::unique_ptr<SomeObserver> cb) noexcept;
@@end

@@expect {"after":"\n\n\n    stati","before":"virtual ~SomeObserver() noexcept {}\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual void onStateChanged(std::string_view a0) const noexcept = 0;
@@end

@@expect {"after":"\n    {\n     ","before":"delete p;\n    }\n\n    ","file":"SomeObserver.hpp","kind":"between"}
static void c_onStateChanged(struct CRustStrView a0, void *opaque)
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_SomeObserver.h","greedy_match":true,"kind":"between"}
struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);

    void (*onStateChanged)(struct CRustStrView a0, void *opaque);

};
@@end
