@@expect {"after":"\n\n    virtua","before":"virtual ~SomeObserver() noexcept {}\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual bool onStateChanged(int32_t a0, bool a1) const noexcept = 0;
@@end

@@expect {"after":"\n\n\n    stati","before":"virtual bool onStateChanged(int32_t a0, bool a1) const noexcept = 0;\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual void onStateChangedWithoutArgs() const noexcept = 0;
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_SomeObserver.h","greedy_match":true,"kind":"between"}
#include <stdint.h>

struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);

    char (*onStateChanged)(int32_t a0, char a1, void *opaque);

    void (*onStateChangedWithoutArgs)(void *opaque);

};
@@end
