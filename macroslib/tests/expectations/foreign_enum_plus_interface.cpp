@@expect {"after":"\n\n\n    stati","before":"virtual ~ControlStateObserver() noexcept {}\n\n    ","file":"ControlStateObserver.hpp","kind":"between"}
virtual void onSessionUpdate(ControlItem item, bool is_ok) const noexcept = 0;
@@end

@@expect {"after":"\n} // namesp","before":"namespace org_examples {\n\n","file":"ControlItem.hpp","kind":"between"}
enum ControlItem {
GNSS = 0,
GPS_PROVIDER = 1
};
@@end

@@expect {"after":"\n","before":"#pragma once\n\n","file":"c_ControlStateObserver.h","greedy_match":true,"kind":"between"}
#include <stdint.h>

struct C_ControlStateObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_ControlStateObserver_deref)(void *opaque);

    void (*onSessionUpdate)(uint32_t item, char is_ok, void *opaque);

};
@@end

@@expect {"after":"\n\n};\n} // na","before":"delete p;\n    }\n\n    ","file":"ControlStateObserver.hpp","kind":"between"}
static void c_onSessionUpdate(uint32_t item, char is_ok, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const ControlStateObserver *>(opaque);

        pi->onSessionUpdate(static_cast<ControlItem>(item), (is_ok != 0));
    }
@@end
