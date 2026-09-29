@@expect {"after":"\n\n    virtua","before":"virtual ~SomeObserver() noexcept {}\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual void onStateChanged1(int32_t a0, bool a1) const noexcept = 0;
@@end

@@expect {"after":"\n\n    static","before":"delete p;\n    }\n\n    ","file":"SomeObserver.hpp","kind":"between"}
static void c_onStateChanged1(int32_t a0, char a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChanged1(a0, (a1 != 0));
    }
@@end

@@expect {"after":"\n\n    char (","before":"void (*C_SomeObserver_deref)(void *opaque);\n\n    ","file":"c_SomeObserver.h","kind":"between"}
void (*onStateChanged1)(int32_t a0, char a1, void *opaque);
@@end

@@expect {"after":"\n\n\n    stati","before":"virtual void onStateChanged1(int32_t a0, bool a1) const noexcept = 0;\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual bool onStateChanged2(bool a0, double a1) const noexcept = 0;
@@end

@@expect {"after":"\n\n};\n} // namespace org_","before":"pi->onStateChanged1(a0, (a1 != 0));\n    }\n\n    ","file":"SomeObserver.hpp","kind":"between"}
static char c_onStateChanged2(char a0, double a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        auto ret = pi->onStateChanged2((a0 != 0), a1);
        return static_cast<char>(ret ? 1 : 0);
    }
@@end

@@expect {"after":"\n\n};\n","before":"void (*onStateChanged1)(int32_t a0, char a1, void *opaque);\n\n    ","file":"c_SomeObserver.h","kind":"between"}
char (*onStateChanged2)(char a0, double a1, void *opaque);
@@end
