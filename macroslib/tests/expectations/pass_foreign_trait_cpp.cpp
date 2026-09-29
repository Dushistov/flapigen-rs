@@expect {"after":"\n\nprivate:\n ","before":"            std::abort();\n        }\n    }\n\n    ","file":"TestPassInterface.hpp","kind":"between"}
static int32_t use_interface(Interface a, int32_t b) noexcept;
@@end

@@expect {"after":"\n\n} // names","before":"template<bool OWN_DATA>\n    ","file":"TestPassInterface.hpp","kind":"between"}
inline int32_t TestPassInterfaceWrapper<OWN_DATA>::use_interface(Interface a, int32_t b) noexcept
    {

        int32_t ret = TestPassInterface_use_interface(a.release(), b);
        return ret;
    }
@@end
