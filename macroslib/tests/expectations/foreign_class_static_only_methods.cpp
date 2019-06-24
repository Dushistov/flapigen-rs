r##"namespace org_examples {

template<bool>
class UtilsWrapper;
using Utils = UtilsWrapper<true>;
using UtilsRef = UtilsWrapper<false>;

//This is documentation comment
template<bool OWN_DATA>
class UtilsWrapper {
public:
    using value_type = UtilsWrapper<true>;
    friend class UtilsWrapper<true>;
    friend class UtilsWrapper<false>;
    //Very important function
    static int32_t f(int32_t a0) noexcept;

};


    template<bool OWN_DATA>
    inline int32_t UtilsWrapper<OWN_DATA>::f(int32_t a0) noexcept
    {

        int32_t ret = Utils_f(a0);
        return ret;
    }

}"##;
