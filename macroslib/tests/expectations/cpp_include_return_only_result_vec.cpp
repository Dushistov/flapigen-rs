r##"#pragma once

//for assert
#include <cassert>
//for std::abort
#include <cstdlib>
//for std::move
#include <utility>
//for std::conditional
#include <type_traits>

#include "LatLon.h"
#include "WindVelocity.hpp"
#include "c_WindVelocity.h"
#include "RustForeignVecWindVelocity.h"
#include "rust_vec.h"
#include "RemoteApiError.hpp"
#include "c_RemoteApiError.h"
#include "rust_resultCRustForeignVec4232mut3232c_void.h"
#include <variant>

#include "c_Weather.h"

namespace org_examples {

template<bool>
class WeatherWrapper;
using Weather = WeatherWrapper<true>;
using WeatherRef = WeatherWrapper<false>;


template<bool OWN_DATA>
class WeatherWrapper {
public:
    using value_type = WeatherWrapper<true>;
    friend class WeatherWrapper<true>;
    friend class WeatherWrapper<false>;

    static std::variant<RustForeignVecWindVelocity, RemoteApiError> get_wind_for(struct CLatLon pos) noexcept;

};"##;

r#"template<bool OWN_DATA>
    inline std::variant<RustForeignVecWindVelocity, RemoteApiError> WeatherWrapper<OWN_DATA>::get_wind_for(struct CLatLon pos) noexcept
    {

        struct CRustResultCRustForeignVec4232mut3232c_void ret = Weather_get_wind_for(pos);
        return ret.is_ok != 0 ?
              std::variant<RustForeignVecWindVelocity, RemoteApiError> { RustForeignVecWindVelocity{ret.data.ok} } :
              std::variant<RustForeignVecWindVelocity, RemoteApiError> { RemoteApiError(static_cast<RemoteApiErrorOpaque *>(ret.data.err)) };
    }"#;
