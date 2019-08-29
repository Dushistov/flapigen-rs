r##"#pragma once

//for assert
#include <cassert>
//for std::abort
#include <cstdlib>
//for std::move
#include <utility>
//for std::conditional
#include <type_traits>

#include "rust_str.h"

#include "c_BtAddr.h"

namespace org_examples {

template<bool>
class BtAddrWrapper;
using BtAddr = BtAddrWrapper<true>;
using BtAddrRef = BtAddrWrapper<false>;


template<bool OWN_DATA>
class BtAddrWrapper {
public:"##;
