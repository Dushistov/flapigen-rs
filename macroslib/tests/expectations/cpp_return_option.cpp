r##"#pragma once

//for assert
#include <cassert>
//for std::abort
#include <cstdlib>
//for std::move
#include <utility>
//for std::conditional
#include <type_traits>

#include "Boo.hpp"
#include "c_Boo.h"
#include "rust_option.h"
#include <optional>
#include <stdint.h>
#include "ControlItem.hpp"
#include "rust_str.h"
#include <string_view>

#include "c_Foo.h"

namespace org_examples {

template<bool>
class FooWrapper;
using Foo = FooWrapper<true>;
using FooRef = FooWrapper<false>;


template<bool OWN_DATA>
class FooWrapper {"##;

"std::optional<Boo> f1()";

"std::optional<double> f2()";

"std::optional<uint32_t> f3()";

"std::optional<uintptr_t> f4()";

"std::optional<BooRef> f5()";

"std::optional<ControlItem> f6() const noexcept;";

"std::optional<uint64_t> f7()";

"std::optional<std::string_view> f8()";

r#"template<bool OWN_DATA>
    inline std::optional<std::string_view> FooWrapper<OWN_DATA>::f8() const noexcept
    {

        struct CRustOptionCRustStrView ret = Foo_f8(this->self_);
        return (ret.is_some != 0) ? std::optional<std::string_view>(std::string_view{ ret.val.data.data, ret.val.data.len }) : std::optional<std::string_view>();
    }"#;

"std::optional<RustString> f9()";

"std::optional<bool> f10()";

r#"template<bool OWN_DATA>
    inline std::optional<ControlItem> FooWrapper<OWN_DATA>::f6() const noexcept
    {

        struct CRustOptionu32 ret = Foo_f6(this->self_);
        return (ret.is_some != 0) ? std::optional<ControlItem>(static_cast<ControlItem>(ret.val.data)) : std::optional<ControlItem>();
    }"#;

"struct CRustOptionu32 Foo_f6(const FooOpaque * const self);";
