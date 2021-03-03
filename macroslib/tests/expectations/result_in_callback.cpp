r##"#pragma once

#include <cassert>
#include <memory> //for std::unique_ptr

#include "rust_str.h"
#include <string_view>
#include "Error.hpp"
#include "c_Error.h"
#include "rust_resultCRustStrView4232mut3232c_void.h"
#include <variant>

#include "c_Foo.h"

namespace org_examples {

class Foo {
public:
    virtual ~Foo() noexcept {}

    virtual std::variant<std::string_view, Error> unpack(std::string_view x) noexcept = 0;"##;
