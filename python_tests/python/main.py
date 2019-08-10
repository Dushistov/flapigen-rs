#!/usr/bin/python3

from rust_swig_test_python import TestStaticClass, TestEnum, TestClass, TestArc, TestArcMutex, TestBox, Error as TestError

def test_static_methods():
    assert TestStaticClass.hello() == "Hello from rust"
    assert TestStaticClass.format_number(123) == "format_number: 123"
    assert TestStaticClass.format_str("python str") == "format_str: python str"
    assert TestStaticClass.format_string("python string") == "format_string: python string"
    assert TestStaticClass.add(1, 2) == 3
    assert TestStaticClass.get_tuple() == (0, "0")

def test_enum():
    assert TestEnum.A == 0
    assert TestEnum.B == 1
    enum = TestEnum.A
    assert TestStaticClass.reverse_enum(enum) == TestEnum.B
    exception_occured = False
    try:
        # Pass invalid enum value
        TestStaticClass.reverse_enum(2)
    except ValueError as ex:
        exception_occured = True
    assert exception_occured

def test_class():
    test_class = TestClass()
    assert test_class.format() == "TestClass::i: 0"
    test_class.increment()
    assert test_class.format() == "TestClass::i: 1"
    test_class.add(3)
    assert test_class.get() == 4
    # pass this class as an argument
    assert TestStaticClass.call_test_class_format(test_class) == "TestClass::i: 4"
    test_class.add_ref(1)
    assert test_class.get_ref() == 5


def test_options():
    test_class = TestClass()
    assert test_class.maybe_add(1) == 1
    assert test_class.maybe_add(None) == None

def test_arrays():
    assert TestStaticClass.increment_vec([1, 2]) == [2, 3]
    assert TestStaticClass.return_slice([3, 4]) == [3, 4]
    assert TestStaticClass.count_slice_of_objects([TestClass(), TestClass()]) == 2

def test_results():
    TestStaticClass.test_result_ok()
    exception_occured = False
    try:
        TestStaticClass.test_result_err()
    except TestError as ex:
        exception_occured = True
    assert exception_occured

def test_arc():
    arc = TestArc()
    assert str(arc) == "0"
    assert TestArc.to_string_arc(arc) == "0"
    assert TestArc.to_string_ref_arc(arc) == "0"

def test_arc_mutex():
    arc = TestArcMutex()
    assert str(arc) == "0"
    arc.inc()
    assert TestArcMutex.to_string_arc(arc) == "1"
    assert TestArcMutex.to_string_ref_arc(arc) == "1"

def test_box():
    box = TestBox()
    assert str(box) == "0"

print("Testing python API")
test_enum()
test_static_methods()
test_class()
test_options()
test_arrays()
test_results()
test_arc()
test_arc_mutex()
test_box()

print("Testing python API successful")
