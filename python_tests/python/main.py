#!/usr/bin/python3

import rust_swig_test_python

print("rust_swig_test_python module imported.")
print("Doc: ", rust_swig_test_python.__doc__)
print("Members: ", dir(rust_swig_test_python))

print("TestEnum variants", dir(rust_swig_test_python.TestEnum))
rust_swig_test_python.TestClass.print_hello()
