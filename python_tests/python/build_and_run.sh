#!/bin/bash

cargo build
cp ../../target/debug/librust_swig_test_python.so rust_swig_test_python.so
python3 main.py
