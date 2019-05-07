cargo build
copy ..\..\target\debug\rust_swig_test_python.dll rust_swig_test_python.pyd
python main.py
