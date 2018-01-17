#!/usr/bin/env python3

import subprocess
import os
import sys

print("Starting build")
sys.stdout.flush()

has_jdk = "JAVA_HOME" in os.environ
print("has_jdk %s" % has_jdk)

subprocess.check_call(["cargo", "build"], cwd = "macroslib", shell = False)
if has_jdk:
    subprocess.check_call(["cargo", "build"], cwd = "jni_tests", shell = False)

#to create directory target
subprocess.check_call(["cargo", "check"], cwd = "c++_tests", shell = False)
cmake_build_dir = os.path.join("c++_tests", "c++", "build")

if sys.platform == 'win32':
    cmake_generator = "Visual Studio 14 2015"
    if os.getenv('platform') == "x64":
        cmake_generator = "Visual Studio 14 2015 Win64"
else:
    cmake_generator = "Unix Makefiles"

skip_cpp_tests = sys.platform == 'win32' and os.getenv("TARGET") == "nightly-x86_64-pc-windows-gnu"
if not skip_cpp_tests:
    if not os.path.exists(cmake_build_dir):
        os.makedirs(cmake_build_dir)
    subprocess.check_call(["cmake", "-G", cmake_generator, "-DCMAKE_BUILD_TYPE=RelWithDebInfo", ".."], cwd = str(cmake_build_dir))
    subprocess.check_call(["cmake", "--build", "."], cwd = str(cmake_build_dir))

    
print("Starting tests")
sys.stdout.flush()

subprocess.check_call(["cargo", "test"], cwd = "macroslib", shell = False)
subprocess.check_call(["cargo", "test", "--release"], cwd = "macroslib", shell = False)

if has_jdk:
    subprocess.check_call(["cargo", "test"], cwd = "jni_tests", shell = False)
    subprocess.check_call(["python", "run_tests.py", "--skip-android-test"], cwd = "jni_tests", shell = False)    

if not skip_cpp_tests:
    if sys.platform == 'win32':
        subprocess.check_call(["msbuild", "RUN_TESTS.vcxproj"], cwd = str(cmake_build_dir))
    else:
        subprocess.check_call(["ctest", "-V"], cwd = str(cmake_build_dir))
