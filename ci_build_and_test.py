#!/usr/bin/env python3

import subprocess
import os
import sys
import re
import time

def show_timing(function):
    def _wrapper(*args, **kwargs):
        start = time.time()
        ret = function(*args, **kwargs)
        elapsed = (time.time() - start)
        print("%s elapsed time: %f" % (function.__name__, elapsed))
        return ret
    return _wrapper

def purge(dir, pattern):
    for f in os.listdir(dir):
        if re.search(pattern, f):
            #            print("removing %s" % os.path.join(dir, f))
            os.remove(os.path.join(dir, f))

def find_dir(dir_name, start_dir):
    origin_cwd = os.getcwd()
    os.chdir(start_dir)
    dir = os.getcwd()
    last_dir = ''
    while last_dir != dir:
        dir = os.getcwd()
        if dir_name in [o for o in os.listdir(dir) if os.path.isdir(os.path.join(dir, o))]:
            ret = os.path.join(dir, dir_name)
            os.chdir(origin_cwd)
            return ret
        os.chdir('..')
        last_dir = os.getcwd()
    os.chdir(origin_cwd)
    raise Exception("Can not find %s" % dir_name)

@show_timing
def run_jar(target_dir, jar_dir, use_shell):
    #unimplemented
    return
    subprocess.check_call(["java", "-Xcheck:jni", "-verbose:jni", "-ea", "-Djava.library.path=" + target_dir,
                           "-cp", "Test.jar", "com.example.Main"],
                          cwd=jar_dir, shell=use_shell)

@show_timing
def build_jar(java_dir, java_native_dir, use_shell):
    #unimplemented
    return
    generated_java = [os.path.join("rust", f) for f in os.listdir(java_native_dir)
                      if os.path.isfile(os.path.join(java_native_dir, f)) and f.endswith(".java")]
    javac_cmd_args = ["javac", "Main.java"]
    javac_cmd_args.extend(generated_java)

    subprocess.check_call(javac_cmd_args,
                          cwd=java_dir, shell=use_shell)

    jar_dir = str(os.path.join(os.getcwd(), "jni_tests", "java"))
    purge(java_dir, ".*\.jar$")
    subprocess.check_call(["jar", "cfv", "Test.jar", "com"], cwd=jar_dir, shell=use_shell)
    return jar_dir

def has_option(option):
    return any(option == s for s in sys.argv[1:])

@show_timing
def run_jni_tests(use_shell, fast_run):
    print("run_jni_tests begin: cwd %s" % os.getcwd())
    sys.stdout.flush()
    subprocess.check_call(["cargo", "build", "-v", "--package", "rust_swig_test_jni"], shell=False)
    java_dir = str(os.path.join(os.getcwd(), "jni_tests", "java", "com", "example"))
    purge(java_dir, ".*\.class$")
    java_native_dir = str(os.path.join(os.getcwd(), "jni_tests", "java", "com", "example", "rust"))
    if not os.path.exists(java_native_dir):
        os.makedirs(java_native_dir)
    else:
        purge(java_native_dir, ".*\.class$")
    jar_dir = build_jar(java_dir, java_native_dir, use_shell)
    target_dir = os.path.join(find_dir("target", "jni_tests"), "debug")
    run_jar(target_dir, jar_dir, use_shell)
    if fast_run:
        return
    subprocess.check_call(["cargo", "build", "-v", "--release", "--package", "rust_swig_test_jni"], shell=False)
    target_dir = os.path.join(find_dir("target", "jni_tests"), "release")
    run_jar(target_dir, jar_dir, use_shell)

@show_timing
def build_cpp_code_with_cmake(cmake_build_dir, addon_params):
    #unimplemented
    return
    if sys.platform == 'win32':
        cmake_generator = "Visual Studio 14 2015"
        if os.getenv('platform') == "x64":
            cmake_generator = "Visual Studio 14 2015 Win64"
    else:
        cmake_generator = "Unix Makefiles"
    cmake_args = ["cmake", "-G", cmake_generator, "-DCMAKE_BUILD_TYPE=RelWithDebInfo"] \
                                                                       + addon_params + [".."]
    if not os.path.exists(cmake_build_dir):
        os.makedirs(cmake_build_dir)
        subprocess.check_call(cmake_args,
                              cwd = str(cmake_build_dir))
    subprocess.check_call(["cmake", "--build", "."], cwd = str(cmake_build_dir))
    if sys.platform == 'win32' or sys.platform == 'win64':
        os.environ["CTEST_OUTPUT_ON_FAILURE"] = "1"
        subprocess.check_call(["msbuild", "RUN_TESTS.vcxproj"], cwd = str(cmake_build_dir))
    else:
        subprocess.check_call(["ctest", "--output-on-failure"], cwd = str(cmake_build_dir))
        if sys.platform == "linux":
            subprocess.check_call(["valgrind", "--error-exitcode=1", "--leak-check=full",
                                   "--show-leak-kinds=all", "--errors-for-leak-kinds=all",
                                   "--suppressions=../../valgrind.supp",
                                   "./c++-rust-swig-test"], cwd = str(cmake_build_dir))

@show_timing
def build_cargo_docs():
    print("build docs")
    subprocess.check_call(["cargo", "doc", "-v", "--package", "rust_swig"])

@show_timing
def build_for_android():
    #unimplemented
    return
    gradle_cmd = "gradlew.bat" if is_windows else "./gradlew"
    subprocess.check_call([gradle_cmd, "build"], cwd=os.path.join(os.getcwd(), "android-example"))

@show_timing
def run_unit_tests(fast_run, has_jdk, skip_cpp_tests):
    cmd_base = ["cargo", "test", "-v", "-p", "rust_swig"]
    if not skip_cpp_tests:
        cmd_base.append("-p")
        cmd_base.append("rust_swig_test_cpp")
    if has_jdk:
        cmd_base.append("-p")
        cmd_base.append("rust_swig_test_jni")
    subprocess.check_call(cmd_base)
    if not fast_run:
        cmd_base.append("--release")
        subprocess.check_call(cmd_base)

@show_timing
def main():
    print("Starting build and test")
    sys.stdout.flush()

    has_jdk = "JAVA_HOME" in os.environ
    print("has_jdk %s" % has_jdk)
    has_android_sdk = ("ANDROID_SDK" in os.environ) or ("ANDROID_HOME" in os.environ)
    print("has_android_sdk %s" % has_android_sdk)
    skip_android_test = has_option("--skip-android-tests")
    print("skip_android_test %s" % skip_android_test)
    #becuase of http://bugs.python.org/issue17023
    is_windows = os.name == 'nt'
    use_shell = is_windows
    print("use_shell %s" % use_shell)
    fast_run = has_option("--fast-run")
    print("fast_run %s" % fast_run)
    skip_cpp_tests = sys.platform == 'win32' and os.getenv("TARGET") == "nightly-x86_64-pc-windows-gnu"
    print("skip_cpp_tests %s" % skip_cpp_tests)
    java_only = has_option("--java-only-tests")
    print("java_only %s" % java_only)
    sys.stdout.flush()

    build_cargo_docs()
    print("start tests\n macrolib tests")
    if not (has_jdk and java_only):
        run_unit_tests(fast_run, has_jdk, skip_cpp_tests)
    if has_jdk:
        run_jni_tests(use_shell, fast_run)
        if java_only:
            return

    if not skip_cpp_tests:
        print("Check cmake version")
        subprocess.check_call(["cmake", "--version"], shell = False)
        subprocess.check_call(["cargo", "test", "-v", "--package", "rust_swig_test_cpp"], shell = False)
        if not fast_run:
            subprocess.check_call(["cargo", "test", "-v", "--release", "--package", "rust_swig_test_cpp"], shell = False)
        build_cpp_code_with_cmake(os.path.join("c++_tests", "c++", "build"), [])
        purge(os.path.join("c++_tests", "c++", "rust_interface"), ".*\.h.*$")
        build_cpp_code_with_cmake(os.path.join("c++_tests", "c++", "build_with_boost"), ["-DUSE_BOOST:BOOL=ON"])

    if has_android_sdk and (not skip_android_test):
        build_for_android()

if __name__ == "__main__":
    main()
