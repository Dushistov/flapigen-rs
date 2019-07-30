#!/usr/bin/env python3

import subprocess
import os
import sys
import re
import time
import shutil

JNI_TESTS = "jni_tests"
CPP_TESTS = "cpp_tests"
ANDROID_TESTS = "android-example"
UNIT_TESTS = "unit_tests"
DOC_TESTS = "doc_tests"
RELEASE = "release"
DEBUG = "debug"

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
    subprocess.check_call(["java", "-Xcheck:jni", "-verbose:jni", "-ea", "-Djava.library.path=" + target_dir,
                           "-cp", "Test.jar", "com.example.Main"],
                          cwd=jar_dir, shell=use_shell)

@show_timing
def build_jar(java_dir, java_native_dir, use_shell):
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

@show_timing
def run_jni_tests(use_shell, test_cfg):
    print("run_jni_tests begin: cwd %s" % os.getcwd())
    sys.stdout.flush()
    for cfg in test_cfg:
        if cfg == DEBUG:
            subprocess.check_call(["cargo", "build", "-v", "--package", "rust_swig_test_jni"], shell=False)
        elif cfg == RELEASE:
            subprocess.check_call(["cargo", "build", "-v", "--release", "--package", "rust_swig_test_jni"], shell=False)
        else:
            raise Exception("Fatal Error: Unknown cfg %s" % cfg)

    java_dir = str(os.path.join(os.getcwd(), "jni_tests", "java", "com", "example"))
    purge(java_dir, ".*\.class$")
    java_native_dir = str(os.path.join(os.getcwd(), "jni_tests", "java", "com", "example", "rust"))
    if not os.path.exists(java_native_dir):
        os.makedirs(java_native_dir)
    else:
        purge(java_native_dir, ".*\.class$")
    jar_dir = build_jar(java_dir, java_native_dir, use_shell)

    for cfg in test_cfg:
        target_dir = os.path.join(find_dir("target", "jni_tests"), cfg)
        run_jar(target_dir, jar_dir, use_shell)

def calc_cmake_generator():
    if sys.platform == 'win32':
        cmake_generator = "Visual Studio 15 2017"
        if os.getenv('platform') == "x64":
            cmake_generator = "Visual Studio 15 2017 Win64"
    else:
        cmake_generator = "Unix Makefiles"
    return cmake_generator

def find_target_path_in_cmakecache(cmake_build_dir):
    with open(os.path.join(cmake_build_dir, "CMakeCache.txt")) as search:
        for line in search:
            line = line.rstrip()
            if line.startswith('TARGET_PATH:PATH='):
                line = line.replace('TARGET_PATH:PATH=', '')
                return line
    return None

@show_timing
def build_cpp_example():
    cmake_generator = calc_cmake_generator()
    dir_path = os.path.join("cpp-example", "cpp-part")
    cmake_build_dir = os.path.join(dir_path, "build")
    if not os.path.exists(cmake_build_dir):
        os.makedirs(cmake_build_dir)
    cmake_args = ["cmake", "-G", cmake_generator, "-DCMAKE_BUILD_TYPE:String=Release"]
    subprocess.check_call(cmake_args + [".."], cwd = str(cmake_build_dir))
    if sys.platform == 'win32' or sys.platform == 'win64':
        subprocess.check_call(["cmake", "--build", ".", "--config", RELEASE], cwd = str(cmake_build_dir))
        # hack to force dll to work
        target_path = find_target_path_in_cmakecache(cmake_build_dir)
        dll_name = "cpp_example_rust_part.dll"
        shutil.copy(os.path.join(target_path, "release", dll_name),
                    os.path.join(cmake_build_dir, "Release", dll_name))
        subprocess.check_call(["app"], cwd = str(os.path.join(cmake_build_dir, "Release")), shell = True)
    else:
        subprocess.check_call(["cmake", "--build", "."], cwd = str(cmake_build_dir))
        subprocess.check_call(["./app"], cwd = str(cmake_build_dir))


@show_timing
def build_cpp_code_with_cmake(test_cfg, cmake_build_dir, addon_params):
    cmake_generator = calc_cmake_generator()
    cmake_args = ["cmake", "-G", cmake_generator] + addon_params
    if sys.platform == 'win32' or sys.platform == 'win64':
        if os.path.exists(cmake_build_dir):
            #at there is problem with multiply build directories for one source tree
            #TODO: move generated header from source tree to build tree
            print("%s exists, we removing it" % cmake_build_dir)
            shutil.rmtree(cmake_build_dir)
        os.makedirs(cmake_build_dir)
        subprocess.check_call(cmake_args + [".."], cwd = str(cmake_build_dir))
        os.environ["CTEST_OUTPUT_ON_FAILURE"] = "1"
        for cfg in test_cfg:
            subprocess.check_call(["cmake", "--build", ".", "--config", cfg], cwd = str(cmake_build_dir))
            subprocess.check_call(["cmake", "--build", ".", "--target", "RUN_TESTS", "--config", cfg],
                                  cwd = str(cmake_build_dir))
    else:
        for cfg in test_cfg:
            cur_cmake_args = cmake_args[:]
            cur_cmake_build_dir = cmake_build_dir
            if cfg == RELEASE:
                cur_cmake_args.append("-DCMAKE_BUILD_TYPE:String=Release")
            elif cfg == DEBUG:
                cur_cmake_args.append("-DCMAKE_BUILD_TYPE:String=Debug")
                cur_cmake_build_dir = cur_cmake_build_dir + "_dbg"
            print("cur_cmake_build_dir %s" % cur_cmake_build_dir)
            if os.path.exists(cur_cmake_build_dir):
                #at there is problem with multiply build directories for one source tree
                #TODO: move generated header from source tree to build tree
                print("%s exists, we removing it" % cur_cmake_build_dir)
                shutil.rmtree(cur_cmake_build_dir)

            os.makedirs(cur_cmake_build_dir)
            subprocess.check_call(cur_cmake_args + [".."], cwd = str(cur_cmake_build_dir))

            subprocess.check_call(["cmake", "--build", "."], cwd = str(cur_cmake_build_dir))
            subprocess.check_call(["ctest", "--output-on-failure"], cwd = str(cur_cmake_build_dir))
            if sys.platform == "linux" or sys.platform == "linux2":
                subprocess.check_call(["valgrind", "--error-exitcode=1", "--leak-check=full",
                                       "--show-leak-kinds=all", "--errors-for-leak-kinds=all",
                                       "--suppressions=../../valgrind.supp",
                                       "./c++-rust-swig-test"], cwd = str(cur_cmake_build_dir))

@show_timing
def build_cargo_docs():
    print("build docs")
    subprocess.check_call(["cargo", "doc", "-v", "--package", "rust_swig"])

@show_timing
def build_for_android(is_windows):
    gradle_cmd = "gradlew.bat" if is_windows else "./gradlew"

    for d in ["android-example", "android-tests"]:
        subprocess.check_call(["cargo", "test", "--target=arm-linux-androideabi", "--release"], cwd=os.path.join(os.getcwd(), d))
        subprocess.check_call([gradle_cmd, "build"], cwd=os.path.join(os.getcwd(), d))
        subprocess.check_call([gradle_cmd, "connectedAndroidTest"], cwd=os.path.join(os.getcwd(), d))

@show_timing
def run_unit_tests(test_cfg, test_set):
    for cfg in test_cfg:
        cmd_base = ["cargo", "test", "-v", "-p", "rust_swig"]
        if CPP_TESTS in test_set:
            cmd_base.append("-p")
            cmd_base.append("rust_swig_test_cpp")
            cmd_base.append("cpp-example-rust-part")
        if JNI_TESTS in test_set:
            cmd_base.append("-p")
            cmd_base.append("rust_swig_test_jni")
        if cfg == DEBUG:
            pass
        elif cfg == RELEASE:
            cmd_base.append("--release")
        else:
            raise Exception("Fatal Error: Unknown cfg %s" % cfg)
        subprocess.check_call(cmd_base)

@show_timing
def main():
    print("Starting build and test: %s" % sys.version)
    sys.stdout.flush()

    test_cfg = set([RELEASE, DEBUG])
    test_set = set([JNI_TESTS, CPP_TESTS, ANDROID_TESTS, UNIT_TESTS, DOC_TESTS])
    for arg in sys.argv[1:]:
        if arg == "--skip-android-tests":
            test_set.remove(ANDROID_TESTS)
        elif arg == "--java-only-tests":
            test_set = set([JNI_TESTS])
        elif arg == "--cpp-only-tests":
            test_set = set([CPP_TESTS])
        elif arg == "--skip-java-tests":
            test_set.remove(JNI_TESTS)
        elif arg == "--android-only-tests":
            test_set = set([ANDROID_TESTS])
        else:
            raise Exception("Fatal Error: unknown option: %s" % arg)

    has_jdk = "JAVA_HOME" in os.environ
    if (JNI_TESTS in test_set) and (not has_jdk):
        raise Exception("Fatal error JAVA_HOME not defined, so it is impossible to run %s" % JNI_TESTS)

    has_android_sdk = ("ANDROID_SDK" in os.environ) or ("ANDROID_HOME" in os.environ)
    if (ANDROID_TESTS in test_set) and (not has_android_sdk):
        raise Exception("Fatal error ANDROID_* not defined, so it is impossible to run %s" % ANDROID_TESTS)

    # becuase of http://bugs.python.org/issue17023
    is_windows = os.name == 'nt'
    use_shell = is_windows

    print("test_set %s" % test_set)
    sys.stdout.flush()

    if DOC_TESTS in test_set:
        build_cargo_docs()

    print("start tests\n macrolib tests")
    if UNIT_TESTS in test_set:
        run_unit_tests(test_cfg, test_set)
    if JNI_TESTS in test_set:
        run_jni_tests(use_shell, test_cfg)

    if CPP_TESTS in test_set:
        print("Check cmake version")
        subprocess.check_call(["cmake", "--version"], shell = False)
        build_cpp_example()
        build_cpp_code_with_cmake(test_cfg, os.path.join("cpp_tests", "c++", "build"), [])
        purge(os.path.join("cpp_tests", "c++", "rust_interface"), ".*\.h.*$")
        build_cpp_code_with_cmake(test_cfg, os.path.join("cpp_tests", "c++", "build_with_boost"), ["-DUSE_BOOST:BOOL=ON"])

    if ANDROID_TESTS in test_set:
        build_for_android(is_windows)

if __name__ == "__main__":
    main()
