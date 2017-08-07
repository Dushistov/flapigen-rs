#!/usr/bin/env python3

import subprocess
import os
import re
import glob

def purge(dir, pattern):
    for f in os.listdir(dir):
        if re.search(pattern, f):
#            print("removing %s" % os.path.join(dir, f))
            os.remove(os.path.join(dir, f))

def find_dir(dir_name):
    origin_cwd = os.getcwd()
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
    raise Exception("Can not find %s" % dir_name)

java_dir = str(os.path.join(os.getcwd(), "java/com/example"))
purge(java_dir, ".*\.class$")
java_native_dir = str(os.path.join(os.getcwd(), "java/com/example/rust"))
if not os.path.exists(java_native_dir):
    os.makedirs(java_native_dir)
else:
    purge(java_dir, ".*\.class$")

subprocess.check_call(["cargo", "test"],
                      cwd = str(os.path.join(os.path.abspath('..'), "macroslib")),
                      shell=False)

subprocess.check_call(["cargo", "test"], shell=False)
subprocess.check_call(["cargo", "build"], shell=False)

#becuase of http://bugs.python.org/issue17023
use_shell = os.name == 'nt'


generated_java = [os.path.join("rust", f) for f in os.listdir(java_native_dir)
                  if os.path.isfile(os.path.join(java_native_dir, f)) and f.endswith(".java")]
javac_cmd_args = ["javac", "Main.java"]
javac_cmd_args.extend(generated_java)
    
subprocess.check_call(javac_cmd_args,
                      cwd=java_dir, shell=use_shell)

jar_dir = str(os.path.join(os.getcwd(), "java"))
purge(java_dir, ".*\.jar$")
subprocess.check_call(["jar", "cfv", "Test.jar", "com"], cwd=jar_dir, shell=use_shell)

target_dir = os.path.join(find_dir("target"), "debug")
subprocess.check_call(["java", "-verbose:jni", "-ea", "-Djava.library.path=" + target_dir,
                       "-cp", "Test.jar", "com.example.Main"],
                      cwd=jar_dir, shell=use_shell)

