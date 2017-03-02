#!/usr/bin/env python3

import subprocess
import os
import re
import glob
from pathlib import Path

def purge(dir, pattern):
    for f in os.listdir(dir):
        if re.search(pattern, f):
#            print("removing %s" % os.path.join(dir, f))
            os.remove(os.path.join(dir, f))

def find_dir(dir_name):
    dir = os.getcwd()
    last_dir = ''
    while last_dir != dir:
        dir = os.getcwd()
        if dir_name in [o for o in os.listdir(dir) if os.path.isdir(os.path.join(dir, o))]:
            return os.path.join(dir, dir_name)
        os.chdir('..')
        last_dir = os.getcwd()
    raise Exception("Can not find %s" % dir_name)

subprocess.check_call(["cargo", "test"], cwd=str(Path.cwd().parents[0].joinpath("macroslib")), shell=False)

subprocess.check_call(["cargo", "test"], shell=False)
subprocess.check_call(["cargo", "build"], shell=False)

java_dir = str(Path.cwd().joinpath("src/com/example"))
purge(java_dir, ".*\.class$")
subprocess.check_call(["javac", "Main.java", "Foo.java", "Boo.java", "TestPathAndResult.java"],
                      cwd=java_dir, shell=False)

jar_dir = str(Path.cwd().joinpath("src"))
purge(java_dir, ".*\.jar$")
subprocess.check_call(["jar", "cfv", "Test.jar", "com"], cwd=jar_dir, shell=False)

target_dir = os.path.join(find_dir("target"), "debug")
subprocess.check_call(["java", "-verbose:jni", "-ea", "-Djava.library.path=" + target_dir, "-cp", "Test.jar", "com.example.Main"], cwd=jar_dir, shell=False)

