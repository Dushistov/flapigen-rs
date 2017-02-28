#!/bin/sh

set -e

cargo build

cd src/com/example
rm -f *.class
rm -f *~
javac Main.java Foo.java Boo.java TestPathAndResult.java
cd ../..
rm -f *.jar
jar cfv Test.jar com

java -verbose:jni  -ea -Djava.library.path=`pwd`/../target/debug -cp Test.jar com.example.Main
