#!/bin/sh

set -e

cd src/com/example
rm -f *.class
rm -f *~
javac Main.java Foo.java
cd ../..
rm -f *.jar
jar cfv Test.jar com

java -Djava.library.path=`pwd`/../target/debug -cp Test.jar com.example.Main
