#!/bin/sh

set -e

if [[ ! -f "src/jni.rs" ]]; then
    ./generate_jni.sh
fi

cargo build

cd src/com/example
rm -f *.class
rm -f *~
javac Main.java Foo.java Boo.java TestPathAndResult.java
cd ../..
rm -f *.jar
jar cfv Test.jar com

java -ea -Djava.library.path=`pwd`/../target/debug -cp Test.jar com.example.Main
