PATH_TO_JNI_INCLUDES=$JAVA_HOME/include
#PATH_TO_JNI_INCLUDES=/opt/oracle-jdk-bin-1.8.0.102/include
#PATH_TO_JNI_INCLUDES=/usr/lib/jvm/java-8-openjdk/include

$HOME/.cargo/bin/bindgen --no-unstable-rust --builtins $PATH_TO_JNI_INCLUDES/jni.h --  -I $PATH_TO_JNI_INCLUDES/linux > src/jni.rs
