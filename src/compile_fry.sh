#!/bin/sh
fry -c < $1
fry_ret=$?
if [ "$fry_ret" -ne "0" ]; then
    echo "Error compiling $1 to java"
    exit 1
fi
javac fry.java
java_ret=$?
if [ "$fry_ret" -ne "0" ]; then
   echo "Error compiling $1"
   exit 1
fi
echo "$1 compiled to fry.class"
