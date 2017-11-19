#!/bin/sh

if [ -z $1 ]
then
    TARGET=main.byte
else
    TARGET=$1
fi

# Compile to bytecode
ocamlbuild -I src -pkg extlib $TARGET

# Translate to JavaScript

