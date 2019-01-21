#!/bin/sh

if [ -z $1 ]
then
    TARGET=main
else
    TARGET=$1
fi

# Compile to bytecode
ocamlbuild -I src -pkg extlib $TARGET.byte

# Translate to JavaScript
js_of_ocaml $TARGET.byte

