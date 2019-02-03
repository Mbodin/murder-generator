#!/bin/sh

if [ -z $1 ]
then
    TARGET=main
else
    TARGET=$1
fi

# Compile to bytecode
ocamlbuild -I src \
           -pkg extlib \
           -use-menhir -menhir "menhir --explain" \
           -tag "optimize(3)" \
           $TARGET.byte

# Translate to JavaScript
js_of_ocaml --pretty $TARGET.byte

