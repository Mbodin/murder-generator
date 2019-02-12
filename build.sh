#!/usr/bin/env sh
set -ev

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
js_of_ocaml $TARGET.byte

# Adding license information.
sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/murder-generator */" $TARGET.js
sed -i "1i/* @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later */" $TARGET.js
echo "/* @license-end */" >> $TARGET.js

