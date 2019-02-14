#!/usr/bin/env sh
set -ev

if [ -z $1 ]
then
  TARGET=main
  JS="true"
else
  TARGET=`echo "$1" | sed -e 's/\\..*//'`

  if [ $1 = "murderFiles.ml" ]
  then
    # Overwriting the dummy file [src/murderFiles.ml] with the actual real content.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let files = [\n`ls data/*.murder | sed -e 's/\\(.*\\)/    \"\\1\" ;/'`\n  ]" > src/murderFiles.ml
    exit 0
  fi

  if expr match "$1" ".*\.js"
  then
    JS="true"
  else
    JS="false"
  fi
fi

# Compile to bytecode
ocamlbuild -I src \
           -pkg extlib \
           -use-menhir -menhir "menhir --explain" \
           -tag "optimize(3)" \
           $TARGET.byte

if [ $JS = "true" ]
then

  # Translate to JavaScript
  js_of_ocaml $TARGET.byte

  # Adding license information.
  sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/murder-generator */" $TARGET.js
  sed -i "1i/* @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later */" $TARGET.js
  echo "/* @license-end */" >> $TARGET.js

fi

