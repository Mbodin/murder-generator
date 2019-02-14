#!/usr/bin/env sh
set -ev

COLOR="\e[35m"

if [ -z $1 ]
then
  echo "${COLOR}No argument given: compiling main.js as a default.\e[0m"
  TARGET=main
  JS="true"
else
  TARGET=`echo "$1" | sed -e 's/\\..*//'`

  if [ $1 = "murderFiles.ml" ]
  then
    echo "${COLOR}Creating murderFiles.ml from actual content…\e[0m"

    # Overwriting the dummy file [src/murderFiles.ml] with the actual real content.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let files = [\n`ls data/*.murder | sed -e 's/\\(.*\\)/    \"\\1\" ;/'`\n  ]" > src/murderFiles.ml

    echo "${COLOR}Done.\e[0m"
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
echo "${COLOR}Compiling to bytecode as $TARGET.byte…\e[0m"
ocamlbuild -use-ocamlfind -I src \
           -pkgs extlib,lwt,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx \
           -use-menhir -menhir "menhir --explain" \
           -tag "optimize(3)" \
           $TARGET.byte
echo "${COLOR}Done.\e[0m"

if [ $JS = "true" ]
then

  echo "${COLOR}Compiling to JavaScript as $TARGET.js…\e[0m"

  # Translate to JavaScript
  js_of_ocaml $TARGET.byte

  # Adding license information.
  sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/murder-generator */" $TARGET.js
  sed -i "1i/* @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later */" $TARGET.js
  echo "/* @license-end */" >> $TARGET.js

  echo "${COLOR}Done.\e[0m"
fi

