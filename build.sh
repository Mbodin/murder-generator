#!/usr/bin/env sh
set -e

COLOR="" # "\e[35m"
ROLOC="" # "\e[0m"

if [ -z $1 ]
then
  echo "${COLOR}No argument given: compiling main.js as a default.${ROLOC}"
  TARGET=main
  JS="true"
else
  TARGET=`echo "$1" | sed -e 's/\\..*//'`

  if [ $1 = "murderFiles.ml" ]
  then
    echo "${COLOR}Creating murderFiles.ml from actual content…${ROLOC}"

    # Overwriting the dummy file [src/murderFiles.ml] with the actual real content.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let files = [\n`ls data/*.murder | sed -e 's/\\(.*\\)/    \"\\1\" ;/'`\n  ]" > src/murderFiles.ml

    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if expr match "$1" ".*\.js" > /dev/null
  then
    JS="true"
  else
    JS="false"
  fi
fi

# Compile to bytecode
echo "${COLOR}Compiling to bytecode as $TARGET.byte…${ROLOC}"
ocamlbuild -use-ocamlfind -I src \
           -pkgs extlib,lwt,lwt_ppx,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx,ppx_deriving,js_of_ocaml-ppx.deriving,js_of_ocaml.deriving \
           -use-menhir -menhir "menhir --explain" \
           -tag "optimize(3)" \
           $TARGET.byte
echo "${COLOR}Done.${ROLOC}"

if [ $JS = "true" ]
then

  echo "${COLOR}Compiling to JavaScript as $TARGET.js…${ROLOC}"

  # Translate to JavaScript
  js_of_ocaml $TARGET.byte

  # Adding license information.
  sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/murder-generator */" $TARGET.js
  sed -i "1i/* @license magnet:?xt=urn:btih:1f739d935676111cfff4b4693e3816e664797050&dn=gpl-3.0.txt GPL-v3-or-Later */" $TARGET.js
  echo "/* @license-end */" >> $TARGET.js

  echo "${COLOR}Done.${ROLOC}"
fi

