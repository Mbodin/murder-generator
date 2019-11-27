#!/usr/bin/env sh
set -e

COLOR=`tput setab 5`
ROLOC=`tput sgr0`

DEBUG="false"
MOVE="false"
CHECK="false"

if [ -n "$1" ]
then
  if [ "$1" = "debug" ]
  then
    DEBUG="true"
    shift
  fi
fi

if [ -n "$1" ]
then
  # The “check” command checks that the given file was not committed before use.
  if [ "$1" = "check" ]
  then
    CHECK="true"
    shift
  else
    # The “checkout” command reverts back to normal any file that this script may
    # have changed, then exists.
    # It is generally a good thing to do it before committing.
    if [ "$1" = "checkout" ]
    then
      git checkout dummy/murderFiles.ml dummy/nameFiles.ml dummy/usedTranslations.ml dummy/version.ml web/main.js
      exit 0
    else
      CHECK="false"
    fi
  fi
fi

if [ -n "$1" ]
then
  if [ "$1" = "move" ]
  then
    MOVE="true"
    shift
  fi
fi

if [ -z "$1" ]
then
  echo "${COLOR}No argument given: compiling main.js and main.native as a default.${ROLOC}"
  ./build.sh move main.js
  ./build.sh main.native
  exit 0
else
  REALTARGET=$1

  if [ "$1" = "tests" ]
  then
    echo "${COLOR}Building testing file…${ROLOC}"
    ./build.sh tests.byte
    echo "${COLOR}Done.${ROLOC}"
    echo "${COLOR}Running testing file…${ROLOC}"
    ./tests.byte
    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if [ "$1" = "local" ]
  then
    echo "${COLOR}Building local files…${ROLOC}"
    ./build.sh check murderFiles.ml
    ./build.sh check nameFiles.ml
    ./build.sh check usedTranslations.ml
    ./build.sh check version.ml
    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if [ "$1" = "murderFiles.ml" ]
  then
    if [ $CHECK = "true" ]
    then
      grep '^let .* = \[\]$' dummy/murderFiles.ml && echo "${COLOR}File $1 was safe.${ROLOC}" || (echo "${COLOR}Unsafe file $1.${ROLOC}"; exit 1)
    fi

    echo "${COLOR}Creating $1 from actual content…${ROLOC}"

    # Overwriting the dummy file [dummy/murderFiles.ml] with the actual real content.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let files = [\n`find data/ -type f | grep '\\.murder$' | sed -e 's/\\(.*\\)/    \"\\1\" ;/'`\n  ]\n" > dummy/murderFiles.ml

    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if [ "$1" = "nameFiles.ml" ]
  then
    if [ $CHECK = "true" ]
    then
      grep '^let .* = \[\]$' dummy/nameFiles.ml && echo "${COLOR}File $1 was safe.${ROLOC}" || (echo "${COLOR}Unsafe file $1.${ROLOC}"; exit 1)
    fi

    echo "${COLOR}Creating $1 from actual content…${ROLOC}"

    # Overwriting the dummy file [dummy/nameFiles.ml] with the actual real content.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let files = [\n`find data/ -type f | grep '\\.names$' | sed -e 's/\\(.*\\)/    \"\\1\" ;/'`\n  ]\n" > dummy/nameFiles.ml

    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if [ "$1" = "usedTranslations.ml" ]
  then
    if [ $CHECK = "true" ]
    then
      grep '^let .* = \[\]$' dummy/usedTranslations.ml && echo "${COLOR}File $1 was safe.${ROLOC}" || (echo "${COLOR}Unsafe file $1.${ROLOC}"; exit 1)
    fi

    echo "${COLOR}Creating $1 from actual content…${ROLOC}"

    # Overwriting the dummy file [dummy/usedTranslations.ml] with the actually
    # used translations.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let used = [\n`grep -o 'get_translation \"[^\"]*\"' src/main.ml | sed -e 's/get_translation \\(\"[^\"]*\"\\)/    \\1 ;/'`\n  ]\n" > dummy/usedTranslations.ml

    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  if [ "$1" = "version.ml" ]
  then
    if [ $CHECK = "true" ]
    then
      grep '^let version = "<dummy>"$' dummy/version.ml && echo "${COLOR}File $1 was safe.${ROLOC}" || (echo "${COLOR}Unsafe file $1.${ROLOC}"; exit 1)
    fi

    echo "${COLOR}Creating $1 from actual content…${ROLOC}"

    # Overwriting the dummy file [dummy/version.ml] with the current version.
    # As this overwrites a committed file, please only do that on deployment.
    echo "let version = \"`git rev-parse HEAD`\"\n" > dummy/version.ml

    echo "${COLOR}Done.${ROLOC}"
    exit 0
  fi

  TARGET=`echo "$1" | sed -e 's/\\..*//'`

  if expr match "$1" ".*\.js" > /dev/null
  then
    JS="true"
    NATIVE="false"
  else
    JS="false"
    if expr match "$1" ".*\.native" > /dev/null
    then
      NATIVE="true"
    else
      NATIVE="false"
    fi
  fi
fi

if [ $NATIVE = "true" ]
then
  EXT="native"
else
  EXT="byte"
fi

if [ $TARGET = "main" ]
then
  if [ $JS = "true" ]
  then
    TARGET="main_js"
  else
    if [ $NATIVE = "true" ]
    then
      TARGET="main_native"
    fi
  fi
fi

if [ $DEBUG = "true" ]
then
  DEBUGFLAG=",debug"
else
  DEBUGFLAG=""
fi

if [ $JS = "true" ]
then
  ADDITIONALPACKAGES=",js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx,js_of_ocaml-ppx.deriving,js_of_ocaml.deriving"
  ADDITIONALFLAGS=""
else
  ADDITIONALPACKAGES=",uuseg.string,lwt.unix"
  ADDITIONALFLAGS=",thread"
fi

# Compile to bytecode
echo "${COLOR}Compiling to bytecode as ${TARGET}.${EXT}…${ROLOC}"
ocamlbuild -use-ocamlfind -Is src,lib,dummy \
           -pkgs unix,extlib,yojson,re,lwt,lwt_ppx,ppx_deriving${ADDITIONALPACKAGES} \
           -use-menhir -menhir "menhir --explain" \
           -tags "optimize(3)${DEBUGFLAG}${ADDITIONALFLAGS}" \
           ${TARGET}.${EXT}
echo "${COLOR}Done.${ROLOC}"

REALTARGETBASE=`echo "$REALTARGET" | sed -e 's/\\..*//'`
if [ $TARGET.$EXT != $REALTARGETBASE ]
then
  echo "${COLOR}Moving file from ${TARGET}.${EXT} to ${REALTARGETBASE}.${EXT}.${ROLOC}"
  if [ $TARGET.$EXT != $REALTARGETBASE.$EXT ]
  then
    mv -f $TARGET.$EXT $REALTARGETBASE.$EXT
  fi
  TARGET=$REALTARGETBASE
fi

if [ $JS = "true" ]
then

  if [ $CHECK = "true" ]
  then
    grep '^throw .* has not been compiled.*dummy.*$' web/main.js && echo "${COLOR}File main.js was safe.${ROLOC}" || (echo "${COLOR}Unsafe file main.js.${ROLOC}"; exit 1)
  fi

  if [ $DEBUG = "true" ]
  then
    DEBUGFLAG="--pretty"
  else
    DEBUGFLAG=""
  fi

  echo "${COLOR}Compiling to JavaScript as ${TARGET}.js…${ROLOC}"

  # Translate to JavaScript
  js_of_ocaml ${DEBUGFLAG} ${TARGET}.byte

  # Adding license information.
  sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/murder-generator */" ${TARGET}.js
  sed -i "1i/* @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL-v3-or-Later */" ${TARGET}.js
  echo "/* @license-end */" >> ${TARGET}.js
  echo "//# sourceURL=main.js" >> ${TARGET}.js

  echo "${COLOR}Done.${ROLOC}"

  if [ $MOVE = "true" ]
  then
    echo "${COLOR}Moving ${TARGET}.js to web/…${ROLOC}"
    mv -f ${TARGET}.js web/
    echo "${COLOR}Done.${ROLOC}"
  fi
fi

