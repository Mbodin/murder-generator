#!/bin/bash

cd web

# Prepare links that are expected to be in the web/ folder.

ln -s ../_build/default/_doc/_html doc || true
ln -s ../_build/default/js/main.js main.js || true

# If being asked to run the server, run it.

if [ -n "$1" -a "$1" = "run" ]
then
	python -m SimpleHTTPServer
fi

