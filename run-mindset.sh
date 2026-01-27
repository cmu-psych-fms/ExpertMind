#!/bin/bash

if [ -z "$SBCL" ]; then
    SBCL=/usr/local/bin/sbcl
fi
if [ -z "$QUICKLISP" ]; then
    QUICKLISP=$HOME/quicklisp
fi

$SBCL --no-userinit --load $QUICKLISP/setup --load ExpertMind.lisp

# For testing example input. To use switch from run-standalone to
# start-server in ExpertMind.lisp
#curl -d @pilot-em-input.json http://localhost:9899/predict