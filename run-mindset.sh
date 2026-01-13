#!/bin/bash

# Should be run when cd'ed to the mindset

if [ $USER != "dfm" ] ; then
    SBCL=$HOME/lisp/bin/sbcl
    QUICKLISP=$HOME/lisp/quicklisp
else
    # kludge for testing and debugging on dfm's local machine
    SBCL=/usr/local/bin/sbcl
    QUICKLISP=$HOME/quicklisp
fi

$SBCL --no-userinit --load $QUICKLISP/setup --load ExpertMind.lisp 
#curl -d @converted-test-server-data.json http://localhost:9899/decision
curl -d @sample.json http://localhost:9899/decision

