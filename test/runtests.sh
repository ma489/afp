#!/bin/sh
currentdir=`basename "$PWD"`
testdir="test"
if [ "$currentdir" != $testdir ]
    then
        cd test
fi

runhaskell -i../src/ TestMasterMind.hs
