#!/bin/sh
currentdir=`basename "$PWD"`
testdir="test"
if [ "$currentdir" != $testdir ]
    then
        cd test
fi

runhaskell -i../src/ TestMasterMind.hs
runhaskell -i../src/ QuickCheckMastermind.hs
runhaskell -i../src/ TestTreeLabeling.hs
