#!/bin/sh

cd ../Tools/Build
cabal configure
cabal build
result=$?
cd ../../C

if [ $result -eq "0" ] && [ -x ../Tools/Build/dist/build/build/build ] ; then
    exec ../Tools/Build/dist/build/build/build $@
else
    echo "\nCan't build the build tool!"
fi

