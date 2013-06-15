#!/bin/sh

PRJ_DIR=`pwd`

if [ ! -n "${1}" ]; then
    echo "${0}: too few arguments"
    echo "Usage: ${0} BUILD_DIR_NAME"
    exit 1
else
    BUILD_NAME="${1}"
fi

if [ ! -e "$BUILD_NAME" ]; then
  echo "${0}: MAKE DIR ${BUILD_NAME}"
  mkdir $BUILD_NAME
fi

echo "${0}: EXECUTE cmake"
(cd $BUILD_NAME; cmake $PRJ_DIR)

echo "${0}: EXECUTE gen_flymake.rb"
ruby cmake/gen_flymake.rb $BUILD_NAME
