#!/bin/sh

PRJ_DIR=`pwd`
TOOLS_DIR=${PRJ_DIR}/cmake
CONF_FILE=${TOOLS_DIR}/run_cmake.conf

if [ ! -n "${1}" ]; then
    echo "${0}: too few arguments"
    echo "Usage: ${0} BUILD_DIR_NAME"
    exit 1
else
  BUILD_DIR="${1}"
fi

post_cmake_hooks=""

if [ -f "${CONF_FILE}" ]; then
  echo "${0}: LOAD ${CONF_FILE}"
  . ${CONF_FILE}
fi

if [ ! -e "${BUILD_DIR}" ]; then
  echo "${0}: MAKE DIR ${BUILD_DIR}"
  mkdir ${BUILD_DIR}
fi

echo "${0}: EXECUTE cmake"
(cd ${BUILD_DIR}; cmake ${PRJ_DIR})

for hook in ${post_cmake_hooks}; do
  if [ -x "${TOOLS_DIR}/hooks/${hook}" ]; then
    echo "${0}: EXECUTE ${hook}"
    ${TOOLS_DIR}/hooks/${hook} ${BUILD_DIR}
  fi
done
