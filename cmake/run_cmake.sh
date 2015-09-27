#!/bin/sh

PRJ_DIR=`pwd`
TOOLS_DIR=${PRJ_DIR}/cmake
CONF_FILE=${TOOLS_DIR}/run_cmake.conf

if [ ! -n "${1}" ]; then
    echo "${0}: too few arguments"
    echo "Usage: ${0} BUILD_BASE_DIR_NAME"
    exit 1
else
  BUILD_BASE_DIR="${1}"
fi

cmake_options=""
build_types="Debug RelWithDebInfo Release"
post_hook_target="RelWithDebInfo"
post_cmake_hooks=""

if [ -f "${CONF_FILE}" ]; then
  echo "${0}: LOAD ${CONF_FILE}"
  . ${CONF_FILE}
fi

for type in ${build_types}; do
  build_dir=${BUILD_BASE_DIR}/${type}

  if [ ! -e "${build_dir}" ]; then
    echo "${0}: MAKE DIR ${build_dir}"
    mkdir -p ${build_dir}
  fi

  echo "${0}: EXECUTE cmake"
  (cd ${build_dir}; cmake ${cmake_options} -DCMAKE_BUILD_TYPE=${type} ${PRJ_DIR})

  if [ "${type}" = "${post_hook_target}" ]; then
    for hook in ${post_cmake_hooks}; do
      if [ -x "${TOOLS_DIR}/hooks/${hook}" ]; then
        echo "${0}: EXECUTE ${hook}"
        ${TOOLS_DIR}/hooks/${hook} ${build_dir}
      fi
    done
  fi
done
