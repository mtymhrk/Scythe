#!/bin/sh

HG=hg
ARCHIVE_TYPE=tbz2
TAR_COMPRESS=j

if [ $# -lt 3 ]; then
  echo "$0: too few arguments"
  echo "Usage: $0 PROJECT_NAME TOP_DIRECTORY REVISION"
  exit 1
fi

PROJ_NAME=$1
PROJ_TOP=$2
TARGET_REV=$3

current_version=`${HG} log -r ${TARGET_REV} --template "{node|short}"`
current_date=`date +%Y%m%d | tr -d '\n'`
archive_prefix=${PROJ_NAME}-${current_date}-${current_version}
archive_file=${archive_prefix}.${ARCHIVE_TYPE}

${HG} archive -r ${TARGET_REV} -t ${ARCHIVE_TYPE} -p ${archive_prefix} ${archive_file}
tar ${TAR_COMPRESS}xf ${archive_file}
cp -a ${PROJ_TOP}/src/compile.c ${archive_prefix}/src/
tar ${TAR_COMPRESS}cf ${archive_file} ${archive_prefix}
rm -rf ${archive_prefix}
