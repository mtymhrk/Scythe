#include "object.h"
#include "numeric.h"

ScmTypeInfo *SCM_OBJ_TAG2TYPE_TBL[SCM_OBJ_TAG_NR_KIND] = {
  NULL,                         /* 0b000: memory managed object */
  &SCM_FIXNUM_TYPE_INFO,        /* 0b001: fixnum object         */
  NULL,                         /* 0b010: unused                */
  &SCM_FIXNUM_TYPE_INFO,        /* 0b011: fixnum object         */
  NULL,                         /* 0b100: unused                */
  &SCM_FIXNUM_TYPE_INFO,        /* 0b101: fixnum object         */
  NULL,                         /* 0b110: unused                */
  &SCM_FIXNUM_TYPE_INFO,        /* 0b111: fixnum object         */
};
