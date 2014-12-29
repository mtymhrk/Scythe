#include "scythe/object.h"
#include "scythe/fixnum.h"
#include "scythe/fcd.h"

ScmTypeInfo SCM_NULL_OBJ_TYPE_INFO = {
  .name                = "INTERNAL-NULL-VALUE",
  .flags               = 0,
  .obj_print_func      = scm_obj_default_print_func,
  .obj_size            = 0,
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

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

int
scm_obj_print_func_nameonly(ScmObj obj, ScmObj port, int kind,
                            ScmObjPrintHandler handker)
{
  char str[256];
  snprintf(str, sizeof(str), "#<%s>", scm_obj_type_name(obj));
  return scm_fcd_write_cstr(str, SCM_ENC_SRC, port);
}

int
scm_obj_default_print_func(ScmObj obj, ScmObj port, int kind,
                           ScmObjPrintHandler handker)
{
  char str[256];

  snprintf(str, sizeof(str), "#<%s %lx>", scm_obj_type_name(obj), obj);
  return scm_fcd_write_cstr(str, SCM_ENC_SRC, port);
}
