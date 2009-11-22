#include "object.h"
#include "obuffer.h"
#include "numeric.h"


ScmTypeInfo SCM_FIXNUM_TYPE_INFO = {
  .pp_func = scm_fixnum_pretty_print,
  .obj_size = 0,
  .gc_ini_func = NULL,
  .gc_fin_func = NULL,
  .gc_accept_func = NULL,
  .gc_accept_func_weak = NULL,
};


void
scm_fixnum_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  return;
}
