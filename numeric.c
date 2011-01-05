#include "object.h"
#include "numeric.h"


ScmTypeInfo SCM_FIXNUM_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = 0,
  .gc_ini_func = NULL,
  .gc_fin_func = NULL,
  .gc_accept_func = NULL,
  .gc_accept_func_weak = NULL,
};

