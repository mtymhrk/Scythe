#include <unistd.h>
#include <stdbool.h>
#include <assert.h>

#include "object.h"

void
scm_obj_init(ScmObj obj, ScmTypeInfo *type)
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));
  assert(type != NULL);

  if (SCM_OBJ_IS_MEM_MANAGED(obj))
    SCM_MMOBJ(obj)->header.type = type;
}

void
scm_obj_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmPrettyPrintFunction pp_func;

  assert(SCM_OBJ_IS_NOT_NULL(obj)); assert(obuffer != NULL);

  pp_func = SCM_OBJ_PP_FUNC(obj);
  pp_func(obj, obuffer);
}

int
scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2)
{
  assert(SCM_OBJ_IS_NOT_NULL(obj1)); assert(SCM_OBJ_IS_NOT_NULL(obj2));

  return (obj1 == obj2) ? 1 : 0;
}

ScmTypeInfo *SCM_OBJ_TAG2TYPE_TBL[] = {};
