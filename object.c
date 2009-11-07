#include <unistd.h>
#include <stdbool.h>
#include <assert.h>

#include "object.h"

void
scm_obj_init(ScmObj obj, ScmTypeInfo *type)
{
  assert(obj != NULL);
  assert(type != NULL);

  obj->header.type = type;
}

void
scm_obj_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmPrettyPrintFunction pp_func;

  assert(obj != NULL); assert(obuffer != NULL);

  pp_func = SCM_TYPE_INFO_PP(obj->header.type);
  pp_func(obj, obuffer);
}

int
scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2)
{
  assert(obj1 != NULL); assert(obj2 != NULL);

  return (obj1 == obj2) ? 1 : 0;
}
