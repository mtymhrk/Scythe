#include <unistd.h>
#include <assert.h>

#include "object.h"

void
scm_obj_init(ScmObj obj, SCM_OBJ_TYPE_T type, ScmPrettyPrintFunction ppfunc)
{
  assert(obj != NULL);

  obj->header.type = type;
  obj->header.pretty_print = ppfunc;
}

SCM_OBJ_TYPE_T
scm_obj_type(ScmObj obj)
{
  assert(obj != NULL);

  return obj->header.type;
}

void
scm_obj_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  assert(obj != NULL); assert(printer != NULL);

  obj->header.pretty_print(obj, printer);
}

int
scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2)
{
  assert(obj1 != NULL); assert(obj2 != NULL);

  return (obj1 == obj2) ? 1 : 0;
}
