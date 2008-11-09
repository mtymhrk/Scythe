#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "bool.h"

struct ScmBoolRec {
  ScmObjHeader header;
  bool value;
};

static void
scm_bool_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmBool *boolv;

  assert(obj != NULL); assert(scm_bool_is_bool(obj));
  assert(obuffer != NULL);

  boolv = SCM_BOOL(obj);

  if (boolv->value)
    scm_obuffer_concatenate_string(obuffer, "#t");
  else
    scm_obuffer_concatenate_string(obuffer, "#f");
}

ScmBool *
scm_bool_construct(bool value)
{
  ScmBool *boolv;

  boolv = scm_memory_allocate(sizeof(ScmBool));
  scm_obj_init(SCM_OBJ(boolv), SCM_OBJ_TYPE_BOOL, scm_bool_pretty_print);
  boolv->value = value;

  return boolv;
}

bool
scm_bool_value(ScmBool *boolv)
{
  assert(boolv != NULL);
  return boolv->value;
}

bool
scm_bool_is_bool(ScmObj obj)
{
  assert(obj != NULL);
  return (scm_obj_type(obj) == SCM_OBJ_TYPE_BOOL);
}
