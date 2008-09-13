#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "printer.h"
#include "bool.h"

struct ScmBoolRec {
  ScmObjHeader header;
  bool value;
};

static void
scm_bool_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  ScmBool *boolv;

  assert(obj != NULL); assert(scm_bool_is_bool(obj));
  assert(printer != NULL);

  boolv = SCM_BOOL(obj);

  if (boolv->value)
    scm_printer_concatenate_string(printer, "#t");
  else
    scm_printer_concatenate_string(printer, "#f");
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
