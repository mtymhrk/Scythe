#include <stdio.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "obuffer.h"
#include "integer.h"

struct ScmIntegerRec {
  ScmObjHeader header;
  long long value;
};

const ScmTypeInfo SCM_INTEGER_TYPE_INFO = {
  SCM_OBJ_TYPE_INTEGER,          /* type            */
  scm_integer_pretty_print,      /* pp_func         */
  sizeof(ScmInteger),            /* obj_size        */
  NULL,                          /* gc_ini_func     */
  NULL,                          /* gc_fin_func     */
  NULL                           /* gc_accept_func  */
};


ScmInteger *
scm_integer_construct(long long value)
{
  ScmInteger *integer;

  integer = scm_memory_allocate(sizeof(ScmInteger));
  scm_obj_init(SCM_OBJ(integer), SCM_OBJ_TYPE_INTEGER);
  integer->value = value;

  return integer;
}

void
scm_integer_destruct(ScmInteger *integer)
{
  assert(integer != NULL);
  scm_memory_release(integer);
}

long long
scm_integer_value(ScmInteger *integer)
{
  assert(integer != NULL);
  return integer->value;
}

bool
scm_integer_is_integer(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_INTEGER);
}

ScmInteger *
scm_integer_plus(ScmInteger *val1, ScmInteger *val2)
{
  assert(val1 != NULL);
  assert(val2 != NULL);

  return scm_integer_construct(val1->value + val2->value);
}

ScmInteger *
scm_integer_minus(ScmInteger *val1, ScmInteger *val2)
{
  assert(val1 != NULL);
  assert(val2 != NULL);

  return scm_integer_construct(val1->value - val2->value);
}

ScmInteger *
scm_integer_multiply(ScmInteger *val1, ScmInteger *val2)
{
  assert(val1 != NULL);
  assert(val2 != NULL);

  return scm_integer_construct(val1->value * val2->value);
}

ScmInteger *
scm_integer_divide(ScmInteger *val1, ScmInteger *val2)
{
  assert(val1 != NULL);
  assert(val2 != NULL);

  return scm_integer_construct(val1->value / val2->value);
}

ScmInteger *
scm_integer_reminder(ScmInteger *val1, ScmInteger *val2)
{
  assert(val1 != NULL);
  assert(val2 != NULL);

  return scm_integer_construct(val1->value % val2->value);
}

void
scm_integer_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmInteger *integer;
  char str[21];

  assert(obj != NULL); assert(scm_integer_is_integer(obj));
  assert(obuffer != NULL);

  integer = SCM_INTEGER(obj);

  snprintf(str, sizeof(str), "%lld", integer->value);
  scm_obuffer_concatenate_string(obuffer, str);
}
