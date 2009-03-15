#include <unistd.h>
#include <stdbool.h>
#include <assert.h>

#include "object.h"

void
scm_obj_init(ScmObj obj, SCM_OBJ_TYPE_T type, ScmPrettyPrintFunction ppfunc)
{
  assert(obj != NULL);

  obj->header.type = type;
}

SCM_OBJ_TYPE_T
scm_obj_type(ScmObj obj)
{
  assert(obj != NULL);

  return obj->header.type;
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


#include "memory.h"
#include "pair.h"
#include "string.h"
#include "symbol.h"
#include "nil.h"
#include "integer.h"
#include "vector.h"
#include "bool.h"
#include "char.h"
#include "port.h"
#include "miscobjects.h"

const ScmTypeInfo const * SCM_TYPE_INFO_TBL[SCM_OBJ_NR_TYPE] = {
  &SCM_FORWARD_TYPE_INFO,
  &SCM_PAIR_TYPE_INFO,
  &SCM_STRING_TYPE_INFO,
  &SCM_SYMBOL_TYPE_INFO,
  &SCM_NIL_TYPE_INFO,
  &SCM_INTEGER_TYPE_INFO,
  &SCM_VECTOR_TYPE_INFO,
  &SCM_BOOL_TYPE_INFO,
  &SCM_CHAR_TYPE_INFO,
  &SCM_EOF_TYPE_INFO,
  &SCM_PORT_TYPE_INFO
};

/* for debug */
bool
smc_obj_is_valid_type_info_tbl(void)
{
  int i;

  for (i = 0; i < SCM_OBJ_NR_TYPE; i++) {
    if (SCM_TYPE_INFO_TBL[i] == NULL
        || i != SCM_TYPE_INFO_TBL[i]->type)
      return false;
  }

  return true;
}
