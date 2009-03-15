#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "pair.h"
#include "nil.h"
#include "obuffer.h"


struct ScmPairRec {
  ScmObjHeader header;
  ScmObj car;
  ScmObj cdr;
};

const ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  SCM_OBJ_TYPE_PAIR,          /* type     */
  scm_pair_pretty_print,      /* pp_func  */
  sizeof(ScmPair)             /* obj_size */
};


ScmPair *
scm_pair_construct(ScmObj car, ScmObj cdr)
{
  ScmPair *pair = NULL;

  assert(car != NULL); assert(cdr != NULL);

  pair = (ScmPair *)scm_memory_allocate(sizeof(ScmPair));
  scm_obj_init(SCM_OBJ(pair), SCM_OBJ_TYPE_PAIR, scm_pair_pretty_print);

  pair->car = car;
  pair->cdr = cdr;

  return pair;
}

ScmObj
scm_pair_car(const ScmPair *pair)
{
  assert(pair != NULL);

  return pair->car;
}

ScmObj
scm_pair_cdr(const ScmPair *pair)
{
  assert(pair != NULL);

  return pair->cdr;
}

bool
scm_pair_is_pair(const ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_PAIR);
}

void
scm_pair_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  ScmPair *pair = NULL;

  assert(obj != NULL); assert(scm_pair_is_pair(obj));
  assert(obuffer != NULL);

  pair = SCM_PAIR(obj);

  scm_obuffer_concatenate_char(obuffer, '(');
  scm_obj_pretty_print(pair->car, obuffer);
  while (scm_pair_is_pair(pair->cdr)) {
    pair = SCM_PAIR(pair->cdr);
    scm_obuffer_concatenate_char(obuffer, ' ');
    scm_obj_pretty_print(pair->car, obuffer);
  }

  if (scm_nil_is_nil(pair->cdr))
    scm_obuffer_concatenate_char(obuffer, ')');
  else {
    scm_obuffer_concatenate_string(obuffer, " . ");
    scm_obj_pretty_print(pair->cdr, obuffer);
    scm_obuffer_concatenate_char(obuffer, ')');
  }
}

