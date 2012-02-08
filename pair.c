#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "pair.h"
#include "miscobjects.h"


ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  NULL,                       /* pp_func              */
  sizeof(ScmPair),            /* obj_size             */
  scm_pair_gc_initialize,     /* gc_ini_func          */
  NULL,                       /* gc_fin_func          */
  scm_pair_gc_accept,         /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};


void
scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(pair, &SCM_PAIR_TYPE_INFO);
  assert(scm_obj_not_null_p(car));
  assert(scm_obj_not_null_p(cdr));

  scm_obj_init(SCM_OBJ(pair), &SCM_PAIR_TYPE_INFO);

  SCM_SETQ(SCM_PAIR_CAR(pair), car);
  SCM_SETQ(SCM_PAIR_CDR(pair), cdr);
}

ScmObj
scm_pair_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj car, ScmObj cdr) /* GC OK */
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  assert(scm_obj_not_null_p(car));
  assert(scm_obj_not_null_p(cdr));

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_PAIR_TYPE_INFO, mtype, SCM_REF_MAKE(pair));

  scm_pair_initialize(pair, car, cdr);

  return pair;
}

ScmObj
scm_pair_car(ScmObj pair)       /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CAR(pair);
}

ScmObj
scm_pair_cdr(ScmObj pair)       /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CDR(pair);
}

bool
scm_pair_is_pair(const ScmObj obj) /* GC OK */
{
  assert(scm_obj_not_null_p(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_PAIR_TYPE_INFO);
}

void
scm_pair_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_PAIR_TYPE_INFO);

  SCM_SETQ_PRIM(SCM_PAIR_CAR(obj), SCM_OBJ_NULL);
  SCM_SETQ_PRIM(SCM_PAIR_CDR(obj), SCM_OBJ_NULL);
}


int
scm_pair_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_PAIR_TYPE_INFO);
  assert(scm_obj_not_null_p(mem));
  assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CAR(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CDR(obj), mem);
  return rslt;
}
