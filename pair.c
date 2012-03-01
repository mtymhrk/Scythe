#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "pair.h"

ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  NULL,                       /* pp_func              */
  sizeof(ScmPair),            /* obj_size             */
  scm_pair_gc_initialize,     /* gc_ini_func          */
  NULL,                       /* gc_fin_func          */
  scm_pair_gc_accept,         /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};


int
scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr) /* GC OK */
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  SCM_SLOT_SETQ(ScmPair, pair, car, car);
  SCM_SLOT_SETQ(ScmPair, pair, cdr, cdr);

  return 0;
}

ScmObj
scm_pair_new(SCM_MEM_TYPE_T mtype, ScmObj car, ScmObj cdr) /* GC OK */
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  pair = scm_capi_mem_alloc(&SCM_PAIR_TYPE_INFO, mtype);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_pair_initialize(pair, car, cdr) < 0)
    return SCM_OBJ_NULL; /* [ERR]: [through] */

  return pair;
}

ScmObj
scm_pair_car(ScmObj pair)       /* GC OK */
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CAR(pair);
}

ScmObj
scm_pair_cdr(ScmObj pair)       /* GC OK */
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);

  return SCM_PAIR_CDR(pair);
}

void
scm_pair_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  SCM_PAIR_CAR(obj) = SCM_OBJ_NULL;
  SCM_PAIR_CDR(obj) = SCM_OBJ_NULL;
}


int
scm_pair_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CAR(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_PAIR_CDR(obj), mem);
  return rslt;
}
