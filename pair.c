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

int
scm_pair_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj nil = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port, &lst, &car, &cdr, &nil);

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  nil = scm_api_nil();

  rslt = scm_capi_write_cstr(port, "(", SCM_ENC_ASCII);
  if (rslt < 0) return -1;

  while (1) {
    car = scm_pair_car(lst);
    cdr = scm_pair_cdr(lst);

    if (scm_capi_eq_p(cdr, nil)) {
      port = scm_api_write_simple(car, port);
      if (scm_obj_null_p(port)) return -1;

      break;
    }
    else if (!scm_capi_pair_p(cdr)) {
      port = scm_api_write_simple(car, port);
      if (scm_obj_null_p(port)) return -1;

      rslt = scm_capi_write_cstr(port, " . ", SCM_ENC_ASCII);
      if (rslt < 0) return -1;

      port = scm_api_write_simple(cdr, port);
      if (scm_obj_null_p(port)) return -1;
      break;
    }

    port = scm_api_write_simple(car, port);
    if (scm_obj_null_p(port)) return -1;

    rslt = scm_capi_write_cstr(port, " ", SCM_ENC_ASCII);
    if (rslt < 0) return -1;

    lst = cdr;
  }

  rslt = scm_capi_write_cstr(port, ")", SCM_ENC_ASCII);
  if (rslt < 0) return -1;

  return 0;
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
