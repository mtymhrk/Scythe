#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "impl_utils.h"
#include "api.h"
#include "pair.h"

ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  .name                = "pair",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_pair_pretty_print,
  .obj_size            = sizeof(ScmPair),
  .gc_ini_func         = scm_pair_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_pair_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
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

int
scm_pair_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj ro = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port, &lst, &car, &cdr, &ro);

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  rslt = scm_capi_write_cstr("(", SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

  lst = obj;
  while (1) {
    car = scm_pair_car(lst);
    cdr = scm_pair_cdr(lst);

    if (scm_capi_nil_p(cdr)) {
      ro = scm_api_write_simple(car, port);
      if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */

      break;
    }
    else if (!scm_capi_pair_p(cdr)) {
      ro = scm_api_write_simple(car, port);
      if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */

      rslt = scm_capi_write_cstr(" . ", SCM_ENC_ASCII, port);
      if (rslt < 0) return -1;  /* [ERR]: [through] */

      ro = scm_api_write_simple(cdr, port);
      if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */
      break;
    }

    ro = scm_api_write_simple(car, port);
    if (scm_obj_null_p(ro)) return -1; /* [ERR]: [thorugh] */

    rslt = scm_capi_write_cstr(" ", SCM_ENC_ASCII, port);
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    lst = cdr;
  }

  rslt = scm_capi_write_cstr(")", SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;      /* [ERR]: [through] */

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
