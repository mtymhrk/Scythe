#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/impl_utils.h"
#include "scythe/fcd.h"
#include "scythe/pair.h"

ScmTypeInfo SCM_PAIR_TYPE_INFO = {
  .name                = "pair",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_pair_obj_print,
  .obj_size            = sizeof(ScmPair),
  .gc_ini_func         = scm_pair_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_pair_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};


int
scm_pair_initialize(ScmObj pair, ScmObj car, ScmObj cdr)
{
  scm_assert_obj_type(pair, &SCM_PAIR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  SCM_SLOT_SETQ(ScmPair, pair, car, car);
  SCM_SLOT_SETQ(ScmPair, pair, cdr, cdr);

  return 0;
}

ScmObj
scm_pair_new(SCM_MEM_TYPE_T mtype, ScmObj car, ScmObj cdr)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&pair, &car, &cdr);

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));

  pair = scm_fcd_mem_alloc(&SCM_PAIR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  if (scm_pair_initialize(pair, car, cdr) < 0)
    return SCM_OBJ_NULL;

  return pair;
}

int
scm_pair_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port, &lst, &car, &cdr);

  scm_assert_obj_type(obj, &SCM_PAIR_TYPE_INFO);

  rslt = scm_fcd_write_cstr("(", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  lst = obj;
  while (1) {
    car = scm_pair_car(lst);
    cdr = scm_pair_cdr(lst);

    if (scm_fcd_nil_p(cdr)) {
      rslt = scm_obj_call_print_func(car, port, ext_rep);
      if (rslt < 0) return -1;
      break;
    }
    else if (!scm_fcd_pair_p(cdr)) {
      rslt = scm_obj_call_print_func(car, port, ext_rep);
      if (rslt < 0) return -1;

      rslt = scm_fcd_write_cstr(" . ", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = scm_obj_call_print_func(cdr, port, ext_rep);
      if (rslt < 0) return -1;
      break;
    }

    rslt = scm_obj_call_print_func(car, port, ext_rep);
    if (rslt < 0) return -1;

    rslt = scm_fcd_write_cstr(" ", SCM_ENC_SRC, port);
    if (rslt < 0) return -1;

    lst = cdr;
  }

  rslt = scm_fcd_write_cstr(")", SCM_ENC_SRC, port);
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
