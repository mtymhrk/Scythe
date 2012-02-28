#include <string.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "earray.h"
#include "instractions.h"
#include "iseq.h"

ScmTypeInfo SCM_ISEQ_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmISeq),
  .gc_ini_func         = scm_iseq_gc_initialize,
  .gc_fin_func         = scm_iseq_gc_finalize,
  .gc_accept_func      = scm_iseq_gc_accept,
  .gc_accept_func_weak = NULL
};


void
scm_iseq_initialize(ScmObj iseq) /* GC OK */
{
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  rslt = eary_init(SCM_ISEQ_EARY_SEQ(iseq),
                   sizeof(scm_iword_t), SCM_ISEQ_DEFAULT_SEQ_SIZE);
  if (rslt != 0)
    ;                           /* TODO: error handling */

  rslt = eary_init(SCM_ISEQ_EARY_IMMVS(iseq),
                   sizeof(ScmObj), SCM_ISEQ_DEFAULT_IMMVS_SIZE);
  if (rslt != 0)
    ;                           /* TODO: error handling */

  /* TODO: fill in by NOOP */
  /* memset(SCM_ISEQ_SEQ(iseq), 0, sizeof(scm_iword_t) * SCM_ISEQ_DEFAULT_SIZE); */
}

ScmObj
scm_iseq_new(SCM_MEM_TYPE_T mtype) /* GC OK */
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  iseq = scm_capi_mem_alloc(&SCM_ISEQ_TYPE_INFO, mtype);

  scm_iseq_initialize(iseq);

  return iseq;
}

void
scm_iseq_finalize(ScmObj obj) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_fin(SCM_ISEQ_EARY_SEQ(obj));
  eary_fin(SCM_ISEQ_EARY_IMMVS(obj));
}

int
scm_iseq_set_immval(ScmObj iseq, ScmObj val) /* GC OK */
{
  size_t idx;
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  idx = SCM_ISEQ_VEC_LENGTH(iseq);
  if (idx >= SCM_ISEQ_IMMVS_MAX) return -1;

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), idx, val, iseq, err);

  if(err != 0) return -1;

  return (int)idx;
}

int
scm_iseq_update_immval(ScmObj iseq, int idx, ScmObj val)
{
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(scm_obj_not_null_p(val));

  if ((size_t)idx >= SCM_ISEQ_VEC_LENGTH(iseq)) return -1;

  EARY_SET_SCMOBJ(SCM_ISEQ_EARY_IMMVS(iseq), (size_t)idx, val, iseq, err);

  if (err != 0) return -1;

  return idx;
}

int
scm_iseq_set_word(ScmObj iseq, size_t index, scm_iword_t word) /* GC OK */
{
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_iword_t, index, word, err);
  if (err != 0) return -1;

  return 0;
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_init(SCM_ISEQ_EARY_SEQ(obj), 0, 0);
  eary_init(SCM_ISEQ_EARY_IMMVS(obj), 0, 0);
}

void
scm_iseq_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  for (size_t i = 0; i < SCM_ISEQ_VEC_LENGTH(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_ISEQ_IMMVAL_VEC(obj)[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt))
      return rslt;
  }

  return rslt;
}
