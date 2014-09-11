#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "chashtbl.h"
#include "api.h"
#include "earray.h"
#include "impl_utils.h"
#include "iseq.h"

ScmTypeInfo SCM_ISEQ_TYPE_INFO = {
  .name                = "iseq",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmISeq),
  .gc_ini_func         = scm_iseq_gc_initialize,
  .gc_fin_func         = scm_iseq_gc_finalize,
  .gc_accept_func      = scm_iseq_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_iseq_initialize(ScmObj iseq)
{
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  rslt = eary_init(SCM_ISEQ_EARY_SEQ(iseq),
                   sizeof(scm_byte_t), SCM_ISEQ_DEFAULT_SEQ_SIZE);
  if (rslt != 0) return -1;

  rslt = eary_init(SCM_ISEQ_EARY_OBJS(iseq),
                   sizeof(size_t), SCM_ISEQ_DEFAULT_OBJS_SIZE);
  if (rslt != 0) return -1;

  return 0;
}

ScmObj
scm_iseq_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = scm_capi_mem_alloc(&SCM_ISEQ_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL;

  if (scm_iseq_initialize(iseq) < 0)
    return SCM_OBJ_NULL;

  return iseq;
}

void
scm_iseq_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_fin(SCM_ISEQ_EARY_SEQ(obj));
  eary_fin(SCM_ISEQ_EARY_OBJS(obj));
}

ssize_t
scm_iseq_push_inst_noopd(ScmObj iseq, scm_opcode_t op)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_NOOPD;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  scm_vminst_set_inst_noopd(ip, op);

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_obj(ScmObj iseq, scm_opcode_t op, ScmObj obj)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_OBJ;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  SCM_WB_EXP(iseq, scm_vminst_set_inst_obj(ip, op, obj));

  EARY_PUSH(SCM_ISEQ_EARY_OBJS(iseq), size_t, offset, err);
  if(err != 0) return -1;

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_obj_obj(ScmObj iseq,
                           scm_opcode_t op, ScmObj obj1, ScmObj obj2)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_OBJ_OBJ;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  SCM_WB_EXP(iseq, scm_vminst_set_inst_obj_obj(ip, op, obj1, obj2));

  EARY_PUSH(SCM_ISEQ_EARY_OBJS(iseq), size_t, offset, err);
  if(err != 0) return -1;

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_si(ScmObj iseq, scm_opcode_t op, int si)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_SI;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  scm_vminst_set_inst_si(ip, op, si);

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_si_si(ScmObj iseq, scm_opcode_t op, int si1, int si2)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_SI_SI;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  scm_vminst_set_inst_si_si(ip, op, si1, si2);

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_si_si_obj(ScmObj iseq,
                             scm_opcode_t op, int si1, int si2, ScmObj obj)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_SI_SI_OBJ;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  SCM_WB_EXP(iseq, scm_vminst_set_inst_si_si_obj(ip, op, si1, si2, obj));

  EARY_PUSH(SCM_ISEQ_EARY_OBJS(iseq), size_t, offset, err);
  if(err != 0) return -1;

  return (ssize_t)(offset + inst_size);
}

ssize_t
scm_iseq_push_inst_iof(ScmObj iseq, scm_opcode_t op, int iof)
{
  const size_t inst_size = SCM_OPFMT_INST_SZ_IOF;
  int err;
  scm_byte_t *ip;
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - inst_size);

  offset = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + inst_size - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  scm_vminst_set_inst_iof(ip, op, iof);

  return (ssize_t)(offset + inst_size);
}

int
scm_iseq_update_opd_iof(ScmObj iseq, size_t offset, int iof)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(offset <= SCM_ISEQ_SEQ_LENGTH(iseq) - SCM_OPFMT_INST_SZ_IOF);

  ip = scm_iseq_to_ip(iseq) + offset;
  SCM_WB_EXP(iseq, scm_vminst_update_opd_iof(ip, iof, SCM_VMINST_UPD_FLG_OPD1));

  return 0;
}

int
scm_iseq_update_opd_obj(ScmObj iseq, size_t offset, ScmObj obj)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(offset <= SCM_ISEQ_SEQ_LENGTH(iseq) - SCM_OPFMT_INST_SZ_OBJ);

  ip = scm_iseq_to_ip(iseq) + offset;
  scm_vminst_update_opd_obj(ip, obj, SCM_VMINST_UPD_FLG_OPD1);

  return 0;
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_init(SCM_ISEQ_EARY_SEQ(obj), 0, 0);
  eary_init(SCM_ISEQ_EARY_OBJS(obj), 0, 0);
}

void
scm_iseq_gc_finalize(ScmObj obj)
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  for (size_t i = 0; i < SCM_ISEQ_OBJS_LENGTH(obj); i++) {
    size_t offset = SCM_ISEQ_OBJS_VEC(obj)[i];
    scm_byte_t *ip, *save;
    scm_opcode_t op;
    ScmObj chld1, chld2;
    int unused;

    ip = save = scm_iseq_to_ip(obj) + offset;
    SCM_VMINST_FETCH_OP(ip, op);
    scm_assert(0 <= op && op <= SCM_VMINST_NR_OP);

    switch (scm_opfmt_table[op]) {
    case SCM_OPFMT_OBJ:
      SCM_VMINST_FETCH_OPD_OBJ(ip, chld1);
      ip = save;

      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, chld1, mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;

      scm_vminst_update_opd_obj(ip, chld1, SCM_VMINST_UPD_FLG_OPD1);
      break;
    case SCM_OPFMT_OBJ_OBJ:
      SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip, chld1, chld2);
      ip = save;

      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, chld1, mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;

      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, chld2, mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;

      scm_vminst_update_opd_obj_obj(ip, chld1, chld2,
                                    (SCM_VMINST_UPD_FLG_OPD1
                                     | SCM_VMINST_UPD_FLG_OPD2));
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip, unused, unused, chld1);
      ip = save;

      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, chld1, mem);
      if (scm_gc_ref_handler_failure_p(rslt))
        return rslt;

      scm_vminst_update_opd_si_si_obj(ip, 0, 0, chld1,
                                      SCM_VMINST_UPD_FLG_OPD3);
      break;
    default:
      scm_assert(false);        /* must not happend */
      break;
    }
  }

  return rslt;
}
