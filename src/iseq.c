#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/chashtbl.h"
#include "scythe/fcd.h"
#include "scythe/earray.h"
#include "scythe/impl_utils.h"
#include "scythe/iseq.h"

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

void
scm_iseq_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_fin(SCM_ISEQ_EARY_SEQ(obj));
  eary_fin(SCM_ISEQ_EARY_OBJS(obj));
}

ssize_t
scm_iseq_ip_to_offset(ScmObj iseq, scm_byte_t *ip)
{
  size_t offset;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  if (ip < SCM_ISEQ_SEQ_VEC(iseq)) {
    scm_fcd_error("failed to calculate offset of VM instruction: out of range",
                  0);
    return -1;
  }

  offset = (size_t)(ip - SCM_ISEQ_SEQ_VEC(iseq));
  if (offset > SCM_ISEQ_SEQ_LENGTH(iseq)) {
    scm_fcd_error("failed to calculate offset of VM instruction: out of range",
                  0);
    return -1;
  }
  else if (offset > SSIZE_MAX) {
    scm_fcd_error("failed to calculate offset of VM instruction: "
                  "offset value too big", 0);
    return -1;
  }

  return (ssize_t)offset;
}

ssize_t
scm_iseq_push_inst(ScmObj iseq, const void *inst, size_t sz,
                   const size_t *objs, size_t n)
{
  size_t offset;
  scm_byte_t *ip;
  int r;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(inst != NULL);
  scm_assert(n == 0 || objs != NULL);

  if (SCM_ISEQ_SEQ_LENGTH(iseq) > SSIZE_MAX - sz) {
    scm_fcd_error("failed to expand ISeq: overflow", 0);
    return -1;
  }

  offset = SCM_ISEQ_SEQ_LENGTH(iseq);
  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), scm_byte_t, offset + sz - 1, 0, r);
  if (r < 0) return -1;

  ip = scm_iseq_to_ip(iseq) + offset;
  if (objs != NULL) {
    for (size_t i = 0; i < n; i++) {
      EARY_PUSH(SCM_ISEQ_EARY_OBJS(iseq), size_t, offset + objs[i], r);
      if(r < 0) return -1;
    }

    SCM_WB_EXP(iseq, memcpy(ip, inst, sz));
  }
  else {
    memcpy(ip, inst, sz);
  }

  return (ssize_t)(offset + sz);
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

int
scm_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt)
{
  ScmObj opd_obj11 = SCM_OBJ_INIT, opd_obj12 = SCM_OBJ_INIT;
  ScmObj opd_obj21 = SCM_OBJ_INIT, opd_obj22 = SCM_OBJ_INIT;
  ScmObj tmp = SCM_OBJ_INIT;
  scm_byte_t *head, *ip1, *ip2;
  int opd_si11, opd_si12, opd_si21, opd_si22;
  int r;

  SCM_REFSTK_INIT_REG(&iseq1, &iseq2,
                      &opd_obj11, &opd_obj12,
                      &opd_obj21, &opd_obj22,
                      &tmp);

  scm_assert_obj_type(iseq1, &SCM_ISEQ_TYPE_INFO);
  scm_assert_obj_type(iseq2, &SCM_ISEQ_TYPE_INFO);
  scm_assert(rslt != NULL);

  if (SCM_ISEQ_SEQ_LENGTH(iseq1) != SCM_ISEQ_SEQ_LENGTH(iseq2))
    goto not_equal;

  ip1 = SCM_ISEQ_SEQ_VEC(iseq1);
  ip2 = SCM_ISEQ_SEQ_VEC(iseq2);
  head = ip1;

  while ((size_t)(ip1 - head) < SCM_ISEQ_SEQ_LENGTH(iseq1)) {
    scm_opcode_t op1, op2;

    op1 = SCM_VMINST_GET_OP(ip1);
    op2 = SCM_VMINST_GET_OP(ip2);
    if (op1 != op2) goto not_equal;

    switch (scm_opfmt_table[op1]) {
    case SCM_OPFMT_NOOPD:
      SCM_VMINST_FETCH_OPD_NOOPD(ip1);
      SCM_VMINST_FETCH_OPD_NOOPD(ip2);
      break;
    case SCM_OPFMT_OBJ:
      SCM_VMINST_FETCH_OPD_OBJ(ip1, opd_obj11);
      SCM_VMINST_FETCH_OPD_OBJ(ip2, opd_obj21);
      r = scm_fcd_equal(opd_obj11, opd_obj21, rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_OBJ_OBJ:
      SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip1, opd_obj11, opd_obj12);
      SCM_VMINST_FETCH_OPD_OBJ_OBJ(ip2, opd_obj21, opd_obj22);
      if (scm_fcd_gloc_p(opd_obj11))
        opd_obj11 = scm_fcd_gloc_symbol(opd_obj11);
      if (scm_fcd_gloc_p(opd_obj21))
        opd_obj21 = scm_fcd_gloc_symbol(opd_obj21);
      r = scm_fcd_equal(opd_obj11, opd_obj21, rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      r = scm_fcd_equal(opd_obj12, opd_obj22, rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_SI:
      SCM_VMINST_FETCH_OPD_SI(ip1, opd_si11);
      SCM_VMINST_FETCH_OPD_SI(ip2, opd_si21);
      if (opd_si11 != opd_si21) goto not_equal;
      break;
    case SCM_OPFMT_SI_SI:
      SCM_VMINST_FETCH_OPD_SI_SI(ip1, opd_si11, opd_si12);
      SCM_VMINST_FETCH_OPD_SI_SI(ip2, opd_si21, opd_si22);
      if (opd_si11 != opd_si21 || opd_si12 != opd_si22) goto not_equal;
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip1, opd_si11, opd_si12, opd_obj11);
      SCM_VMINST_FETCH_OPD_SI_SI_OBJ(ip2, opd_si21, opd_si22, opd_obj21);
      if (opd_si11 != opd_si21 || opd_si12 != opd_si22) goto not_equal;
      r = scm_fcd_equal(opd_obj11, opd_obj21, rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_IOF:
      SCM_VMINST_FETCH_OPD_IOF(ip1, opd_si11);
      SCM_VMINST_FETCH_OPD_IOF(ip2, opd_si21);
      if (opd_si11 != opd_si21) goto not_equal;
      break;
    default:
      scm_assert(false);        /* must not happen */
      break;
    }
  }

  *rslt = true;
  return 0;

 not_equal:
  *rslt = false;
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
  size_t idx, *offset;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  EARY_FOR_EACH(SCM_ISEQ_EARY_OBJS(obj), idx, offset) {
    ScmRef chld = SCM_REF_MAKE_FROM_PTR(scm_iseq_to_ip(obj) + *offset);

    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_REF_DEREF(chld), mem);
    if (scm_gc_ref_handler_failure_p(rslt))
      return rslt;
  }

  return rslt;
}
