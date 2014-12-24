#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/fcd.h"
#include "scythe/iseq.h"

extern inline bool
scm_fcd_iseq_p(ScmObj obj)
{
  return (scm_obj_type_p(obj, &SCM_ISEQ_TYPE_INFO) ? true : false);
}

ScmObj
scm_fcd_iseq_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&iseq);

  iseq = scm_fcd_mem_alloc(&SCM_ISEQ_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL;

  if (scm_iseq_initialize(iseq) < 0)
    return SCM_OBJ_NULL;

  return iseq;
}

ScmObj
scm_fcd_make_iseq(void)
{
  return scm_fcd_iseq_new(SCM_MEM_HEAP);
}

scm_byte_t *
scm_fcd_iseq_to_ip(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_to_ip(iseq);
}

size_t
scm_fcd_iseq_length(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_length(iseq);
}

int
scm_fcd_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt)
{
  bool cmp;
  int r;

  scm_assert(scm_fcd_iseq_p(iseq1));
  scm_assert(scm_fcd_iseq_p(iseq2));

  r = scm_iseq_eq(iseq1, iseq2, &cmp);
  if (r < 0) return 0;

  if (rslt != NULL) *rslt = cmp;
  return 0;
}

ssize_t
scm_fcd_iseq_push_inst_va(ScmObj iseq, scm_opcode_t op, va_list ap)
{
  ScmObj opd_obj1 = SCM_OBJ_INIT, opd_obj2 = SCM_OBJ_INIT;
  int opd_si1, opd_si2;

  SCM_REFSTK_INIT_REG(&iseq,
                      &opd_obj1, &opd_obj2);

  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(0 <= op && op < SCM_VMINST_NR_OP);

  switch (scm_opfmt_table[op]) {
  case SCM_OPFMT_NOOPD:
    return scm_iseq_push_inst_noopd(iseq, op);
    break;
  case SCM_OPFMT_OBJ:
    opd_obj1 = va_arg(ap, ScmObj);
    if (scm_obj_null_p(opd_obj1)) {
      scm_fcd_error("failed to push a instruction to ISeq: Invalid argument",
                    1, opd_obj1);
      break;
    }
    return scm_iseq_push_inst_obj(iseq, op, opd_obj1);
    break;
  case SCM_OPFMT_OBJ_OBJ:
    opd_obj1 = va_arg(ap, ScmObj);
    opd_obj2 = va_arg(ap, ScmObj);

    if (scm_obj_null_p(opd_obj1)) {
      scm_fcd_error("failed to push a instruction to ISeq: Invalid argument",
                    1, opd_obj1);
      break;
    }
    else if (scm_obj_null_p(opd_obj2)) {
      scm_fcd_error("failed to push a instruction to ISeq: Invalid argument",
                    1, opd_obj2);
      break;
    }

    return scm_iseq_push_inst_obj_obj(iseq, op, opd_obj1, opd_obj2);
    break;
  case SCM_OPFMT_SI:
    opd_si1 = va_arg(ap, int);
    return scm_iseq_push_inst_si(iseq, op, opd_si1);
    break;
  case SCM_OPFMT_SI_SI:
    opd_si1 = va_arg(ap, int);
    opd_si2 = va_arg(ap, int);
    return scm_iseq_push_inst_si_si(iseq, op, opd_si1, opd_si2);
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    opd_si1 = va_arg(ap, int);
    opd_si2 = va_arg(ap, int);
    opd_obj1 = va_arg(ap, ScmObj);

    if (scm_obj_null_p(opd_obj1)) {
      scm_fcd_error("failed to push a instruction to ISeq: Invalid argument",
                    1, opd_obj1);
      break;
    }

    return scm_iseq_push_inst_si_si_obj(iseq, op, opd_si1, opd_si2, opd_obj1);
    break;
  case SCM_OPFMT_IOF:
    opd_si1 = va_arg(ap, int);
    return scm_iseq_push_inst_iof(iseq, op, opd_si1);
    break;
  default:
    scm_assert(false);          /* must not happen */
    break;
  }

  return -1;
}

ssize_t
scm_fcd_iseq_push_inst(ScmObj iseq, scm_opcode_t op, ...)
{
  ssize_t ret;
  va_list ap;

  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(0 <= op && op < SCM_VMINST_NR_OP);

  va_start(ap, op);
  ret = scm_fcd_iseq_push_inst_va(iseq, op, ap);
  va_end(ap);

  return ret;
}

int
scm_fcd_iseq_push_br_dst(ScmObj iseq, size_t offset)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_push_dst(iseq, offset);
}

size_t
scm_fcd_iseq_nr_br_dst(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_nr_dst(iseq);
}

const size_t *
scm_fcd_iseq_br_dsts(ScmObj iseq)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_iseq_dsts(iseq);
}

int
scm_fcd_iseq_update_oprand_iof(ScmObj iseq, size_t offset, int iof)
{
  scm_assert(scm_fcd_iseq_p(iseq));
  scm_assert(offset <= SSIZE_MAX);
  scm_assert(scm_iseq_length(iseq) >= SCM_OPFMT_INST_SZ_IOF);
  scm_assert(offset <= scm_iseq_length(iseq) - SCM_OPFMT_INST_SZ_IOF);
  return scm_iseq_update_opd_iof(iseq, offset, iof);
}

int
scm_fcd_inst_update_oprand_obj(scm_byte_t *ip, ScmObj clsr, ScmObj obj)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ssize_t idx;
  int rslt;

  SCM_REFSTK_INIT_REG(&clsr, &obj,
                      &iseq);

  scm_assert(ip != NULL);
  scm_assert(scm_fcd_closure_p(clsr));
  scm_assert(scm_obj_not_null_p(obj));

  iseq = scm_fcd_closure_to_iseq(clsr);
  if (scm_obj_null_p(iseq)) return -1;

  idx = scm_iseq_ip_to_idx(iseq, ip);
  if (idx < 0) {
    scm_fcd_error("can not updated operands: invalid ip", 0);
    return -1;
  }

  rslt = scm_iseq_update_opd_obj(iseq, (size_t)idx, obj);
  if (rslt < 0) return -1;

  return 0;
}
