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
scm_iseq_eq(ScmObj iseq1, ScmObj iseq2, bool *rslt)
{
  ScmObj disasm1 = SCM_OBJ_INIT, disasm2 = SCM_OBJ_INIT;
  const ScmDisasmToken *tk1, *tk2;
  int r;

  SCM_REFSTK_INIT_REG(&iseq1, &iseq2,
                      &disasm1, &disasm2);

  scm_assert_obj_type(iseq1, &SCM_ISEQ_TYPE_INFO);
  scm_assert_obj_type(iseq2, &SCM_ISEQ_TYPE_INFO);
  scm_assert(rslt != NULL);

  if (SCM_ISEQ_SEQ_LENGTH(iseq1) != SCM_ISEQ_SEQ_LENGTH(iseq2))
    goto not_equal;

  disasm1 = scm_fcd_make_disassembler(iseq1);
  if (scm_obj_null_p(disasm1)) return -1;

  disasm2 = scm_fcd_make_disassembler(iseq2);
  if (scm_obj_null_p(disasm2)) return -1;

  while ((tk1 = scm_fcd_disassembler_token(disasm1)) != NULL
         && tk1->type != SCM_DISASM_TK_END) {
    tk2 = scm_fcd_disassembler_token(disasm2);
    if (tk2 == NULL) return -1;

    if (tk1->type != tk2->type)
      goto not_equal;

    if (tk1->type != SCM_DISASM_TK_INST)
      goto next;

    if (tk1->inst.fmt != tk2->inst.fmt || tk1->inst.i.op != tk2->inst.i.op)
        goto not_equal;

    r = scm_fcd_disassembler_cnv_to_marshalable(disasm1);
    if (r < 0) return -1;

    r = scm_fcd_disassembler_cnv_to_marshalable(disasm2);
    if (r < 0) return -1;

    switch (tk1->inst.fmt) {
    case SCM_OPFMT_NOOPD:
      break;
    case SCM_OPFMT_OBJ:
      r = scm_fcd_equal(tk1->inst.i.obj.opd1, tk2->inst.i.obj.opd1, rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_OBJ_OBJ:
      r = scm_fcd_equal(tk1->inst.i.obj_obj.opd1, tk2->inst.i.obj_obj.opd1,
                        rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      r = scm_fcd_equal(tk1->inst.i.obj_obj.opd2, tk2->inst.i.obj_obj.opd2,
                        rslt);
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_SI:
      if (tk1->inst.i.si.opd1 != tk2->inst.i.si.opd1)
        goto not_equal;
      break;
    case SCM_OPFMT_SI_SI:
      if (tk1->inst.i.si_si.opd1 != tk2->inst.i.si_si.opd1
          || tk1->inst.i.si_si.opd2 != tk2->inst.i.si_si.opd2)
        goto not_equal;
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      if (tk1->inst.i.si_si_obj.opd1 != tk2->inst.i.si_si_obj.opd1
          || tk1->inst.i.si_si_obj.opd2 != tk2->inst.i.si_si_obj.opd2)
        goto not_equal;
      if(scm_obj_type_p(tk1->inst.i.si_si_obj.opd3, &SCM_ISEQ_TYPE_INFO)
         && scm_obj_type_p(tk2->inst.i.si_si_obj.opd3, &SCM_ISEQ_TYPE_INFO)) {
        r = scm_iseq_eq(tk1->inst.i.si_si_obj.opd3, tk2->inst.i.si_si_obj.opd3,
                        rslt);
      }
      else {
        r = scm_fcd_equal(tk1->inst.i.si_si_obj.opd3,
                          tk2->inst.i.si_si_obj.opd3,
                        rslt);
      }
      if (r < 0) return -1;
      if (!*rslt) return 0;
      break;
    case SCM_OPFMT_IOF:
      if (tk1->inst.i.iof.opd1 != tk2->inst.i.iof.opd1)
        goto not_equal;
      break;
    default:
      scm_fcd_error("failed to compare ISeq objects", 0);
      return -1;
    }

  next:
    r = scm_fcd_disassembler_next(disasm1);
    if (r < 0) return -1;

    r = scm_fcd_disassembler_next(disasm2);
    if (r < 0) return -1;
  }

  if (scm_fcd_disassembler_token(disasm1) == NULL
      || scm_fcd_disassembler_token(disasm2) == NULL)
    return -1;

  if (scm_fcd_disassembler_token(disasm2)->type != SCM_DISASM_TK_END)
    goto not_equal;

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
