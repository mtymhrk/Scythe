#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/earray.h"
#include "scythe/vminst.h"
#include "scythe/assembler.h"


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

ScmTypeInfo SCM_ASSEMBLER_TYPE_INFO = {
  .name                = "assembler",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmAssembler),
  .gc_ini_func         = scm_asm_gc_initialize,
  .gc_fin_func         = scm_asm_gc_finalize,
  .gc_accept_func      = scm_asm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_asm_initialize(ScmObj asmb, ScmObj iseq)
{
  int r;

  SCM_REFSTK_INIT_REG(&asmb, &iseq);

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_null_p(iseq) || scm_fcd_iseq_p(iseq));

  if (scm_obj_null_p(iseq)) {
    iseq = scm_fcd_make_iseq();
    if (scm_obj_null_p(iseq)) return -1;
  }

  SCM_SLOT_SETQ(ScmAssembler, asmb, iseq, iseq);

  r = eary_init(SCM_ASSEMBLER_LABEL_DECL(asmb), sizeof(ScmAsmLabelDecl), 0);
  if (r < 0) return -1;

  r = eary_init(SCM_ASSEMBLER_LABEL_REF(asmb), sizeof(ScmAsmLabelRef), 0);
  if (r < 0) return -1;

  return 0;
}

void
scm_asm_finalize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);

  SCM_ASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_fin(SCM_ASSEMBLER_LABEL_DECL(obj));
  eary_fin(SCM_ASSEMBLER_LABEL_REF(obj));
}

static inline bool
scm_asm_label_id_assigned_p(ScmObj asmb, size_t id)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  return (id < EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb)));
}

static int
scm_asm_register_label_ref(ScmObj asmb, size_t offset, size_t label_id)
{
  ScmAsmLabelRef ref = { .offset = offset, .label_id = label_id };
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  EARY_PUSH(SCM_ASSEMBLER_LABEL_REF(asmb), ScmAsmLabelRef, ref, r);
  if (r < 0) return -1;

  return 0;
}

static int
scm_asm_calc_iof(const ScmAsmLabelRef *ref, const ScmAsmLabelDecl *decl,
                 int *iof)
{
  ptrdiff_t x;

  scm_assert(ref != NULL);
  scm_assert(decl != NULL);
  scm_assert(iof != NULL);

  if (decl->offset < 0) {
    scm_fcd_error("failed to resolve reference to assembler labels: "
                  "specified label has not undeclared", 0);
    return -1;
  }

  x = ((ptrdiff_t)decl->offset
       - (ptrdiff_t)(ref->offset + SCM_OPFMT_INST_SZ_IOF));
  if ((ref->offset > PTRDIFF_MAX - SCM_OPFMT_INST_SZ_IOF)
      || (x < INT_MIN || INT_MAX < x)) {
    scm_fcd_error("failed to resolve reference to assembler labels: "
                  "arithmetic overflow", 0);
    return -1;
  }

  *iof = (int)x;
  return 0;
}

ssize_t
scm_asm_assign_label_id(ScmObj asmb)
{
  ScmAsmLabelDecl d = { .offset = -1 };
  size_t i;
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  i = EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb));
  if (i >= SSIZE_MAX) {
    scm_fcd_error("failed to assign assembler label id: too many lables", 0);
    return -1;
  }

  EARY_PUSH(SCM_ASSEMBLER_LABEL_DECL(asmb), ScmAsmLabelDecl, d, r);
  if (r < 0) return -1;

  return (ssize_t)i;
}

int
scm_asm_register_label_id(ScmObj asmb, size_t id)
{
  const ScmAsmLabelDecl d = { .offset = -1 };
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (id >= SSIZE_MAX) {
    scm_fcd_error("failed to register assembler label id: too big", 0);
    return -1;
  }

  for (size_t i = EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb)); i <= id; i++) {
    EARY_SET(SCM_ASSEMBLER_LABEL_DECL(asmb), ScmAsmLabelDecl, i, d, r);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_asm_push_inst_noopd(ScmObj asmb, scm_opcode_t op)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_obj(ScmObj asmb, scm_opcode_t op, ScmObj obj)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op, obj);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_obj_obj(ScmObj asmb,
                          scm_opcode_t op, ScmObj obj1, ScmObj obj2)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj1));
  scm_assert(scm_obj_not_null_p(obj2));

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op, obj1, obj2);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si(ScmObj asmb, scm_opcode_t op, int si)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op, si);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si_si(ScmObj asmb, scm_opcode_t op, int si1, int si2)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op, si1, si2);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_si_si_obj(ScmObj asmb,
                            scm_opcode_t op, int si1, int si2, ScmObj obj)
{
  ssize_t r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb), op, si1, si2, obj);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_inst_iof(ScmObj asmb, scm_opcode_t op, bool label, ...)
{
  va_list arg;
  int ret;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  va_start(arg, label);
  if (label) {
    ret = scm_asm_push_inst_rlid(asmb, op, va_arg(arg, size_t));
  }
  else {
    ssize_t r = scm_fcd_iseq_push_inst(SCM_ASSEMBLER_ISEQ(asmb),
                                       op, va_arg(arg, int));
    if (r < 0) ret = -1;
    else       ret = 0;
  }
  va_end(arg);

  return ret;
}

int
scm_asm_push_inst_rlid(ScmObj asmb, scm_opcode_t op, size_t id)
{
  const ScmAsmLabelDecl *decl;
  size_t cur;
  int r, iof;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (!scm_asm_label_id_assigned_p(asmb, id)) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified label has not undeclared", 0);
    return -1;
  }

  cur = scm_fcd_iseq_length(SCM_ASSEMBLER_ISEQ(asmb));

  decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), id);
  if (decl->offset < 0) {
    r = scm_asm_register_label_ref(asmb, cur, id);
    if (r < 0) return -1;
    iof = 0;
  }
  else {
    ScmAsmLabelRef ref = { .offset = cur, .label_id = id };
    r = scm_asm_calc_iof(&ref, decl, &iof);
    if (r < 0) return -1;
  }

  r = scm_asm_push_inst_iof(asmb, op, false, (int)iof);
  if (r < 0) return -1;

  return 0;
}

int
scm_asm_push_pinst_label(ScmObj asmb, scm_opcode_t op, size_t id)
{
  ScmAsmLabelDecl *decl;
  size_t cur;
  int r;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  if (id >= EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb))) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified label has not assigned", 0);
    return -1;
  }

  decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), id);
  if (decl->offset >= 0) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "specified lable has already declared", 0);
    return -1;
  }

  cur = scm_fcd_iseq_length(SCM_ASSEMBLER_ISEQ(asmb));
  if (cur > SSIZE_MAX) {
    scm_fcd_error("failed to assemble pseudo instruction: "
                  "instructin sequence too big", 0);
    return -1;
  }

  r = scm_fcd_iseq_push_br_dst(SCM_ASSEMBLER_ISEQ(asmb), cur);
  if (r < 0) return -1;

  decl->offset = (ssize_t)cur;

  return 0;
}

int
scm_asm_resolve_label_ref(ScmObj asmb)
{
  const ScmAsmLabelDecl *decl;
  const ScmAsmLabelRef *ref;
  size_t idx;
  int r, iof;

  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);

  EARY_FOR_EACH(SCM_ASSEMBLER_LABEL_REF(asmb), idx, ref) {
    decl = eary_idx_to_ptr(SCM_ASSEMBLER_LABEL_DECL(asmb), ref->label_id);
    r = scm_asm_calc_iof(ref, decl, &iof);
    if (r < 0) return -1;
    r = scm_fcd_iseq_update_oprand_iof(SCM_ASSEMBLER_ISEQ(asmb),
                                       ref->offset, iof);
    if (r < 0) return -1;
  }

  eary_truncate(SCM_ASSEMBLER_LABEL_REF(asmb));
  return 0;
}

void
scm_asm_clear_labels(ScmObj asmb)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  if (EARY_SIZE(SCM_ASSEMBLER_LABEL_REF(asmb)) == 0)
    eary_truncate(SCM_ASSEMBLER_LABEL_DECL(asmb));
}

void
scm_asm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);

  SCM_ASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_init(SCM_ASSEMBLER_LABEL_DECL(obj), 0, 0);
  eary_init(SCM_ASSEMBLER_LABEL_REF(obj), 0, 0);
}

void
scm_asm_gc_finalize(ScmObj obj)
{
  scm_asm_finalize(obj);
}

int
scm_asm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_ASSEMBLER_ISEQ(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/**************************************************************************/
/* Disassembler                                                           */
/**************************************************************************/

ScmTypeInfo SCM_DISASSEMBLER_TYPE_INFO = {
  .name                = "disassembler",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmDisassembler),
  .gc_ini_func         = scm_disasm_gc_initialize,
  .gc_fin_func         = scm_disasm_gc_finalize,
  .gc_accept_func      = scm_disasm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static scm_byte_t *
scm_disasm_ip_proceed(scm_byte_t *ip)
{
  scm_opcode_t op = SCM_VMINST_GET_OP(ip);
  switch (scm_opfmt_table[op]) {
  case SCM_OPFMT_NOOPD:
    ip += SCM_OPFMT_INST_SZ_NOOPD;
    break;
  case SCM_OPFMT_OBJ:
    ip += SCM_OPFMT_INST_SZ_OBJ;
    break;
  case SCM_OPFMT_OBJ_OBJ:
    ip += SCM_OPFMT_INST_SZ_OBJ_OBJ;
    break;
  case SCM_OPFMT_SI:
    ip += SCM_OPFMT_INST_SZ_SI;
    break;
  case SCM_OPFMT_SI_SI:
    ip += SCM_OPFMT_INST_SZ_SI_SI;
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    ip += SCM_OPFMT_INST_SZ_SI_SI_OBJ;
    break;
  case SCM_OPFMT_IOF:
    ip += SCM_OPFMT_INST_SZ_IOF;
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instruction format", 0);
    return NULL;
    break;
  }

  return ip;
}

static int
scm_disasm_push_label_decl(ScmObj disasm, size_t offset)
{
  ScmAsmLabelDecl d = { .offset = (ssize_t)offset };
  ScmAsmLabelDecl *ary;
  size_t idx;
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(offset <= SSIZE_MAX);

  EARY_PUSH(SCM_DISASSEMBLER_LABEL_DECL(disasm), ScmAsmLabelDecl, d, r);
  if (r < 0) return -1;

  idx = EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm)) - 1;
  ary = EARY_HEAD(SCM_DISASSEMBLER_LABEL_DECL(disasm));
  while (idx > 0 && d.offset < ary[idx - 1].offset)
    idx--;

  if (idx > 0 && d.offset == ary[idx - 1].offset) {
    EARY_POP(SCM_DISASSEMBLER_LABEL_DECL(disasm), ScmAsmLabelDecl, d);
    return 0;
  }

  for (size_t i = EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm)); i > idx; i--)
    ary[i] = ary[i - 1];
  ary[idx] = d;

  return 0;
}

static int
scm_disasm_acc_labels(ScmObj disasm)
{
  scm_byte_t *ip;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  ip = scm_fcd_iseq_to_ip(SCM_DISASSEMBLER_ISEQ(disasm));
  while (scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm), ip)) {
    scm_opcode_t op = SCM_VMINST_GET_OP(ip);
    if (scm_opfmt_table[op] == SCM_OPFMT_IOF) {
      ssize_t offset;
      int r, iof;

      SCM_VMINST_FETCH_OPD_IOF(ip, iof);
      offset = scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                         ip + iof);
      if (offset < 0) return -1;
      r = scm_disasm_push_label_decl(disasm, (size_t)offset);
      if (r < 0) return -1;
    }
    else {
      ip = scm_disasm_ip_proceed(ip);
    }
  }

  return 0;
}

int
scm_disasm_initialize(ScmObj disasm, ScmObj iseq)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_fcd_iseq_p(iseq));

  r = eary_init(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                sizeof(ScmAsmLabelDecl), 0);
  if (r < 0) return -1;

  SCM_DISASSEMBLER_SET_TOKEN(disasm, scm_fcd_malloc(sizeof(ScmDisasmToken)));
  if (SCM_DISASSEMBLER_TOKEN(disasm) == NULL) return -1;

  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;

  SCM_SLOT_SETQ(ScmDisassembler, disasm, iseq, iseq);
  SCM_DISASSEMBLER_SET_IP(disasm, NULL);
  SCM_DISASSEMBLER_SET_DECL_IDX(disasm, 0);

  return scm_disasm_acc_labels(disasm);
}

void
scm_disasm_finalize(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  eary_fin(SCM_DISASSEMBLER_LABEL_DECL(disasm));
  if (SCM_DISASSEMBLER_TOKEN(disasm) != NULL) {
    scm_fcd_free(SCM_DISASSEMBLER_TOKEN(disasm));
    SCM_DISASSEMBLER_SET_TOKEN(disasm, NULL);
  }
}

static int
label_decl_cmp(const void *x, const void *y)
{
  const ScmAsmLabelDecl *a = x, *b = y;

  if (a->offset < b->offset)
    return -1;
  else if (a->offset == b->offset)
    return 0;
  else
    return 1;
}

static int
scm_disasm_setup_token_inst(ScmObj disasm)
{
  ScmAsmLabelDecl key, *p;
  scm_opcode_t op;
  int iof;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm),
                                        SCM_DISASSEMBLER_IP(disasm)));

  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_INST;

  op = SCM_VMINST_GET_OP(SCM_DISASSEMBLER_IP(disasm));
  SCM_DISASSEMBLER_TOKEN(disasm)->inst.fmt = scm_opfmt_table[op];

  switch (scm_opfmt_table[op]) {
  case SCM_OPFMT_NOOPD:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.noopd,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_NOOPD);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_NOOPD);
    break;
  case SCM_OPFMT_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj,
                      SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_OBJ);
    break;
  case SCM_OPFMT_OBJ_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj,
                      SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_OBJ_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_OBJ_OBJ);
    break;
  case SCM_OPFMT_SI:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_SI);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI);
    break;
  case SCM_OPFMT_SI_SI:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_SI_SI);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI_SI);
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    SCM_WB_EXP(disasm,
               memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si_obj,
                      SCM_DISASSEMBLER_IP(disasm),
                      SCM_OPFMT_INST_SZ_SI_SI_OBJ));
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_SI_SI_OBJ);
    break;
  case SCM_OPFMT_IOF:
    memcpy(&SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.iof,
           SCM_DISASSEMBLER_IP(disasm), SCM_OPFMT_INST_SZ_IOF);
    SCM_DISASSEMBLER_ADD_IP(disasm, SCM_OPFMT_INST_SZ_IOF);
    iof = SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.iof.opd1;
    key.offset = scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                           SCM_DISASSEMBLER_IP(disasm) + iof);
    if (key.offset < 0) return -1;
    p = eary_bsearch(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                     &key, label_decl_cmp);
    scm_assert(p != NULL);      /* must not happen */
    SCM_DISASSEMBLER_TOKEN(disasm)->label_id =
      (size_t)(p - (ScmAsmLabelDecl *)EARY_HEAD(SCM_DISASSEMBLER_LABEL_DECL(disasm)));
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instruction format", 0);
    return -1;
    break;
  }

  return 0;
}

static int
scm_disasm_setup_token(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(SCM_DISASSEMBLER_IP(disasm) != NULL);

  if (SCM_DISASSEMBLER_TOKEN(disasm)->type == SCM_DISASM_TK_END)
    return 0;

  if (SCM_DISASSEMBLER_DECL_IDX(disasm)
      < EARY_SIZE(SCM_DISASSEMBLER_LABEL_DECL(disasm))) {
    ScmAsmLabelDecl *p = eary_idx_to_ptr(SCM_DISASSEMBLER_LABEL_DECL(disasm),
                                         SCM_DISASSEMBLER_DECL_IDX(disasm));
    if (p->offset == scm_fcd_iseq_ip_to_offset(SCM_DISASSEMBLER_ISEQ(disasm),
                                               SCM_DISASSEMBLER_IP(disasm))) {
      SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;
      SCM_DISASSEMBLER_TOKEN(disasm)->inst.fmt = SCM_OPFMT_NOOPD;
      SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op = SCM_ASM_PI_LABEL;
      SCM_DISASSEMBLER_TOKEN(disasm)->label_id = SCM_DISASSEMBLER_DECL_IDX(disasm);
      SCM_DISASSEMBLER_INC_DECL_IDX(disasm);
      return 0;
    }
  }

  if (scm_fcd_iseq_ip_in_range_p(SCM_DISASSEMBLER_ISEQ(disasm),
                                 SCM_DISASSEMBLER_IP(disasm))) {
    return scm_disasm_setup_token_inst(disasm);
  }
  else {
    SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_END;
    return 0;
  }
}

static inline int
scm_disasm_init_token_if_needed(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  if (SCM_DISASSEMBLER_IP(disasm) != NULL)
    return 0;

  SCM_DISASSEMBLER_SET_IP(disasm,
                          scm_fcd_iseq_to_ip(SCM_DISASSEMBLER_ISEQ(disasm)));
  return scm_disasm_setup_token(disasm);
}

const ScmDisasmToken *
scm_disasm_token(ScmObj disasm)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  r = scm_disasm_init_token_if_needed(disasm);
  if (r < 0) return NULL;

  return SCM_DISASSEMBLER_TOKEN(disasm);
}

int
scm_disasm_next(ScmObj disasm)
{
  int r;

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  r = scm_disasm_init_token_if_needed(disasm);
  if (r < 0) return -1;

  return scm_disasm_setup_token(disasm);
}

void
scm_disasm_rewind(ScmObj disasm)
{
  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  SCM_DISASSEMBLER_SET_IP(disasm, NULL);
  SCM_DISASSEMBLER_SET_DECL_IDX(disasm, 0);
  SCM_DISASSEMBLER_TOKEN(disasm)->type = SCM_DISASM_TK_LABEL;
}

int
scm_disasm_cnv_to_marshalable(ScmObj disasm)
{
  SCM_REFSTK_INIT_REG(&disasm);

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  if (scm_disasm_token(disasm) == NULL)
    return -1;

  if (SCM_DISASSEMBLER_TOKEN(disasm)->type != SCM_DISASM_TK_INST)
    return 0;

  switch (SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op) {
  case SCM_OPCODE_GREF:         /* fall through */
  case SCM_OPCODE_GDEF:         /* fall through */
  case SCM_OPCODE_GSET:
    if (scm_fcd_gloc_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1))
      SCM_WB_SETQ(disasm,
                  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1,
                  scm_fcd_gloc_symbol(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd1));
    if (scm_fcd_module_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2))
      SCM_WB_SETQ(disasm,
                  SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2,
                  scm_fcd_module_name(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.obj_obj.opd2));
    break;
  default:
    break;
  }

  return 0;
}

int
scm_disasm_cnv_to_printable(ScmObj disasm)
{
  ScmObj obj = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&disasm,
                      &obj);

  scm_assert_obj_type(disasm, &SCM_DISASSEMBLER_TYPE_INFO);

  r = scm_disasm_cnv_to_marshalable(disasm);
  if (r < 0) return -1;

  if (SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.op == SCM_OPCODE_CLOSE
      && scm_fcd_iseq_p(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si_obj.opd3)) {
    obj = scm_fcd_disassemble(SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si_obj.opd3);
    if (scm_obj_null_p(obj)) return -1;
    SCM_WB_SETQ(disasm,
                SCM_DISASSEMBLER_TOKEN(disasm)->inst.i.si_si_obj.opd3,
                obj);
  }

  return 0;
}

void
scm_disasm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_DISASSEMBLER_TYPE_INFO);

  SCM_DISASSEMBLER(obj)->iseq = SCM_OBJ_NULL;
  eary_init(SCM_DISASSEMBLER_LABEL_DECL(obj), 0, 0);
  SCM_DISASSEMBLER_SET_TOKEN(obj, NULL);
}

void
scm_disasm_gc_finalize(ScmObj obj)
{
  scm_disasm_finalize(obj);
}

int
scm_disasm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_DISASSEMBLER_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_DISASSEMBLER_ISEQ(obj), mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  if (SCM_DISASSEMBLER_TOKEN(obj) != NULL
      && SCM_DISASSEMBLER_TOKEN(obj)->type == SCM_DISASM_TK_INST) {
    switch (SCM_DISASSEMBLER_TOKEN(obj)->inst.fmt) {
    case SCM_OPFMT_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj.opd1,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    case SCM_OPFMT_OBJ_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj_obj.opd1,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.obj_obj.opd2,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                     obj,
                                     SCM_DISASSEMBLER_TOKEN(obj)->inst.i.si_si_obj.opd3,
                                     mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      break;
    default:
      break;
    }
  }

  return rslt;
}


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

static struct {
  int code;
  const char *mne;
} opcode2mnemonic_tbl[] = {
  { SCM_OPCODE_NOP,         "nop" },
  { SCM_OPCODE_HALT,        "halt" },
  { SCM_OPCODE_INT,         "int" },
  { SCM_OPCODE_UNDEF,       "undef" },
  { SCM_OPCODE_UNINIT,      "uninit" },
  { SCM_OPCODE_CFRAME,      "cframe" },
  { SCM_OPCODE_EFRAME,      "eframe" },
  { SCM_OPCODE_EPOP,        "epop" },
  { SCM_OPCODE_ESHIFT,      "eshift" },
  { SCM_OPCODE_IMMVAL,      "immval" },
  { SCM_OPCODE_PUSH,        "push" },
  { SCM_OPCODE_MVPUSH,      "mvpush" },
  { SCM_OPCODE_RETURN,      "return" },
  { SCM_OPCODE_PCALL,       "pcall" },
  { SCM_OPCODE_CALL,        "call" },
  { SCM_OPCODE_TAIL_CALL,   "tcall" },
  { SCM_OPCODE_GREF,        "gref" },
  { SCM_OPCODE_GDEF,        "gdef" },
  { SCM_OPCODE_GSET,        "gset" },
  { SCM_OPCODE_SREF,        "sref" },
  { SCM_OPCODE_SSET,        "sset" },
  { SCM_OPCODE_JMP,         "jmp" },
  { SCM_OPCODE_JMPT,        "jmpt" },
  { SCM_OPCODE_JMPF,        "jmpf" },
  { SCM_OPCODE_BOX,         "box" },
  { SCM_OPCODE_CLOSE,       "close" },
  { SCM_OPCODE_DEMINE,      "demine" },
  { SCM_OPCODE_EMINE,       "emine" },
  { SCM_OPCODE_EDEMINE,     "edemine" },
  { SCM_OPCODE_MRVC,        "mrvc" },
  { SCM_OPCODE_MRVE,        "mrve" },
  { SCM_ASM_PI_LABEL,       "label" },
};

static int
scm_asm_sym2opcode(ScmObj op)
{
  SCM_REFSTK_INIT_REG(&op);

  scm_assert(!scm_obj_null_p(op));

  if (scm_fcd_integer_p(op)) {
    scm_sword_t cd;
    int r = scm_fcd_integer_to_sword(op, &cd);
    if (r < 0) return -1;

    if (cd > SCM_BYTE_MAX) {
      scm_fcd_error("assembler: invalid opcode", 1, op);
      return -1;
    }
    return (int)cd;
  }
  else if (scm_fcd_symbol_p(op)) {
    ssize_t rslt;
    char mne[32];
    char *p = scm_fcd_symbol_to_cstr(op, mne, sizeof(mne));
    if (p == NULL) return -1;

    rslt = scm_asm_mnemonic2opcode(mne);
    if (rslt < 0) {
      scm_fcd_error("assembler: unknown mnemonic", 1, op);
      return -1;
    }

    return (int)rslt;
  }
  else {
    scm_fcd_error("assembler: invalid opcode", 1, op);
    return -1;
  }
}

static int
scm_asm_chk_nr_operand(ScmObj operator, ScmObj operands,
                       size_t min, size_t max)
{
  ssize_t n;

  n = scm_fcd_length(operands);
  if (n < 0) return 0;

  if ((size_t)n < min) {
    scm_fcd_error("assembler: too few operands", 2, operator, operands);
    return -1;
  }
  else if ((size_t)n > max) {
    scm_fcd_error("assembler: too many operands", 2, operator, operands);
    return -1;
  }

  return 0;
}

static int
scm_asm_cnv_operand_to_si(ScmObj operand, ScmObj operator, int *si)
{
  scm_sword_t val;
  int r;

  scm_assert(scm_obj_not_null_p(operand));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(si != NULL);

  if (!scm_fcd_integer_p(operand)) {
    scm_fcd_error("assembler: invalid operands: integer requried",
                  2, operator, operand);
    return -1;
  }

  r = scm_fcd_integer_to_sword(operand, &val);
  if (r < 0) return -1;

  if (val < INT_MIN || INT_MAX < val) {
    scm_fcd_error("assembler: invalid operands: out of range",
                  2, operator, operand);
    return -1;
  }

  *si = (int)val;
  return 0;
}

static int
scm_asm_asm_inst_noopd(ScmObj asmb,
                       int opcode, ScmObj operator, ScmObj operands)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 0, 0);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_noopd(asmb, opcode);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_obj(ScmObj asmb, int opcode, ScmObj operator, ScmObj operands)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 1, 1);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_obj(asmb, opcode, scm_fcd_car(operands));
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_obj_obj(ScmObj asmb,
                         int opcode, ScmObj operator, ScmObj operands)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands,
                      &arg1, &arg2);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 2, 2);
  if (rslt < 0) return -1;

  arg1 = scm_fcd_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_fcd_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  rslt = scm_asm_push_inst_obj_obj(asmb, opcode, arg1, arg2);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si(ScmObj asmb, int opcode, ScmObj operator, ScmObj operands)
{
  int rslt, val;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 1, 1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(scm_fcd_car(operands), operator, &val);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_si(asmb, opcode, val);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si_si(ScmObj asmb,
                       int opcode, ScmObj operator, ScmObj operands)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT;
  int rslt, val1, val2;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands,
                      &arg1, &arg2);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 2, 2);
  if (rslt < 0) return -1;

  arg1 = scm_fcd_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_fcd_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  rslt = scm_asm_cnv_operand_to_si(arg1, operator, &val1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(arg2, operator, &val2);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_inst_si_si(asmb, opcode, val1, val2);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_si_si_obj(ScmObj asmb,
                           int opcode, ScmObj operator, ScmObj operands)
{
  ScmObj arg1 = SCM_OBJ_INIT, arg2 = SCM_OBJ_INIT, arg3 = SCM_OBJ_INIT;
  int rslt, val1, val2;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands,
                      &arg1, &arg2, &arg3);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 3, 3);
  if (rslt < 0) return -1;

  arg1 = scm_fcd_list_ref(operands, 0);
  if (scm_obj_null_p(arg1)) return -1;

  arg2 = scm_fcd_list_ref(operands, 1);
  if (scm_obj_null_p(arg2)) return -1;

  arg3 = scm_fcd_list_ref(operands, 2);
  if (scm_obj_null_p(arg3)) return -1;

  rslt = scm_asm_cnv_operand_to_si(arg1, operator, &val1);
  if (rslt < 0) return -1;

  rslt = scm_asm_cnv_operand_to_si(arg2, operator, &val2);
  if (rslt < 0) return -1;

  if (!scm_fcd_iseq_p(arg3)) {
    arg3 = scm_fcd_assemble(arg3, SCM_OBJ_NULL);
    if (scm_obj_null_p(arg3)) return -1;
  }

  rslt = scm_asm_push_inst_si_si_obj(asmb, opcode, val1, val2, arg3);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst_iof(ScmObj asmb, int opcode, ScmObj operator, ScmObj operands)
{
  ScmObj arg = SCM_OBJ_INIT, label = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands,
                      &arg, &label);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 1, 1);
  if (rslt < 0) return -1;

  arg = scm_fcd_car(operands);
  if (scm_fcd_pair_p(arg)) {
    size_t id;

    label = scm_fcd_list_ref(arg, 1);
    if (scm_obj_null_p(label)) return -1;

    if (!scm_fcd_integer_p(label) || !scm_fcd_positive_p(label)) {
      scm_fcd_error("assembler: label id: positive integer required",
                    2, operator, arg);
      return -1;
    }

    rslt = scm_fcd_integer_to_size_t(label, &id);
    if (rslt < 0) return -1;

    rslt = scm_asm_register_label_id(asmb, id);
    if (rslt < 0) return -1;

    rslt = scm_asm_push_inst_iof(asmb, opcode, true, id);
    if (rslt < 0) return -1;
  }
  else if (scm_fcd_integer_p(arg)) {
    int iof;

    rslt = scm_asm_cnv_operand_to_si(arg, operator, &iof);
    if (rslt < 0) return -1;

    rslt = scm_asm_push_inst_iof(asmb, opcode, false, iof);
    if (rslt < 0) return -1;
  }
  else {
    scm_fcd_error("assembler: invalid operands", 2, operator, operands);
    return -1;
  }

  return 0;
}

static int
scm_asm_asm_inst_label(ScmObj asmb,
                       int opcode, ScmObj operator, ScmObj operands)

{
  ScmObj arg = SCM_OBJ_INIT;
  size_t id;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &operator, &operands,
                      &arg);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(opcode >= SCM_ASM_PI_START);
  scm_assert(scm_fcd_symbol_p(operator));
  scm_assert(scm_fcd_nil_p(operands) || scm_fcd_pair_p(operands));

  rslt = scm_asm_chk_nr_operand(operator, operands, 1, 1);
  if (rslt < 0) return -1;

  arg = scm_fcd_car(operands);
  if (!scm_fcd_integer_p(arg) || !scm_fcd_positive_p(arg)) {
    scm_fcd_error("assembler: label id: positive integer requried",
                  2, operator, arg);
    return -1;
  }

  rslt = scm_fcd_integer_to_size_t(arg, &id);
  if (rslt < 0) return -1;

  rslt = scm_asm_register_label_id(asmb, id);
  if (rslt < 0) return -1;

  rslt = scm_asm_push_pinst_label(asmb, opcode, id);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_asm_asm_inst(ScmObj asmb, ScmObj inst)
{
  ScmObj operator = SCM_OBJ_INIT, operands = SCM_OBJ_INIT;
  int opcode;

  SCM_REFSTK_INIT_REG(&asmb, &inst,
                      &operator, &operands);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_obj_not_null_p(inst));

  if (scm_fcd_pair_p(inst)) {
    operator = scm_fcd_car(inst);
    operands = scm_fcd_cdr(inst);
  }
  else {
    operator = inst;
    operands = SCM_NIL_OBJ;
  }

  opcode = scm_asm_sym2opcode(operator);
  if (opcode < 0) return -1;

  if (opcode < SCM_ASM_PI_START) {
    switch (scm_opfmt_table[opcode]) {
    case SCM_OPFMT_NOOPD:
      return scm_asm_asm_inst_noopd(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_OBJ:
      return scm_asm_asm_inst_obj(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_OBJ_OBJ:
      return scm_asm_asm_inst_obj_obj(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_SI:
      return scm_asm_asm_inst_si(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_SI_SI:
      return scm_asm_asm_inst_si_si(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      return scm_asm_asm_inst_si_si_obj(asmb, opcode, operator, operands);
      break;
    case SCM_OPFMT_IOF:
      return scm_asm_asm_inst_iof(asmb, opcode, operator, operands);
      break;
    default:
      scm_assert(false);
      break;
    }
  }
  else {
    switch (opcode) {
    case SCM_ASM_PI_LABEL:
      return scm_asm_asm_inst_label(asmb, opcode, operator, operands);
      break;
    default:
      scm_assert(false);
      break;
    }
  }

  return -1;
}

int
scm_asm_assemble(ScmObj asmb, ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&asmb, &lst,
                      &cur, &inst);

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_pair_p(lst) || scm_fcd_nil_p(lst));

  for (cur = lst; scm_fcd_pair_p(cur); cur = scm_fcd_cdr(cur)) {
    inst = scm_fcd_car(cur);
    rslt = scm_asm_asm_inst(asmb, inst);
    if (rslt < 0) return -1;
  }

  return 0;
}

static ScmObj
scm_asm_disasm_inst_noopd(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_cons(mne, SCM_NIL_OBJ);
}

static ScmObj
scm_asm_disasm_inst_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_list(2, mne, tk->inst.i.obj.opd1);
}

static ScmObj
scm_asm_disasm_inst_obj_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  return scm_fcd_list(3, mne, tk->inst.i.obj_obj.opd1, tk->inst.i.obj_obj.opd2);
}

static ScmObj
scm_asm_disasm_inst_si(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, num1);
}

static ScmObj
scm_asm_disasm_inst_si_si(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1, &num2);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si_si.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_fcd_make_number_from_sword(tk->inst.i.si_si.opd2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_fcd_list(3, mne, num1, num2);
}

static ScmObj
scm_asm_disasm_inst_si_si_obj(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num1, &num2);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num1 = scm_fcd_make_number_from_sword(tk->inst.i.si_si_obj.opd1);
  if (scm_obj_null_p(num1)) return SCM_OBJ_NULL;

  num2 = scm_fcd_make_number_from_sword(tk->inst.i.si_si_obj.opd2);
  if (scm_obj_null_p(num2)) return SCM_OBJ_NULL;

  return scm_fcd_list(4, mne, num1, num2, tk->inst.i.si_si_obj.opd3);
}

static ScmObj
scm_asm_disasm_inst_iof(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj sym = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &sym, &inst);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  sym = scm_fcd_make_symbol_from_cstr("label", SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  inst = scm_fcd_make_number_from_size_t(tk->label_id);
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  inst = scm_fcd_list(2, sym, inst);
  if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, inst);
}

static ScmObj
scm_asm_disasm_inst(const ScmDisasmToken *tk, ScmObj mne)
{
  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  switch (tk->inst.fmt) {
  case SCM_OPFMT_NOOPD:
    return scm_asm_disasm_inst_noopd(tk, mne);
    break;
  case SCM_OPFMT_OBJ:
    return scm_asm_disasm_inst_obj(tk, mne);
    break;
  case SCM_OPFMT_OBJ_OBJ:
    return scm_asm_disasm_inst_obj_obj(tk, mne);
    break;
  case SCM_OPFMT_SI:
    return scm_asm_disasm_inst_si(tk, mne);
    break;
  case SCM_OPFMT_SI_SI:
    return scm_asm_disasm_inst_si_si(tk, mne);
    break;
  case SCM_OPFMT_SI_SI_OBJ:
    return scm_asm_disasm_inst_si_si_obj(tk, mne);
    break;
  case SCM_OPFMT_IOF:
    return scm_asm_disasm_inst_iof(tk, mne);
    break;
  default:
    scm_fcd_error("failed to disassemble: unknown instructin format", 0);
    break;
  }

  return SCM_OBJ_NULL;
}

static ScmObj
scm_asm_disasm_label(const ScmDisasmToken *tk, ScmObj mne)
{
  ScmObj num = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mne,
                      &num);

  scm_assert(tk != NULL);
  scm_assert(scm_fcd_symbol_p(mne));

  num = scm_fcd_make_number_from_size_t(tk->label_id);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  return scm_fcd_list(2, mne, num);
}

ScmObj
scm_asm_disassemble(ScmObj disasm)
{
  ScmObj list = SCM_OBJ_INIT, tail = SCM_OBJ_INIT, inst = SCM_OBJ_INIT;
  ScmObj mne = SCM_OBJ_INIT;
  const ScmDisasmToken *tk;
  int r;

  SCM_REFSTK_INIT_REG(&disasm,
                      &list, &tail, &inst);

  scm_assert(scm_fcd_disassembler_p(disasm));

  list = tail = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_NIL_OBJ);
  if (scm_obj_null_p(list)) return SCM_OBJ_NULL;

  while ((tk = scm_disasm_token(disasm)) != NULL
         && tk->type != SCM_DISASM_TK_END) {
    mne = scm_asm_mnemonic(tk->inst.i.op);
    if (scm_obj_null_p(mne)) return SCM_OBJ_NULL;

    switch (tk->type) {
    case SCM_DISASM_TK_INST:
      r = scm_disasm_cnv_to_printable(disasm);
      if (r < 0) return SCM_OBJ_NULL;
      inst = scm_asm_disasm_inst(tk, mne);
      break;
    case SCM_DISASM_TK_LABEL:
      inst = scm_asm_disasm_label(tk, mne);
      break;
    default:
      scm_assert(false);
      return SCM_OBJ_NULL;
      break;
    }

    if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

    inst = scm_fcd_cons(inst, SCM_NIL_OBJ);
    if (scm_obj_null_p(inst)) return SCM_OBJ_NULL;

    scm_fcd_set_cdr_i(tail, inst);
    tail = inst;

    r = scm_disasm_next(disasm);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (tk == NULL) return SCM_OBJ_NULL;

  return scm_fcd_cdr(list);
}

int
scm_asm_mnemonic2opcode(const char *mne)
{
  scm_assert(mne != NULL);

  for (size_t i = 0;
       i < sizeof(opcode2mnemonic_tbl)/sizeof(opcode2mnemonic_tbl[0]);
       i++) {
    if (strcmp(opcode2mnemonic_tbl[i].mne, mne) == 0)
      return opcode2mnemonic_tbl[i].code;
  }

  return -1;
}

const char *
scm_asm_opcode2mnemonic(int code)
{
  for (size_t i = 0;
       i < sizeof(opcode2mnemonic_tbl)/sizeof(opcode2mnemonic_tbl[0]);
       i++) {
    if (opcode2mnemonic_tbl[i].code == code)
      return opcode2mnemonic_tbl[i].mne;
  }

  return NULL;
}

ScmObj
scm_asm_mnemonic(int opcode)
{
  const char *p;

  p = scm_asm_opcode2mnemonic(opcode);
  if (p == NULL) {
    scm_fcd_error("assembler: unknown opcode", 0);
    return SCM_OBJ_NULL;
  }

  return  scm_fcd_make_symbol_from_cstr(p, SCM_ENC_SRC);
}
