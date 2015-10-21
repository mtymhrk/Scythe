#include <sys/types.h>
#include <unistd.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/bedrock.h"
#include "scythe/refstk.h"
#include "scythe/assembler.h"
#include "scythe/equivalence.h"
#include "scythe/file.h"
#include "scythe/exception.h"
#include "scythe/iseq.h"
#include "scythe/miscobjects.h"
#include "scythe/module.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/procedure.h"
#include "scythe/string.h"
#include "scythe/symbol.h"
#include "scythe/vector.h"
#include "scythe/impl_utils.h"
#include "scythe/vmstack.h"
#include "scythe/vminst.h"
#include "scythe/vm.h"


/*******************************************************************/
/*  VM Continuation Capture                                        */
/*******************************************************************/


ScmTypeInfo SCM_CONTCAP_TYPE_INFO = {
  .name                = "contcap",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmContCap),
  .gc_ini_func         = scm_contcap_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_contcap_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_contcap_new(scm_mem_type_t mtype)
{
  ScmObj cc = SCM_OBJ_INIT;

  cc = scm_alloc_mem(&SCM_CONTCAP_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(cc)) return SCM_OBJ_NULL;

  return cc;
}

void
scm_contcap_cap(ScmObj cc,  ScmObj stack, const ScmVMReg *regs)
{
  int n;

  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);
  scm_assert_obj_type(stack, &SCM_VMSTCKRC_TYPE_INFO);
  scm_assert(regs != NULL);

  SCM_SLOT_SETQ(ScmContCap, cc, stack, stack);
  SCM_SLOT_SETQ(ScmContCap, cc, reg.cp, regs->cp);
  SCM_CONTCAP(cc)->reg.ip = regs->ip;
  n = (regs->vc <= SCM_VM_NR_VAL_REG) ? regs->vc : SCM_VM_NR_VAL_REG;
  for (int i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmContCap, cc, reg.val[i], regs->val[i]);
  SCM_CONTCAP(cc)->reg.vc = regs->vc;
  SCM_SLOT_SETQ(ScmContCap, cc, reg.prm, regs->prm);
  SCM_SLOT_SETQ(ScmContCap, cc, reg.exc.obj, regs->exc.obj);
  SCM_SLOT_SETQ(ScmContCap, cc, reg.exc.hndlr, regs->exc.hndlr);
  SCM_SLOT_SETQ(ScmContCap, cc, reg.dw.hndlr, regs->dw.hndlr);
  SCM_CONTCAP(cc)->reg.dw.n = regs->dw.n;
  SCM_CONTCAP(cc)->reg.flags = regs->flags;
}

void
scm_contcap_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_CONTCAP_TYPE_INFO);

  SCM_CONTCAP(obj)->stack = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.ip = NULL;
  SCM_CONTCAP(obj)->reg.vc = 0;
  SCM_CONTCAP(obj)->reg.prm = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.exc.obj = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.exc.hndlr = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.dw.hndlr = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.dw.n = 0;
}

int
scm_contcap_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  int n;

  scm_assert_obj_type(obj, &SCM_CONTCAP_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->stack);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->reg.cp);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  n = ((SCM_CONTCAP(obj)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_CONTCAP(obj)->reg.vc : SCM_VM_NR_VAL_REG);
  for (int i = 0; i < n; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_CONTCAP(obj)->reg.val[i]);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->reg.prm);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.exc.obj);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.exc.hndlr);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.dw.hndlr);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/***************************************************************************/
/*  ScmVM                                                                  */
/***************************************************************************/

#define SCM_VM_STACK_INIT_SIZE (sizeof(ScmObj) * 2048)

#define SCM_VM_SYMTBL_SIZE 256


ScmTypeInfo SCM_VM_TYPE_INFO = {
  .name                = "vm",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmVM),
  .gc_ini_func         = scm_vm_gc_initialize,
  .gc_fin_func         = scm_vm_gc_finalize,
  .gc_accept_func      = scm_vm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static const void **vm_dispatch_table = NULL;
static struct scm_vm_inst_si vm_int_instructions[SCM_VM_NR_INTERRUPTIONS] = {
  { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_GC },
  { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_HALT },
  { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RAISE },
  { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RAISE_CONT },
  { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RETURN },
};
static struct scm_vm_inst_noopd vm_halt_instruction = { .op = SCM_OPCODE_HALT };

static inline void
scm_vm_ctrl_flg_set(ScmObj vm, scm_vm_ctrl_flg_t flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags |= flg;
}

static inline void
scm_vm_ctrl_flg_clr(ScmObj vm, scm_vm_ctrl_flg_t flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags &= ~flg;
}

static inline bool
scm_vm_ctrl_flg_set_p(ScmObj vm, scm_vm_ctrl_flg_t flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return (SCM_VM(vm)->reg.flags & flg) ? true : false;
}

static int scm_vm_handle_stack_overflow(ScmObj vm);

static int
scm_vm_copy_pef_to_top_of_stack_if_needed(ScmObj vm,
                                          ScmCntFrame *cfp, ScmEnvFrame *efp,
                                          ScmObj stack)
{
  size_t size;
  scm_byte_t *next_sp, *src;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (cfp != NULL) {
    if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF))
      return 0;

    src = (scm_byte_t *)scm_vm_cf_partial_base(cfp);
  }
  else if (efp != NULL) {
    if (scm_vm_ef_boxed_p(efp)
        || scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)efp))
      return 0;

    src = (scm_byte_t *)scm_vm_ef_partial_base(efp);
  }
  else if (scm_obj_not_null_p(stack)){
    if (scm_eq_p(SCM_VM(vm)->stack, stack))
      return 0;

    src = (scm_byte_t *)scm_vmsr_partial_base(stack);
  }
  else {
    return 0;
  }

  if (src == NULL)
    return 0;

  size = sizeof(ScmObj) * (size_t)SCM_VM(vm)->reg.partial;
  next_sp = SCM_VM(vm)->reg.sp + size;

  if (scm_vmsr_reach_to_ceiling_p(SCM_VM(vm)->stack, next_sp))
    return scm_vm_handle_stack_overflow(vm);

  SCM_WB_EXP(vm,
             memcpy(SCM_VM(vm)->reg.sp, src, size));
  SCM_VM(vm)->reg.sp = next_sp;

  return 0;
}

static ScmObj
scm_vm_capture_stack(ScmObj vm)
{
  ScmObj vmsr = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &vmsr, &next);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp == scm_vmsr_base(SCM_VM(vm)->stack)) {
    if (SCM_VM(vm)->reg.cfp == NULL
        && !scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
      SCM_VM(vm)->reg.cfp = scm_vmsr_next_cf(SCM_VM(vm)->stack);
      if (scm_vmsr_next_cf_ucf_p(SCM_VM(vm)->stack))
        scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);
      else
        scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);
    }
    return scm_vmsr_next(SCM_VM(vm)->stack);
  }

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack,
                       SCM_VM(vm)->reg.cfp,
                       scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF));
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);
  }

  scm_vmsr_rec(SCM_VM(vm)->stack,
               SCM_VM(vm)->reg.sp, SCM_VM(vm)->reg.cfp, SCM_VM(vm)->reg.efp,
               SCM_VM(vm)->reg.partial,
               scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF));
  next = SCM_VM(vm)->stack;

  vmsr = scm_vmsr_new(SCM_MEM_HEAP,
                      scm_vmsr_segment(next), scm_vmsr_ceiling(next), next);
  if (scm_obj_null_p(vmsr)) return SCM_OBJ_NULL;

  SCM_VM(vm)->stack = vmsr;

  SCM_VM(vm)->reg.sp = scm_vmsr_base(vmsr);

  /* 新しい stack record を current stack record としたが、cfp レジスタの */
  /* 値を更新していなため、cfp レジスタはその次の stack record の領域を指 */
  /* していることになる。                                                 */
  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm, NULL, NULL, next);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_vmsr_next(SCM_VM(vm)->stack);;
}

static int
scm_vm_restore_stack(ScmObj vm, ScmObj stack)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(stack, &SCM_VMSTCKRC_TYPE_INFO);

  scm_vmsr_relink(SCM_VM(vm)->stack, stack,
                  scm_vmsr_cfp(stack), scm_vmsr_ucf_p(stack));

  SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  SCM_VM(vm)->reg.cfp = scm_vmsr_cfp(stack);
  SCM_VM(vm)->reg.efp = scm_vmsr_efp(stack);
  SCM_VM(vm)->reg.partial = scm_vmsr_partial(stack);

  if (scm_vmsr_ucf_p(stack))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm, NULL, NULL, stack);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_handle_stack_overflow(ScmObj vm)
{
  ScmObj vmss = SCM_OBJ_INIT, vmsr = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  ScmCntFrame *next_cf;
  bool next_cf_ucf;
  size_t size;
  int rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &vmss, &vmsr, &next);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack,
                       SCM_VM(vm)->reg.cfp,
                       scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF));
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);
  }

  if (SCM_VM(vm)->reg.sp != scm_vmsr_base(SCM_VM(vm)->stack)) {
    scm_vmsr_rec(SCM_VM(vm)->stack,
                 SCM_VM(vm)->reg.sp, SCM_VM(vm)->reg.cfp, SCM_VM(vm)->reg.efp,
                 SCM_VM(vm)->reg.partial,
                 scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF));
    next = SCM_VM(vm)->stack;
    next_cf = SCM_VM(vm)->reg.cfp;
    next_cf_ucf = scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF);
  }
  else {
    next = scm_vmsr_next(SCM_VM(vm)->stack);
    next_cf = scm_vmsr_next_cf(SCM_VM(vm)->stack);
    next_cf_ucf = scm_vmsr_next_cf_ucf_p(SCM_VM(vm)->stack);
  }

  /* partial environment frame のコピーでスタックオーバーフローが繰り返し発
   * 生するのを防ぐため、作りかけの環境フレームがスタックのトップにある場合
   * は、そのサイズに応じて、新しく作成するスタックセグメントのサイズを変え
   * る。スタックセグメントのサイズが「環境フレームのサイズ * 2」なのはテキ
   * トーに決めた計算。
   */
  size = sizeof(ScmObj) * (size_t)SCM_VM(vm)->reg.partial;
  scm_assert(size != SIZE_MAX);
  size = (size <= SIZE_MAX / 2) ? size * 2 : SIZE_MAX;

  if (size < SCM_VM_STACK_INIT_SIZE)
    size = SCM_VM_STACK_INIT_SIZE;

  vmss = scm_vmss_new(SCM_MEM_HEAP, size);
  if (scm_obj_null_p(vmss)) return -1;

  vmsr = scm_vmsr_new(SCM_MEM_HEAP, vmss, scm_vmss_base(vmss), next);
  if (scm_obj_null_p(vmsr)) return -1;

  SCM_VM(vm)->stack = vmsr;

  SCM_VM(vm)->reg.sp = scm_vmsr_base(vmsr);
  SCM_VM(vm)->reg.cfp = next_cf;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);

  if (next_cf_ucf)
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);
  else
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm, NULL, NULL, next);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_handle_stack_underflow(ScmObj vm)
{
  ScmObj next = SCM_OBJ_INIT;
  ScmCntFrame *next_cf;
  bool next_cf_ucf;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm,
                      &next);

  if (SCM_VM(vm)->reg.cfp != NULL)
    return 0;

  next_cf = SCM_VM(vm)->reg.cfp;
  next_cf_ucf = scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF);

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF))
    next = scm_vmsr_next(SCM_VM(vm)->stack);
  else
    next = SCM_VM(vm)->stack;

  while (next_cf == NULL) {
    next_cf = scm_vmsr_next_cf(next);
    next_cf_ucf = scm_vmsr_next_cf_ucf_p(next);
    next = scm_vmsr_next(next);
    if (scm_obj_null_p(next)) {
      scm_error("stack underflow has occurred", 0);
      return -1;
    }
  }

  scm_vmsr_relink(SCM_VM(vm)->stack, next, next_cf, next_cf_ucf);

  /* XXX: pop_cframe で underflow を検出した場合、この地点以降、efp レジス
   *      タが、どこからも参照されていないスタックセグメント上のフレームを
   *      指している可能性があり、その場合、efp レジスタが指している領域が
   *      開放される。そのため、この関数の呼び出した後は、継続フレームの
   *      ポップ処理をすぐに行う必要がある。
   */

  SCM_VM(vm)->reg.cfp = next_cf;
  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);
  if (next_cf_ucf)
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);
  else
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);

  return 0;
}

static int
scm_vm_stack_push(ScmObj vm, ScmObj val)
{
  scm_byte_t *sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  sp = SCM_VM(vm)->reg.sp + sizeof(ScmObj);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, sp)) {
    SCM_REFSTK_INIT_REG(&vm, &val);

    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;
    sp = SCM_VM(vm)->reg.sp + sizeof(ScmObj);
  }

  SCM_WB_EXP(vm, *(ScmObj *)SCM_VM(vm)->reg.sp = val);

  SCM_VM(vm)->reg.partial++;
  SCM_VM(vm)->reg.sp = sp;

  return 0;
}

static int
scm_vm_make_cframe(ScmObj vm, ScmEnvFrame *efp, ScmObj cp, scm_byte_t *ip)
{
  ScmCntFrame *cfp;
  scm_byte_t *next_sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm);

  next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmCntFrame);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, next_sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;

    next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmCntFrame);
  }

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack,
                       SCM_VM(vm)->reg.cfp,
                       scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF));
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);
  }

  cfp = (ScmCntFrame *)SCM_VM(vm)->reg.sp;

  SCM_WB_EXP(vm,
             scm_vm_cf_init(cfp,
                            SCM_VM(vm)->reg.cfp,
                            efp,
                            SCM_VM(vm)->reg.partial,
                            cp,
                            ip,
                            scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_UCF)));

  SCM_VM(vm)->reg.partial = 0;
  SCM_VM(vm)->reg.cfp = cfp;
  SCM_VM(vm)->reg.sp = next_sp;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);

  return 0;
}

static int
scm_vm_pop_cframe(ScmObj vm)
{
  ScmCntFrame *cfp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm);

  if (SCM_VM(vm)->reg.cfp == NULL) {
    rslt = scm_vm_handle_stack_underflow(vm);
    if (rslt < 0) return -1;
  }

  cfp = SCM_VM(vm)->reg.cfp;

  SCM_VM(vm)->reg.cfp = scm_vm_cf_next(cfp);
  SCM_VM(vm)->reg.partial = cfp->partial;
  SCM_VM(vm)->reg.efp = cfp->efp;
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, cfp->cp);
  SCM_VM(vm)->reg.ip = cfp->ip;

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF))
    SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  else
    SCM_VM(vm)->reg.sp = scm_vm_cf_bottom(cfp);

  if (scm_vm_cf_maked_on_ucf_p(cfp))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_UCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm, cfp, NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_make_eframe(ScmObj vm, int nr_arg)
{
  ScmEnvFrame *efp;
  scm_byte_t *next_sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(nr_arg > 0);

  SCM_REFSTK_INIT_REG(&vm);

  next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmEnvFrame);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, next_sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;

    next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmEnvFrame);
  }

  efp = (ScmEnvFrame *)SCM_VM(vm)->reg.sp;

  scm_vm_ef_init(efp,
                 SCM_VM(vm)->reg.efp,
                 SCM_VM(vm)->reg.partial - nr_arg,
                 nr_arg);

  SCM_VM(vm)->reg.efp = efp;
  SCM_VM(vm)->reg.partial = 0;
  SCM_VM(vm)->reg.sp = next_sp;

  return 0;
}

static int
scm_vm_pop_eframe(ScmObj vm)
{
  ScmEnvFrame *efp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  efp = SCM_VM(vm)->reg.efp;

  SCM_VM(vm)->reg.efp = scm_vm_ef_outer(efp);

  if (scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)efp))
    SCM_VM(vm)->reg.sp = scm_vm_ef_bottom(efp);
  else
    SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);

  SCM_VM(vm)->reg.partial = efp->partial;

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm, NULL, efp, SCM_OBJ_NULL);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_box_eframe(ScmObj vm, ScmEnvFrame *efp, size_t depth, scm_csetter_t *box)
{
  ScmObj efb = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &efb);

  if (depth == 0) {
    scm_csetter_setq(box, SCM_OBJ_NULL);
    return 0;
  }

  if (efp == NULL) {
    scm_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if (scm_vm_ef_boxed_p(efp)) {
    efb = scm_efbox_efp_to_owner(efp);
    if (efp == scm_efbox_to_efp(efb)) {
      scm_csetter_setq(box, efb);
      return 0;
    }
  }

  efb = scm_efbox_new(SCM_MEM_HEAP, efp, depth);
  if (scm_obj_null_p(efb)) return -1;

  scm_csetter_setq(box, efb);
  return 0;
}

static ScmEnvFrame *
scm_vm_eframe_list_ref(ScmEnvFrame *efp_list, size_t n)
{
  ScmEnvFrame *efp;
  size_t i;

  for (i = 0, efp = efp_list;
       i < n && efp != NULL;
       i++, efp = scm_vm_ef_outer(efp))
    ;

  return efp;
}

static ScmObj
scm_vm_eframe_arg_ref(ScmEnvFrame *efp_list, int idx, size_t layer,
                      ScmEnvFrame **efp)
{
  ScmEnvFrame *e;

  scm_assert(idx >= 0);

  e = scm_vm_eframe_list_ref(efp_list, layer);

  if (e == NULL) {
    scm_error("invalid access to envrionment frame: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (idx >= e->len) {
    scm_error("invalid access to envrionment frame: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (efp != NULL) *efp = e;

  return scm_vm_ef_values(e)[idx];
}

static int
scm_vm_shift_eframe(ScmObj vm, int e, int n)
{
  ScmEnvFrame *dst_efp, *new_efp, *src_efp;
  scm_byte_t *new_sp;
  void *dst;
  size_t sz;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(e == 0 || e == 1);
  scm_assert(-1 <= n && n <= INT_MAX);
  scm_assert(e == 0 || SCM_VM(vm)->reg.efp != NULL);

  if (e == 1)
    src_efp = SCM_VM(vm)->reg.efp;
  else
    src_efp = NULL;

  if (n == 0) {
    return 0;
  }
  else if (n > 0) {
    dst_efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)(n + e - 1));
    if (dst_efp == NULL || scm_vm_ef_boxed_p(dst_efp)) {
      scm_error("invalid access to envrionment frame: out of range", 0);
      return -1;
    }

    dst = scm_vm_ef_bottom(dst_efp);
  }
  else {
    dst_efp = NULL;
    if (SCM_VM(vm)->reg.cfp != NULL)
      dst = scm_vm_cf_ceiling(SCM_VM(vm)->reg.cfp);
    else
      dst = NULL;
  }

  if (!scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)dst))
    dst = (ScmEnvFrame *)scm_vmsr_base(SCM_VM(vm)->stack);

  if (src_efp != NULL) {
    new_efp = (ScmEnvFrame *)((scm_byte_t *)dst
                              + sizeof(ScmObj) * (size_t)src_efp->len);
    new_sp = (scm_byte_t *)(new_efp + 1);
  }
  else if (dst_efp != NULL) {
    new_efp = scm_vm_ef_outer(dst_efp);
    new_sp = dst;
  }
  else {
    new_efp = NULL;
    new_sp = dst;
  }

  if (src_efp != NULL) {
    if (dst_efp != NULL) {
      scm_vm_ef_replace_outer(src_efp, scm_vm_ef_outer(dst_efp));
      src_efp->partial = dst_efp->partial;
      scm_vm_ef_copy_flag(src_efp, dst_efp);
    }
    else {
      scm_vm_ef_replace_outer(src_efp, NULL);
      src_efp->partial = 0;
    }

    sz = sizeof(ScmEnvFrame) + sizeof(ScmObj) * (size_t)src_efp->len;
    memmove(dst, scm_vm_ef_bottom(src_efp), sz);
  }

  SCM_VM(vm)->reg.efp = new_efp;
  SCM_VM(vm)->reg.sp = new_sp;

  return 0;
}

static int
scm_vm_make_proc_call_code(ScmObj asmb, ScmObj proc, ScmObj args, bool tail)
{
  ScmObj cur = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t len, label_call;
  int i, r, arity, nr_decons;
  bool unwished;

  SCM_REFSTK_INIT_REG(&asmb, &proc, &args,
                      &cur, &arg);

  scm_assert(scm_assembler_p(asmb));
  scm_assert(scm_procedure_p(proc));
  scm_assert(scm_nil_p(args) || scm_pair_p(args));

  arity = scm_proc_arity(proc);
  unwished = scm_proc_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED);

  len = scm_length(args);
  if (len < 0) return -1;

  if (arity >= 0) {
    if (len != arity) {
      scm_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = arity;
  }
  else {
    if (len < -arity - 1) {
      scm_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = unwished ? (int)len : -arity - 1;
  }

  label_call = 0;

  if (!tail) {
    label_call = scm_asm_assign_label_id(asmb);
    if (label_call < 0) return -1;

    r = scm_asm_push(asmb, SCM_OPCODE_CFRAME, true, label_call);
    if (r < 0) return -1;
  }

  if (nr_decons > 0 || arity < 0) {
    for (cur = args, i = 0;
         scm_pair_p(cur) && i < nr_decons;
         cur = scm_cdr(cur), i++) {
      arg = scm_car(cur);
      r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, arg);
      if (r < 0) return -1;

      r = scm_asm_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }

    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (arity < 0 && !unwished) {
      cur = scm_list_copy(cur);
      if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, cur);
      if (r < 0) return -1;

      r = scm_asm_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }
  }

  r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, proc);
  if (r < 0) return -1;

  if (tail) {
    r = scm_asm_push(asmb, SCM_OPCODE_TAIL_CALL, unwished ? (int)len : arity);
    if (r < 0) return -1;
  }
  else {
    r = scm_asm_push(asmb, SCM_OPCODE_CALL, unwished ? (int)len : arity);
    if (r < 0) return -1;

    r = scm_asm_push(asmb, SCM_OPCODE_NOP);
    if (r < 0) return -1;

    r = scm_asm_push(asmb, SCM_ASM_PI_LABEL, (size_t)label_call);
    if (r < 0) return -1;
  }

  return 0;
}

int
scm_vm_subr_trmp_apply(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj args = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&subr,
                      &args);

  if (scm_nil_p(argv[2])) {
    args = argv[1];
  }
  else {
    args = scm_car(argv[2]);
    args = scm_cons(argv[1], args);
    if (scm_obj_null_p(args))
      return -1;
  }

  return scm_trampolining(argv[0], args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}

static ScmObj
scm_vm_make_trampolining_code(ScmObj vm, ScmObj proc,
                              ScmObj args, ScmObj postproc, ScmObj handover,
                              bool tail)
{
  ScmObj asmb = SCM_OBJ_INIT, apply = SCM_OBJ_INIT;
  ssize_t label_call;
  int apply_argc, r;

  SCM_REFSTK_INIT_REG(&vm, &proc, &args, &postproc, &handover,
                      &asmb, &apply);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_null_p(postproc) || scm_procedure_p(postproc));

  /* 以下の処理を実行する iseq オブエクトを生成する
   * l args を引数として proc プロシージャを呼び出す
   *   (tail が true かつ、postproc が NULL の場合、proc の呼び出しは
   *    tail-call と する)
   * 2 postproc が非 NULL の場合、handover と proc の戻り値を引数として
   *   postproc を呼び出す
   *   (tail が true の場合、postprco の呼び出しは tail-call とする)
   */

  apply_argc = 2;
  label_call = 0;

  asmb = scm_make_assembler(SCM_OBJ_NULL);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_asm_push(asmb, SCM_OPCODE_NOP);
  if (r < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    if (!tail) {
      label_call = scm_asm_assign_label_id(asmb);
      if (label_call < 0) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_OPCODE_CFRAME, true, label_call);
      if (r < 0) return SCM_OBJ_NULL;
    }

    r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, postproc);
    if (r < 0) return SCM_OBJ_NULL;

    r = scm_asm_push(asmb, SCM_OPCODE_PUSH);
    if (r < 0) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(handover)) {
      r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, handover);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return SCM_OBJ_NULL;

      apply_argc = 3;
    }
  }

  r = scm_vm_make_proc_call_code(asmb, proc, args,
                                 (tail && scm_obj_null_p(postproc)));
  if (r < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    r = scm_asm_push(asmb, SCM_OPCODE_MRVC, -1);
    if (r < 0) return SCM_OBJ_NULL;

    r = scm_asm_push(asmb, SCM_OPCODE_MVPUSH);
    if (r < 0) return SCM_OBJ_NULL;

    apply = scm_premade_procedure(SCM_PREMADE_PROC_TRMP_APPLY);
    r = scm_asm_push(asmb, SCM_OPCODE_IMMVAL, apply);
    if (r < 0) return SCM_OBJ_NULL;

    if (tail) {
      r = scm_asm_push(asmb, SCM_OPCODE_TAIL_CALL, apply_argc);
      if (r < 0) return SCM_OBJ_NULL;
    }
    else {
      r = scm_asm_push(asmb, SCM_OPCODE_CALL, apply_argc);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_OPCODE_NOP);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_ASM_PI_LABEL, label_call);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_asm_push(asmb, SCM_OPCODE_HALT);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  r = scm_asm_commit(asmb);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_asm_iseq(asmb);
}

static inline bool
scm_vm_interrupt_act_p(ScmObj vm, int num)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  return ((SCM_VM(vm)->inttbl.activated & (0x01u << num)) ? true : false);
}

static inline bool
scm_vm_interrupt_act_any_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return (SCM_VM(vm)->inttbl.activated ? true : false);
}

static inline void
scm_vm_interrupt_act_flg_set(ScmObj vm, int num)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  SCM_VM(vm)->inttbl.activated |= (0x01u << num);
}

static inline void
scm_vm_interrupt_restore(ScmObj vm, int num)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  if (scm_vm_interrupt_act_p(vm, num)) {
    SCM_VM(vm)->reg.ip = SCM_VM(vm)->inttbl.table[num].save;
    SCM_VM(vm)->inttbl.table[num].save = NULL;
    SCM_VM(vm)->inttbl.activated ^= 0x01u << num;
  }
}

static inline int
scm_vm_interrupt_prior(ScmObj vm, int num)
{
  unsigned int bits;
  int p;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  bits = SCM_VM(vm)->inttbl.activated & ((0x01u << num) - 1u);
  if (bits == 0) return -1;

  p = 0;

#if SCM_VM_NR_INTERRUPTIONS >= 16
  if (bits & 0xffff0000) { bits &= 0xffff0000; p |= 0x10; }
#endif  /* SCM_VM_NR_INTERRUPTIONS >= 16 */

#if SCM_VM_NR_INTERRUPTIONS >= 8
  if (bits & 0xff00ff00) { bits &= 0xff00ff00; p |= 0x08; }
#endif  /* SCM_VM_NR_INTERRUPTIONS >= 8 */

#if SCM_VM_NR_INTERRUPTIONS >= 4
  if (bits & 0xf0f0f0f0) { bits &= 0xf0f0f0f0; p |= 0x04; }
#endif  /* SCM_VM_NR_INTERRUPTIONS >= 4 */

#if SCM_VM_NR_INTERRUPTIONS >= 2
  if (bits & 0xcccccccc) { bits &= 0xcccccccc; p |= 0x02; }
#endif  /* SCM_VM_NR_INTERRUPTIONS >= 2 */

  if (bits & 0xaaaaaaaa) { p |= 0x01; }
  return p;
}

static int
scm_vm_interrupt_activate(ScmObj vm, int num)
{
  int prior;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  if (scm_vm_interrupt_act_p(vm, num))
    return 0;

  prior = scm_vm_interrupt_prior(vm, num);
  if (prior >= 0) {
    SCM_VM(vm)->inttbl.table[num].save = SCM_VM(vm)->inttbl.table[prior].save;
    SCM_VM(vm)->inttbl.table[prior].save =
      (scm_byte_t *)&vm_int_instructions[num];
  }
  else {
    SCM_VM(vm)->inttbl.table[num].save = SCM_VM(vm)->reg.ip;
    SCM_VM(vm)->reg.ip = (scm_byte_t *)&vm_int_instructions[num];
  }

  scm_vm_interrupt_act_flg_set(vm, num);

  return 0;
}

static int
scm_vm_cmp_arity(int argc, int arity, bool unwished)
{
  scm_assert(-INT_MAX <= argc && argc <= INT_MAX);
  scm_assert(-INT_MAX <= arity && arity <= INT_MAX);

  if (argc < 0) {
    if (argc != arity || unwished)  return -2; /* manual adjustment mismatch */
    return 0;
  }

  if (arity >= 0) {
    if (argc == arity) return 0;
    else if (argc < arity) return -1; /* too few arguments */
    else return 1; /* too many arguments */
  }

  if (argc < -arity - 1) return -1; /* too few arguments */

  return 0;
}

static int
scm_vm_adjust_val_to_arity(ScmObj vm, int arity)
{
  ScmObj lst = SCM_OBJ_INIT, obj = SCM_OBJ_INIT;
  int rslt, nr;

  SCM_REFSTK_INIT_REG(&vm,
                      &lst, &obj);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(-INT_MAX <= arity && arity <= INT_MAX);

  rslt = scm_vm_cmp_arity(SCM_VM(vm)->reg.vc, arity, false);
  switch (rslt) {
  case 1:
    scm_error("too many return values", 0);
    return -1;
    break;
  case 0:
    break;
  case -1:
    scm_error("too few return values", 0);
    return -1;
    break;
  case -2:                    /* fall through */
  default:
    scm_assert(false);        /* must not happen */
    break;
  }

  if (arity >= 0)
    return arity;

  nr = -arity;
  lst = SCM_NIL_OBJ;
  for (int i = SCM_VM(vm)->reg.vc; i >= nr; i--) {
    if (i >= SCM_VM_NR_VAL_REG && SCM_VM(vm)->reg.vc > SCM_VM_NR_VAL_REG)
      obj = scm_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                           (size_t)(i - SCM_VM_NR_VAL_REG));
    else
      obj = SCM_VM(vm)->reg.val[i - 1];

    lst = scm_cons(obj, lst);
    if (scm_obj_null_p(lst)) return -1;
  }

  if (nr > SCM_VM_NR_VAL_REG) {
    if (SCM_VM(vm)->reg.vc == SCM_VM_NR_VAL_REG) {
      obj = scm_vector(2, SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1], lst);
      if (scm_obj_null_p(obj)) return -1;

      SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1] = obj;
    }
    else if (SCM_VM(vm)->reg.vc == nr - 1) {
      rslt = scm_vector_push(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                                  lst);
      if (rslt < 0) return -1;
    }
    else {
      scm_vector_set(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                     (size_t)(nr - SCM_VM_NR_VAL_REG),  lst);
      /* インデックス (nr - SCM_VM_NR_VAL_REG) 以降の要素は不要になるが、vc レ
       * ジスタで必要な値の数はわかるため、ベクタを作り直す必要もないので、そ
       * のまま残す。
       */
    }
  }
  else {
    SCM_VM(vm)->reg.val[nr - 1] = lst;
  }

  SCM_VM(vm)->reg.vc = nr;

  return nr;
}

static int
scm_vm_adjust_arg_to_arity(ScmObj vm, int argc, ScmObj proc, int *adjusted)
{
  ScmObj lst = SCM_OBJ_INIT;
  int rslt, len, arity, nr_bind;
  bool unwished;

  SCM_REFSTK_INIT_REG(&vm, &proc,
                      &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(-INT_MAX <= argc && argc <= INT_MAX);
  scm_assert(scm_procedure_p(proc));
  scm_assert(adjusted != NULL);

  arity = scm_proc_arity(proc);
  unwished = scm_proc_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED);

  rslt = scm_vm_cmp_arity(argc, arity, unwished);
  if (rslt != 0) {
    switch (rslt) {
    case 1:
      scm_error("too many arguments", 0);
      break;
    case -1:
      scm_error("too few arguments", 0);
      break;
    case -2:
      scm_error("manual adjustment error", 0);
      break;
    }
    return -1;
  }

  nr_bind = (arity >= 0) ? arity : -arity;
  *adjusted = argc;

  if (arity >= 0 || argc < 0)
    return nr_bind;

  if (unwished)
    return argc;

  *adjusted = arity;

  lst = SCM_NIL_OBJ;

  if (argc > 0) {
    ScmObj *values = scm_vm_ef_values(SCM_VM(vm)->reg.efp);

    len = argc - (nr_bind - 1);
    for (int i = 0; i < len; i++) {
      lst = scm_cons(values[argc - i - 1], lst);
      if (scm_obj_null_p(lst)) return -1;
    }

    for (int i = 0; i < nr_bind - 1; i++) {
      rslt = scm_vm_stack_push(vm, values[i]);
      if (rslt < 0) return -1;
    }
  }

  rslt = scm_vm_stack_push(vm, lst);
  if (rslt < 0) return -1;

  rslt = scm_vm_make_eframe(vm, nr_bind);
  if (rslt < 0) return -1;

  if (argc > 0) {
    rslt = scm_vm_shift_eframe(vm, 1, 1);
    if (rslt < 0) return -1;
  }

  return nr_bind;
}

static ScmObj
scm_vm_get_module_specified_by_opd(ScmObj spec)
{
  ScmObj mod = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&spec,
                      &mod);

  if (scm_module_p(spec))
    return spec;

  r = scm_find_module(spec, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_error("unknown module", 1, spec);
    return SCM_OBJ_NULL;
  }

  return mod;
}

static int
scm_vm_do_op_int(ScmObj vm, int num)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  scm_vm_interrupt_restore(vm, num);
  return SCM_VM(vm)->inttbl.table[num].func(vm);
}

static int
scm_vm_do_op_cframe(ScmObj vm, int dst)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            SCM_VM(vm)->reg.cp,
                            SCM_VM(vm)->reg.ip + dst);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_do_op_eframe(ScmObj vm, int argc)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(argc >= 0);

  rslt = scm_vm_make_eframe(vm, argc);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_do_op_epop(ScmObj vm)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_pop_eframe(vm);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_do_op_eshift(ScmObj vm, int e, int n)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(e == 0 || e == 1);
  scm_assert(-1 <= n && n <= INT_MAX);

  rslt = scm_vm_shift_eframe(vm, e, n);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_do_op_immval(ScmObj vm, ScmObj val)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_do_op_push(ScmObj vm)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(SCM_VM(vm)->reg.vc > 0);

  rslt = scm_vm_stack_push(vm, SCM_VM(vm)->reg.val[0]);
  if (rslt < 0) return 0;

  return 0;
}

static int
scm_vm_do_op_mvpush(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT;
  scm_byte_t *sp;
  int n, rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  sp = SCM_VM(vm)->reg.sp + sizeof(ScmObj) * (size_t)SCM_VM(vm)->reg.vc;
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;
  }

  n = ((SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG - 1);

  for (int i = 0; i < n; i++) {
    SCM_WB_EXP(vm, *(ScmObj *)SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.val[i]);
    SCM_VM(vm)->reg.partial++;
    SCM_VM(vm)->reg.sp += sizeof(ScmObj);
  }

  if (SCM_VM(vm)->reg.vc > SCM_VM_NR_VAL_REG) {
    for (int i = 0; i < SCM_VM(vm)->reg.vc - (SCM_VM_NR_VAL_REG - 1); i++) {
      val = scm_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                           (size_t)i);
      SCM_WB_EXP(vm, *(ScmObj *)SCM_VM(vm)->reg.sp = val);
      SCM_VM(vm)->reg.partial++;
      SCM_VM(vm)->reg.sp += sizeof(ScmObj);
    }
  }

  return 0;
}

static int
scm_vm_do_op_return(ScmObj vm)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_pop_cframe(vm);
  if (rslt < 0) return -1;

  if (SCM_VM(vm)->reg.vc != 1)
    SCM_VM(vm)->reg.ip -= SCM_INST_SZ_NOP;

  return 0;
}

static int
scm_vm_do_op_pcall(ScmObj vm, int argc)
{
  ScmObj efb = SCM_OBJ_INIT, contcap = SCM_OBJ_INIT;
  int rslt, nr_bind;

  SCM_REFSTK_INIT_REG(&vm,
                      &efb, &contcap);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(argc == 0 || SCM_VM(vm)->reg.efp != NULL);
  scm_assert(-INT_MAX <= argc && argc <= INT_MAX);

  if (!scm_procedure_p(SCM_VM(vm)->reg.val[0])) {
    scm_error("inapplicable object", 1, SCM_VM(vm)->reg.val[0]);
    return -1;
  }

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);

  nr_bind = scm_vm_adjust_arg_to_arity(vm, argc, SCM_VM(vm)->reg.val[0], &argc);
  if (nr_bind < 0) return -1;

  if (scm_subrutine_p(SCM_VM(vm)->reg.val[0])) {
    SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
    SCM_VM(vm)->reg.ip = NULL;

    if (nr_bind > 0)
      rslt = scm_subrutine_call(SCM_VM(vm)->reg.val[0],
                                argc, scm_vm_ef_values(SCM_VM(vm)->reg.efp));
    else
      rslt = scm_subrutine_call(SCM_VM(vm)->reg.val[0], 0, NULL);

    if (rslt < 0) return -1;

    if (scm_vm_interrupt_act_any_p(vm))
      rslt = scm_vm_setup_stat_return(vm);
    else
      rslt = scm_vm_do_op_return(vm);
    if (rslt < 0) return -1;
  }
  else if (scm_closure_p(SCM_VM(vm)->reg.val[0])) {
    ScmEnvFrame *efp;

    efb = scm_closure_env(SCM_VM(vm)->reg.val[0]);
    efp = scm_efbox_to_efp(efb);
    if (nr_bind > 0)
      SCM_WB_EXP(vm, scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, efp));
    else
      SCM_WB_EXP(vm, SCM_VM(vm)->reg.efp = efp);

    SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->reg.val[0]);
    SCM_VM(vm)->reg.ip = scm_closure_to_ip(SCM_VM(vm)->reg.val[0]);
  }
  else {
    scm_assert(false);          /* must not happen */
  }

  return 0;
}

static int
scm_vm_do_op_gref(ScmObj vm, scm_byte_t *ip, ScmObj var, ScmObj mod)
{
  ScmObj gloc = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &var, &mod,
                      &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_symbol_p(var) || scm_gloc_p(var));
  scm_assert(scm_module_specifier_p(mod));

  if (scm_symbol_p(var)) {
    mod = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(mod)) return -1;

    r = scm_module_find_gloc(mod, var, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_error("unbound variable", 1, var);
      return -1;
    }

    if (ip != NULL) {
      r = scm_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, ip, gloc);
      if (r < 0) return -1;
    }
  }
  else if (scm_gloc_p(var)) {
    gloc = var;
  }
  else {
    scm_assert(0);
  }

  val = scm_gloc_variable_value(gloc);
  if (scm_landmine_object_p(val)) {
    scm_error("unbound variable", 1, scm_gloc_symbol(gloc));
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_do_op_gdef(ScmObj vm, scm_byte_t *ip, ScmObj var, ScmObj mod)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &var, &mod,
                      &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_symbol_p(var) || scm_gloc_p(var));
  scm_assert(scm_module_specifier_p(mod));

  if (scm_symbol_p(var)) {
    mod = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(mod)) return -1;

    gloc = scm_module_gloc(mod, var);
    if (scm_obj_null_p(gloc)) return -1;

    if (ip != NULL) {
      r = scm_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, ip, gloc);
      if (r < 0) return -1;
    }
  }
  else if (scm_gloc_p(var)) {
    gloc = var;
  }
  else {
    scm_assert(0);
  }

  scm_gloc_bind_variable(gloc, SCM_VM(vm)->reg.val[0]);
  return 0;
}

static int
scm_vm_do_op_gset(ScmObj vm, scm_byte_t *ip, ScmObj var, ScmObj mod)
{
  ScmObj gloc = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &var, &mod,
                      &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_symbol_p(var) || scm_gloc_p(var));
  scm_assert(scm_module_specifier_p(mod));

  if (scm_symbol_p(var)) {
    mod = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(mod)) return -1;

    r = scm_module_find_gloc(mod, var, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_error("unbound variable", 1, var);
      return -1;
    }

    if (ip != NULL) {
      r = scm_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, ip, gloc);
      if (r < 0) return -1;
    }
  }
  else if (scm_gloc_p(var)) {
    gloc = var;
  }
  else {
    scm_assert(0);
  }

  val = scm_gloc_variable_value(gloc);
  if (r < 0) return -1;

  if (scm_landmine_object_p(val)) {
    scm_error("unbound variable", 1, scm_gloc_symbol(gloc));
    return -1;
  }

  scm_gloc_bind_variable(gloc, SCM_VM(vm)->reg.val[0]);
  return 0;
}

static int
scm_vm_do_op_sref(ScmObj vm, int idx, int layer)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (scm_box_object_p(val)) {
    val = scm_box_unbox(val);
    if (scm_obj_null_p(val)) return -1;
  }

  if (scm_landmine_object_p(val)) {
    scm_error("refarence to uninitialized variable", 0);
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_do_op_sset(ScmObj vm, int idx, int layer)
{
  ScmObj val = SCM_OBJ_INIT, o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &val, &o);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_box_object_p(val)) {
    scm_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  o = scm_box_unbox(val);
  if (scm_landmine_object_p(o)) {
    scm_error("refarence to uninitialized variable", 0);
    return -1;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

static int
scm_vm_do_op_jmp(ScmObj vm, int dst)
{
  SCM_VM(vm)->reg.ip += dst;
  return 0;
}

static int
scm_vm_do_op_jmpt(ScmObj vm, int dst)
{
  if (scm_true_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip +=  dst;
  return 0;
}

static int
scm_vm_do_op_jmpf(ScmObj vm, int dst)
{
  if (scm_false_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip += dst;
  return 0;
}

static int
scm_vm_do_op_box(ScmObj vm, int idx, int layer)
{
  ScmObj box = SCM_OBJ_INIT;
  ScmEnvFrame *efp;

  SCM_REFSTK_INIT_REG(&vm,
                      &box);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return -1;

  /* box 化できるのは VM stack 上にある環境のみに限定する */
  /* XXX: 現在のスタックセグメント上にある環境のみに限定したほうがいいかも
   *      しれない。今の制限でも問題ないはずだが。
   */
  if (scm_vm_ef_boxed_p(efp)) {
    scm_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if (idx >= efp->len) {
    scm_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  box = scm_box_new(SCM_MEM_HEAP, scm_vm_ef_values(efp)[idx]);
  if (scm_obj_null_p(box)) return -1;

  SCM_WB_EXP(vm, scm_vm_ef_values(efp)[idx] = box);

  return 0;
}

static int
scm_vm_do_op_close(ScmObj vm, int nr_env, int arity, ScmObj iseq)
{
  ScmObj clsr = SCM_OBJ_INIT,  env = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&vm, &iseq,
                      &clsr, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(nr_env >= 0);

  rslt = scm_vm_box_eframe(vm, SCM_VM(vm)->reg.efp,
                           (size_t)nr_env, SCM_CSETTER_L(env));
  if (rslt < 0) return -1;

  clsr = scm_make_closure(iseq, env, arity);
  if (scm_obj_null_p(clsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], clsr);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_do_op_demine(ScmObj vm, int idx, int layer)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(idx >= 0);
  scm_assert(layer >= 0);

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_box_object_p(val)) {
    scm_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

static int
scm_vm_do_op_mrvc(ScmObj vm, int arity)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_adjust_val_to_arity(vm, arity);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_do_op_mrve(ScmObj vm)
{
  scm_error("multiple-return-value error", 0);
  return -1;
}

static int
scm_vm_do_op_module(ScmObj vm, ScmObj mod)
{
  SCM_REFSTK_INIT_REG(&vm,
                      &mod);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_module_specifier_p(mod));

  mod = scm_vm_get_module_specified_by_opd(mod);
  if (scm_obj_null_p(mod)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], mod);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

#define SCM_VM_OP_NOP() do {                            \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
} while (0)

#define SCM_VM_OP_HALT() do {                           \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
} while (0)

#define SCM_VM_OP_INT() do {                                    \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
    scm_vm_do_op_int(vm, opd_si1);                              \
  } while (0)

#define SCM_VM_OP_CFRAME() do {                                 \
    SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, opd_iof);      \
    scm_vm_do_op_cframe(vm, opd_iof);                           \
  } while (0)

#define SCM_VM_OP_EFRAME() do {                                 \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
    scm_vm_do_op_eframe(vm, opd_si1);                           \
  } while (0)

#define SCM_VM_OP_EPOP() do {                           \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
    scm_vm_do_op_epop(vm);                              \
  } while (0)

#define SCM_VM_OP_ESHIFT() do {                           \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1); \
    scm_vm_do_op_eshift(vm, 1, opd_si1);                  \
 } while (0)

#define SCM_VM_OP_IMMVAL() do {                                 \
    SCM_VMINST_FETCH_OPD_OBJ(SCM_VM(vm)->reg.ip, opd_obj1);     \
    scm_vm_do_op_immval(vm, opd_obj1);                          \
  } while (0)

#define SCM_VM_OP_PUSH() do {                           \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
    scm_vm_do_op_push(vm);                              \
  } while (0)

#define SCM_VM_OP_MVPUSH() do {                         \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
    scm_vm_do_op_mvpush(vm);                            \
 } while (0)

#define SCM_VM_OP_RETURN() do {                         \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
    scm_vm_do_op_return(vm);                            \
  } while (0)

#define SCM_VM_OP_PCALL() do {                                  \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
    scm_vm_do_op_pcall(vm, opd_si1);                            \
  } while (0)

#define SCM_VM_OP_CALL() do {                                   \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
                                                                \
    if (opd_si1 != 0) {                                         \
      int r = scm_vm_do_op_eframe(vm, abs(opd_si1));            \
      if (r < 0) break;                                         \
    }                                                           \
                                                                \
    scm_vm_do_op_pcall(vm, opd_si1);                            \
  } while (0)

#define SCM_VM_OP_TAIL_CALL() do {                                      \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);               \
                                                                        \
    if (opd_si1 != 0) {                                                 \
      int r = scm_vm_do_op_eframe(vm, abs(opd_si1));                    \
      if (r < 0) break;                                                 \
    }                                                                   \
                                                                        \
    if (scm_vm_do_op_eshift(vm, (opd_si1 == 0) ? 0 : 1, -1) < 0)        \
      break;                                                            \
                                                                        \
    scm_vm_do_op_pcall(vm, opd_si1);                                    \
  } while (0)

#define SCM_VM_OP_GREF() do {                                           \
    scm_byte_t *ip = SCM_VM(vm)->reg.ip;                                \
    SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, opd_obj1, opd_obj2); \
    scm_vm_do_op_gref(vm, ip, opd_obj1, opd_obj2);                      \
  } while (0)

#define SCM_VM_OP_GDEF() do {                                           \
    scm_byte_t *ip = SCM_VM(vm)->reg.ip;                                \
    SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, opd_obj1, opd_obj2); \
    scm_vm_do_op_gdef(vm, ip, opd_obj1, opd_obj2);                      \
 } while (0)

#define SCM_VM_OP_GSET() do {                                           \
    scm_byte_t *ip = SCM_VM(vm)->reg.ip;                                \
    SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, opd_obj1, opd_obj2); \
    scm_vm_do_op_gset(vm, ip, opd_obj1, opd_obj2);                      \
 } while (0)

#define SCM_VM_OP_SREF() do {                                           \
    SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, opd_si1, opd_si2);   \
    scm_vm_do_op_sref(vm, opd_si1, opd_si2);                            \
 } while (0)

#define SCM_VM_OP_SSET() do {                                         \
    SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, opd_si1, opd_si2); \
    scm_vm_do_op_sset(vm, opd_si1, opd_si2);                          \
  } while (0)

#define SCM_VM_OP_JMP() do {                                    \
    SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, opd_si1);      \
    scm_vm_do_op_jmp(vm, opd_si1);                              \
  } while (0)

#define SCM_VM_OP_JMPT() do {                                   \
    SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, opd_si1);      \
    scm_vm_do_op_jmpt(vm, opd_si1);                             \
  } while (0)

#define SCM_VM_OP_JMPF() do {                                   \
    SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, opd_si1);      \
    scm_vm_do_op_jmpf(vm, opd_si1);                             \
  } while (0)

#define SCM_VM_OP_BOX() do {                                            \
    SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, opd_si1, opd_si2);   \
    scm_vm_do_op_box(vm, opd_si1, opd_si2);                             \
  } while (0)

#define SCM_VM_OP_CLOSE() do {                                  \
    SCM_VMINST_FETCH_OPD_SI_SI_OBJ(SCM_VM(vm)->reg.ip,          \
                                   opd_si1, opd_si2, opd_obj1); \
    scm_vm_do_op_close(vm, opd_si1, opd_si2, opd_obj1);         \
  } while (0)

#define SCM_VM_OP_DEMINE() do {                                         \
    SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, opd_si1, opd_si2);   \
    scm_vm_do_op_demine(vm, opd_si1, opd_si2);                          \
  } while (0)

#define SCM_VM_OP_EMINE() do {                                  \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
                                                                \
    for (int i = 0; i < opd_si1; i++) {                         \
      int r = scm_vm_do_op_immval(vm, SCM_LANDMINE_OBJ);        \
      if (r < 0) goto scm_vm_op_emine__end;                     \
                                                                \
      r = scm_vm_do_op_push(vm);                                \
      if (r < 0) goto scm_vm_op_emine__end;                     \
    }                                                           \
                                                                \
    if (scm_vm_do_op_eframe(vm, opd_si1) < 0)                   \
      goto scm_vm_op_emine__end;                                \
                                                                \
    for (int i = 0; i < opd_si1; i++) {                         \
      int r = scm_vm_do_op_box(vm, i, 0);                       \
      if (r < 0) goto scm_vm_op_emine__end;                     \
    }                                                           \
                                                                \
  scm_vm_op_emine__end:                                         \
    break;                                                      \
                                                                \
  } while (0)

#define SCM_VM_OP_EDEMINE() do {                                        \
    SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, opd_si1, opd_si2);   \
                                                                        \
    if (scm_vm_do_op_eframe(vm, opd_si1) < 0)                           \
      goto scm_vm_op_edemine__end;                                      \
                                                                        \
    for (int i = 0; i < opd_si1; i++) {                                 \
      int r = scm_vm_do_op_sref(vm, i, 0);                              \
      if (r < 0) goto scm_vm_op_edemine__end;                           \
                                                                        \
      r = scm_vm_do_op_demine(vm, i, opd_si2 + 1);                      \
      if (r < 0) goto scm_vm_op_edemine__end;                           \
    }                                                                   \
                                                                        \
    if (scm_vm_do_op_epop(vm) < 0)                                      \
      goto scm_vm_op_edemine__end;                                      \
                                                                        \
  scm_vm_op_edemine__end:                                               \
    break;                                                              \
                                                                        \
  } while (0)

#define SCM_VM_OP_MRVC() do {                                   \
    SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, opd_si1);       \
    scm_vm_do_op_mrvc(vm, opd_si1);                             \
  } while (0)

#define SCM_VM_OP_MRVE() do {                           \
    SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);     \
    scm_vm_do_op_mrve(vm);                              \
  } while (0)

#define SCM_VM_OP_MODULE() do {                                 \
    SCM_VMINST_FETCH_OPD_OBJ(SCM_VM(vm)->reg.ip, opd_obj1);     \
    scm_vm_do_op_module(vm, opd_obj1);                          \
 } while (0)

static const void **
scm_vm_run_loop(ScmObj vm)
{
  static const void *tbl[] = {
    &&inst_nop, &&inst_halt, &&inst_int, &&inst_cframe, &&inst_eframe,
    &&inst_epop, &&inst_eshift, &&inst_immval, &&inst_push, &&inst_mvpush,
    &&inst_return, &&inst_pcall, &&inst_call, &&inst_tail_call, &&inst_gref,
    &&inst_gdef, &&inst_gset, &&inst_sref, &&inst_sset, &&inst_jmp, &&inst_jmpt,
    &&inst_jmpf, &&inst_box, &&inst_close, &&inst_demine, &&inst_emine,
    &&inst_edemine, &&inst_mrvc, &&inst_mrve, &&inst_module, NULL,
  };

  if (scm_obj_null_p(vm))
    return tbl;

  do {
    ScmObj opd_obj1 = SCM_OBJ_INIT, opd_obj2 = SCM_OBJ_INIT;
    int opd_si1, opd_si2, opd_iof;

    SCM_REFSTK_INIT_REG(&vm,
                        &opd_obj1, &opd_obj2);

    scm_assert_obj_type_accept_null(vm, &SCM_VM_TYPE_INFO);

    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_nop:
    SCM_VM_OP_NOP();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_halt:
    SCM_VM_OP_HALT();
    return tbl;

  inst_int:
    SCM_VM_OP_INT();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_cframe:
    SCM_VM_OP_CFRAME();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_eframe:
    SCM_VM_OP_EFRAME();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_epop:
    SCM_VM_OP_EPOP();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_eshift:
    SCM_VM_OP_ESHIFT();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_immval:
    SCM_VM_OP_IMMVAL();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_push:
    SCM_VM_OP_PUSH();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_mvpush:
    SCM_VM_OP_MVPUSH();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_return:
    SCM_VM_OP_RETURN();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_pcall:
    SCM_VM_OP_PCALL();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_call:
    SCM_VM_OP_CALL();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_tail_call:
    SCM_VM_OP_TAIL_CALL();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_gref:
    SCM_VM_OP_GREF();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_gdef:
    SCM_VM_OP_GDEF();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_gset:
    SCM_VM_OP_GSET();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_sref:
    SCM_VM_OP_SREF();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_sset:
    SCM_VM_OP_SSET();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_jmp:
    SCM_VM_OP_JMP();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_jmpt:
    SCM_VM_OP_JMPT();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_jmpf:
    SCM_VM_OP_JMPF();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_box:
    SCM_VM_OP_BOX();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_close:
    SCM_VM_OP_CLOSE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_demine:
    SCM_VM_OP_DEMINE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_emine:
    SCM_VM_OP_EMINE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_edemine:
    SCM_VM_OP_EDEMINE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_mrvc:
    SCM_VM_OP_MRVC();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_mrve:
    SCM_VM_OP_MRVE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  inst_module:
    SCM_VM_OP_MODULE();
    goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

  } while (0);
}

static int
scm_vm_interrupt_func_run_gc(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_gc_start();
  return 0;
}

static int
scm_vm_interrupt_func_halt(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  SCM_VM(vm)->reg.ip = (scm_byte_t *)&vm_halt_instruction;
  return 0;
}

static int
scm_vm_interrupt_func_raise(ScmObj vm)
{
  int r;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_raised_p(vm))
    return 0;

  r = scm_vm_setup_stat_call_exc_hndlr(vm);
  if (r < 0) return -1;

  r = scm_vm_setup_stat_return(vm);
  if (r < 0) return -1;

  return 0;
}

static int
scm_vm_interrupt_func_raise_cont(ScmObj vm)
{
  int r;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_raised_p(vm))
    return 0;

  r = scm_vm_setup_stat_call_exc_hndlr_cont(vm);
  if (r < 0) return -1;

  r = scm_vm_setup_stat_return(vm);
  if (r < 0) return -1;

  return 0;
}

static int
scm_vm_interrupt_func_return(ScmObj vm)
{
  int r;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  r = scm_vm_do_op_return(vm);
  if (r < 0) return -1;

  return 0;
}

int
scm_vm_initialize(ScmObj vm, ScmObj main_vm)
{
  ScmObj vmss = SCM_OBJ_INIT, vmsr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &main_vm,
                      &vmss, &vmsr);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(main_vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, main, main_vm);

  vmss = scm_vmss_new(SCM_MEM_HEAP, SCM_VM_STACK_INIT_SIZE);
  if (scm_obj_null_p(vmss)) return -1;

  vmsr = scm_vmsr_new(SCM_MEM_HEAP, vmss, scm_vmss_base(vmss), SCM_OBJ_NULL);
  if (scm_obj_null_p(vmsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, stack, vmsr);

  SCM_VM(vm)->reg.sp = scm_vmsr_base(vmsr);
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.partial = 0;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_UNDEF_OBJ);
  SCM_VM(vm)->reg.vc = 1;
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, SCM_NIL_OBJ);
  SCM_VM(vm)->reg.exc.obj = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, SCM_NIL_OBJ);
  SCM_SLOT_SETQ(ScmVM, vm, reg.dw.hndlr, SCM_NIL_OBJ);
  SCM_VM(vm)->reg.dw.n = 0;
  SCM_VM(vm)->reg.flags = 0;

  SCM_VM(vm)->inttbl.table[SCM_VM_INT_GC].func = scm_vm_interrupt_func_run_gc;
  SCM_VM(vm)->inttbl.table[SCM_VM_INT_HALT].func = scm_vm_interrupt_func_halt;
  SCM_VM(vm)->inttbl.table[SCM_VM_INT_RAISE].func = scm_vm_interrupt_func_raise;
  SCM_VM(vm)->inttbl.table[SCM_VM_INT_RAISE_CONT].func = scm_vm_interrupt_func_raise_cont;
  SCM_VM(vm)->inttbl.table[SCM_VM_INT_RETURN].func = scm_vm_interrupt_func_return;
  SCM_VM(vm)->inttbl.activated = 0;

  return 0;
}

void
scm_vm_finalize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->main = SCM_OBJ_NULL;
  SCM_VM(vm)->stack = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.partial = 0;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.vc = 0;
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, SCM_OBJ_NULL);
  SCM_VM(vm)->reg.exc.obj = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, SCM_OBJ_NULL);
  SCM_SLOT_SETQ(ScmVM, vm, reg.dw.hndlr, SCM_OBJ_NULL);
  SCM_VM(vm)->reg.dw.n = 0;
  SCM_VM(vm)->reg.flags = 0;
}

ScmObj
scm_vm_new(void)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  vm = scm_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, vm);
  if (rslt < 0) {
    scm_free_root(vm);
    return SCM_OBJ_NULL;
  }

  return vm;
}

void
scm_vm_end(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_free_root(vm);
}

ScmObj
scm_vm_clone(ScmObj parent)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&parent,
                      &vm);

  scm_assert_obj_type(parent, &SCM_VM_TYPE_INFO);

  vm = scm_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, SCM_VM(parent)->main);
  if (rslt < 0) {
    scm_free_root(vm);
    return SCM_OBJ_NULL;
  }

  return vm;
}

static void
scm_vm_run_init(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vmsr_clear(SCM_VM(vm)->stack);

  SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.partial = 0;
  SCM_VM(vm)->reg.flags = 0;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_UNDEF_OBJ);
  SCM_VM(vm)->reg.vc = 1;

  SCM_VM(vm)->inttbl.activated = 0;
}

void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  SCM_REFSTK_INIT_REG(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_iseq_p(iseq));

  scm_vm_run_init(vm);

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp,
                scm_make_closure(iseq, SCM_OBJ_NULL, 0));
  SCM_VM(vm)->reg.ip = scm_iseq_to_ip(iseq);

  scm_vm_run_loop(vm);

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
}

static ScmObj
scm_vm_val_reg_to_vector(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  int n, rest;

  SCM_REFSTK_INIT_REG(&vm,
                      &val, &elm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if ((size_t)SCM_VM(vm)->reg.vc > SSIZE_MAX) {
    scm_error("failed to make vector from VAL registers: too many values",
                  0);
    return SCM_OBJ_NULL;
  }

  val = scm_make_vector((size_t)SCM_VM(vm)->reg.vc, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  n = (SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
    SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG - 1;
  for (size_t i = 0; i < (size_t)n; i++)
    scm_vector_set(val, i, SCM_VM(vm)->reg.val[i]);

  rest = SCM_VM(vm)->reg.vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    for (int i = 0; i < rest; i++) {
      elm = scm_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                           (size_t)i);
      scm_vector_set(val, (size_t)(SCM_VM_NR_VAL_REG - 1 + i), elm);
    }
  }

  return val;
}

ScmObj
scm_vm_apply(ScmObj vm, ScmObj proc, ScmObj args)
{
  ScmObj asmb = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &proc, &args,
                      &asmb);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_procedure_p(proc));
  scm_assert(scm_nil_p(args) || scm_pair_p(args));

  asmb = scm_make_assembler(SCM_OBJ_NULL);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_vm_make_proc_call_code(asmb, proc, args, false);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_asm_push(asmb, SCM_OPCODE_HALT);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_asm_commit(asmb);
  if (r < 0) return SCM_OBJ_NULL;

  scm_vm_run(vm, scm_asm_iseq(asmb));

  if (scm_vm_raised_p(vm))
    return SCM_OBJ_NULL;
  else
    return scm_vm_val_reg_to_vector(vm);
}

int
scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc)
{
  ScmObj vec = SCM_OBJ_INIT;
  int n, rest;

  SCM_REFSTK_INIT_REG(&vm,
                      &vec);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(vc == 0 || val != NULL);
  scm_assert(vc >= 0);

  n = (vc <= SCM_VM_NR_VAL_REG) ? vc : SCM_VM_NR_VAL_REG - 1;
  for (int i = 0; i < n; i++) {
    if (scm_obj_null_p(val[i])) {
      scm_error("invalid return value is set", 0);
      return -1;
    }
    SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], val[i]);
  }

  rest = vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    vec = scm_make_vector((size_t)rest, SCM_OBJ_NULL);
    if (scm_obj_null_p(vec)) return -1;

    for (int i = 0; i < rest; i++) {
      if (scm_obj_null_p(val[SCM_VM_NR_VAL_REG - 1 + i])) {
        scm_error("invalid return value is set", 0);
        return -1;
      }

      scm_vector_set(vec, (size_t)i, val[SCM_VM_NR_VAL_REG - 1 + i]);
    }

    SCM_SLOT_SETQ(ScmVM, vm, reg.val[SCM_VM_NR_VAL_REG - 1], vec);
  }

  SCM_VM(vm)->reg.vc = vc;

  return 0;
}

ScmObj
scm_vm_capture_cont(ScmObj vm)
{
  ScmObj cc = SCM_OBJ_INIT, stack = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &cc, &stack);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  cc = scm_contcap_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(cc)) return SCM_OBJ_NULL;

  stack = scm_vm_capture_stack(vm);
  if (scm_obj_null_p(stack)) return SCM_OBJ_NULL;

  scm_contcap_cap(cc, stack, &SCM_VM(vm)->reg);

  return cc;
}

int
scm_vm_reinstatement_cont(ScmObj vm, ScmObj cc)
{
  const ScmObj *v;
  int n, rslt;

  SCM_REFSTK_INIT_REG(&vm, &cc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  SCM_VM(vm)->reg.vc = scm_contcap_vc(cc);
  n = ((SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG);
  v = scm_contcap_val(cc);
  for (int i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], v[0]);

  rslt = scm_vm_restore_stack(vm, scm_contcap_stack(cc));
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_contcap_cp(cc));
  SCM_VM(vm)->reg.ip = scm_contcap_ip(cc);
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, scm_contcap_prm(cc));
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.obj, scm_contcap_exc_obj(cc));
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, scm_contcap_exc_hndlr(cc));
  SCM_SLOT_SETQ(ScmVM, vm, reg.dw.hndlr, scm_contcap_dw_hndlr(cc));
  SCM_VM(vm)->reg.dw.n = scm_contcap_dw_num(cc);
  SCM_VM(vm)->reg.flags = scm_contcap_flags(cc);

  return 0;
}

int
scm_vm_push_dynamic_bindings(ScmObj vm, ScmObj alist)
{
  ScmObj x = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &x);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_nil_p(alist) || scm_pair_p(alist));

  x = scm_cons(alist, SCM_VM(vm)->reg.prm);
  if (scm_obj_null_p(x)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, x);

  return 0;
}

void
scm_vm_pop_dynamic_bindings(ScmObj vm)
{
  ScmObj x = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_nil_p(SCM_VM(vm)->reg.prm))
    return;

  x = scm_cdr(SCM_VM(vm)->reg.prm);
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, x);
}

ScmObj
scm_vm_parameter_value(ScmObj vm, ScmObj var)
{
  ScmObj v = SCM_OBJ_INIT, x = SCM_OBJ_INIT, p = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &var,
                      &v, &x, &p);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(var));

  for (x = SCM_VM(vm)->reg.prm; scm_pair_p(x); x = scm_cdr(x)) {
    p = scm_assq(var, scm_car(x));
    if (scm_obj_null_p(p)) return SCM_OBJ_NULL;

    if (scm_pair_p(p))
      return scm_cdr(p);
  }

  if (!scm_parameter_p(var)) {
    scm_error("failed to get bound value: unbound variable", 1, var);
    return SCM_OBJ_NULL;
  }

  v = scm_parameter_init_val(var);
  if (scm_obj_null_p(v)) {
    scm_error("failed to get bound value: "
                   "parameter does not have initial value", 1, var);
    return SCM_OBJ_NULL;
  }

  return v;
}

int
scm_vm_setup_stat_trmp(ScmObj vm, ScmObj proc, ScmObj args,
                       ScmObj postproc, ScmObj handover,
                       bool tail)
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int rslt;

  SCM_REFSTK_INIT_REG(&vm, &proc, &args, &postproc, &handover,
                      &trmp_code, &trmp_clsr, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_procedure_p(proc));
  scm_assert(scm_nil_p(args) || scm_pair_p(args));
  scm_assert(scm_obj_null_p(postproc) || scm_procedure_p(postproc));

  trmp_code = scm_vm_make_trampolining_code(vm, proc, args,
                                            postproc, handover, tail);
  if (scm_obj_null_p(trmp_code)) return -1;

  env = SCM_OBJ_NULL;
  if (scm_closure_p(SCM_VM(vm)->reg.cp))
    env = scm_closure_env(SCM_VM(vm)->reg.cp);

  trmp_clsr = scm_make_closure(trmp_code, env, 0);
  if (scm_obj_null_p(trmp_clsr)) return -1;


  ip = scm_iseq_to_ip(trmp_code);
  if (ip == NULL) return -1;

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            trmp_clsr,
                            ip);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_NIL_OBJ);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

int
scm_vm_setup_stat_halt(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return scm_vm_interrupt_activate(vm, SCM_VM_INT_HALT);
}

int
scm_vm_setup_stat_raise(ScmObj vm, ScmObj obj, bool continuable)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_RAISE)) {
    scm_fatal("Exception has raised in the situation can be addressed");
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.obj, obj);
  if (continuable)
    return scm_vm_interrupt_activate(vm, SCM_VM_INT_RAISE_CONT);
  else
    return scm_vm_interrupt_activate(vm, SCM_VM_INT_RAISE);
}

int
scm_vm_setup_stat_return(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return scm_vm_interrupt_activate(vm, SCM_VM_INT_RETURN);
}

int
scm_vm_setup_stat_call_exc_hndlr(ScmObj vm)
{
  ScmObj proc = SCM_OBJ_INIT, args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm,
                      &proc, &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_cons(SCM_VM(vm)->reg.exc.obj, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  proc = scm_premade_procedure(SCM_PREMADE_PROC_EXC_HANDLER_CALLER);
  return scm_vm_setup_stat_trmp(vm, proc, args,
                                SCM_OBJ_NULL, SCM_OBJ_NULL, false);
}

int
scm_vm_setup_stat_call_exc_hndlr_cont(ScmObj vm)
{
  ScmObj proc = SCM_OBJ_INIT,  args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm,
                      &proc, &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_cons(SCM_VM(vm)->reg.exc.obj, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  proc = scm_premade_procedure(SCM_PREMADE_PROC_EXC_HANDLER_CALLER_CONT);
  return scm_vm_setup_stat_trmp(vm, proc, args,
                                SCM_OBJ_NULL, SCM_OBJ_NULL, true);

  return 0;
}

int
scm_vm_push_exc_handler(ScmObj vm, ScmObj hndlr)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &hndlr, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_procedure_p(hndlr));

  lst = scm_cons(hndlr, SCM_VM(vm)->reg.exc.hndlr);
  if (scm_obj_null_p(lst)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, lst);

  return 0;
}

int
scm_vm_pop_exc_handler(ScmObj vm)
{
  ScmObj rest = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &rest);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_pair_p(SCM_VM(vm)->reg.exc.hndlr))
    return 0;

  rest = scm_cdr(SCM_VM(vm)->reg.exc.hndlr);
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, rest);

  return 0;
}

int
scm_vm_exc_handler(ScmObj vm, scm_csetter_t *hndlr)
{
  ScmObj val = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(hndlr != NULL);

  if (scm_pair_p(SCM_VM(vm)->reg.exc.hndlr))
    val = scm_car(SCM_VM(vm)->reg.exc.hndlr);
  else
    val = SCM_OBJ_NULL;

  scm_csetter_setq(hndlr, val);

  return 0;
}

int
scm_vm_subr_exc_hndlr_caller(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj vm = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT, hndlr_arg = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT;
  int rslt, ret;

  SCM_REFSTK_INIT_REG(&subr,
                      &vm, &hndlr, &hndlr_arg,
                      &val);

  vm = scm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_exc_handler(vm, SCM_CSETTER_L(hndlr));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(hndlr)) {
    SCM_SLOT_SETQ(ScmVM, vm, reg.exc.obj, argv[0]);
    scm_vm_setup_stat_halt(vm);
    val = SCM_UNDEF_OBJ;
    ret = scm_vm_set_val_reg(vm, &val, 1);
    goto end;
  }

  rslt = scm_vm_pop_exc_handler(vm);
  if (rslt < 0) return -1;

  hndlr_arg = scm_cons(argv[0], SCM_NIL_OBJ);
  if (scm_obj_null_p(hndlr_arg)) return -1;

  ret = scm_vm_setup_stat_trmp(vm, hndlr, hndlr_arg, subr, argv[0], true);

 end:
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
  return ret;
}

int
scm_vm_subr_exc_hndlr_caller_cont(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj vm = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT, hndlr_arg = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT, proc = SCM_OBJ_INIT;
  int rslt, ret;

  SCM_REFSTK_INIT_REG(&subr,
                      &vm, &hndlr, &hndlr_arg,
                      &val, &proc);

  vm = scm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_exc_handler(vm, SCM_CSETTER_L(hndlr));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(hndlr)) {
    SCM_SLOT_SETQ(ScmVM, vm, reg.exc.obj, argv[0]);
    scm_vm_setup_stat_halt(vm);
    val = SCM_UNDEF_OBJ;
    ret = scm_vm_set_val_reg(vm, &val, 1);
    goto end;
  }

  rslt = scm_vm_pop_exc_handler(vm);
  if (rslt < 0) return -1;

  hndlr_arg = scm_cons(argv[0], SCM_NIL_OBJ);
  if (scm_obj_null_p(hndlr_arg)) return -1;

  proc = scm_premade_procedure(SCM_PREMADE_PROC_EXC_HANDLER_CALLER_POST);
  ret = scm_vm_setup_stat_trmp(vm, hndlr, hndlr_arg, proc, hndlr, true);

 end:
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
  return ret;
}

int
scm_vm_subr_exc_hndlr_caller_post(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj vm = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&subr,
                      &vm, &hndlr, &val)

    vm = scm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_push_exc_handler(vm, argv[0]);
  if (rslt < 0) goto end;

  rslt = scm_return_val(argv + 1, argc - 1);

 end:
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
  return rslt;
}

void
scm_vm_disposal_unhandled_exc(ScmObj vm)
{
  ScmObj raised = SCM_OBJ_INIT, port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  char msg[256], *p;
  int rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &raised, &port, &str);

  if (scm_obj_null_p(SCM_VM(vm)->reg.exc.obj))
    return;

  port = scm_open_output_string();
  if (scm_obj_null_p(port)) return;

  rslt = scm_write_cstr("Unhandled Exception: ", SCM_ENC_SRC, port);
  if (rslt < 0) return;

  rslt = scm_display(SCM_VM(vm)->reg.exc.obj, port);
  if (rslt < 0) return;

  rslt = scm_newline(port);
  if (rslt < 0) return;

  str = scm_get_output_string(port);
  if (scm_obj_null_p(str)) return;

  p = scm_string_to_cstr(str, msg, sizeof(msg));
  if (p == NULL) return;

  scm_bedrock_error(scm_current_br(), msg);
}

int
scm_vm_push_dw_handler(ScmObj vm, ScmObj before, ScmObj after)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &before, &after, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_procedure_p(before));
  scm_assert(scm_procedure_p(after));
  scm_assert(SCM_VM(vm)->reg.dw.n < SIZE_MAX);

  lst = scm_cons(before, after);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_cons(lst, SCM_VM(vm)->reg.dw.hndlr);
  if (scm_obj_null_p(lst)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.dw.hndlr, lst);
  SCM_VM(vm)->reg.dw.n++;

  return 0;
}

int
scm_vm_pop_dw_handler(ScmObj vm)
{
  ScmObj rest = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &rest);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_pair_p(SCM_VM(vm)->reg.dw.hndlr))
    return 0;

  rest = scm_cdr(SCM_VM(vm)->reg.dw.hndlr);
  SCM_SLOT_SETQ(ScmVM, vm, reg.dw.hndlr, rest);
  SCM_VM(vm)->reg.dw.n--;

  return 0;
}

static int
scm_vm_collect_dw_handler_internal(ScmObj from, size_t nf, ScmObj to, size_t nt,
                                   scm_csetter_t *acc)
{
  ScmObj ff = SCM_OBJ_INIT, tt = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT;
  ScmObj ac = SCM_OBJ_INIT;
  size_t f, t;
  int r;

  SCM_REFSTK_INIT_REG(&from, &to,
                      &ff, &tt, &hndlr);

  scm_assert(scm_pair_p(from) || scm_nil_p(from));
  scm_assert(scm_pair_p(to) || scm_nil_p(to));

  if (scm_eq_p(from, to))
    return 0;

  ff = from; f = nf;
  tt = to; t = nt;
  if (nf >= nt) {
    ff = scm_cdr(from);
    f = nf - 1;
  }

  if (nt >= nf) {
    hndlr = scm_cxr(to, "aa");
    scm_assert(scm_procedure_p(hndlr));

    ac = scm_cons(hndlr, scm_csetter_val(acc));
    if (scm_obj_null_p(ac)) return -1;

    scm_csetter_setq(acc, ac);
    tt = scm_cdr(to);
    t = nt - 1;
  }

  r = scm_vm_collect_dw_handler_internal(ff, f, tt, t, acc);
  if (r < 0) return -1;

  if (nf >= nt) {
    hndlr = scm_cxr(from, "da");
    scm_assert(scm_procedure_p(hndlr));

    ac = scm_cons(hndlr, scm_csetter_val(acc));
    if (scm_obj_null_p(ac)) return -1;

    scm_csetter_setq(acc, ac);
  }

  return 0;
}

ScmObj
scm_vm_collect_dw_handler(ScmObj vm, ScmObj contcap)
{
  ScmObj acc = SCM_OBJ_NULL;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &contcap,
                      &acc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(contcap, &SCM_CONTCAP_TYPE_INFO);

  acc = SCM_NIL_OBJ;
  r = scm_vm_collect_dw_handler_internal(SCM_VM(vm)->reg.dw.hndlr,
                                         SCM_VM(vm)->reg.dw.n,
                                         scm_contcap_dw_hndlr(contcap),
                                         scm_contcap_dw_num(contcap),
                                         SCM_CSETTER_L(acc));
  if (r < 0) return SCM_OBJ_NULL;

  return acc;
}

const void *
scm_vm_opcode2ptr(scm_opcode_t op)
{
  return vm_dispatch_table[op];
}

scm_opcode_t
scm_vm_ptr2opcode(const void *ptr)
{
  for (const void **p = vm_dispatch_table; *p != NULL; p++) {
    if (*p == ptr)
      return (scm_opcode_t)(p - vm_dispatch_table);
  }

  scm_assert(false);
  return 0;
}

void
scm_vm_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);

  SCM_VM(obj)->main = SCM_OBJ_NULL;
  SCM_VM(obj)->stack = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.sp = NULL;
  SCM_VM(obj)->reg.cfp = NULL;
  SCM_VM(obj)->reg.efp = NULL;
  SCM_VM(obj)->reg.partial = 0;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.vc = 0;
  SCM_VM(obj)->reg.prm = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.exc.obj = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.exc.hndlr = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.dw.hndlr = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.dw.n = 0;
  SCM_VM(obj)->reg.flags = 0;
}

void
scm_vm_gc_finalize(ScmObj obj)
{
  scm_vm_finalize(obj);
}

static int
scm_vm_gc_accept_stack(ScmObj vm, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_obj_null_p(SCM_VM(vm)->stack)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, SCM_VM(vm)->stack);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  if (SCM_VM(vm)->reg.sp != NULL) {
    ScmObj *partial = (ScmObj *)SCM_VM(vm)->reg.sp - SCM_VM(vm)->reg.partial;
    for (int i = 0; i < SCM_VM(vm)->reg.partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, partial[i]);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  rslt = scm_vm_cf_gc_accept(vm, SCM_VM(vm)->reg.cfp, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(vm, SCM_VM(vm)->reg.efp, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

int
scm_vm_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int n, rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->main);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  n = ((SCM_VM(obj)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(obj)->reg.vc : SCM_VM_NR_VAL_REG);
  for (int i = 0; i < n; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val[i]);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.cp);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.prm);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.exc.obj);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.exc.hndlr);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.dw.hndlr);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_stack(obj, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

void
scm_prepare_vm(void)
{
  scm_assert(vm_dispatch_table == NULL);

  vm_dispatch_table = scm_vm_run_loop(SCM_OBJ_NULL);

  for (int i = 0; i < SCM_VM_NR_INTERRUPTIONS; i++) {
    vm_int_instructions[i].op =
      (scm_opcode_t)scm_vm_opcode2ptr(vm_int_instructions[i].op);
  }

  vm_halt_instruction.op =
    (scm_opcode_t)scm_vm_opcode2ptr(vm_halt_instruction.op);
}


/***************************************************************************/
/*  Facade                                                                 */
/***************************************************************************/

ScmObj scm__current_vm = SCM_OBJ_INIT;

int
scm_exec_iseq(ScmObj iseq)
{
  ScmObj o = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&iseq,
                      &o);

  scm_assert(scm_iseq_p(iseq));

  o = scm_make_assembler(iseq);
  if (scm_obj_null_p(o)) return -1;

  r = scm_asm_push(o, SCM_OPCODE_HALT);
  if (r < 0) return -1;

  r = scm_asm_commit(o);
  if (r < 0) return -1;

  scm_vm_run(scm_current_vm(), scm_asm_iseq(o));
  if (scm_vm_raised_p(scm_current_vm()))
    return -1;

  return 0;
}
