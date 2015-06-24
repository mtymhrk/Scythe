#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>

#include "scythe/vm.h"
#include "scythe/vmstack.h"
#include "scythe/vminst.h"
#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/impl_utils.h"


/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

#define SCM_BEDROCK_ERR_MSG_SIZE 256

static void
scm_bedrock_print_msg(ScmBedrock *br, const char *msg)
{
  size_t len;

  scm_assert(br != NULL);
  scm_assert(msg != NULL);

  fputs(msg, br->output);

  len = strlen(msg);
  if (msg[len - 1] != '\n')
    fputc('\n', br->output);
}

int
scm_bedrock_setup(ScmBedrock *br)
{
  scm_assert(br != NULL);

  br->cnsts.nil = scm_fcd_nil_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.nil)) return -1;

  br->cnsts.eof = scm_fcd_eof_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.eof)) return -1;

  br->cnsts.b_true = scm_fcd_bool_new(SCM_MEM_ROOT, true);
  if (scm_obj_null_p(br->cnsts.b_true)) return -1;

  br->cnsts.b_false = scm_fcd_bool_new(SCM_MEM_ROOT, false);
  if (scm_obj_null_p(br->cnsts.b_false)) return -1;

  br->cnsts.undef = scm_fcd_undef_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.undef)) return -1;

  br->cnsts.landmine = scm_fcd_landmine_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.landmine)) return -1;

  br->symtbl = scm_fcd_symtbl_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->symtbl)) return -1;

  br->modtree = scm_fcd_moduletree_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->modtree)) return -1;

  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller));

  br->subr.exc_hndlr_caller =
    scm_fcd_make_subrutine(scm_vm_subr_exc_hndlr_caller, -2, 0, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller)) return -1;

  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller_cont));

  br->subr.exc_hndlr_caller_cont =
    scm_fcd_make_subrutine(scm_vm_subr_exc_hndlr_caller_cont,
                            1, 0, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller_cont)) return -1;

  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller_post));

  br->subr.exc_hndlr_caller_post =
    scm_fcd_make_subrutine(scm_vm_subr_exc_hndlr_caller_post,
                            -2, SCM_PROC_ADJ_UNWISHED, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller_post)) return -1;

  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.trmp_apply));

  br->subr.trmp_apply =
    scm_fcd_make_subrutine(scm_vm_subr_trmp_apply, -3, 0, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.trmp_apply)) return -1;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->gv[i]));

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(br->sym[i]));

  return 0;
}

int
scm_bedrock_cleanup(ScmBedrock *br)
{
  scm_assert(br != NULL);

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    br->sym[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    br->gv[i] = SCM_OBJ_NULL;

  br->subr.trmp_apply = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller_post = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller_cont = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller = SCM_OBJ_NULL;

  if (scm_obj_not_null_p(br->modtree)) {
    scm_fcd_mem_free_root(br->modtree);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->symtbl)) {
    scm_fcd_mem_free_root(br->symtbl);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.landmine)) {
    scm_fcd_mem_free_root(br->cnsts.landmine);
    br->cnsts.landmine = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.undef)) {
    scm_fcd_mem_free_root(br->cnsts.undef);
    br->cnsts.undef = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_false)) {
    scm_fcd_mem_free_root(br->cnsts.b_false);
    br->cnsts.b_false = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_true)) {
    scm_fcd_mem_free_root(br->cnsts.b_true);
    br->cnsts.b_true = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.eof)) {
    scm_fcd_mem_free_root(br->cnsts.eof);
    br->cnsts.eof = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.nil)) {
    scm_fcd_mem_free_root(br->cnsts.nil);
    br->cnsts.nil = SCM_OBJ_NULL;
  }

  return 0;
}

void
scm_bedrock_set_mem(ScmBedrock *br, ScmMem *mem)
{
  scm_assert(br != NULL);

  br->mem = mem;
}

int
scm_bedrock_initialize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  br->output = stderr;
  br->err_type = SCM_BEDROCK_ERR_NONE;
  br->exit_stat = 0;

  br->mem = scm_fcd_mem_new();
  if (br->mem == NULL) return -1;

  br->cnsts.nil = SCM_OBJ_NULL;
  br->cnsts.eof = SCM_OBJ_NULL;
  br->cnsts.b_true = SCM_OBJ_NULL;
  br->cnsts.b_false = SCM_OBJ_NULL;
  br->cnsts.undef = SCM_OBJ_NULL;
  br->cnsts.landmine = SCM_OBJ_NULL;

  br->symtbl = SCM_OBJ_NULL;
  br->modtree = SCM_OBJ_NULL;

  br->subr.exc_hndlr_caller = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller_cont = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller_post = SCM_OBJ_NULL;
  br->subr.trmp_apply = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_GV_NR; i++)
    br->gv[i] = SCM_OBJ_NULL;

  for (size_t i = 0; i < SCM_CACHED_SYM_NR; i++)
    br->sym[i] = SCM_OBJ_NULL;

  br->encoding = SCM_ENC_UTF8;

  return 0;
}

void
scm_bedrock_finalize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if (br->mem != NULL) {
    scm_fcd_mem_end(br->mem);
    br->mem = NULL;
  }
}

ScmBedrock *
scm_bedrock_new(void)
{
  int rslt;
  ScmBedrock *br;

  br = malloc(sizeof(*br));
  if (br == NULL) return NULL;

  rslt = scm_bedrock_initialize(br);
  if (rslt < 0) {
    free(br);
    return NULL;
  }

  scm_fcd_chg_current_br(br);

  return br;
}

void
scm_bedrock_end(ScmBedrock *br)
{
  scm_assert(br != NULL);

  scm_fcd_chg_current_br(NULL);
  scm_bedrock_finalize(br);
  free(br);
}


void
scm_bedrock_fatal(ScmBedrock *br, const char *msg)
{
  scm_assert(br != NULL);

  br->err_type = SCM_BEDROCK_ERR_FATAL;

  if (msg != NULL)
    scm_bedrock_print_msg(br, msg);

  if (scm_obj_not_null_p(scm_fcd_current_vm()))
    scm_vm_setup_stat_halt(scm_fcd_current_vm());
}

void
scm_bedrock_error(ScmBedrock *br, const char *msg)
{
  scm_assert(br != NULL);

  if (br->err_type != SCM_BEDROCK_ERR_FATAL)
    br->err_type = SCM_BEDROCK_ERR_ERROR;

  if (msg != NULL)
    scm_bedrock_print_msg(br, msg);
}

bool
scm_bedrock_fatal_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  return (br->err_type == SCM_BEDROCK_ERR_FATAL) ? true : false;
}

bool
scm_bedrock_error_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if ((br->err_type == SCM_BEDROCK_ERR_FATAL
       || br->err_type == SCM_BEDROCK_ERR_ERROR))
    return true;
  else
    return false;
}

static int
scm_bedrock_search_gv(const char *sym_str, const char * const *name_str, size_t n,
                      scm_csetter_t *gloc)
{
  ScmObj sym = SCM_OBJ_INIT, mod = SCM_OBJ_INIT, name = SCM_OBJ_INIT;
  ScmObj name_sym[n];
  int r;

  for (size_t i = 0; i < n; i++) name_sym[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&sym, &mod, &name);
  SCM_REFSTK_REG_ARY(name_sym, n);

  scm_assert(sym_str != NULL);
  scm_assert(name_str != NULL);
  scm_assert(gloc != NULL);

  for (size_t i = 0; i < n; i++) {
    name_sym[i] = scm_fcd_make_symbol_from_cstr(name_str[i], SCM_ENC_SRC);
    if (scm_obj_null_p(name_sym[i])) return -1;
  }

  sym = scm_fcd_make_symbol_from_cstr(sym_str, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return -1;

  name = scm_fcd_list_cv(name_sym, n);
  if (scm_obj_null_p(name)) return -1;

  r = scm_fcd_find_module(name, SCM_CSETTER_L(mod));
  if (r < 0) return -1;

  if (scm_obj_null_p(mod)) {
    scm_csetter_setq(gloc, SCM_OBJ_NULL);
    return 0;
  }

  return scm_fcd_find_gloc(mod, sym, gloc);
}

static int
scm_bedrock_cached_gv_aux(ScmRef holder,
                          const char *sym, const char * const *name, size_t n,
                          scm_csetter_t *gloc)
{
  int r;

  scm_assert(holder != SCM_REF_NULL);
  scm_assert(sym != NULL);
  scm_assert(name != NULL);
  scm_assert(gloc != NULL);

  if (scm_obj_not_null_p(SCM_REF_DEREF(holder))) {
    scm_csetter_setq(gloc, SCM_REF_DEREF(holder));
    return 0;
  }

  r = scm_bedrock_search_gv(sym, name, n, gloc);
  if (r < 0) return -1;

  SCM_REF_UPDATE(holder, scm_csetter_val(gloc));
  return 0;
}

int
scm_bedrock_cached_gv(ScmBedrock *br, int kind, scm_csetter_t *gloc)
{
  static const struct {
    const char *sym_name;    const char *mod_name[3]; size_t n;
  } tbl[] = {
    { "compile",             { "scythe", "internal", "compile" }, 3 },
    { "eval",                { "scythe", "base" }, 2 },
    { "current-input-port",  { "scythe", "base" }, 2 },
    { "current-output-port", { "scythe", "base" }, 2 },
  };

  scm_assert(br != NULL);
  scm_assert(gloc != NULL);
  scm_assert(kind < SCM_CACHED_GV_NR);

  return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv[kind]),
                                   tbl[kind].sym_name,
                                   tbl[kind].mod_name,
                                   tbl[kind].n,
                                   gloc);
}

ScmObj
scm_bedrock_cached_sym(ScmBedrock *br, int kind)
{
  static const char *tbl[] = {
    "quote", "quasiquote", "unquote", "unquote-splicing",
  };

  scm_assert(br != NULL);
  scm_assert(kind < SCM_CACHED_SYM_NR);

  if (scm_obj_not_null_p(br->sym[kind]))
    return br->sym[kind];

  br->sym[kind] = scm_fcd_make_symbol_from_cstr(tbl[kind], SCM_ENC_SRC);
  return br->sym[kind];
}


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
  .gc_accept_func      = scm_contcap_gc_accepct,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_contcap_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj cc = SCM_OBJ_INIT;

  cc = scm_fcd_mem_alloc(&SCM_CONTCAP_TYPE_INFO, 0, mtype);
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
scm_contcap_gc_initialize(ScmObj obj, ScmObj mem)
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
scm_contcap_gc_accepct(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  int n;

  scm_assert_obj_type(obj, &SCM_CONTCAP_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->stack, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->reg.cp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  n = ((SCM_CONTCAP(obj)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_CONTCAP(obj)->reg.vc : SCM_VM_NR_VAL_REG);
  for (int i = 0; i < n; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_CONTCAP(obj)->reg.val[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_CONTCAP(obj)->reg.prm, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.exc.obj, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.exc.hndlr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.dw.hndlr, mem);
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


static inline void
scm_vm_ctrl_flg_set(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags |= flg;
}

static inline void
scm_vm_ctrl_flg_clr(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags &= ~flg;
}

static inline bool
scm_vm_ctrl_flg_set_p(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
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
    if (scm_fcd_eq_p(SCM_VM(vm)->stack, stack))
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
      scm_fcd_error("stack underflow has occurred", 0);
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
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
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
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (idx >= e->len) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
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
      scm_fcd_error("invalid access to envrionment frame: out of range", 0);
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

  scm_assert(scm_fcd_assembler_p(asmb));
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));

  arity = scm_fcd_arity(proc);
  unwished = scm_fcd_procedure_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED);

  len = scm_fcd_length(args);
  if (len < 0) return -1;

  if (arity >= 0) {
    if (len != arity) {
      scm_fcd_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = arity;
  }
  else {
    if (len < -arity - 1) {
      scm_fcd_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = unwished ? (int)len : -arity - 1;
  }

  label_call = 0;

  if (!tail) {
    label_call = scm_fcd_assembler_assign_label_id(asmb);
    if (label_call < 0) return -1;

    r = scm_fcd_assembler_push(asmb,
                               SCM_OPCODE_CFRAME, true, label_call);
    if (r < 0) return -1;
  }

  if (nr_decons > 0 || arity < 0) {
    for (cur = args, i = 0;
         scm_fcd_pair_p(cur) && i < nr_decons;
         cur = scm_fcd_cdr(cur), i++) {
      arg = scm_fcd_car(cur);
      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, arg);
      if (r < 0) return -1;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }

    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (arity < 0 && !unwished) {
      cur = scm_fcd_list_copy(cur);
      if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, cur);
      if (r < 0) return -1;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }
  }

  r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, proc);
  if (r < 0) return -1;

  if (tail) {
    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_TAIL_CALL,
                               unwished ? (int)len : arity);
    if (r < 0) return -1;
  }
  else {
    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_CALL,
                               unwished ? (int)len : arity);
    if (r < 0) return -1;

    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_NOP);
    if (r < 0) return -1;

    r = scm_fcd_assembler_push(asmb, SCM_ASM_PI_LABEL, (size_t)label_call);
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

  if (scm_fcd_nil_p(argv[2])) {
    args = argv[1];
  }
  else {
    args = scm_fcd_car(argv[2]);
    args = scm_fcd_cons(argv[1], args);
    if (scm_obj_null_p(args))
      return -1;
  }

  return scm_fcd_trampolining(argv[0], args, SCM_OBJ_NULL, SCM_OBJ_NULL);
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
  scm_assert(scm_obj_null_p(postproc) || scm_fcd_procedure_p(postproc));

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

  asmb = scm_fcd_make_assembler(SCM_OBJ_NULL);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_fcd_assembler_push(asmb, SCM_OPCODE_NOP);
  if (r < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    if (!tail) {
      label_call = scm_fcd_assembler_assign_label_id(asmb);
      if (label_call < 0) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb,
                                 SCM_OPCODE_CFRAME, true, label_call);
      if (r < 0) return SCM_OBJ_NULL;
    }

    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, postproc);
    if (r < 0) return SCM_OBJ_NULL;

    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_PUSH);
    if (r < 0) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(handover)) {
      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, handover);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_PUSH);
      if (r < 0) return SCM_OBJ_NULL;

      apply_argc = 3;
    }
  }

  r = scm_vm_make_proc_call_code(asmb, proc, args,
                                 (tail && scm_obj_null_p(postproc)));
  if (r < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_MRVC, -1);
    if (r < 0) return SCM_OBJ_NULL;

    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_MVPUSH);
    if (r < 0) return SCM_OBJ_NULL;

    apply = scm_bedrock_trmp_apply(scm_fcd_current_br());
    r = scm_fcd_assembler_push(asmb, SCM_OPCODE_IMMVAL, apply);
    if (r < 0) return SCM_OBJ_NULL;

    if (tail) {
      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_TAIL_CALL, apply_argc);
      if (r < 0) return SCM_OBJ_NULL;
    }
    else {
      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_CALL, apply_argc);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_NOP);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb, SCM_ASM_PI_LABEL, label_call);
      if (r < 0) return SCM_OBJ_NULL;

      r = scm_fcd_assembler_push(asmb, SCM_OPCODE_HALT);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  r = scm_fcd_assembler_commit(asmb);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_assembler_iseq(asmb);
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
  static struct scm_vm_inst_si scm_vm_int_inst[SCM_VM_NR_INTERRUPTIONS] = {
    { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_GC },
    { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_HALT },
    { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RAISE },
    { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RAISE_CONT },
    { .op = SCM_OPCODE_INT, .opd1 = SCM_VM_INT_RETURN },
  };
  int prior;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(0 <= num && num < SCM_VM_NR_INTERRUPTIONS);

  if (scm_vm_interrupt_act_p(vm, num))
    return 0;

  prior = scm_vm_interrupt_prior(vm, num);
  if (prior >= 0) {
    SCM_VM(vm)->inttbl.table[num].save = SCM_VM(vm)->inttbl.table[prior].save;
    SCM_VM(vm)->inttbl.table[prior].save = (scm_byte_t *)&scm_vm_int_inst[num];
  }
  else {
    SCM_VM(vm)->inttbl.table[num].save = SCM_VM(vm)->reg.ip;
    SCM_VM(vm)->reg.ip = (scm_byte_t *)&scm_vm_int_inst[num];
  }

  /* TODO: opcode を変換しているのは非効率なので改善する */
  scm_vm_int_inst[num].op = (scm_opcode_t)scm_vm_opcode2ptr(SCM_OPCODE_INT);

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
    scm_fcd_error("too many return values", 0);
    return -1;
    break;
  case 0:
    break;
  case -1:
    scm_fcd_error("too few return values", 0);
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
      obj = scm_fcd_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                                (size_t)(i - SCM_VM_NR_VAL_REG));
    else
      obj = SCM_VM(vm)->reg.val[i - 1];

    lst = scm_fcd_cons(obj, lst);
    if (scm_obj_null_p(lst)) return -1;
  }

  if (nr > SCM_VM_NR_VAL_REG) {
    if (SCM_VM(vm)->reg.vc == SCM_VM_NR_VAL_REG) {
      obj = scm_fcd_vector(2, SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1], lst);
      if (scm_obj_null_p(obj)) return -1;

      SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1] = obj;
    }
    else if (SCM_VM(vm)->reg.vc == nr - 1) {
      rslt = scm_fcd_vector_push(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                                  lst);
      if (rslt < 0) return -1;
    }
    else {
      scm_fcd_vector_set_i(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
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
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(adjusted != NULL);

  arity = scm_fcd_arity(proc);
  unwished = scm_fcd_procedure_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED);

  rslt = scm_vm_cmp_arity(argc, arity, unwished);
  if (rslt != 0) {
    switch (rslt) {
    case 1:
      scm_fcd_error("too many arguments", 0);
      break;
    case -1:
      scm_fcd_error("too few arguments", 0);
      break;
    case -2:
      scm_fcd_error("manual adjustment error", 0);
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
      lst = scm_fcd_cons(values[argc - i - 1], lst);
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

  if (scm_fcd_module_p(spec))
    return spec;

  r = scm_fcd_find_module(spec, SCM_CSETTER_L(mod));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(mod)) {
    scm_fcd_error("unknown module", 1, spec);
    return SCM_OBJ_NULL;
  }

  return mod;
}

static int
scm_vm_do_op_int(ScmObj vm, int num)
{
  if (num < 0 || SCM_VM_NR_INTERRUPTIONS <= num) {
    scm_fcd_error("unsupported interruption number", 0);
    return -1;
  }

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

  if (argc < 0) {
    scm_fcd_error("invalid operand", 0);
    return -1;
  }

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
      val = scm_fcd_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
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

  if (!scm_fcd_procedure_p(SCM_VM(vm)->reg.val[0])) {
    scm_fcd_error("inapplicable object", 1, SCM_VM(vm)->reg.val[0]);
    return -1;
  }

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_UCF);

  nr_bind = scm_vm_adjust_arg_to_arity(vm, argc, SCM_VM(vm)->reg.val[0], &argc);
  if (nr_bind < 0) return -1;

  if (scm_fcd_subrutine_p(SCM_VM(vm)->reg.val[0])) {
    SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
    SCM_VM(vm)->reg.ip = NULL;

    if (nr_bind > 0)
      rslt = scm_fcd_call_subrutine(SCM_VM(vm)->reg.val[0],
                                    argc,
                                    scm_vm_ef_values(SCM_VM(vm)->reg.efp));
    else
      rslt = scm_fcd_call_subrutine(SCM_VM(vm)->reg.val[0], 0, NULL);

    if (rslt < 0) return -1;

    if (scm_vm_interrupt_act_any_p(vm))
      rslt = scm_vm_setup_stat_return(vm);
    else
      rslt = scm_vm_do_op_return(vm);
    if (rslt < 0) return -1;
  }
  else if (scm_fcd_closure_p(SCM_VM(vm)->reg.val[0])) {
    ScmEnvFrame *efp;

    efb = scm_fcd_closure_env(SCM_VM(vm)->reg.val[0]);
    efp = scm_efbox_to_efp(efb);
    if (nr_bind > 0)
      SCM_WB_EXP(vm, scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, efp));
    else
      SCM_WB_EXP(vm, SCM_VM(vm)->reg.efp = efp);

    SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->reg.val[0]);
    SCM_VM(vm)->reg.ip = scm_fcd_closure_to_ip(SCM_VM(vm)->reg.val[0]);
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
  scm_assert(scm_fcd_symbol_p(var) || scm_fcd_gloc_p(var));
  scm_assert(scm_fcd_module_specifier_p(mod));

  if (scm_fcd_symbol_p(var)) {
    mod = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(mod)) return -1;

    r = scm_fcd_find_gloc(mod, var, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_fcd_error("unbound variable", 1, var);
      return -1;
    }

    if (ip != NULL) {
      r = scm_fcd_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, ip, gloc);
      if (r < 0) return -1;
    }
  }
  else if (scm_fcd_gloc_p(var)) {
    gloc = var;
  }
  else {
    scm_assert(0);
  }

  val = scm_fcd_gloc_variable_value(gloc);
  if (scm_fcd_landmine_object_p(val)) {
    scm_fcd_error("unbound variable", 1, scm_fcd_gloc_symbol(gloc));
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_do_op_sref(ScmObj vm, int idx, int layer)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (idx < 0 || layer < 0) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (scm_fcd_box_object_p(val)) {
    val = scm_fcd_box_unbox(val);
    if (scm_obj_null_p(val)) return -1;
  }

  if (scm_fcd_landmine_object_p(val)) {
    scm_fcd_error("refarence to uninitialized variable", 0);
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

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

  if (idx < 0 || layer < 0) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return -1;

  /* box 化できるのは VM stack 上にある環境のみに限定する */
  /* XXX: 現在のスタックセグメント上にある環境のみに限定したほうがいいかも
   *      しれない。今の制限でも問題ないはずだが。
   */
  if (scm_vm_ef_boxed_p(efp)) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if (idx >= efp->len) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  box = scm_fcd_box_new(SCM_MEM_HEAP, scm_vm_ef_values(efp)[idx]);
  if (scm_obj_null_p(box)) return -1;

  SCM_WB_EXP(vm, scm_vm_ef_values(efp)[idx] = box);

  return 0;
}

static int
scm_vm_do_op_demine(ScmObj vm, int idx, int layer)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm,
                      &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (idx < 0 || layer < 0) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_fcd_box_object_p(val)) {
    scm_fcd_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  scm_fcd_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

static int
scm_vm_op_int(ScmObj vm)
{
  int rslt, num;

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, num);

  rslt = scm_vm_do_op_int(vm, num);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_cframe(ScmObj vm)
{
  int rslt, dst;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, dst);

  rslt = scm_vm_do_op_cframe(vm, dst);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_eframe(ScmObj vm)
{
  int argc;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, argc);

  return scm_vm_do_op_eframe(vm, argc);
}

static int
scm_vm_op_epop(ScmObj vm)
{
  int rslt;

  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);

  rslt = scm_vm_do_op_epop(vm);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_eshift(ScmObj vm)
{
  int n, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, n);

  rslt = scm_vm_do_op_eshift(vm, 1, n);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_immval(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_OBJ(SCM_VM(vm)->reg.ip, val);

  rslt = scm_vm_do_op_immval(vm, val);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_push(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  return scm_vm_do_op_push(vm);
}

static int
scm_vm_op_mvpush(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  return scm_vm_do_op_mvpush(vm);
}

static int
scm_vm_op_return(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  return scm_vm_do_op_return(vm);
}

static int
scm_vm_op_pcall(ScmObj vm)
{
  int argc, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, argc);

  rslt = scm_vm_do_op_pcall(vm, argc);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_call(ScmObj vm)
{
  int argc, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, argc);

  if (argc != 0) {
    rslt = scm_vm_do_op_eframe(vm, abs(argc));
    if (rslt < 0) return -1;
  }

  rslt = scm_vm_do_op_pcall(vm, argc);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_tail_call(ScmObj vm)
{
  int argc, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, argc);

  if (argc != 0) {
    rslt = scm_vm_do_op_eframe(vm, abs(argc));
    if (rslt < 0) return -1;
  }

  rslt = scm_vm_do_op_eshift(vm, (argc == 0) ? 0 : 1, -1);
  if (rslt < 0) return -1;

  rslt = scm_vm_do_op_pcall(vm, argc);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_gref(ScmObj vm)
{
  ScmObj var = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  scm_byte_t *ip;

  SCM_REFSTK_INIT_REG(&vm,
                      &var, &mod);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = SCM_VM(vm)->reg.ip;
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, var, mod);

  return scm_vm_do_op_gref(vm, ip, var, mod);
}

static int
scm_vm_op_gdef(ScmObj vm)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  scm_byte_t *prv_ip;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &gloc, &arg, &mod, &module);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, arg, mod);

  if (scm_fcd_symbol_p(arg)) {
    module = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(module)) return -1;

    gloc = scm_fcd_get_gloc(module, arg);
    if (scm_obj_null_p(gloc)) return -1;

    r = scm_fcd_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, prv_ip, gloc);
    if (r < 0) return -1;
  }
  else if (scm_fcd_gloc_p(arg)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  scm_fcd_gloc_bind_variable(gloc, SCM_VM(vm)->reg.val[0]);
  return 0;
}

static int
scm_vm_op_gset(ScmObj vm)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  scm_byte_t *prv_ip;
  int r;

  SCM_REFSTK_INIT_REG(&vm, &gloc, &arg, &mod, &module, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VMINST_FETCH_OPD_OBJ_OBJ(SCM_VM(vm)->reg.ip, arg, mod);

  if (scm_fcd_symbol_p(arg)) {
    module = scm_vm_get_module_specified_by_opd(mod);
    if (scm_obj_null_p(module)) return -1;

    r = scm_fcd_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_fcd_error("unbound variable", 1, arg);
      return -1;
    }

    val = scm_fcd_gloc_variable_value(gloc);
    if (r < 0) return -1;

    if (scm_fcd_landmine_object_p(val)) {
      scm_fcd_error("unbound variable", 1, arg);
      return -1;
    }

    r = scm_fcd_update_vminst_opd_obj_obj_1(SCM_VM(vm)->reg.cp, prv_ip, gloc);
    if (r < 0) return -1;
  }
  else if (scm_fcd_gloc_p(arg)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  scm_fcd_gloc_bind_variable(gloc, SCM_VM(vm)->reg.val[0]);
  return 0;
}

static int
scm_vm_op_sref(ScmObj vm)
{
  int idx, layer, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, idx, layer);

  rslt = scm_vm_do_op_sref(vm, idx, layer);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_sset(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int idx, layer;

  SCM_REFSTK_INIT_REG(&vm,
                      &val, &o);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, idx, layer);

  if (idx < 0 || layer < 0) {
    scm_fcd_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_fcd_box_object_p(val)) {
    scm_fcd_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  o = scm_fcd_box_unbox(val);
  if (scm_fcd_landmine_object_p(o)) {
    scm_fcd_error("refarence to uninitialized variable", 0);
    return -1;
  }

  scm_fcd_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

static int
scm_vm_op_jmp(ScmObj vm)
{
  int dst;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, dst);

  SCM_VM(vm)->reg.ip += dst;

  return 0;
}

static int
scm_vm_op_jmpt(ScmObj vm)
{
  int dst;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, dst);

  if (scm_fcd_true_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip +=  dst;

  return 0;
}

static int
scm_vm_op_jmpf(ScmObj vm)
{
  int dst;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_IOF(SCM_VM(vm)->reg.ip, dst);

  if (scm_fcd_false_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip += dst;

  return 0;
}

static int
scm_vm_op_box(ScmObj vm)
{
  int idx, layer, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, idx, layer);

  rslt = scm_vm_do_op_box(vm, idx, layer);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_close(ScmObj vm)
{
  ScmObj clsr = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  int nr_env, arity, rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &clsr, &iseq, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI_OBJ(SCM_VM(vm)->reg.ip, nr_env, arity, iseq);

  if (nr_env < 0) {
    scm_fcd_error("invalid access to VM Stack: out of range", 0);
    return -1;
  }

  rslt = scm_vm_box_eframe(vm, SCM_VM(vm)->reg.efp,
                           (size_t)nr_env, SCM_CSETTER_L(env));
  if (rslt < 0) return -1;

  clsr = scm_fcd_make_closure(iseq, env, arity);
  if (scm_obj_null_p(clsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], clsr);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

static int
scm_vm_op_demine(ScmObj vm)
{
  int idx, layer, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, idx, layer);

  rslt = scm_vm_do_op_demine(vm, idx, layer);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_emine(ScmObj vm)
{
  ScmObj box = SCM_OBJ_INIT;
  int len, rslt;

  SCM_REFSTK_INIT_REG(&vm,
                      &box);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, len);

  for (int i = 0; i < len; i++) {
    rslt = scm_vm_do_op_immval(vm, SCM_LANDMINE_OBJ);
    if (rslt < 0) return -1;

    rslt = scm_vm_do_op_push(vm);
    if (rslt < 0) return -1;
  }

  rslt = scm_vm_do_op_eframe(vm, len);
  if (rslt < 0) return -1;

  for (int i = 0; i < len; i++) {
    rslt = scm_vm_do_op_box(vm, i, 0);
    if (rslt < 0) return -1;
  }

  return 0;
}

static int
scm_vm_op_edemine(ScmObj vm)
{
  int argc, layer, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI_SI(SCM_VM(vm)->reg.ip, argc, layer);

  rslt = scm_vm_do_op_eframe(vm, argc);
  if (rslt < 0) return -1;

  for (int i = 0; i < argc; i++) {
    rslt = scm_vm_do_op_sref(vm, i, 0);
    if (rslt < 0) return -1;

    rslt = scm_vm_do_op_demine(vm, i, layer + 1);
    if (rslt < 0) return -1;
  }

  rslt = scm_vm_do_op_epop(vm);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_mrvc(ScmObj vm)
{
  int arity, rslt;

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VMINST_FETCH_OPD_SI(SCM_VM(vm)->reg.ip, arity);

  rslt = scm_vm_adjust_val_to_arity(vm, arity);
  if (rslt < 0) return -1;

  return 0;
}

static int
scm_vm_op_mrve(ScmObj vm)
{
  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  scm_fcd_error("multiple-return-value error", 0);
  return -1;
}

static int
scm_vm_op_module(ScmObj vm)
{
  ScmObj mod = SCM_OBJ_INIT;

  SCM_VMINST_FETCH_OPD_OBJ(SCM_VM(vm)->reg.ip, mod);

  mod = scm_vm_get_module_specified_by_opd(mod);
  if (scm_obj_null_p(mod)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], mod);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

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

  SCM_REFSTK_INIT_REG(&vm);

  scm_assert_obj_type_accept_null(vm, &SCM_VM_TYPE_INFO);

  if (scm_obj_null_p(vm))
    return tbl;

  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_nop:
  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_halt:
  SCM_VMINST_FETCH_OPD_NOOPD(SCM_VM(vm)->reg.ip);
  return tbl;

 inst_int:
  scm_vm_op_int(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_cframe:
  scm_vm_op_cframe(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_eframe:
  scm_vm_op_eframe(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_epop:
  scm_vm_op_epop(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_eshift:
  scm_vm_op_eshift(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_immval:
  scm_vm_op_immval(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_push:
  scm_vm_op_push(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_mvpush:
  scm_vm_op_mvpush(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_return:
  scm_vm_op_return(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_pcall:
  scm_vm_op_pcall(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_call:
  scm_vm_op_call(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_tail_call:
  scm_vm_op_tail_call(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_gref:
  scm_vm_op_gref(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_gdef:
  scm_vm_op_gdef(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_gset:
  scm_vm_op_gset(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_sref:
  scm_vm_op_sref(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_sset:
  scm_vm_op_sset(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_jmp:
  scm_vm_op_jmp(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_jmpt:
  scm_vm_op_jmpt(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_jmpf:
  scm_vm_op_jmpf(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_box:
  scm_vm_op_box(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_close:
  scm_vm_op_close(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_demine:
  scm_vm_op_demine(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_emine:
  scm_vm_op_emine(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_edemine:
  scm_vm_op_edemine(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_mrvc:
  scm_vm_op_mrvc(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_mrve:
  scm_vm_op_mrve(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);

 inst_module:
  scm_vm_op_module(vm);
  goto *(void *)SCM_VMINST_GET_OP(SCM_VM(vm)->reg.ip);
}

static int
scm_vm_interrupt_func_run_gc(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_fcd_gc_start();
  return 0;
}

static int
scm_vm_interrupt_func_halt(ScmObj vm)
{
  static struct scm_vm_inst_noopd inst = { .op = SCM_OPCODE_HALT };
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* TODO: opcode を変換しているのは非効率なので改善する */
  inst.op = (scm_opcode_t)scm_vm_opcode2ptr(SCM_OPCODE_HALT);
  SCM_VM(vm)->reg.ip = (scm_byte_t *)&inst;

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
scm_vm_bootup(void)
{
  ScmObj stack = SCM_OBJ_INIT;
  ScmBedrock *bedrock;
  int r;

  bedrock = scm_bedrock_new();
  if (bedrock == NULL) return -1;

  stack = scm_fcd_ref_stack_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(stack)) return -1;

  scm_fcd_chg_current_ref_stack(stack);

  r = scm_bedrock_setup(bedrock);
  if (r < 0) return -1;

  return 0;
}

void
scm_vm_shutdown(void)
{
  scm_bedrock_cleanup(scm_fcd_current_br());

  scm_fcd_mem_free_root(scm_fcd_current_ref_stack());
  scm_fcd_chg_current_ref_stack(SCM_OBJ_NULL);

  scm_bedrock_end(scm_fcd_current_br());
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
scm_vm_clone(ScmObj parent)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&parent,
                      &vm);

  scm_assert_obj_type(parent, &SCM_VM_TYPE_INFO);

  vm = scm_fcd_mem_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, SCM_VM(parent)->main);
  if (rslt < 0) {
    scm_fcd_mem_free_root(vm);
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
  scm_assert(scm_fcd_iseq_p(iseq));

  scm_vm_run_init(vm);

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp,
                scm_fcd_make_closure(iseq, SCM_OBJ_NULL, 0));
  SCM_VM(vm)->reg.ip = scm_fcd_iseq_to_ip(iseq);

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
    scm_fcd_error("failed to make vector from VAL registers: too many values",
                  0);
    return SCM_OBJ_NULL;
  }

  val = scm_fcd_make_vector((size_t)SCM_VM(vm)->reg.vc, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  n = (SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
    SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG - 1;
  for (size_t i = 0; i < (size_t)n; i++)
    scm_fcd_vector_set_i(val, i, SCM_VM(vm)->reg.val[i]);

  rest = SCM_VM(vm)->reg.vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    for (int i = 0; i < rest; i++) {
      elm = scm_fcd_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                               (size_t)i);
      scm_fcd_vector_set_i(val, (size_t)(SCM_VM_NR_VAL_REG - 1 + i), elm);
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
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));

  asmb = scm_fcd_make_assembler(SCM_OBJ_NULL);
  if (scm_obj_null_p(asmb)) return SCM_OBJ_NULL;

  r = scm_vm_make_proc_call_code(asmb, proc, args, false);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_assembler_push(asmb, SCM_OPCODE_HALT);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_assembler_commit(asmb);
  if (r < 0) return SCM_OBJ_NULL;

  scm_vm_run(vm, scm_fcd_assembler_iseq(asmb));

  if (scm_vm_raised_p(vm))
    return SCM_OBJ_NULL;
  else
    return scm_vm_val_reg_to_vector(vm);
}

ScmObj
scm_vm_run_cloned(ScmObj vm, ScmObj iseq)
{
  ScmObj cloned = SCM_OBJ_INIT, raised = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &iseq,
                      &cloned, &raised, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_fcd_iseq_p(iseq));

  cloned = scm_vm_clone(vm);
  if (scm_obj_null_p(cloned)) return SCM_OBJ_NULL;

  scm_fcd_chg_current_vm(cloned);
  scm_vm_run(cloned, iseq);
  scm_fcd_chg_current_vm(vm);

  if (scm_vm_raised_p(cloned)) {
    raised = scm_vm_raised_obj(cloned);
    scm_fcd_vm_end(cloned);
    scm_vm_setup_stat_raise(vm, raised, false);
    return SCM_OBJ_NULL;
  }

  val = scm_vm_val_reg_to_vector(cloned);
  scm_fcd_vm_end(cloned);

  return val;
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
      scm_fcd_error("invalid return value is set", 0);
      return -1;
    }
    SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], val[i]);
  }

  rest = vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    vec = scm_fcd_make_vector((size_t)rest, SCM_OBJ_NULL);
    if (scm_obj_null_p(vec)) return -1;

    for (int i = 0; i < rest; i++) {
      if (scm_obj_null_p(val[SCM_VM_NR_VAL_REG - 1 + i])) {
        scm_fcd_error("invalid return value is set", 0);
        return -1;
      }

      scm_fcd_vector_set_i(vec, (size_t)i, val[SCM_VM_NR_VAL_REG - 1 + i]);
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
  scm_assert(scm_fcd_nil_p(alist) || scm_fcd_pair_p(alist));

  x = scm_fcd_cons(alist, SCM_VM(vm)->reg.prm);
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

  if (scm_fcd_nil_p(SCM_VM(vm)->reg.prm))
    return;

  x = scm_fcd_cdr(SCM_VM(vm)->reg.prm);
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

  for (x = SCM_VM(vm)->reg.prm; scm_fcd_pair_p(x); x = scm_fcd_cdr(x)) {
    p = scm_fcd_assq(var, scm_fcd_car(x));
    if (scm_obj_null_p(p)) return SCM_OBJ_NULL;

    if (scm_fcd_pair_p(p))
      return scm_fcd_cdr(p);
  }

  if (!scm_fcd_parameter_p(var)) {
    scm_fcd_error("failed to get bound value: unbound variable", 1, var);
    return SCM_OBJ_NULL;
  }

  v = scm_fcd_parameter_init_val(var);
  if (scm_obj_null_p(v)) {
    scm_fcd_error("failed to get bound value: "
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
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));
  scm_assert(scm_obj_null_p(postproc) || scm_fcd_procedure_p(postproc));

  trmp_code = scm_vm_make_trampolining_code(vm, proc, args,
                                            postproc, handover, tail);
  if (scm_obj_null_p(trmp_code)) return -1;

  env = SCM_OBJ_NULL;
  if (scm_fcd_closure_p(SCM_VM(vm)->reg.cp))
    env = scm_fcd_closure_env(SCM_VM(vm)->reg.cp);

  trmp_clsr = scm_fcd_make_closure(trmp_code, env, 0);
  if (scm_obj_null_p(trmp_clsr)) return -1;


  ip = scm_fcd_iseq_to_ip(trmp_code);
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
    scm_fcd_fatal("Exception has raised in the situation can be addressed");
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
  ScmObj args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm,
                      &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_fcd_cons(SCM_VM(vm)->reg.exc.obj, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  return scm_vm_setup_stat_trmp(vm,
                                scm_bedrock_exc_hndlr_caller(scm_fcd_current_br()),
                                args, SCM_OBJ_NULL, SCM_OBJ_NULL, false);
}

int
scm_vm_setup_stat_call_exc_hndlr_cont(ScmObj vm)
{
  ScmObj args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&vm,
                      &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_fcd_cons(SCM_VM(vm)->reg.exc.obj, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  return scm_vm_setup_stat_trmp(vm,
                                scm_bedrock_exc_hndlr_caller_cont(scm_fcd_current_br()),
                                args, SCM_OBJ_NULL, SCM_OBJ_NULL, true);

  return 0;
}

int
scm_vm_push_exc_handler(ScmObj vm, ScmObj hndlr)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &hndlr, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_fcd_procedure_p(hndlr));

  lst = scm_fcd_cons(hndlr, SCM_VM(vm)->reg.exc.hndlr);
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

  if (!scm_fcd_pair_p(SCM_VM(vm)->reg.exc.hndlr))
    return 0;

  rest = scm_fcd_cdr(SCM_VM(vm)->reg.exc.hndlr);
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc.hndlr, rest);

  return 0;
}

int
scm_vm_exc_handler(ScmObj vm, scm_csetter_t *hndlr)
{
  ScmObj val = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(hndlr != NULL);

  if (scm_fcd_pair_p(SCM_VM(vm)->reg.exc.hndlr))
    val = scm_fcd_car(SCM_VM(vm)->reg.exc.hndlr);
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

  vm = scm_fcd_current_vm();

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

  hndlr_arg = scm_fcd_cons(argv[0], SCM_NIL_OBJ);
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
  ScmObj val = SCM_OBJ_INIT;
  int rslt, ret;

  SCM_REFSTK_INIT_REG(&subr,
                      &vm, &hndlr, &hndlr_arg,
                      &val);

  vm = scm_fcd_current_vm();

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

  hndlr_arg = scm_fcd_cons(argv[0], SCM_NIL_OBJ);
  if (scm_obj_null_p(hndlr_arg)) return -1;

  ret = scm_vm_setup_stat_trmp(vm, hndlr, hndlr_arg,
                               scm_bedrock_exc_hndlr_caller_post(scm_fcd_current_br()),
                               hndlr, true);

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

    vm = scm_fcd_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_push_exc_handler(vm, argv[0]);
  if (rslt < 0) goto end;

  rslt = scm_fcd_return_val(argv + 1, argc - 1);

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

  port = scm_fcd_open_output_string();
  if (scm_obj_null_p(port)) return;

  rslt = scm_fcd_write_cstr("Unhandled Exception: ", SCM_ENC_SRC, port);
  if (rslt < 0) return;

  rslt = scm_fcd_display(SCM_VM(vm)->reg.exc.obj, port);
  if (rslt < 0) return;

  rslt = scm_fcd_newline(port);
  if (rslt < 0) return;

  str = scm_fcd_get_output_string(port);
  if (scm_obj_null_p(str)) return;

  p = scm_fcd_string_to_cstr(str, msg, sizeof(msg));
  if (p == NULL) return;

  scm_bedrock_error(scm_fcd_current_br(), msg);
}

int
scm_vm_push_dw_handler(ScmObj vm, ScmObj before, ScmObj after)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&vm, &before, &after, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_fcd_procedure_p(before));
  scm_assert(scm_fcd_procedure_p(after));
  scm_assert(SCM_VM(vm)->reg.dw.n < SIZE_MAX);

  lst = scm_fcd_cons(before, after);
  if (scm_obj_null_p(lst)) return -1;

  lst = scm_fcd_cons(lst, SCM_VM(vm)->reg.dw.hndlr);
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

  if (!scm_fcd_pair_p(SCM_VM(vm)->reg.dw.hndlr))
    return 0;

  rest = scm_fcd_cdr(SCM_VM(vm)->reg.dw.hndlr);
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

  scm_assert(scm_fcd_pair_p(from) || scm_fcd_nil_p(from));
  scm_assert(scm_fcd_pair_p(to) || scm_fcd_nil_p(to));

  if (scm_fcd_eq_p(from, to))
    return 0;

  ff = from; f = nf;
  tt = to; t = nt;
  if (nf >= nt) {
    ff = scm_fcd_cdr(from);
    f = nf - 1;
  }

  if (nt >= nf) {
    hndlr = scm_fcd_cxr(to, "aa");
    scm_assert(scm_fcd_procedure_p(hndlr));

    ac = scm_fcd_cons(hndlr, scm_csetter_val(acc));
    if (scm_obj_null_p(ac)) return -1;

    scm_csetter_setq(acc, ac);
    tt = scm_fcd_cdr(to);
    t = nt - 1;
  }

  r = scm_vm_collect_dw_handler_internal(ff, f, tt, t, acc);
  if (r < 0) return -1;

  if (nf >= nt) {
    hndlr = scm_fcd_cxr(from, "da");
    scm_assert(scm_fcd_procedure_p(hndlr));

    ac = scm_fcd_cons(hndlr, scm_csetter_val(acc));
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
  return scm_vm_run_loop(SCM_OBJ_NULL)[op];
}

scm_opcode_t
scm_vm_ptr2opcode(const void *ptr)
{
  for (const void **p = scm_vm_run_loop(SCM_OBJ_NULL); *p != NULL; p++) {
    if (*p == ptr)
      return (scm_opcode_t)(p - scm_vm_run_loop(SCM_OBJ_NULL));
  }

  scm_assert(false);
  return 0;
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

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
scm_vm_gc_accept_stack(ScmObj vm, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (scm_obj_null_p(SCM_VM(vm)->stack)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, SCM_VM(vm)->stack, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  if (SCM_VM(vm)->reg.sp != NULL) {
    ScmObj *partial = (ScmObj *)SCM_VM(vm)->reg.sp - SCM_VM(vm)->reg.partial;
    for (int i = 0; i < SCM_VM(vm)->reg.partial; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, partial[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  rslt = scm_vm_cf_gc_accept(vm, SCM_VM(vm)->reg.cfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(vm, SCM_VM(vm)->reg.efp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

int
scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int n, rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->main, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  n = ((SCM_VM(obj)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(obj)->reg.vc : SCM_VM_NR_VAL_REG);
  for (int i = 0; i < n; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.cp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.prm, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.exc.obj, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.exc.hndlr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.dw.hndlr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_stack(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}


/***************************************************************************/
/*  VM (interface)                                                         */
/***************************************************************************/

ScmBedrock *scm_fcd__current_br = NULL;
ScmObj scm_fcd__current_vm = SCM_OBJ_INIT;
ScmObj scm_fcd__current_ref_stack = SCM_OBJ_INIT;

/* Memo:
 *  scm_fcd_nil() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_NIL_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_nil(void)
{
  return scm_bedrock_nil(scm_fcd_current_br());
}

/* Memo:
 *  scm_fcd_true() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_TRUE_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_true(void)
{
  return scm_bedrock_true(scm_fcd_current_br());
}

/* Memo:
 *  scm_fcd_false() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_FALSE_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_false(void)
{
  return scm_bedrock_false(scm_fcd_current_br());
}

/* Memo:
 *  scm_fcd_eof() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_EOF_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_eof(void)
{
  return scm_bedrock_eof(scm_fcd_current_br());
}

/* Memo:
 *  scm_fcd_undef() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_UNDEF_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_undef(void)
{
  return scm_bedrock_undef(scm_fcd_current_br());
}

/* Memo:
 *  scm_fcd_landmine() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_LANDMINE_OBJ を定義して定数的に使うため)
 */
ScmObj
scm_fcd_landmine(void)
{
  return scm_bedrock_landmine(scm_fcd_current_br());
}

void
scm_fcd_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_fcd_current_br(), msg);
}

extern inline void
scm_fcd_fatalf(const char *fmt, ...)
{
}

extern inline bool
scm_fcd_fatal_p(void)
{
  return scm_bedrock_fatal_p(scm_fcd_current_br());
}

int
scm_fcd_halt(void)
{
  return scm_vm_setup_stat_halt(scm_fcd_current_vm());
}

int
scm_fcd_cached_global_var_ref(int kind, scm_csetter_t *val)
{
  ScmObj gloc = SCM_OBJ_INIT, v = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&gloc, &v);

  r = scm_bedrock_cached_gv(scm_fcd_current_br(), kind, SCM_CSETTER_L(gloc));
  if (r < 0) return -1;

  if (scm_obj_not_null_p(gloc)) {
    v = scm_fcd_gloc_variable_value(gloc);
    if (scm_fcd_landmine_object_p(v))
      v = SCM_OBJ_NULL;
  }
  else
    v = SCM_OBJ_NULL;

  if (val != NULL)
    scm_csetter_setq(val, v);

  return 0;
}

ScmObj
scm_fcd_cached_symbol(int kind)
{
  return scm_bedrock_cached_sym(scm_fcd_current_br(), kind);
}

ScmEncoding *
scm_fcd_system_encoding(void)
{
  return scm_bedrock_encoding(scm_fcd_current_br());
}

void *
scm_fcd_current_memory_manager(void)
{
  return scm_bedrock_mem(scm_fcd_current_br());
}

ScmObj
scm_fcd_current_symbol_table(void)
{
  return scm_bedrock_symtbl(scm_fcd_current_br());
}

ScmObj
scm_fcd_current_module_tree(void)
{
  return scm_bedrock_modtree(scm_fcd_current_br());
}

bool
scm_fcd_vm_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_VM_TYPE_INFO);
}

ScmObj
scm_fcd_vm_new(void)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  rslt = scm_vm_bootup();
  if (rslt < 0) return SCM_OBJ_NULL;

  vm = scm_fcd_mem_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, vm);
  if (rslt < 0) {
    scm_fcd_mem_free_root(vm);
    return SCM_OBJ_NULL;
  }

  scm_fcd_chg_current_vm(vm);

  return vm;
}

void
scm_fcd_vm_end(ScmObj vm)
{
  bool main_vm;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  main_vm = scm_obj_same_instance_p(vm, SCM_VM(vm)->main);

  if (main_vm) {
    scm_fcd_gc_start();
    scm_fcd_chg_current_vm(SCM_OBJ_NULL);
  }

  scm_fcd_mem_free_root(vm);

  if (main_vm)
    scm_vm_shutdown();
}

ScmObj
scm_fcd_vm_apply(ScmObj vm, ScmObj proc, ScmObj args)
{
  scm_assert(scm_fcd_vm_p(vm));
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));
  return scm_vm_apply(vm, proc, args);
}

ScmObj
scm_fcd_vm_run_cloned(ScmObj vm, ScmObj iseq)
{
  scm_assert(scm_fcd_vm_p(vm));
  scm_assert(scm_fcd_iseq_p(iseq));
  return scm_vm_run_cloned(vm, iseq);
}

void
scm_fcd_vm_disposal_unhandled_exc(ScmObj vm)
{
  scm_assert(scm_fcd_vm_p(vm));
  return scm_vm_disposal_unhandled_exc(vm);
}

int
scm_fcd_return_val(const ScmObj *val, int vc)
{
  scm_assert(vc >= 0);
  scm_assert(vc == 0 || val != NULL);
  return scm_vm_set_val_reg(scm_fcd_current_vm(), val, vc);
}

ScmObj
scm_fcd_capture_continuation(void)
{
  return scm_vm_capture_cont(scm_fcd_current_vm());
}

int
scm_fcd_reinstantemnet_continuation(ScmObj cc)
{
  return scm_vm_reinstatement_cont(scm_fcd_current_vm(), cc);
}

int
scm_fcd_push_dynamic_bindings(ScmObj alist)
{
  return scm_vm_push_dynamic_bindings(scm_fcd_current_vm(), alist);
}

void
scm_fcd_pop_dynamic_bindings(void)
{
  scm_vm_pop_dynamic_bindings(scm_fcd_current_vm());
}

ScmObj
scm_fcd_parameter_value(ScmObj var)
{
  scm_assert(scm_obj_not_null_p(var));
  return scm_vm_parameter_value(scm_fcd_current_vm(), var);
}

int
scm_fcd_trampolining(ScmObj proc, ScmObj args,
                     ScmObj postproc, ScmObj handover)
{
  scm_assert(scm_fcd_procedure_p(proc));
  scm_assert(scm_fcd_nil_p(args) || scm_fcd_pair_p(args));
  scm_assert(scm_obj_null_p(postproc) || scm_fcd_procedure_p(postproc));

  return scm_vm_setup_stat_trmp(scm_fcd_current_vm(), proc, args,
                                postproc, handover, true);
}

void
scm_fcd_exit(ScmObj obj)
{
  /* TODO: obj の内容に応じた VM の終了ステータスの設定*/

  scm_vm_setup_stat_halt(scm_fcd_current_vm());
}

int
scm_fcd_raise(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_fcd_current_vm(), obj, false);
}

int
scm_fcd_raise_continuable(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_fcd_current_vm(), obj, true);
}

bool
scm_fcd_raised_p(void)
{
  return scm_vm_raised_p(scm_fcd_current_vm());
}

ScmObj
scm_fcd_raised_obj(void)
{
  return scm_vm_raised_obj(scm_fcd_current_vm());
}

void
scm_fcd_discard_raised_obj(void)
{
  scm_vm_discard_raised_obj(scm_fcd_current_vm());
}

int
scm_fcd_push_exception_handler(ScmObj handler)
{
  return scm_vm_push_exc_handler(scm_fcd_current_vm(), handler);
}

int
scm_fcd_pop_exception_handler(void)
{
  return scm_vm_pop_exc_handler(scm_fcd_current_vm());
}

int
scm_fcd_push_dynamic_wind_handler(ScmObj before, ScmObj after)
{
  return scm_vm_push_dw_handler(scm_fcd_current_vm(), before, after);
}

int
scm_fcd_pop_dynamic_wind_handler(void)
{
  return scm_vm_pop_dw_handler(scm_fcd_current_vm());
}

ScmObj
scm_fcd_collect_dynamic_wind_handler(ScmObj contcap)
{
  return scm_vm_collect_dw_handler(scm_fcd_current_vm(), contcap);
}

void
scm_fcd_disposal_unhandled_exec(void)
{
  scm_vm_disposal_unhandled_exc(scm_fcd_current_vm());
}

scm_opcode_t
scm_fcd_internal_opcode(scm_opcode_t op)
{
  return (scm_opcode_t)scm_vm_opcode2ptr(op);
}

scm_opcode_t
scm_fcd_external_opcode(scm_opcode_t op)
{
  return scm_vm_ptr2opcode((const void *)op);
}


int
scm_fcd_load_iseq(ScmObj iseq)
{
  ScmObj o = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&iseq,
                      &o);

  scm_assert(scm_fcd_iseq_p(iseq));

  o = scm_fcd_make_assembler(iseq);
  if (scm_obj_null_p(o)) return -1;

  r = scm_fcd_assembler_push(o, SCM_OPCODE_HALT);
  if (r < 0) return -1;

  r = scm_fcd_assembler_commit(o);
  if (r < 0) return -1;

  o = scm_fcd_vm_run_cloned(scm_fcd_current_vm(), scm_fcd_assembler_iseq(o));
  if (scm_obj_null_p(o)) return -1;

  return 0;
}
