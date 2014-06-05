#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>

#include "vm.h"
#include "vmstack.h"
#include "memory.h"
#include "reference.h"
#include "object.h"
#include "string.h"
#include "symbol.h"
#include "module.h"
#include "procedure.h"
#include "miscobjects.h"
#include "api.h"
#include "impl_utils.h"


ScmBedrock *scm__current_br = NULL;
ScmObj scm__current_vm = SCM_OBJ_INIT;
ScmObj scm__current_ref_stack = SCM_OBJ_INIT;


/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

#define SCM_BEDROCK_REF_STACK_INIT_SIZE 512
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

  br->cnsts.nil = scm_nil_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.nil)) return -1;

  br->cnsts.eof = scm_eof_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.eof)) return -1;

  br->cnsts.b_true = scm_bool_new(SCM_MEM_ROOT, true);
  if (scm_obj_null_p(br->cnsts.b_true)) return -1;

  br->cnsts.b_false = scm_bool_new(SCM_MEM_ROOT, false);
  if (scm_obj_null_p(br->cnsts.b_false)) return -1;

  br->cnsts.undef = scm_undef_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.undef)) return -1;

  br->cnsts.landmine = scm_landmine_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->cnsts.landmine)) return -1;

  br->symtbl = scm_symtbl_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->symtbl)) return -1;

  br->modtree = scm_moduletree_new(SCM_MEM_ROOT);
  if (scm_obj_null_p(br->modtree)) return -1;

  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller));

  br->subr.exc_hndlr_caller =
    scm_capi_make_subrutine(scm_vm_subr_exc_hndlr_caller, -2, 0, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller)) return -1;

  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller_cont));

  br->subr.exc_hndlr_caller_cont =
    scm_capi_make_subrutine(scm_vm_subr_exc_hndlr_caller_cont,
                            1, 0, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller_cont)) return -1;

  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->subr.exc_hndlr_caller_post));

  br->subr.exc_hndlr_caller_post =
    scm_capi_make_subrutine(scm_vm_subr_exc_hndlr_caller_post,
                            -2, SCM_PROC_ADJ_UNWISHED, SCM_OBJ_NULL);
  if (scm_obj_null_p(br->subr.exc_hndlr_caller_post)) return -1;

  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->gv.compile));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->gv.eval));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->gv.current_input_port));
  scm_capi_mem_register_extra_rfrn(SCM_REF_MAKE(br->gv.current_output_port));

  return 0;
}

int
scm_bedrock_cleanup(ScmBedrock *br)
{
  scm_assert(br != NULL);

  br->gv.current_input_port = SCM_OBJ_NULL;
  br->gv.current_output_port = SCM_OBJ_NULL;
  br->gv.eval = SCM_OBJ_NULL;
  br->gv.compile = SCM_OBJ_NULL;

  br->subr.exc_hndlr_caller_post = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller_cont = SCM_OBJ_NULL;
  br->subr.exc_hndlr_caller = SCM_OBJ_NULL;

  if (scm_obj_not_null_p(br->modtree)) {
    scm_mem_free_root(br->mem, br->modtree);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->symtbl)) {
    scm_mem_free_root(br->mem, br->symtbl);
    br->modtree = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.landmine)) {
    scm_mem_free_root(br->mem, br->cnsts.landmine);
    br->cnsts.landmine = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.undef)) {
    scm_mem_free_root(br->mem, br->cnsts.undef);
    br->cnsts.undef = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_false)) {
    scm_mem_free_root(br->mem, br->cnsts.b_false);
    br->cnsts.b_false = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.b_true)) {
    scm_mem_free_root(br->mem, br->cnsts.b_true);
    br->cnsts.b_true = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.eof)) {
    scm_mem_free_root(br->mem, br->cnsts.eof);
    br->cnsts.eof = SCM_OBJ_NULL;
  }

  if (scm_obj_not_null_p(br->cnsts.nil)) {
    scm_mem_free_root(br->mem, br->cnsts.nil);
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

  br->mem = scm_mem_new();
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

  br->gv.compile = SCM_OBJ_NULL;
  br->gv.eval = SCM_OBJ_NULL;
  br->gv.current_input_port = SCM_OBJ_NULL;
  br->gv.current_output_port = SCM_OBJ_NULL;

  br->encoding = SCM_ENC_UTF8;

  return 0;
}

void
scm_bedrock_finalize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  if (br->mem != NULL) {
    scm_mem_end(br->mem);
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

  scm_vm_chg_current_br(br);

  return br;
}

void
scm_bedrock_end(ScmBedrock *br)
{
  scm_assert(br != NULL);

  scm_vm_chg_current_br(NULL);
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

  if (scm_obj_not_null_p(scm_vm_current_vm()))
    scm_vm_setup_stat_halt(scm_vm_current_vm());
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

  SCM_STACK_FRAME_PUSH(&sym, &mod, &name);

  scm_assert(sym_str != NULL);
  scm_assert(name_str != NULL);
  scm_assert(gloc != NULL);

  for (size_t i = 0; i < n; i++) {
    name_sym[i] = scm_capi_make_symbol_from_cstr(name_str[i], SCM_ENC_SRC);
    if (scm_obj_null_p(name_sym[i])) return -1;
    SCM_STACK_PUSH(name_sym + i);
  }

  sym = scm_capi_make_symbol_from_cstr(sym_str, SCM_ENC_SRC);
  if (scm_obj_null_p(sym)) return -1;

  name = scm_capi_list_cv(name_sym, n);
  if (scm_obj_null_p(name)) return -1;

  r = scm_capi_find_module(name, SCM_CSETTER_L(mod));
  if (r < 0) return -1;

  if (scm_obj_null_p(mod)) {
    scm_csetter_setq(gloc, SCM_OBJ_NULL);
    return 0;
  }

  return scm_capi_find_gloc(mod, sym, gloc);
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
  scm_assert(br != NULL);
  scm_assert(gloc != NULL);

  switch (kind) {
  case SCM_CACHED_GV_COMPILE:
    return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv.compile),
                                     "compile",
                                     (const char *[]){"scythe", "internal", "compile"},
                                     3,
                                     gloc);
    break;
  case SCM_CACHED_GV_EVAL:
    return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv.eval),
                                     "eval",
                                     (const char *[]){"scythe", "base"},
                                     2,
                                     gloc);
    break;
  case SCM_CACHED_GV_CURRENT_INPUT_PORT:
    return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv.current_input_port),
                                     "current-input-port",
                                     (const char *[]){"scheme", "base"},
                                     2,
                                     gloc);
    break;
  case SCM_CACHED_GV_CURRENT_OUTPUT_PORT:
    return scm_bedrock_cached_gv_aux(SCM_REF_MAKE(br->gv.current_output_port),
                                     "current-output-port",
                                     (const char *[]){"scheme", "base"},
                                     2,
                                     gloc);

    break;
  default:
    scm_capi_error("failed to refer global variable: invalid argument", 0);
    return -1;
    break;
  }
}

/***************************************************************************/
/*  ScmBox                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_BOX_TYPE_INFO = {
  .name                = "box",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmBox),
  .gc_ini_func         = scm_box_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_box_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_box_initialize(ScmObj box, ScmObj obj)
{
  scm_assert_obj_type(box, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmBox, box, obj, obj);

  return 0;
}

ScmObj
scm_box_new(SCM_MEM_TYPE_T mtype, ScmObj obj)
{
  ScmObj box = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj, &box);

  scm_assert(scm_obj_not_null_p(obj));

  box = scm_capi_mem_alloc(&SCM_BOX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  if (scm_box_initialize(box, obj) < 0)
    return SCM_OBJ_NULL;

  return box;
}

void
scm_box_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_BOX(obj)->obj = SCM_OBJ_NULL;
}

int
scm_box_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_BOX(obj)->obj, mem);
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

  cc = scm_capi_mem_alloc(&SCM_CONTCAP_TYPE_INFO, 0, mtype);
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
  SCM_SLOT_SETQ(ScmContCap, cc, reg.exc, regs->exc);
  SCM_SLOT_SETQ(ScmContCap, cc, reg.hndlr, regs->hndlr);
  SCM_CONTCAP(cc)->reg.flags = regs->flags;
}

void
scm_contcap_replace_val(ScmObj cc, const ScmObj *val, int vc)
{
  int n;

  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  n = (vc <= SCM_VM_NR_VAL_REG) ? vc : SCM_VM_NR_VAL_REG;
  for (int i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmContCap, cc, reg.val[i], val[i]);
  SCM_CONTCAP(cc)->reg.vc = vc;
}

void
scm_contcap_replace_ip(ScmObj cc, scm_byte_t *ip, ScmObj cp)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  SCM_SLOT_SETQ(ScmContCap, cc, reg.cp, cp);
  SCM_CONTCAP(cc)->reg.ip = ip;
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
  SCM_CONTCAP(obj)->reg.exc = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.hndlr = SCM_OBJ_NULL;
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
                                 SCM_CONTCAP(obj)->reg.exc, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_CONTCAP(obj)->reg.hndlr, mem);
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

/* scm_local_func int */
/* scm_vm_stack_push(ScmObj vm, ScmObj elm) */
/* { */
/*   scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO); */

/*   if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, */
/*                           SCM_VM(vm)->reg.sp + sizeof(ScmObj))) { */
/*     scm_capi_fatal("VM stack overflow"); */
/*     return -1; /\* stack overflow; TODO: handle stack overflow error  *\/ */
/*   } */

/*   SCM_WB_SETQ(vm, *(ScmObj *)SCM_VM(vm)->reg.sp, elm); */
/*   SCM_VM(vm)->reg.sp += sizeof(ScmObj); */

/*   return 0; */
/* } */

/* scm_local_func ScmObj */
/* scm_vm_stack_pop(ScmObj vm) */
/* { */
/*   scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO); */

/*   if (SCM_VM(vm)->reg.sp - sizeof(ScmObj) < SCM_VM(vm)->stack) { */
/*     scm_capi_fatal("VM stack underflow"); */
/*     /\* stack underflow; TODO; handle stack underflow error *\/ */
/*     return SCM_OBJ_NULL; */
/*   } */

/*   SCM_VM(vm)->reg.sp -= sizeof(ScmObj); */

/*   return *(ScmObj *)SCM_VM(vm)->reg.sp; */
/* } */

scm_local_inline void
scm_vm_ctrl_flg_set(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags |= flg;
}

scm_local_inline void
scm_vm_ctrl_flg_clr(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.flags &= ~flg;
}

scm_local_inline bool
scm_vm_ctrl_flg_set_p(ScmObj vm, SCM_VM_CTRL_FLG_T flg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return (SCM_VM(vm)->reg.flags & flg) ? true : false;
}

scm_local_inline int
scm_vm_update_pef_len_if_needed(ScmObj vm)
{
  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    ptrdiff_t n = SCM_VM(vm)->reg.sp - (scm_byte_t *)SCM_VM(vm)->reg.pefp;
    n -= (ptrdiff_t)sizeof(ScmEnvFrame);
    n /= (ptrdiff_t)sizeof(ScmObj);

    SCM_VM(vm)->reg.pefp->len = (size_t)n;
  }

  return 0;
}

scm_local_func int scm_vm_handle_stack_overflow(ScmObj vm);

scm_local_func int
scm_vm_copy_pef_to_top_of_stack_if_needed(ScmObj vm)
{
  size_t size;
  scm_byte_t *next_sp;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF))
    return 0;

  if (scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)SCM_VM(vm)->reg.pefp))
    return 0;

  size = sizeof(ScmEnvFrame) + sizeof(ScmObj) * SCM_VM(vm)->reg.pefp->len;
  next_sp = SCM_VM(vm)->reg.sp + size;

  if (scm_vmsr_reach_to_ceiling_p(SCM_VM(vm)->stack, next_sp))
    return scm_vm_handle_stack_overflow(vm);

  memcpy(SCM_VM(vm)->reg.sp, SCM_VM(vm)->reg.pefp, size);
  SCM_VM(vm)->reg.pefp = (ScmEnvFrame *)SCM_VM(vm)->reg.sp;
  SCM_VM(vm)->reg.sp = next_sp;

  return 0;
}

scm_local_func ScmObj
scm_vm_capture_stack(ScmObj vm)
{
  ScmObj vmsr = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &vmsr, &next);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp == scm_vmsr_base(SCM_VM(vm)->stack)) {
    if (SCM_VM(vm)->reg.cfp == NULL
        && !scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
      SCM_VM(vm)->reg.cfp = scm_vmsr_next_cf(SCM_VM(vm)->stack);
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);
    }
    return scm_vmsr_next(SCM_VM(vm)->stack);
  }

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack, SCM_VM(vm)->reg.cfp);
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
  }

  rslt = scm_vm_update_pef_len_if_needed(vm);
  if (rslt < 0) return SCM_OBJ_NULL;

  scm_vmsr_rec(SCM_VM(vm)->stack,
               SCM_VM(vm)->reg.sp, SCM_VM(vm)->reg.cfp,
               SCM_VM(vm)->reg.efp, SCM_VM(vm)->reg.pefp,
               scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PCF),
               scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF));
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

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_vmsr_next(SCM_VM(vm)->stack);;
}

scm_local_func int
scm_vm_restore_stack(ScmObj vm, ScmObj stack)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(stack, &SCM_VMSTCKRC_TYPE_INFO);

  scm_vmsr_relink(SCM_VM(vm)->stack, stack, scm_vmsr_cfp(stack));

  SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  SCM_VM(vm)->reg.cfp = scm_vmsr_cfp(stack);
  SCM_VM(vm)->reg.efp = scm_vmsr_efp(stack);
  SCM_VM(vm)->reg.pefp = scm_vmsr_pefp(stack);

  if (scm_vmsr_pcf_p(stack))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PCF);

  if (scm_vmsr_pef_p(stack))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PEF);

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_handle_stack_overflow(ScmObj vm)
{
  ScmObj vmss = SCM_OBJ_INIT, vmsr = SCM_OBJ_INIT, next = SCM_OBJ_INIT;
  ScmCntFrame *next_cf;
  size_t size;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &vmss, &vmsr, &next);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack, SCM_VM(vm)->reg.cfp);
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
  }

  rslt = scm_vm_update_pef_len_if_needed(vm);
  if (rslt < 0) return -1;

  if (SCM_VM(vm)->reg.sp != scm_vmsr_base(SCM_VM(vm)->stack)) {
    scm_vmsr_rec(SCM_VM(vm)->stack,
                 SCM_VM(vm)->reg.sp, SCM_VM(vm)->reg.cfp,
                 SCM_VM(vm)->reg.efp, SCM_VM(vm)->reg.pefp,
                 scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PCF),
                 scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF));
    next = SCM_VM(vm)->stack;
    next_cf = SCM_VM(vm)->reg.cfp;
  }
  else {
    next = scm_vmsr_next(SCM_VM(vm)->stack);
    next_cf = scm_vmsr_next_cf(SCM_VM(vm)->stack);
  }

  /* partial environment frame のコピーでスタックオーバーフローが繰り返し発
   * 生するのを防ぐため、作りかけの環境フレームがスタックのトップにある場合
   * は、そのサイズに応じて、新しく作成するスタックセグメントのサイズを変え
   * る。スタックセグメントのサイズが「環境フレームのサイズ * 2」なのはテキ
   * トーに決めた計算。
   */
  size = 0;
  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    size = sizeof(ScmEnvFrame) + sizeof(ScmObj) * SCM_VM(vm)->reg.pefp->len;
    scm_assert(size == SIZE_MAX);
    size = (size <= SIZE_MAX / 2) ? size * 2 : SIZE_MAX;
  }

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

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_handle_stack_underflow(ScmObj vm)
{
  ScmObj next = SCM_OBJ_INIT;
  ScmCntFrame *next_cf;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm,
                       &next);

  if (SCM_VM(vm)->reg.cfp != NULL)
    return 0;

  next_cf = SCM_VM(vm)->reg.cfp;
  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF))
    next = scm_vmsr_next(SCM_VM(vm)->stack);
  else
    next = SCM_VM(vm)->stack;

  while (next_cf == NULL) {
    next_cf = scm_vmsr_next_cf(next);
    next = scm_vmsr_next(next);
    if (scm_obj_null_p(next)) {
      scm_capi_error("stack underflow has occurred", 0);
      return -1;
    }
  }

  scm_vmsr_relink(SCM_VM(vm)->stack, next, next_cf);

  /* XXX: この地点以降、efp レジスタが、どこからも参照されていないスタック
   *      セグメント上のフレームを指している可能性があり、その場合、efp レ
   *      ジスタが指している領域が開放される。そのため、この関数の呼び出し
   *      た後は、継続フレームのポップ処理をすぐに行う必要がある。
   */

  SCM_VM(vm)->reg.cfp = next_cf;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_CCF);

  return 0;
}

scm_local_func int
scm_vm_make_cframe(ScmObj vm, ScmEnvFrame *efp, ScmEnvFrame *pefp, ScmObj cp)
{
  ScmCntFrame *cfp;
  scm_byte_t *next_sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm);

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF)) {
    scm_vmsr_relink_cf(SCM_VM(vm)->stack, SCM_VM(vm)->reg.cfp);
    SCM_VM(vm)->reg.cfp = NULL;
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);
  }

  next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmCntFrame);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, next_sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;

    next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmCntFrame);
  }

  rslt = scm_vm_update_pef_len_if_needed(vm);
  if (rslt < 0) return -1;

  cfp = (ScmCntFrame *)SCM_VM(vm)->reg.sp;

  /* instraction pointer は FRAME インストラクション段階ではダミー */
  /* 値(NULL)をプッシュする。本当の値は CALL 時に設定する */

  SCM_WB_EXP(vm,
             scm_vm_cf_init(cfp,
                            SCM_VM(vm)->reg.cfp,
                            efp,
                            pefp,
                            cp,
                            NULL,
                            scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF),
                            scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PCF)));

  SCM_VM(vm)->reg.cfp = cfp;
  SCM_VM(vm)->reg.sp = next_sp;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PCF);
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_PEF);
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_CCF);

  return 0;
}

scm_local_func int
scm_vm_commit_cframe(ScmObj vm, scm_byte_t *ip)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->reg.cfp->ip = ip;

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_PCF);

  return 0;
}

scm_local_func int
scm_vm_pop_cframe(ScmObj vm)
{
  ScmCntFrame *cfp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm);

  if (SCM_VM(vm)->reg.cfp == NULL) {
    rslt = scm_vm_handle_stack_underflow(vm);
    if (rslt < 0) return -1;
  }

  cfp = SCM_VM(vm)->reg.cfp;

  SCM_VM(vm)->reg.cfp = scm_vm_cf_next(cfp);
  SCM_VM(vm)->reg.efp = cfp->efp;
  SCM_VM(vm)->reg.pefp = cfp->pefp;
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, cfp->cp);
  SCM_VM(vm)->reg.ip = cfp->ip;

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_CCF))
    SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  else
    SCM_VM(vm)->reg.sp = (scm_byte_t *)cfp;

  if (scm_vm_cf_maked_on_pef_p(cfp))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PEF);

  if (scm_vm_cf_maked_on_pcf_p(cfp))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PCF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_make_eframe(ScmObj vm, size_t nr_arg)
{
  ScmEnvFrame *pefp;
  scm_byte_t *next_sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm);

  next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmEnvFrame);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, next_sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;

    next_sp = SCM_VM(vm)->reg.sp + sizeof(ScmEnvFrame);
  }

  rslt = scm_vm_update_pef_len_if_needed(vm);
  if (rslt < 0) return -1;

  pefp = (ScmEnvFrame *)SCM_VM(vm)->reg.sp;

  scm_vm_ef_init(pefp,
                 SCM_VM(vm)->reg.pefp,
                 nr_arg,
                 scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF));

  SCM_VM(vm)->reg.pefp = pefp;
  SCM_VM(vm)->reg.sp = next_sp;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PEF);

  return 0;
}

scm_local_func int
scm_vm_commit_eframe(ScmObj vm, ScmEnvFrame *efp, size_t nr_arg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return -1;
  }

  SCM_VM(vm)->reg.efp = SCM_VM(vm)->reg.pefp;
  SCM_VM(vm)->reg.pefp = scm_vm_ef_outer(SCM_VM(vm)->reg.pefp);

  scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, efp);
  SCM_VM(vm)->reg.efp->len = nr_arg;

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_PEF);

  return 0;
}

scm_local_func int
scm_vm_cancel_eframe(ScmObj vm)
{
  ScmEnvFrame *pefp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return -1;
  }

  pefp = SCM_VM(vm)->reg.pefp;
  SCM_VM(vm)->reg.pefp = scm_vm_ef_outer(pefp);
  SCM_VM(vm)->reg.sp = (scm_byte_t *)pefp;

  if (!scm_vm_ef_maked_on_pef_p(pefp))
    scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_PEF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_pop_eframe(ScmObj vm)
{
  ScmEnvFrame *efp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  efp = SCM_VM(vm)->reg.efp;

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial stack frame link will be broken", 0);
    return -1;
  }

  SCM_VM(vm)->reg.efp = scm_vm_ef_outer(efp);

  if (scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)efp))
    SCM_VM(vm)->reg.sp = (scm_byte_t *)efp;
  else
    SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);

  if (scm_vm_ef_maked_on_pef_p(efp))
    scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_PEF);

  rslt = scm_vm_copy_pef_to_top_of_stack_if_needed(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_box_eframe(ScmObj vm, ScmEnvFrame *efp, size_t depth, scm_csetter_t *box)
{
  ScmObj efb = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&efb, &prev, &efb, &prev);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (depth == 0) {
    scm_csetter_setq(box, SCM_OBJ_NULL);
    return 0;
  };

  if (efp == NULL) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if (scm_vm_ef_boxed_p(efp))
    efb = scm_efbox_efp_to_efbox(efp);
  else
    efb = scm_efbox_new(SCM_MEM_HEAP, efp);

  if (scm_obj_null_p(efb)) return -1;

  scm_csetter_setq(box, efb);

  prev = efb;
  efp = scm_vm_ef_outer(efp);
  for (size_t i = 1; i < depth; i++) {
    if (efp == NULL) {
      scm_capi_error("invalid access to envrionment frame: out of range", 0);
      return -1;
    }

    if (scm_vm_ef_boxed_p(efp))
      return 0;

    efb = scm_efbox_new(SCM_MEM_HEAP, efp);
    if (scm_obj_null_p(efb)) return -1;

    scm_efbox_update_outer(prev, efb);

    prev = efb;
    efp = scm_vm_ef_outer(efp);
  }

  scm_efbox_update_outer(prev, SCM_OBJ_NULL);

  return 0;
}

scm_local_func ScmEnvFrame *
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

scm_local_func ScmObj
scm_vm_eframe_arg_ref(ScmEnvFrame *efp_list, size_t idx, size_t layer,
                      ScmEnvFrame **efp)
{
  ScmEnvFrame *e;

  e = scm_vm_eframe_list_ref(efp_list, layer);

  if (e == NULL) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (idx >= e->len) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return SCM_OBJ_NULL;
  }

  if (efp != NULL) *efp = e;

  return e->arg[idx];
}

scm_local_func int
scm_vm_push_dynamic_bindings(ScmObj vm, ScmObj *param, size_t n)
{
  ScmObj rib = SCM_OBJ_INIT, x = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm,
                       &rib, &x);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(param != NULL);
  scm_assert(n > 0);

  if (n > SSIZE_MAX / 2) {
    scm_capi_error("failed to extend dynamic bindings: too many parameters", 0);
    return -1;
  }

  rib = scm_capi_make_vector(n * 2, SCM_OBJ_NULL);
  if (scm_obj_null_p(rib)) return -1;

  for (size_t i = 0; i < n * 2; i += 2) {
    int rslt = scm_capi_vector_set_i(rib, i, param[i]);
    if (rslt < 0) return -1;

    rslt = scm_capi_vector_set_i(rib, i + 1, param[i + 1]);
    if (rslt < 0) return -1;
  }

  x = scm_api_cons(rib, SCM_VM(vm)->reg.prm);
  if (scm_obj_null_p(x)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, x);

  return 0;
}

scm_local_func int
scm_vm_pop_dynamic_bindings(ScmObj vm)
{
  ScmObj x = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_capi_nil_p(SCM_VM(vm)->reg.prm))
    return 0;

  x = scm_api_cdr(SCM_VM(vm)->reg.prm);
  if (scm_obj_null_p(x)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, x);

  return 0;
}

scm_local_func int
scm_vm_make_proc_call_code(ScmObj iseq, ScmObj proc, ScmObj args, bool tail)
{
  ScmObj cur = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t r, len;
  int i, rslt, arity, nr_decons;
  bool unwished;

  SCM_STACK_FRAME_PUSH(&iseq, &proc, &args,
                       &cur, &arg);

  scm_assert(scm_capi_iseq_p(iseq));
  scm_assert(scm_capi_procedure_p(proc));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));

  rslt = scm_capi_arity(proc, &arity);
  if (rslt < 0) return -1;

  rslt = scm_capi_procedure_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED, &unwished);
  if (rslt < 0) return -1;

  len = scm_capi_length(args);
  if (len < 0) return -1;

  if (arity >= 0) {
    if (len != arity) {
      scm_capi_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = arity;
  }
  else {
    if (len < -arity - 1) {
      scm_capi_error("", 0);    /* TODO: error message */
      return -1;
    }
    nr_decons = unwished ? (int)len : -arity - 1;
  }

  if (!tail) {
    r = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_CFRAME);
    if (r < 0) return SCM_OBJ_NULL;
  }

  if (nr_decons > 0 || arity < 0) {
    r = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_EFRAME);
    if (r < 0) return -1;

    for (cur = args, i = 0;
         scm_capi_pair_p(cur) && i < nr_decons;
         cur = scm_api_cdr(cur), i++) {
      arg = scm_api_car(cur);
      if (scm_obj_null_p(arg)) return -1;

      r = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, arg);
      if (r < 0) return -1;

      r = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }

    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

    if (arity < 0 && !unwished) {
      cur = scm_api_list_copy(cur);
      if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

      r = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, cur);
      if (r < 0) return -1;

      r = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
      if (r < 0) return -1;
    }
  }

  r = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, proc);
  if (r < 0) return -1;

  if (tail) {
    r = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_TAIL_CALL,
                                    unwished ? (int)len : arity);
    if (r < 0) return -1;
  }
  else {
    r = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_CALL, arity);
    if (r < 0) return -1;

    r = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_ARITY, -1);
    if (r < 0) return -1;
  }

  return 0;
}

scm_local_func ScmObj
scm_vm_make_trampolining_code(ScmObj vm, ScmObj proc,
                              ScmObj args, ScmObj postproc, ScmObj handover)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &proc, &args, &postproc,
                       &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_null_p(postproc) || scm_capi_procedure_p(postproc));
  scm_assert(scm_obj_null_p(postproc) || scm_obj_not_null_p(handover));

  /* 以下の処理を実行する iseq オブエクトを生成する
   * l args を引数として target クロージャを呼出す
   *   (postproc が NULL の場合、target クロージャ の呼出は tail call とする)
   * 2 postproc が非 NULL の場合、handover と target クロージャの戻り値を引数として
   *   postproc を tail call する
   */

  iseq = scm_api_make_iseq();
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL;

  rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_ARITY, -1);
  if (rslt < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_EFRAME);
    if (rslt < 0) return SCM_OBJ_NULL;

    rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, handover);
    if (rslt < 0) return SCM_OBJ_NULL;

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  rslt = scm_vm_make_proc_call_code(iseq, proc, args, scm_obj_null_p(postproc));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (!scm_obj_null_p(postproc)) {
    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_MVPUSH);
    if (rslt < 0) return SCM_OBJ_NULL;

    rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, postproc);
    if (rslt < 0) return SCM_OBJ_NULL;

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_TAIL_APPLY);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  return iseq;
}

scm_local_func int
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

scm_local_func int
scm_vm_adjust_arg_to_arity(ScmObj vm, int argc, ScmObj proc, int *adjusted)
{
  ScmObj lst = SCM_OBJ_INIT;
  int rslt, len, arity, nr_bind;
  bool unwished;

  SCM_STACK_FRAME_PUSH(&vm,
                       &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(-INT_MAX <= argc && argc <= INT_MAX);
  scm_assert(scm_capi_procedure_p(proc));
  scm_assert(adjusted != NULL);

  rslt = scm_capi_arity(proc, &arity);
  if (rslt < 0) return -1;

  rslt = scm_capi_procedure_flg_set_p(proc, SCM_PROC_ADJ_UNWISHED, &unwished);
  if (rslt < 0) return -1;

  rslt = scm_vm_cmp_arity(argc, arity, unwished);
  if (rslt != 0) {
    switch (rslt) {
    case 1:
      scm_capi_error("too many arguments", 0);
      break;
    case -1:
      scm_capi_error("too few arguments", 0);
      break;
    case -2:
      scm_capi_error("manual adjustment error", 0);
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

  if (argc == 0) {
    rslt = scm_vm_make_eframe(vm, 0);
    if (rslt < 0) return -1;
  }

  lst = SCM_NIL_OBJ;
  len = argc - (nr_bind - 1);
  for (int i = 0; i < len; i++) {
    lst = scm_api_cons(SCM_VM(vm)->reg.pefp->arg[argc - i - 1], lst);
    if (scm_obj_null_p(lst)) return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.pefp->arg[nr_bind - 1], lst);

  SCM_VM(vm)->reg.sp = ((scm_byte_t *)SCM_VM(vm)->reg.pefp
                        + sizeof(ScmEnvFrame)
                        + sizeof(ScmObj) * (size_t)nr_bind);

  return nr_bind;
}

scm_local_func int
scm_vm_do_op_return(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_pop_cframe(vm);
  if (rslt < 0) return -1;

  if (SCM_VM(vm)->reg.vc == 1)
    SCM_VM(vm)->reg.ip += SCM_INST_SZ_ARITY;

  return 0;
}

scm_local_func int
scm_vm_do_op_call(ScmObj vm, SCM_OPCODE_T op, int argc, bool tail_p)
{
  ScmObj efb = SCM_OBJ_INIT, contcap = SCM_OBJ_INIT;
  int rslt, nr_bind;

  SCM_STACK_FRAME_PUSH(&vm,
                       &efb, &contcap);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(-INT_MAX <= argc && argc <= INT_MAX);

  if (!scm_capi_procedure_p(SCM_VM(vm)->reg.val[0])) {
    scm_capi_error("object is not applicable", 1, SCM_VM(vm)->reg.val[0]);
    return -1;
  }

  nr_bind = scm_vm_adjust_arg_to_arity(vm, argc, SCM_VM(vm)->reg.val[0], &argc);
  if (nr_bind < 0) return -1;

  if (nr_bind > 0) {
    rslt = scm_vm_commit_eframe(vm, NULL, (size_t)nr_bind);
    if (rslt < 0) return -1;
  }

  if (tail_p) {
    scm_byte_t *ef_dst;

    if (scm_vmsr_include_p(SCM_VM(vm)->stack,
                           (scm_byte_t *)SCM_VM(vm)->reg.cfp))
      ef_dst = (scm_byte_t *)SCM_VM(vm)->reg.cfp + sizeof(ScmCntFrame);
    else
      ef_dst = scm_vmsr_base(SCM_VM(vm)->stack);

    if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
      scm_capi_error("invalid operation of VM stack: "
                     "partial environment frame link will be broken", 0);
      return -1;
    }

    if (nr_bind > 0) {
      if (SCM_VM(vm)->reg.efp > (ScmEnvFrame *)ef_dst) {
        size_t ef_sz = sizeof(ScmEnvFrame) + sizeof(ScmObj) * (size_t)nr_bind;
        memmove(ef_dst, SCM_VM(vm)->reg.efp, ef_sz);
        SCM_VM(vm)->reg.efp = (ScmEnvFrame *)ef_dst;
        SCM_VM(vm)->reg.sp = ef_dst + ef_sz;
      }
    }
    else {
      SCM_VM(vm)->reg.sp = ef_dst;
    }
  }

  if (!tail_p) {
    rslt = scm_vm_commit_cframe(vm, SCM_VM(vm)->reg.ip);
    if (rslt < 0) return -1;
  }

  if (scm_capi_subrutine_p(SCM_VM(vm)->reg.val[0])) {
    SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
    SCM_VM(vm)->reg.ip = NULL;

    if (nr_bind > 0)
      rslt = scm_api_call_subrutine(SCM_VM(vm)->reg.val[0],
                                    argc, SCM_VM(vm)->reg.efp->arg);
    else
      rslt = scm_api_call_subrutine(SCM_VM(vm)->reg.val[0], 0, NULL);

    if (rslt < 0) return -1;

    rslt = scm_vm_do_op_return(vm, SCM_OPCODE_RETURN);
    if (rslt < 0) return -1;
  }
  else if (scm_capi_closure_p(SCM_VM(vm)->reg.val[0])) {
    ScmEnvFrame *efp;

    rslt = scm_capi_closure_env(SCM_VM(vm)->reg.val[0], SCM_CSETTER_L(efb));
    if (rslt < 0) return -1;

    efp = scm_efbox_to_efp(efb);
    if (nr_bind > 0)
      SCM_WB_EXP(vm, scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, efp));
    else
      SCM_WB_EXP(vm, SCM_VM(vm)->reg.efp = efp);

    SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->reg.val[0]);
    SCM_VM(vm)->reg.ip = scm_capi_closure_to_ip(SCM_VM(vm)->reg.val[0]);
  }
  else if (scm_capi_continuation_p(SCM_VM(vm)->reg.val[0])) {
    scm_assert(argc >= 0);      /* continuation への引数の可変部分はリスト化 */
                                /* されないことが前提                        */

    contcap = scm_capi_cont_capture_obj(SCM_VM(vm)->reg.val[0]);
    if (scm_obj_null_p(contcap)) return -1;

    if (nr_bind > 0)
      rslt = scm_vm_reinstatement_cont(vm, contcap,
                                       SCM_VM(vm)->reg.efp->arg, argc);
    else
      rslt = scm_vm_reinstatement_cont(vm, contcap, NULL, 0);

    if (rslt < 0) return -1;

    rslt = scm_vm_do_op_return(vm, SCM_OPCODE_RETURN);
    if (rslt < 0) return -1;
  }
  else {
    scm_assert(false);          /* must not happen */
  }

  return 0;
}

scm_local_func int
scm_vm_do_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_byte_t *sp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(SCM_VM(vm)->reg.vc > 0);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    /* 現状、VM stack の GC の制約上、pertial environmnet frame がスタック
       のトップにないとプッシュできない */
    scm_capi_error("invlid operation of VM stack: "
                   "push instruction can be executed only while "
                   "partial environment frame is in top of stack", 0);
    return -1;
  }

  sp = SCM_VM(vm)->reg.sp + sizeof(ScmObj);
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;
  }

  SCM_WB_EXP(SCM_VM(vm)->stack,
             *(ScmObj *)SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.val[0]);

  SCM_VM(vm)->reg.sp = sp;

  return 0;
}

scm_local_func int
scm_vm_do_op_mvpush(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  scm_byte_t *sp;
  int n, rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    /* 現状、VM stack の GC の制約上、pertial environmnet frame がスタック
       のトップにないとプッシュできない */
    scm_capi_error("invlid operation of VM stack: "
                   "push instruction can be executed only while "
                   "partial environment frame is in top of stack", 0);
    return -1;
  }

  sp = SCM_VM(vm)->reg.sp + sizeof(ScmObj) * (size_t)SCM_VM(vm)->reg.vc;
  if (scm_vmsr_overflow_p(SCM_VM(vm)->stack, sp)) {
    rslt = scm_vm_handle_stack_overflow(vm);
    if (rslt < 0) return -1;
  }

  n = ((SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG - 1);
  sp = SCM_VM(vm)->reg.sp;

  for (int i = 0; i < n; i++) {
    SCM_WB_EXP(SCM_VM(vm)->stack, *(ScmObj *)sp = SCM_VM(vm)->reg.val[i]);
    sp += sizeof(ScmObj);
  }

  if (SCM_VM(vm)->reg.vc > SCM_VM_NR_VAL_REG) {
    for (int i = 0; i < SCM_VM(vm)->reg.vc - (SCM_VM_NR_VAL_REG - 1); i++) {
      val = scm_capi_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                                (size_t)i);
      if (scm_obj_null_p(val)) return -1;
      SCM_WB_EXP(SCM_VM(vm)->stack, *(ScmObj *)sp = val);
      sp += sizeof(ScmObj);
    }
  }

  SCM_VM(vm)->reg.sp = sp;

  return 0;
}

scm_local_func int
scm_vm_do_op_frame(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            SCM_VM(vm)->reg.pefp,
                            SCM_VM(vm)->reg.cp);
  if (rslt < 0) return -1;

  rslt = scm_vm_make_eframe(vm, 0);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_do_op_eframe(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_make_eframe(vm, 0);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_do_op_ecommit(ScmObj vm, SCM_OPCODE_T op, size_t argc)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_commit_eframe(vm, SCM_VM(vm)->reg.efp, argc);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_undef(ScmObj vm, SCM_OPCODE_T op)
{
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_UNDEF_OBJ);
  SCM_VM(vm)->reg.vc = 1;
  return 0;
}

scm_local_func int
scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op)
{
  int argc;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;
  return scm_vm_do_op_call(vm, op, argc, (op == SCM_OPCODE_TAIL_CALL));
}

scm_local_func int
scm_vm_op_apply(ScmObj vm, SCM_OPCODE_T op)
{
  ptrdiff_t argc;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return -1;
  }

  argc = SCM_VM(vm)->reg.sp - (scm_byte_t *)SCM_VM(vm)->reg.pefp;
  argc -= (ptrdiff_t)sizeof(ScmEnvFrame);
  argc /= (ptrdiff_t)sizeof(ScmObj);

  scm_assert(argc >= 0);

  if (argc == 0) {
    int rslt = scm_vm_cancel_eframe(vm);
    if (rslt < 0) return -1;
  }

  return scm_vm_do_op_call(vm, op, (int)argc, (op == SCM_OPCODE_TAIL_APPLY));
}

scm_local_func int
scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_CSETTER_L(val));
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

scm_local_func int
scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_do_op_push(vm, op);
}

scm_local_func int
scm_vm_op_mvpush(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_do_op_mvpush(vm, op);
}

scm_local_func int
scm_vm_op_frame(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_do_op_frame(vm, op);
}

scm_local_func int
scm_vm_op_cframe(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            SCM_VM(vm)->reg.pefp,
                            SCM_VM(vm)->reg.cp);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_eframe(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_do_op_eframe(vm, op);
}

scm_local_func int
scm_vm_op_ecommit(ScmObj vm, SCM_OPCODE_T op)
{
  int argc;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return -1;

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return -1;
  }

  SCM_VM(vm)->reg.ip = ip;

  return scm_vm_do_op_ecommit(vm, op, (size_t)argc);
}

scm_local_func int
scm_vm_op_epop(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_pop_eframe(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_erebind(ScmObj vm, SCM_OPCODE_T op)
{
  ScmEnvFrame *efp;
  int argc, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return -1;

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return -1;
  }

  SCM_VM(vm)->reg.ip = ip;

  if (SCM_VM(vm)->reg.efp == NULL) {
    scm_capi_error("invalid operation of enviromnet frame: "
                   "enviroment frame to be rebound is not exist", 0);
    return -1;
  }

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return -1;
  }

  if (scm_vmsr_include_p(SCM_VM(vm)->stack,
                         (scm_byte_t *)SCM_VM(vm)->reg.efp)) {
    if (scm_vm_ef_maked_on_pef_p(SCM_VM(vm)->reg.pefp)) {
      scm_capi_error("invalid operation of VM stack: "
                     "partial stack frame link will be broken", 0);
      return -1;
    }
  }

  rslt = scm_vm_do_op_ecommit(vm, op, (size_t)argc);
  if (rslt < 0) return -1;

  efp = scm_vm_ef_outer(SCM_VM(vm)->reg.efp);
  scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, scm_vm_ef_outer(efp));

  if (scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)efp)) {
    memmove(efp, SCM_VM(vm)->reg.efp,
            sizeof(ScmEnvFrame) + sizeof(ScmObj) * (size_t)argc);
    SCM_VM(vm)->reg.efp = efp;
    SCM_VM(vm)->reg.sp = (scm_byte_t *)efp->arg + sizeof(ScmObj) * (size_t)argc;
  }

  return 0;
}

scm_local_func int
scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_do_op_return(vm, op);
}

scm_local_func int
scm_vm_op_gref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT, module = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  ssize_t rslt;
  int r;
  scm_byte_t *ip, *prv_ip;

  SCM_STACK_FRAME_PUSH(&vm, &gloc, &arg, &mod, &val, &module, &sym);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj_obj(SCM_VM(vm)->reg.ip,
                                          SCM_CSETTER_L(arg),
                                          SCM_CSETTER_L(mod));
  if (ip == NULL) return -1;

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return -1;

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1, mod);
      return -1;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_capi_error("unbound variable", 1, arg);
      return -1;
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return -1;

    sym = arg;
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
    r = scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym));
    if (r < 0) return -1;
  }
  else {
    scm_assert(0);
  }

  r = scm_capi_gloc_value(gloc, SCM_CSETTER_L(val));
  if (r < 0) return -1;

  if (scm_obj_null_p(val)) {
    scm_capi_error("unbound variable", 1, sym);
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

scm_local_func int
scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  ssize_t rslt;
  scm_byte_t *ip, *prv_ip;
  int r;

  SCM_STACK_FRAME_PUSH(&vm, &gloc, &arg, &mod, &module);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj_obj(SCM_VM(vm)->reg.ip,
                                          SCM_CSETTER_L(arg),
                                          SCM_CSETTER_L(mod));
  if (ip == NULL) return -1;

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    int r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return -1;

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1, mod);
      return -1;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      gloc = scm_capi_make_gloc(module, arg);
      if (scm_obj_null_p(gloc)) return -1;
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return -1;
  }
  else if (!scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  r = scm_capi_gloc_bind(gloc, SCM_VM(vm)->reg.val[0]);
  if (r < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t rslt;
  scm_byte_t *ip, *prv_ip;
  int r;

  SCM_STACK_FRAME_PUSH(&vm, &gloc, &arg, &mod, &module, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj_obj(SCM_VM(vm)->reg.ip,
                                          SCM_CSETTER_L(arg),
                                          SCM_CSETTER_L(mod));
  if (ip == NULL) return -1;

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return -1;

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1,mod);
      return -1;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return -1;

    if (scm_obj_null_p(gloc)) {
      scm_capi_error("unbound variable", 1, arg);
      return -1;
    }

    r = scm_capi_gloc_value(gloc, SCM_CSETTER_L(val));
    if (r < 0) return -1;

    if (scm_obj_null_p(val)) {
      scm_capi_error("unbound variable", 1, arg);
      return -1;
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return -1;
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  r = scm_capi_gloc_bind(gloc, SCM_VM(vm)->reg.val[0]);
  if (r < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_sref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    val = scm_box_unbox(val);
    if (scm_obj_null_p(val)) return -1;
  }

  if (scm_capi_landmine_object_p(val)) {
    scm_capi_error("refarence to uninitialized variable", 0);
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

scm_local_func int
scm_vm_op_sset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val, &o);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    scm_capi_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  o = scm_box_unbox(val);
  if (scm_capi_landmine_object_p(o)) {
    scm_capi_error("refarence to uninitialized variable", 0);
    return -1;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

scm_local_func int
scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip + dst;

  return 0;
}

scm_local_func int
scm_vm_op_jmpt(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return -1;

  if (scm_capi_true_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip = ip + dst;
  else
    SCM_VM(vm)->reg.ip = ip;

  return 0;
}

scm_local_func int
scm_vm_op_jmpf(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return -1;

  if (scm_capi_false_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip = ip + dst;
  else
    SCM_VM(vm)->reg.ip = ip;

  return 0;
}

scm_local_func int
scm_vm_op_box(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj box = SCM_OBJ_INIT;
  ScmEnvFrame *efp;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &box);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return -1;

  /* box 化できるのは VM stack 上にある環境のみに限定する */
  /* XXX: 現在のスタックセグメント上にある環境のみに限定したほうがいいかも
          しれない。今の制限でも問題ないはずだが。*/
  if (scm_vm_ef_boxed_p(efp)) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if ((size_t)idx >= efp->len) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  box = scm_box_new(SCM_MEM_HEAP, efp->arg[idx]);
  if (scm_obj_null_p(box)) return -1;

  SCM_WB_SETQ(vm, efp->arg[idx], box);

  return 0;
}

scm_local_func int
scm_vm_op_close(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj clsr = SCM_OBJ_INIT, iseq = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  int nr_env, arity, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &clsr, &iseq, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si_obj(SCM_VM(vm)->reg.ip,
                                            &nr_env, &arity,
                                            SCM_CSETTER_L(iseq));
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (nr_env < 0) {
    scm_capi_error("invalid access to VM Stack: out of range", 0);
    return -1;
  }

  rslt = scm_vm_box_eframe(vm, SCM_VM(vm)->reg.efp,
                           (size_t)nr_env, SCM_CSETTER_L(env));
  if (rslt < 0) return -1;

  clsr = scm_capi_make_closure(iseq, env, arity);
  if (scm_obj_null_p(clsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], clsr);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

scm_local_func int
scm_vm_op_demine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return -1;

  if (!scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    scm_capi_error("update to variable bound by unboxed object", 0);
    return -1;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);

  return 0;
}

scm_local_func int
scm_vm_op_emine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj box = SCM_OBJ_INIT;
  int len, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &box);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &len);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (len < 0) {
    scm_capi_error("bytecode format error", 0);
    return -1;
  }

  rslt = scm_vm_do_op_eframe(vm, op);
  if (rslt < 0) return -1;

  SCM_VM(vm)->reg.vc = 1;
  for (int i = 0; i < len; i++) {
    box = scm_box_new(SCM_MEM_HEAP, scm_bedrock_landmine(scm_vm_current_br()));
    if (scm_obj_null_p(box)) return -1;

    SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], box);
    rslt = scm_vm_do_op_push(vm, op);
    if (rslt < 0) return -1;
  }

  return scm_vm_do_op_ecommit(vm, op, (size_t)len);
}

scm_local_func int
scm_vm_op_edemine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  ScmEnvFrame *efp, *pefp;
  size_t n;
  int argc, layer, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &argc, &layer);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return -1;
  }

  if (layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  pefp = SCM_VM(vm)->reg.pefp;
  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return -1;
  }

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return -1;

  /* XXX: 以降の処理で GC が発生すると efp の値が不正なになる */

  n = ((size_t) argc < efp->len) ? (size_t)argc : efp->len;
  for (size_t i = 0; i < n; i++) {
    if (!scm_obj_type_p(efp->arg[i], &SCM_BOX_TYPE_INFO)) {
      scm_capi_error("update to variable bound by unboxed object", 0);
      return -1;
    }

    scm_box_update(efp->arg[i], pefp->arg[i]);
  }

  rslt = scm_vm_cancel_eframe(vm);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_op_arity(ScmObj vm, SCM_OPCODE_T op)
{
  int arity, rslt, ret;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &arity);
  if (ip == NULL) return -1;

  SCM_VM(vm)->reg.ip = ip;

  rslt = scm_vm_cmp_arity(SCM_VM(vm)->reg.vc, arity, false);
  switch (rslt) {
  case 1:
    scm_capi_error("too many return values", 0);
    ret = -1;
    break;
  case 0:
    ret = 0;
    break;
  case -1:
    scm_capi_error("too few return values", 0);
    ret = -1;
    break;
  case -2:                    /* fall through */
  default:
    scm_assert(false);        /* must not happen */
    ret = -1;
    break;
  }

  return ret;
}

int
scm_vm_bootup(void)
{
  ScmObj stack = SCM_OBJ_INIT;
  ScmBedrock *bedrock;
  int r;

  bedrock = scm_bedrock_new();
  if (bedrock == NULL) return -1;

  stack = scm_ref_stack_new(SCM_MEM_ROOT, SCM_BEDROCK_REF_STACK_INIT_SIZE);
  if (scm_obj_null_p(stack)) return -1;

  scm_vm_chg_current_ref_stack(stack);

  r = scm_bedrock_setup(bedrock);
  if (r < 0) return -1;

  return 0;
}

void
scm_vm_shutdown(void)
{
  scm_bedrock_cleanup(scm_vm_current_br());

  scm_mem_free_root(scm_bedrock_mem(scm_vm_current_br()),
                    scm_vm_current_ref_stack());
  scm_vm_chg_current_ref_stack(SCM_OBJ_NULL);

  scm_bedrock_end(scm_vm_current_br());
}

int
scm_vm_initialize(ScmObj vm, ScmObj main_vm)
{
  ScmObj vmss = SCM_OBJ_INIT, vmsr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &main_vm,
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
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_UNDEF_OBJ);
  SCM_VM(vm)->reg.vc = 1;
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, SCM_NIL_OBJ);
  SCM_VM(vm)->reg.exc = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.hndlr, SCM_NIL_OBJ);
  SCM_VM(vm)->reg.flags = 0;

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
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.vc = 0;
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, SCM_OBJ_NULL);
  SCM_VM(vm)->reg.exc = SCM_OBJ_NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.hndlr, SCM_OBJ_NULL);
  SCM_VM(vm)->reg.flags = 0;
}

ScmObj
scm_vm_new(void)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  rslt = scm_vm_bootup();
  if (rslt < 0) return SCM_OBJ_NULL;

  vm = scm_capi_mem_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, vm);
  if (rslt < 0) {
    scm_capi_mem_free_root(vm);
    return SCM_OBJ_NULL;
  }

  scm_vm_chg_current_vm(vm);

  return vm;
}

ScmObj
scm_vm_clone(ScmObj parent)
{
  ScmObj vm = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&parent,
                       &vm);

  scm_assert_obj_type(parent, &SCM_VM_TYPE_INFO);

  vm = scm_capi_mem_alloc_root(&SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) return SCM_OBJ_NULL;

  rslt = scm_vm_initialize(vm, SCM_VM(parent)->main);
  if (rslt < 0) {
    scm_capi_mem_free_root(vm);
    return SCM_OBJ_NULL;
  }

  return vm;
}

void
scm_vm_end(ScmObj vm)
{
  bool main_vm;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  main_vm = scm_obj_same_instance_p(vm, SCM_VM(vm)->main);

  if (main_vm) {
    scm_capi_gc_start();
    scm_vm_chg_current_vm(SCM_OBJ_NULL);
  }

  scm_capi_mem_free_root(vm);

  if (main_vm)
    scm_vm_shutdown();
}


#define SCM_VM_RUN_EXC_HNDLR_CALLER_IF_NEEDED(vm, r, op)        \
  if ((r) < 0 && scm_vm_raised_p(vm)) {                         \
    r = scm_vm_setup_stat_call_exc_hndlr(vm);                   \
    if (r < 0) continue;                                        \
    r = scm_vm_do_op_return(vm, op);                            \
    continue;                                                   \
  }                                                             \
  else {                                                        \
    scm_vm_discard_raised_obj(vm);                              \
  }

void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  int op, r;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  scm_vmsr_clear(SCM_VM(vm)->stack);

  SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.flags = 0;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_UNDEF_OBJ);
  SCM_VM(vm)->reg.vc = 1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp,
                scm_capi_make_closure(iseq, SCM_OBJ_NULL, 0));
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);

  op = SCM_OPCODE_NOP;
  r = 0;
  while (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_HALT)) {
    SCM_VM_RUN_EXC_HNDLR_CALLER_IF_NEEDED(vm, r, op);

    SCM_CAPI_INST_FETCH_OP(SCM_VM(vm)->reg.ip, op);

    switch(op) {
    case SCM_OPCODE_NOP:
      /* nothing to do */
      r = 0;
      break;
    case SCM_OPCODE_HALT:
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
      r = 0;
      break;
    case SCM_OPCODE_UNDEF:
      r = scm_vm_op_undef(vm, op);
      break;
    case SCM_OPCODE_CALL:       /* fall through */
    case SCM_OPCODE_TAIL_CALL:
      r = scm_vm_op_call(vm, op);
      break;
    case SCM_OPCODE_APPLY:      /* fall through */
    case SCM_OPCODE_TAIL_APPLY:
      r = scm_vm_op_apply(vm, op);
      break;
    case SCM_OPCODE_RETURN:
      r = scm_vm_op_return(vm, op);
      break;
    case SCM_OPCODE_FRAME:
      r = scm_vm_op_frame(vm, op);
      break;
    case SCM_OPCODE_CFRAME:
      r = scm_vm_op_cframe(vm, op);
      break;
    case SCM_OPCODE_EFRAME:
      r = scm_vm_op_eframe(vm, op);
      break;
    case SCM_OPCODE_ECOMMIT:
      r = scm_vm_op_ecommit(vm, op);
      break;
    case SCM_OPCODE_EPOP:
      r = scm_vm_op_epop(vm, op);
      break;
    case SCM_OPCODE_EREBIND:
      r = scm_vm_op_erebind(vm, op);
      break;
    case SCM_OPCODE_IMMVAL:
      r = scm_vm_op_immval(vm, op);
      break;
    case SCM_OPCODE_PUSH:
      r = scm_vm_op_push(vm, op);
      break;
    case SCM_OPCODE_MVPUSH:
      r = scm_vm_op_mvpush(vm, op);
      break;
    case SCM_OPCODE_GREF:
      r = scm_vm_op_gref(vm, op);
      break;
    case SCM_OPCODE_GDEF:
      r = scm_vm_op_gdef(vm, op);
      break;
    case SCM_OPCODE_GSET:
      r = scm_vm_op_gset(vm, op);
      break;
    case SCM_OPCODE_SREF:
      r = scm_vm_op_sref(vm, op);
      break;
    case SCM_OPCODE_SSET:
      r = scm_vm_op_sset(vm, op);
      break;
    case SCM_OPCODE_JMP:
      r = scm_vm_op_jmp(vm, op);
      break;
    case SCM_OPCODE_JMPT:
      r = scm_vm_op_jmpt(vm, op);
      break;
    case SCM_OPCODE_JMPF:
      r = scm_vm_op_jmpf(vm, op);
      break;
    case SCM_OPCODE_BOX:
      r = scm_vm_op_box(vm, op);
      break;
    case SCM_OPCODE_CLOSE:
      r = scm_vm_op_close(vm, op);
      break;
    case SCM_OPCODE_DEMINE:
      r = scm_vm_op_demine(vm, op);
      break;
    case SCM_OPCODE_EMINE:
      r = scm_vm_op_emine(vm, op);
      break;
    case SCM_OPCODE_EDEMINE:
      r = scm_vm_op_edemine(vm, op);
      break;
    case SCM_OPCODE_ARITY:
      r = scm_vm_op_arity(vm, op);
      break;
    default:
      scm_capi_error("invalid instruction code", 0);
      r = -1;
      break;
    }
  }

  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_HALT);
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
}

scm_local_func ScmObj
scm_vm_val_reg_to_vector(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  int r, n, rest;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val, &elm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  val = scm_capi_make_vector((size_t)SCM_VM(vm)->reg.vc, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  n = (SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
    SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG - 1;
  for (size_t i = 0; i < (size_t)n; i++) {
    r = scm_capi_vector_set_i(val, i, SCM_VM(vm)->reg.val[i]);
    if (r < 0) return SCM_OBJ_NULL;;
  }

  rest = SCM_VM(vm)->reg.vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    for (int i = 0; i < rest; i++) {
      elm = scm_capi_vector_ref(SCM_VM(vm)->reg.val[SCM_VM_NR_VAL_REG - 1],
                                (size_t)i);
      if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

      r = scm_capi_vector_set_i(val, (size_t)(SCM_VM_NR_VAL_REG - 1 + i), elm);
      if (r < 0) return SCM_OBJ_NULL;
    }
  }

  return val;
}

ScmObj
scm_vm_apply(ScmObj vm, ScmObj proc, ScmObj args)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ssize_t s;
  int r;

  SCM_STACK_FRAME_PUSH(&vm, &proc, &args,
                       &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_procedure_p(proc));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));

  iseq = scm_api_make_iseq();
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL;

  r = scm_vm_make_proc_call_code(iseq, proc, args, false);
  if (r < 0) return SCM_OBJ_NULL;

  s = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_HALT);
  if (s < 0) return SCM_OBJ_NULL;

  scm_vm_run(vm, iseq);

  if (scm_vm_raised_p(vm))
    return SCM_OBJ_NULL;
  else
    return scm_vm_val_reg_to_vector(vm);
}

ScmObj
scm_vm_run_cloned(ScmObj vm, ScmObj iseq)
{
  ScmObj cloned = SCM_OBJ_INIT, raised = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &iseq,
                       &cloned, &raised);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  cloned = scm_vm_clone(vm);
  if (scm_obj_null_p(cloned)) return SCM_OBJ_NULL;

  scm_vm_chg_current_vm(cloned);
  scm_vm_run(cloned, iseq);
  scm_vm_chg_current_vm(vm);

  if (scm_vm_raised_p(cloned)) {
    raised = scm_vm_raised_obj(cloned);
    scm_vm_end(cloned);
    scm_vm_setup_stat_raise(vm, raised);
    return SCM_OBJ_NULL;
  }

  return scm_vm_val_reg_to_vector(vm);
}

int
scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc)
{
  ScmObj vec = SCM_OBJ_INIT;
  int n, rest, rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &vec);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(vc == 0 || val != NULL);
  scm_assert(vc >= 0);

  n = (vc <= SCM_VM_NR_VAL_REG) ? vc : SCM_VM_NR_VAL_REG - 1;
  for (int i = 0; i < n; i++) {
    if (scm_obj_null_p(val[i])) {
      scm_capi_error("invalid return value is set", 0);
      return -1;
    }
    SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], val[i]);
  }

  rest = vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    vec = scm_capi_make_vector((size_t)rest, SCM_OBJ_NULL);
    if (scm_obj_null_p(vec)) return -1;

    for (int i = 0; i < rest; i++) {
      if (scm_obj_null_p(val[SCM_VM_NR_VAL_REG - 1 + i])) {
        scm_capi_error("invalid return value is set", 0);
        return -1;
      }

      rslt = scm_capi_vector_set_i(vec, (size_t)i,
                                   val[SCM_VM_NR_VAL_REG - 1 + i]);
      if (rslt < 0) return -1;
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

  SCM_STACK_FRAME_PUSH(&vm,
                       &cc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  cc = scm_contcap_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(cc)) return SCM_OBJ_NULL;

  stack = scm_vm_capture_stack(vm);
  if (scm_obj_null_p(stack)) return SCM_OBJ_NULL;

  scm_contcap_cap(cc, stack, &SCM_VM(vm)->reg);

  return cc;
}

int
scm_vm_reinstatement_cont(ScmObj vm, ScmObj cc, const ScmObj *val, int vc)
{
  const ScmObj *v;
  int n, rslt;

  SCM_STACK_FRAME_PUSH(&vm, &cc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  if (vc >= 0) {
    rslt = scm_vm_set_val_reg(vm, val, vc);
    if (rslt < 0) return -1;
  }
  else {
    SCM_VM(vm)->reg.vc = scm_contcap_vc(cc);

    n = ((SCM_VM(vm)->reg.vc <= SCM_VM_NR_VAL_REG) ?
         SCM_VM(vm)->reg.vc : SCM_VM_NR_VAL_REG);
    v = scm_contcap_val(cc);
    for (int i = 0; i < n; i++)
      SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], v[0]);
  }

  rslt = scm_vm_restore_stack(vm, scm_contcap_stack(cc));
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_contcap_cp(cc));
  SCM_VM(vm)->reg.ip = scm_contcap_ip(cc);
  SCM_SLOT_SETQ(ScmVM, vm, reg.prm, scm_contcap_prm(cc));
  SCM_SLOT_SETQ(ScmVM, vm, reg.exc, scm_contcap_exc(cc));
  SCM_SLOT_SETQ(ScmVM, vm, reg.hndlr, scm_contcap_hndlr(cc));
  SCM_VM(vm)->reg.flags = scm_contcap_flags(cc);

  return 0;
}

ScmObj
scm_vm_parameter_value(ScmObj vm, ScmObj var)
{
  ScmObj rib = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ScmObj x = SCM_OBJ_INIT, p = SCM_OBJ_INIT;
  ssize_t n;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &var,
                       &rib, &val,
                       &x, &p);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(var));

  for (x = SCM_VM(vm)->reg.prm; scm_capi_pair_p(x); x = scm_api_cdr(x)) {
    rib = scm_api_car(x);
    if (scm_obj_null_p(rib)) return SCM_OBJ_NULL;

    n = scm_capi_vector_length(rib);
    if (n < 0) return SCM_OBJ_NULL;

    for (ssize_t i = 0; i < n; i += 2) {
      p = scm_capi_vector_ref(rib, (size_t)i);
      if (scm_obj_null_p(p)) return SCM_OBJ_NULL;

      if (scm_capi_eq_p(p, var))
        return scm_capi_vector_ref(rib, (size_t)i + 1);
    }
  }

  if (scm_obj_null_p(x)) return SCM_OBJ_NULL;

  if (!scm_capi_parameter_p(var)) {
    scm_capi_error("failed to get bound value: unbound variable", 1, var);
    return SCM_OBJ_NULL;
  }

  rslt = scm_capi_parameter_init_val(var, SCM_CSETTER_L(val));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_capi_error("failed to get bound value: "
                   "parameter does not have initial value", 1, var);
    return SCM_OBJ_NULL;
  }

  return val;
}

int
scm_vm_setup_stat_trmp(ScmObj vm, ScmObj proc, ScmObj args,
                       ScmObj postproc, ScmObj handover)
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int rslt;

  SCM_STACK_FRAME_PUSH(&proc, &args, &handover,
                       &trmp_code, &trmp_clsr, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_procedure_p(proc));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));
  scm_assert(scm_obj_null_p(postproc) || scm_capi_procedure_p(postproc));

  if (scm_obj_null_p(handover))
    handover = SCM_NIL_OBJ;

  trmp_code = scm_vm_make_trampolining_code(vm, proc, args, postproc, handover);
  if (scm_obj_null_p(trmp_code)) return -1;

  env = SCM_OBJ_NULL;
  if (scm_capi_closure_p(SCM_VM(vm)->reg.cp)) {
    rslt = scm_capi_closure_env(SCM_VM(vm)->reg.cp, SCM_CSETTER_L(env));
    if (rslt < 0) return -1;
  }

  trmp_clsr = scm_capi_make_closure(trmp_code, env, 0);
  if (scm_obj_null_p(trmp_clsr)) return -1;


  ip = scm_capi_iseq_to_ip(trmp_code);
  if (ip == NULL) return -1;

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            SCM_VM(vm)->reg.pefp,
                            trmp_clsr);
  if (rslt < 0) return -1;

  rslt = scm_vm_commit_cframe(vm, ip);
  if (rslt < 0) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_NIL_OBJ);
  SCM_VM(vm)->reg.vc = 1;

  return 0;
}

void
scm_vm_setup_stat_halt(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
}

int
scm_vm_setup_stat_raise(ScmObj vm, ScmObj obj)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_RAISE)) {
    scm_capi_fatal("Exception has raised in the situation can be addressed");
    return -1;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.exc, obj);
  return 0;
}

int
scm_vm_setup_stat_call_exc_hndlr(ScmObj vm)
{
  ScmObj args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm,
                       &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_api_cons(SCM_VM(vm)->reg.exc, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  return scm_vm_setup_stat_trmp(vm,
                                scm_bedrock_exc_hndlr_caller(scm_vm_current_br()),
                                args, SCM_OBJ_NULL, SCM_OBJ_NULL);
}

int
scm_vm_setup_stat_call_exc_hndlr_cont(ScmObj vm)
{
  ScmObj args = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm,
                       &args);

  if (!scm_vm_raised_p(vm))
    return 0;

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  args = scm_api_cons(SCM_VM(vm)->reg.exc, SCM_NIL_OBJ);
  if (scm_obj_null_p(args)) return -1;

  scm_vm_discard_raised_obj(vm);

  return scm_vm_setup_stat_trmp(vm,
                                scm_bedrock_exc_hndlr_caller_cont(scm_vm_current_br()),
                                args, SCM_OBJ_NULL, SCM_OBJ_NULL);

  return 0;
}

int
scm_vm_push_exc_handler(ScmObj vm, ScmObj hndlr)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &hndlr, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_procedure_p(hndlr));

  lst = scm_api_cons(hndlr, SCM_VM(vm)->reg.hndlr);
  if (scm_obj_null_p(lst)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.hndlr, lst);

  return 0;
}

int
scm_vm_pop_exc_handler(ScmObj vm)
{
  ScmObj rest = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm,
                       &rest);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_capi_pair_p(SCM_VM(vm)->reg.hndlr))
    return 0;

  rest = scm_api_cdr(SCM_VM(vm)->reg.hndlr);
  if (scm_obj_null_p(rest)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.hndlr, rest);

  return 0;
}

int
scm_vm_exc_handler(ScmObj vm, scm_csetter_t *hndlr)
{
  ScmObj val = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  assert(hndlr != NULL);

  if (scm_capi_pair_p(SCM_VM(vm)->reg.hndlr)) {
    val = scm_api_car(SCM_VM(vm)->reg.hndlr);
    if (scm_obj_null_p(val)) return -1;
  }
  else {
    val = SCM_OBJ_NULL;
  }

  scm_csetter_setq(hndlr, val);

  return 0;
}

int
scm_vm_subr_exc_hndlr_caller(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj vm = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT, hndlr_arg = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT;
  int rslt, ret;

  SCM_STACK_FRAME_PUSH(&subr,
                       &vm, &hndlr, &hndlr_arg,
                       &val);

  vm = scm_vm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_exc_handler(vm, SCM_CSETTER_L(hndlr));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(hndlr)) {
    SCM_SLOT_SETQ(ScmVM, vm, reg.exc, argv[0]);
    scm_vm_setup_stat_halt(vm);
    ret = -1;   /* 戻り値を -1 にするのは exception handler caller サブルーチ
                   ンの return 処理を抑制するため。抑制しないと、cframe が一つ
                   も積まれていない状況で例外処理機構が起動した場合に return
                   処理で stack underflow が発生してしまう */
    goto end;
  }

  rslt = scm_vm_pop_exc_handler(vm);
  if (rslt < 0) return -1;

  hndlr_arg = scm_api_cons(argv[0], SCM_NIL_OBJ);
  if (scm_obj_null_p(hndlr_arg)) return -1;

  ret = scm_vm_setup_stat_trmp(vm, hndlr, hndlr_arg, subr, argv[0]);

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

  SCM_STACK_FRAME_PUSH(&subr,
                       &vm, &hndlr, &hndlr_arg,
                       &val);

  vm = scm_vm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_exc_handler(vm, SCM_CSETTER_L(hndlr));
  if (rslt < 0) return -1;

  if (scm_obj_null_p(hndlr)) {
    SCM_SLOT_SETQ(ScmVM, vm, reg.exc, argv[0]);
    scm_vm_setup_stat_halt(vm);
    ret = -1;   /* scm_vm_subr_exc_hndlr_caller とは異り、return 処理を抑制
                   する必要はないが、exception handler が無い場合の戻り値を
                   scm_vm_subr_exc_hndlr_caller と統一するため、-1 を戻り値
                   にする */
    goto end;
  }

  rslt = scm_vm_pop_exc_handler(vm);
  if (rslt < 0) return -1;

  hndlr_arg = scm_api_cons(argv[0], SCM_NIL_OBJ);
  if (scm_obj_null_p(hndlr_arg)) return -1;

  ret = scm_vm_setup_stat_trmp(vm, hndlr, hndlr_arg,
                                scm_bedrock_exc_hndlr_caller_post(scm_vm_current_br()),
                                hndlr);

 end:
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
  return ret;
}

int
scm_vm_subr_exc_hndlr_caller_post(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj vm = SCM_OBJ_INIT, hndlr = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &vm, &hndlr, &val)

  vm = scm_vm_current_vm();

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  rslt = scm_vm_push_exc_handler(vm, argv[0]);
  if (rslt < 0) goto end;

  rslt = scm_capi_return_val(argv + 1, argc - 1);

 end:
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
  return rslt;
}

void
scm_vm_disposal_unhandled_exc(ScmObj vm)
{
  ScmObj raised = SCM_OBJ_INIT, port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  ScmObj ro = SCM_OBJ_INIT;
  char msg[256], *p;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &raised, &port, &str,
                       &ro);

  if (scm_obj_null_p(SCM_VM(vm)->reg.exc))
    return;

  port = scm_api_open_output_string();
  if (scm_obj_null_p(port)) return;

  rslt = scm_capi_write_cstr("Unhandled Exception: ", SCM_ENC_SRC, port);
  if (rslt < 0) return;

  ro = scm_api_display(SCM_VM(vm)->reg.exc, port);
  if (scm_obj_null_p(ro)) return;

  ro = scm_api_newline(port);
  if (scm_obj_null_p(ro)) return;

  str = scm_api_get_output_string(port);
  if (scm_obj_null_p(str)) return;

  p = scm_capi_string_to_cstr(str, msg, sizeof(msg));
  if (p == NULL) return;

  scm_bedrock_error(scm_vm_current_br(), msg);
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
  SCM_VM(obj)->reg.pefp = NULL;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.vc = 0;
  SCM_VM(obj)->reg.prm = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.exc = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.hndlr = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.flags = 0;
}

void
scm_vm_gc_finalize(ScmObj obj)
{
  scm_vm_finalize(obj);
}

scm_local_func int
scm_vm_gc_accept_stack(ScmObj vm, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT, r;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (scm_obj_null_p(SCM_VM(vm)->stack)) return rslt;

  r = scm_vm_update_pef_len_if_needed(vm);
  if (r < 0) return -1;                /* TODO: use gc error number */

  rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, SCM_VM(vm)->stack, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_cf_gc_accept(vm, SCM_VM(vm)->reg.cfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(vm, &SCM_VM(vm)->reg.efp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(vm, &SCM_VM(vm)->reg.pefp, mem, handler);
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

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.exc, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.hndlr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_stack(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
