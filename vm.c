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
#include "core_subr.h"
#include "miscobjects.h"
#include "compiler.h"
#include "api.h"
#include "impl_utils.h"


/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

#define SCM_BEDROCK_REF_STACK_INIT_SIZE 512
#define SCM_BEDROCK_ERR_MSG_SIZE 256

ScmBedrock *scm_bedrock__current_br = NULL;

int
scm_bedrock_initialize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  br->ref_stack = NULL;
  br->err.message = NULL;

  br->ref_stack = scm_ref_stack_new(SCM_BEDROCK_REF_STACK_INIT_SIZE);
  if (br->ref_stack == NULL) goto err;

  br->err.type = SCM_BEDROCK_ERR_NONE;
  br->err.message = malloc(SCM_BEDROCK_ERR_MSG_SIZE);
  if (br->err.message == NULL) goto err;

  br->encoding = SCM_ENC_UTF8;

  br->vm = SCM_OBJ_NULL;

  return 0;

 err:

  scm_ref_stack_end(br->ref_stack);
  free(br->err.message);
  br->ref_stack = NULL;
  br->err.message = NULL;

  return -1;
}

void
scm_bedrock_finalize(ScmBedrock *br)
{
  scm_assert(br != NULL);

  scm_ref_stack_end(br->ref_stack);
  free(br->err.message);
  br->ref_stack = NULL;
  br->err.message = NULL;
  br->vm = SCM_OBJ_NULL;
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

  return br;
}

void
scm_bedrock_end(ScmBedrock *br)
{
  scm_bedrock_finalize(br);
  free(br);
}

void
scm_bedrock_clean(ScmBedrock *br)
{
  scm_ref_stack_init_sp(br->ref_stack);
  br->err.type = SCM_BEDROCK_ERR_NONE;
  br->err.message[0] = '\0';
}

void
scm_bedrock_fatal(ScmBedrock *br, const char *msg)
{
  scm_assert(br != NULL);

  br->err.type = SCM_BEDROCK_ERR_FATAL;
  if (msg == NULL) {
    br->err.message[0] = '\0';
  }
  else {
    size_t len = strlen(msg);
    if (len > SCM_BEDROCK_ERR_MSG_SIZE - 1) len = SCM_BEDROCK_ERR_MSG_SIZE - 1;
    memcpy(br->err.message, msg, len);
    br->err.message[len] = '\0';
  }

  if (scm_obj_not_null_p(br->vm))
    scm_vm_setup_stat_halt(br->vm);
}

void
scm_bedrock_fatal_fmt(ScmBedrock *br, const char *msgfmt, va_list ap)
{
  scm_assert(br != NULL);

  br->err.type = SCM_BEDROCK_ERR_FATAL;
  if (msgfmt == NULL)
    br->err.message[0] = '\0';
  else
    vsnprintf(br->err.message, SCM_BEDROCK_ERR_MSG_SIZE, msgfmt, ap);
}

bool
scm_bedrock_fatal_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  return (br->err.type == SCM_BEDROCK_ERR_FATAL) ? true : false;
}

bool
scm_bedrock_error_p(ScmBedrock *br)
{
  scm_assert(br != NULL);

  return ((br->err.type == SCM_BEDROCK_ERR_FATAL
           || br->err.type == SCM_BEDROCK_ERR_ERROR) ?
          true : false);
}


/***************************************************************************/
/*  ScmBox                                                                 */
/***************************************************************************/

ScmTypeInfo SCM_BOX_TYPE_INFO = {
  .name                = "box",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = NULL,
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
  .name = "contcap",
  .flags = SCM_TYPE_FLG_MMO,
  .pp_func = NULL,
  .obj_size = sizeof(ScmContCap),
  .gc_ini_func = scm_contcap_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_contcap_gc_accepct,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
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
scm_contcap_cap(ScmObj cc,  ScmObj stack,
                ScmCntFrame *cfp, ScmEnvFrame *efp, ScmEnvFrame *pefp,
                ScmObj cp, scm_byte_t *ip, const ScmObj *val, int vc,
                unsigned int flags)
{
  int n;

  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);
  scm_assert_obj_type(stack, &SCM_VMSTCKRC_TYPE_INFO);

  SCM_SLOT_SETQ(ScmContCap, cc, stack, stack);
  SCM_CONTCAP(cc)->reg.cfp = cfp;
  SCM_CONTCAP(cc)->reg.efp = efp;
  SCM_CONTCAP(cc)->reg.pefp = pefp;
  SCM_SLOT_SETQ(ScmContCap, cc, reg.cp, cp);
  SCM_CONTCAP(cc)->reg.ip = ip;
  n = (vc <= SCM_VM_NR_VAL_REG) ? vc : SCM_VM_NR_VAL_REG;
  for (int i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmContCap, cc, reg.val[i], val[i]);
  SCM_CONTCAP(cc)->reg.vc = vc;
  SCM_CONTCAP(cc)->reg.flags = flags;
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
  SCM_CONTCAP(obj)->reg.cfp = NULL;
  SCM_CONTCAP(obj)->reg.efp = NULL;
  SCM_CONTCAP(obj)->reg.pefp = NULL;
  SCM_CONTCAP(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_CONTCAP(obj)->reg.ip = NULL;
  SCM_CONTCAP(obj)->reg.vc = 0;
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

  rslt = scm_vm_cf_gc_accept(obj, SCM_CONTCAP(obj)->reg.cfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(obj, &SCM_CONTCAP(obj)->reg.efp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_ef_gc_accept(obj, &SCM_CONTCAP(obj)->reg.pefp, mem, handler);
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
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmVM),
  .gc_ini_func         = scm_vm_gc_initialize,
  .gc_fin_func         = scm_vm_gc_finalize,
  .gc_accept_func      = scm_vm_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj scm_vm__current_vm = SCM_OBJ_INIT;
ScmMem *scm_vm__current_mm = NULL;

scm_local_func int
scm_vm_setup_singletons(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.nil, scm_nil_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.nil))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.eof, scm_eof_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.eof))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_true,
                scm_bool_new(SCM_MEM_ROOT, true));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_true))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_false,
                scm_bool_new(SCM_MEM_ROOT, false));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_false))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.undef,
                scm_undef_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.undef))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.landmine,
                scm_landmine_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.landmine))
    return -1;                  /* [ERR]: [through] */

  return 0;
}

scm_local_func void
scm_vm_clean_singletons(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.nil))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.nil);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.eof))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.eof);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.b_true))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_true);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.b_false))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_false);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.undef))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.undef);

  if (scm_obj_not_null_p(SCM_VM(vm)->cnsts.landmine))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.landmine);

  SCM_VM(vm)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_false = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.undef = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.landmine = SCM_OBJ_NULL;
}

scm_local_func int
scm_vm_setup_global_env(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, ge.symtbl, scm_symtbl_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->ge.symtbl))
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM,vm, ge.modtree, scm_moduletree_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->ge.modtree))
    return -1;                  /* [ERR]: [through] */

  SCM_VM(vm)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.err = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.curio.out = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;

  return 0;
}

scm_local_func void
scm_vm_clean_global_env(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_obj_null_p(SCM_VM(vm)->ge.symtbl))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->ge.symtbl);

  if (scm_obj_null_p(SCM_VM(vm)->ge.modtree))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->ge.modtree);

  SCM_VM(vm)->ge.symtbl = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.modtree = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.err = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.curio.out = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;
}

scm_local_func int
scm_vm_init_eval_env(ScmObj vm)
{
  int in_fd = -1, out_fd = -1, err_fd = -1;
  ScmObj in = SCM_OBJ_INIT, out = SCM_OBJ_INIT, err = SCM_OBJ_INIT;
  ScmObj vmss = SCM_OBJ_INIT, vmsr = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm,
                       &in, &out, &err,
                       &vmss, &vmsr);

  scm_symtbl_clean(SCM_VM(vm)->ge.symtbl);
  scm_moduletree_clean(SCM_VM(vm)->ge.modtree);

  vmss = scm_vmss_new(SCM_MEM_HEAP, SCM_VM_STACK_INIT_SIZE);
  if (scm_obj_null_p(vmss)) return -1;

  vmsr = scm_vmsr_new(SCM_MEM_HEAP, vmss, scm_vmss_base(vmss), SCM_OBJ_NULL);
  if (scm_obj_null_p(vmsr)) return -1;

  in_fd = dup(0);
  if (in_fd < 0) {
    scm_capi_error("system call error: dup", 0);
    return -1;
  }

  out_fd = dup(0);
  if (out_fd < 0) {
    scm_capi_error("system call error: dup", 0);
    close(in_fd);
    return -1;
  }

  err_fd = dup(0);
  if (err_fd < 0) {
    scm_capi_error("system call error: dup", 0);
    close(in_fd); close(out_fd);
    return -1;
  }

  in = scm_capi_open_input_fd(in_fd, SCM_PORT_BUF_DEFAULT,
                              scm_capi_system_encoding());
  if (scm_obj_null_p(in)) {
    close(in_fd); close(out_fd); close(err_fd);
    return -1;                  /* [ERR]: [through] */
  }

  out = scm_capi_open_output_fd(out_fd, SCM_PORT_BUF_DEFAULT,
                               scm_capi_system_encoding());
  if (scm_obj_null_p(out)) {
    close(out_fd); close(err_fd);
    return -1;                  /* [ERR]: [through] */
  }

  err = scm_capi_open_output_fd(err_fd, SCM_PORT_BUF_DEFAULT,
                                scm_capi_system_encoding());
  if (scm_obj_null_p(err)) {
    close(err_fd);
    return -1;                  /* [ERR]: [through] */
  }

  SCM_SLOT_SETQ(ScmVM, vm, ge.stdio.in, in);
  SCM_SLOT_SETQ(ScmVM, vm, ge.stdio.out, out);
  SCM_SLOT_SETQ(ScmVM, vm, ge.stdio.err, err);

  SCM_SLOT_SETQ(ScmVM, vm, ge.curio.in, in);
  SCM_SLOT_SETQ(ScmVM, vm, ge.curio.out, out);

  SCM_SLOT_SETQ(ScmVM, vm, ge.excpt.hndlr, SCM_VM(vm)->cnsts.nil);
  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;

  SCM_VM(vm)->stack = vmsr;

  SCM_VM(vm)->reg.sp = scm_vmsr_base(vmsr);
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val[0] = SCM_VM(vm)->cnsts.undef;
  SCM_VM(vm)->reg.vc = 1;
  SCM_VM(vm)->reg.flags = 0;

  return 0;
}

scm_local_func void
scm_vm_clean_eval_env(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_symtbl_clean(SCM_VM(vm)->ge.symtbl);
  scm_moduletree_clean(SCM_VM(vm)->ge.modtree);

  SCM_VM(vm)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.err = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.curio.out = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;

  SCM_VM(vm)->stack = SCM_OBJ_NULL;

  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.vc = 0;
  SCM_VM(vm)->reg.flags = 0;
}

scm_local_func int
scm_vm_load_builtin_modules(ScmObj vm)
{
  const char *imported_list[] = { "core-syntax", "core" };
  ScmObj name = SCM_OBJ_INIT, module = SCM_OBJ_INIT, imported = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &name, &module, &imported);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_initialize_module_core_syntax();
  if (rslt < 0) return -1;

  rslt = scm_initialize_module_core();
  if (rslt < 0) return -1;

  name = scm_capi_make_symbol_from_cstr("main", SCM_ENC_ASCII);
  if (scm_obj_null_p(name)) return -1;

  module = scm_api_make_module(name);
  if (scm_obj_null_p(module)) return -1;

  for (size_t i = 0; i < sizeof(imported_list)/sizeof(imported_list[0]); i++) {
    name = scm_capi_make_symbol_from_cstr(imported_list[i], SCM_ENC_ASCII);
    if (scm_obj_null_p(name)) return -1;

    rslt = scm_capi_find_module(name, SCM_CSETTER_L(imported));
    if (rslt < 0) return -1;

    if (scm_obj_null_p(imported)) {
      scm_capi_error("failed to import a module: not exist", 0);
      return -1;
    }

    rslt = scm_capi_import(module, imported);
    if (rslt < 0) return -1;
  }

  return 0;
}

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
scm_vm_make_cframe(ScmObj vm, ScmEnvFrame *efp, ScmObj cp)
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

  if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame link will be broken", 0);
    return -1;
  }

  if (SCM_VM(vm)->reg.cfp == NULL) {
    rslt = scm_vm_handle_stack_underflow(vm);
    if (rslt < 0) return -1;
  }

  cfp = SCM_VM(vm)->reg.cfp;

  SCM_VM(vm)->reg.cfp = scm_vm_cf_next(cfp);
  SCM_VM(vm)->reg.efp = cfp->efp;
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

scm_local_func ScmObj
scm_vm_copy_list(ScmObj lst)
{
  ScmObj cur = SCM_OBJ_INIT, elm = SCM_OBJ_INIT, nil = SCM_OBJ_INIT;
  ScmObj head = SCM_OBJ_INIT, pair = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst,
                       &cur, &elm, &nil,
                       &head, &pair, &prev,
                       &rslt);

  scm_assert(scm_capi_nil_p(lst) || scm_capi_pair_p(lst));

  nil = scm_api_nil();

  prev = SCM_OBJ_NULL;
  head = SCM_OBJ_NULL;
  for (cur = lst; scm_capi_pair_p(cur); cur = scm_api_cdr(cur)) {
    elm = scm_api_car(cur);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;

    pair = scm_api_cons(elm, nil);
    if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

    if (scm_obj_not_null_p(prev)) {
      rslt = scm_api_set_cdr(prev, pair);
      if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;
    }
    else {
      head = pair;
    }
    prev = pair;
  }

  if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

  rslt = scm_api_set_cdr(prev, cur);
  if (scm_obj_null_p(rslt)) return SCM_OBJ_NULL;

  return scm_obj_null_p(head) ? nil : head;
}

scm_local_func ScmObj
scm_vm_make_trampolining_code(ScmObj vm, ScmObj clsr,
                              ScmObj args, ScmObj callback)
{
  ScmObj iseq = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  int i, arity, nr_decons;
  ssize_t len, rslt;
  bool unwished;

  SCM_STACK_FRAME_PUSH(&vm, &clsr, &args, &callback, &iseq, &cur, &arg);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_closure_p(clsr));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));
  scm_assert(scm_obj_null_p(callback)
             || scm_capi_subrutine_p(callback)
             || scm_capi_closure_p(callback));

  /* 以下の処理を実行する iseq オブエクトを生成する
   * l args を引数として target クロージャを呼出す
   *   (callback が NULL の場合、target クロージャ の呼出は tail call とする)
   * 2 callback が非 NULL の場合、target クロージャの戻り値を引数として
   *   callback を tail call する
   */

  iseq = scm_api_make_iseq();
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_ARITY, 1);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (!scm_obj_null_p(callback)) {
    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_FRAME);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  rslt = scm_capi_arity(clsr, &arity);
  if (rslt < 0) return SCM_OBJ_NULL;

  rslt = scm_capi_procedure_flg_set_p(clsr, SCM_PROC_ADJ_UNWISHED, &unwished);
  if (rslt < 0) return SCM_OBJ_NULL;

  len = scm_capi_length(args);
  if (len < 0) return SCM_OBJ_NULL;

  if (arity >= 0) {
    if (len != arity) {
      scm_capi_error("", 0);    /* TODO: error message */
      return SCM_OBJ_NULL;
    }
    nr_decons = arity;
  }
  else {
    if (len < -arity - 1) {
      scm_capi_error("", 0);    /* TODO: error message */
      return SCM_OBJ_NULL;
    }
    nr_decons = unwished ? (int)len : -arity - 1;
  }

  if (scm_obj_not_null_p(callback)) {
    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_EFRAME);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  if (nr_decons > 0 || arity < 0) {
    if (scm_obj_null_p(callback)) {
      rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_EFRAME);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
    }
    else {
      rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_FRAME);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
    }

    for (cur = args, i = 0;
         scm_capi_pair_p(cur) && i < nr_decons;
         cur = scm_api_cdr(cur), i++) {
      arg = scm_api_car(cur);
      if (scm_obj_null_p(arg)) return SCM_OBJ_NULL; /* [ERR: [through] */

      rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, arg);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

      rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
    }

    if (scm_obj_null_p(cur)) return SCM_OBJ_NULL; /* [ERR: [through] */

    if (arity < 0 && !unwished) {
      /* TODO: scm_api_list_copy が実装された場合は、そちらを使う */
      cur = scm_vm_copy_list(cur);
      if (scm_obj_null_p(cur)) return SCM_OBJ_NULL;

      rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, cur);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

      rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
      if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
    }
  }

  rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, clsr);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_obj_null_p(callback)) {
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_TAIL_CALL, arity);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else {
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_CALL, arity);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    /*  TODO: 1 固定ではなく、callback の arity でチェックする */
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_ARITY, 1);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, callback);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    /*  TODO: call ではなく、aply を使用する */
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_TAIL_CALL, 1);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  return iseq;
}

scm_local_func ScmObj
scm_vm_make_exception_handler_code(ScmObj vm)
{
  ScmObj iseq = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  iseq = scm_api_make_iseq();
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_RAISE);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return iseq;
}

scm_local_func int
scm_vm_setup_to_call_exception_handler(ScmObj vm)
{
  ScmObj iseq = SCM_OBJ_INIT, env = SCM_OBJ_INIT, clsr = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &iseq, &env, &clsr);

  iseq = scm_vm_make_exception_handler_code(vm);
  if (scm_obj_null_p(iseq)) return -1;

  env = SCM_OBJ_NULL;
  if (scm_capi_closure_p(SCM_VM(vm)->reg.cp)) {
    rslt = scm_capi_closure_env(SCM_VM(vm)->reg.cp, SCM_CSETTER_L(env));
    if (rslt < 0) return -1;
  }

  clsr = scm_capi_make_closure(iseq, env, 0);
  if (scm_obj_null_p(clsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, clsr);
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);

  return 0;
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

  lst = SCM_VM(vm)->cnsts.nil;
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
    scm_capi_error("object is not applicable", 1, SCM_VM(vm)->reg.val);
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

    if (rslt < 0) return -1;               /* [ERR]: [through] */

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
                            SCM_VM(vm)->reg.cp);
  if (rslt < 0) return -1;

  rslt = scm_vm_make_eframe(vm, 0);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func int
scm_vm_do_op_eframe(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_make_eframe(vm, 0);
}

scm_local_func int
scm_vm_do_op_ecommit(ScmObj vm, SCM_OPCODE_T op, size_t argc)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_commit_eframe(vm, SCM_VM(vm)->reg.efp, argc);
}

scm_local_func void
scm_vm_op_undef(ScmObj vm, SCM_OPCODE_T op)
{
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_VM(vm)->cnsts.undef);
  SCM_VM(vm)->reg.vc = 1;
}

scm_local_func void
scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op)
{
  int argc;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  SCM_VM(vm)->reg.ip = ip;
  scm_vm_do_op_call(vm, op, argc, (op == SCM_OPCODE_TAIL_CALL));
}

scm_local_func void
scm_vm_op_apply(ScmObj vm, SCM_OPCODE_T op)
{
  ptrdiff_t argc;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return;
  }

  argc = SCM_VM(vm)->reg.sp - (scm_byte_t *)SCM_VM(vm)->reg.pefp;
  argc -= (ptrdiff_t)sizeof(ScmEnvFrame);
  argc /= (ptrdiff_t)sizeof(ScmObj);

  scm_assert(argc >= 0);

  if (argc == 0) {
    rslt = scm_vm_cancel_eframe(vm);
    if (rslt < 0) return;
  }

  scm_vm_do_op_call(vm, op, (int)argc, (op == SCM_OPCODE_TAIL_APPLY));
}

scm_local_func void
scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_CSETTER_L(val));
  if (ip == NULL) return;       /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;
}

scm_local_func void
scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_push(vm, op);
}

scm_local_func void
scm_vm_op_mvpush(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_mvpush(vm, op);
}

scm_local_func void
scm_vm_op_frame(ScmObj vm, SCM_OPCODE_T op) /* GC OK */
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_frame(vm, op);
}

scm_local_func void
scm_vm_op_cframe(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_make_cframe(vm,
                     SCM_VM(vm)->reg.efp,
                     SCM_VM(vm)->reg.cp);
}

scm_local_func void
scm_vm_op_eframe(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_eframe(vm, op);
}

scm_local_func void
scm_vm_op_ecommit(ScmObj vm, SCM_OPCODE_T op)
{
  int argc;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  SCM_VM(vm)->reg.ip = ip;

  scm_vm_do_op_ecommit(vm, op, (size_t)argc);
}

scm_local_func void
scm_vm_op_epop(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_pop_eframe(vm);
}

scm_local_func void
scm_vm_op_erebind(ScmObj vm, SCM_OPCODE_T op)
{
  ScmEnvFrame *efp;
  int argc;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  SCM_VM(vm)->reg.ip = ip;

  if (SCM_VM(vm)->reg.efp == NULL) {
    scm_capi_error("invalid operation of enviromnet frame: "
                   "enviroment frame to be rebound is not exist", 0);
    return;
  }

  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return;
  }

  if (scm_vmsr_include_p(SCM_VM(vm)->stack,
                         (scm_byte_t *)SCM_VM(vm)->reg.efp)) {
    if (scm_vm_ef_maked_on_pef_p(SCM_VM(vm)->reg.pefp)) {
      scm_capi_error("invalid operation of VM stack: "
                     "partial stack frame link will be broken", 0);
      return;
    }
  }

  scm_vm_do_op_ecommit(vm, op, (size_t)argc);
  efp = scm_vm_ef_outer(SCM_VM(vm)->reg.efp);
  scm_vm_ef_replace_outer(SCM_VM(vm)->reg.efp, scm_vm_ef_outer(efp));

  if (scm_vmsr_include_p(SCM_VM(vm)->stack, (scm_byte_t *)efp)) {
    memmove(efp, SCM_VM(vm)->reg.efp,
            sizeof(ScmEnvFrame) + sizeof(ScmObj) * (size_t)argc);
    SCM_VM(vm)->reg.efp = efp;
    SCM_VM(vm)->reg.sp = (scm_byte_t *)efp->arg + sizeof(ScmObj) * (size_t)argc;
  }
}

scm_local_func void
scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op) /* GC OK */
{
  scm_vm_do_op_return(vm, op);
}

scm_local_func void
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
  if (ip == NULL) return;              /* [ERR]: [through] */

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1, mod);
      return;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(gloc)) {
      scm_capi_error("unbound variable", 1, arg);
      return;
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return;      /* [ERR]: [through] */

    sym = arg;
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
    r = scm_capi_gloc_symbol(gloc, SCM_CSETTER_L(sym));
    if (r < 0) return;          /* [ERR]: [through] */
  }
  else {
    scm_assert(0);
  }

  r = scm_capi_gloc_value(gloc, SCM_CSETTER_L(val));
  if (r < 0) return;          /* [ERR]: [through] */

  if (scm_obj_null_p(val)) {
    scm_capi_error("unbound variable", 1, sym);
    return;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;

  return;
}

scm_local_func void
scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT;
  ssize_t rslt;
  scm_byte_t *ip, *prv_ip;

  SCM_STACK_FRAME_PUSH(&vm, &gloc, &arg, &mod, &module);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj_obj(SCM_VM(vm)->reg.ip,
                                          SCM_CSETTER_L(arg),
                                          SCM_CSETTER_L(mod));
  if (ip == NULL) return;              /* [ERR]: [through] */

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    int r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1, mod);
      return;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(gloc)) {
      gloc = scm_capi_make_gloc(module, arg);
      if (scm_obj_null_p(gloc)) return; /* [ERR]: [through] */
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return;             /* [ERR]: [through] */
  }
  else if (!scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  scm_capi_gloc_bind(gloc, SCM_VM(vm)->reg.val[0]);
}

scm_local_func void
scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, mod = SCM_OBJ_INIT;
  ScmObj module = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t rslt;
  scm_byte_t *ip, *prv_ip;

  SCM_STACK_FRAME_PUSH(&vm, &gloc, &arg, &mod, &module, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj_obj(SCM_VM(vm)->reg.ip,
                                          SCM_CSETTER_L(arg),
                                          SCM_CSETTER_L(mod));
  if (ip == NULL) return;              /* [ERR]: [through] */

  prv_ip = SCM_VM(vm)->reg.ip;
  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    int r = scm_capi_find_module(mod, SCM_CSETTER_L(module));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(module)) {
      scm_capi_error("unknown module", 1,mod);
      return;
    }

    r = scm_capi_find_gloc(module, arg, SCM_CSETTER_L(gloc));
    if (r < 0) return;          /* [ERR]: [through] */

    r = scm_capi_gloc_value(gloc, SCM_CSETTER_L(val));
    if (r < 0) return;          /* [ERR]: [through] */

    if (scm_obj_null_p(gloc) || scm_obj_null_p(val)) {
      scm_capi_error("unbound variable", 1, arg);
      return;
    }

    rslt = scm_capi_inst_update_oprand_obj(prv_ip, SCM_VM(vm)->reg.cp, gloc);
    if (rslt < 0) return;      /* [ERR]: [through] */
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    gloc = arg;
  }
  else {
    scm_assert(0);
  }

  scm_capi_gloc_bind(gloc, SCM_VM(vm)->reg.val[0]);
}

scm_local_func void
scm_vm_op_sref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return;

  if (scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    val = scm_box_unbox(val);
    if (scm_obj_null_p(val)) return;
  }

  if (scm_obj_same_instance_p(val, SCM_VM(vm)->cnsts.landmine)) {
    scm_capi_error("refarence to uninitialized variable", 0);
    return;
  }

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], val);
  SCM_VM(vm)->reg.vc = 1;
}

scm_local_func void
scm_vm_op_sset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val, &o);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return;

  if (!scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    scm_capi_error("update to variable bound by unboxed object", 0);
    return;
  }

  o = scm_box_unbox(val);
  if (scm_obj_same_instance_p(o, SCM_VM(vm)->cnsts.landmine)) {
    scm_capi_error("refarence to uninitialized variable", 0);
    return;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);
}

scm_local_func void
scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip + dst;
}

scm_local_func void
scm_vm_op_jmpt(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (scm_capi_true_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip = ip + dst;
  else
    SCM_VM(vm)->reg.ip = ip;
}

scm_local_func void
scm_vm_op_jmpf(ScmObj vm, SCM_OPCODE_T op)
{
  int dst;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (scm_capi_false_p(SCM_VM(vm)->reg.val[0]))
    SCM_VM(vm)->reg.ip = ip + dst;
  else
    SCM_VM(vm)->reg.ip = ip;
}

/* exception handler リストにある先頭の handler を val レジスタに設定し、
 * exception handler リストから先頭の handler を取り除く
 *
 * TODO: この raise の実装は handler がクロージャの場合うまく動かないの
 *       で、仕組みを変える
 */
scm_local_func void
scm_vm_op_raise(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj hndlr = SCM_OBJ_INIT, rest = SCM_OBJ_INIT, fp_fn = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &hndlr, &rest, &fp_fn);

  if (scm_capi_nil_p(SCM_VM(vm)->ge.excpt.hndlr)) {
    scm_capi_fatal("raised, but exception handler is not installed");
    return;
  }

  hndlr = scm_api_car(SCM_VM(vm)->ge.excpt.hndlr);
  if (scm_obj_null_p(hndlr)) return; /* [ERR]: [through] */

  rest = scm_api_cdr(SCM_VM(vm)->ge.excpt.hndlr);
  if (scm_obj_null_p(rest)) return; /* [ERR]: [through] */

  rslt = scm_vm_do_op_frame(vm, op);
  if (rslt < 0) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_VM(vm)->ge.excpt.raised);
  SCM_VM(vm)->reg.vc = 1;

  rslt = scm_vm_do_op_push(vm, op);
  if (rslt < 0) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], hndlr);
  SCM_VM(vm)->reg.vc = 1;
  SCM_SLOT_SETQ(ScmVM, vm, ge.excpt.hndlr, rest);

  rslt = scm_vm_do_op_call(vm, op, 1, false);
  if (rslt < 0) return;

  scm_capi_raise(SCM_VM(vm)->ge.excpt.raised);
}

scm_local_func void
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
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return;

  /* box 化できるのは VM stack 上にある環境のみに限定する */
  /* XXX: 現在のスタックセグメント上にある環境のみに限定したほうがいいかも
          しれない。今の制限でも問題ないはずだが。*/
  if (scm_vm_ef_boxed_p(efp)) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  if ((size_t)idx >= efp->len) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  box = scm_box_new(SCM_MEM_HEAP, efp->arg[idx]);
  if (scm_obj_null_p(box)) return;

  SCM_WB_SETQ(vm, efp->arg[idx], box);
}

scm_local_func void
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
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (nr_env < 0) {
    scm_capi_error("invalid access to VM Stack: out of range", 0);
    return;
  }

  rslt = scm_vm_box_eframe(vm, SCM_VM(vm)->reg.efp,
                           (size_t)nr_env, SCM_CSETTER_L(env));
  if (rslt < 0) return;

  clsr = scm_capi_make_closure(iseq, env, arity);
  if (scm_obj_null_p(clsr)) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], clsr);
  SCM_VM(vm)->reg.vc = 1;
}

scm_local_func void
scm_vm_op_demine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val;
  int idx, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &idx, &layer);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (idx < 0 || layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  val = scm_vm_eframe_arg_ref(SCM_VM(vm)->reg.efp,
                              (size_t)idx, (size_t)layer, NULL);
  if (scm_obj_null_p(val)) return;

  if (!scm_obj_type_p(val, &SCM_BOX_TYPE_INFO)) {
    scm_capi_error("update to variable bound by unboxed object", 0);
    return;
  }

  scm_box_update(val, SCM_VM(vm)->reg.val[0]);
}

scm_local_func void
scm_vm_op_emine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj box = SCM_OBJ_INIT;
  int len, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &box);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &len);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (len < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  rslt = scm_vm_do_op_eframe(vm, op);
  if (rslt < 0) return;

  SCM_VM(vm)->reg.vc = 1;
  for (int i = 0; i < len; i++) {
    box = scm_box_new(SCM_MEM_HEAP, SCM_VM(vm)->cnsts.landmine);
    if (scm_obj_null_p(box)) return;

    SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], box);
    rslt = scm_vm_do_op_push(vm, op);
    if (rslt < 0) return;
  }

  scm_vm_do_op_ecommit(vm, op, (size_t)len);
}

scm_local_func void
scm_vm_op_edemine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val;
  ScmEnvFrame *efp, *pefp;
  size_t n;
  int argc, layer;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_si(SCM_VM(vm)->reg.ip, &argc, &layer);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  if (layer < 0) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return;
  }

  pefp = SCM_VM(vm)->reg.pefp;
  if (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_PEF)) {
    scm_capi_error("invalid operation of VM stack: "
                   "partial environment frame is not pushed", 0);
    return;
  }

  efp = scm_vm_eframe_list_ref(SCM_VM(vm)->reg.efp, (size_t)layer);
  if (efp == NULL) return;

  /* XXX: 以降の処理で GC が発生すると efp の値が不正なになる */

  n = ((size_t) argc < efp->len) ? (size_t)argc : efp->len;
  for (size_t i = 0; i < n; i++) {
    if (!scm_obj_type_p(efp->arg[i], &SCM_BOX_TYPE_INFO)) {
      scm_capi_error("update to variable bound by unboxed object", 0);
      return;
    }

    scm_box_update(efp->arg[i], pefp->arg[i]);
  }

  scm_vm_cancel_eframe(vm);
}

scm_local_func void
scm_vm_op_arity(ScmObj vm, SCM_OPCODE_T op)
{
  int arity, rslt;
  scm_byte_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &arity);
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  rslt = scm_vm_cmp_arity(SCM_VM(vm)->reg.vc, arity, false);
  if (rslt != 0) {
    switch (rslt) {
    case 1:
      scm_capi_error("too many return values", 0);
      break;
    case -1:
      scm_capi_error("too few return values", 0);
      break;
    case -2:
      scm_assert(false);        /* must not happen */
      break;
    }
  }
}

void
scm_vm_initialize(ScmObj vm,  ScmBedrock *bedrock)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(bedrock != NULL);

  SCM_VM(vm)->bedrock = bedrock;

  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  /* TODO: undefined オブジェクトみたいなものを初期値にする */
  SCM_VM(vm)->reg.vc = 0;
  SCM_VM(vm)->reg.flags = SCM_OBJ_NULL;

  rslt = scm_vm_setup_singletons(vm);
  if (rslt < 0) return;

  rslt = scm_vm_setup_global_env(vm);
  if (rslt < 0) return;

  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_beadrock_bind_vm(SCM_VM(vm)->bedrock, SCM_OBJ_NULL);

  scm_vm_clean_global_env(vm);
  scm_vm_clean_singletons(vm);

  SCM_VM(vm)->stack = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.vc = 0;
}

ScmObj
scm_vm_new(void)
{
  ScmBedrock *bedrock = NULL;
  ScmMem *mem = NULL;
  ScmObj vm = SCM_OBJ_INIT;

  bedrock = scm_bedrock_new();
  if (bedrock == NULL) goto err;

  scm_bedrock__current_br = bedrock;

  mem = scm_mem_new();
  if (mem == NULL) goto err;

  scm_vm__current_mm = mem;

  vm = scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO, 0);
  if (scm_obj_null_p(vm)) goto err;

  scm_vm_initialize(vm, bedrock);

  scm_beadrock_bind_vm(bedrock, vm);

  scm_vm__current_mm = NULL;
  scm_bedrock__current_br = NULL;

  return vm;

 err:
  if (mem != NULL) scm_mem_end(mem);
  if (bedrock != NULL) scm_bedrock_end(bedrock);
  scm_vm__current_mm = NULL;
  scm_bedrock__current_br = NULL;
  return SCM_OBJ_NULL;
}

void
scm_vm_end(ScmObj vm)
{
  ScmBedrock *br;
  ScmMem *mem;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  br = SCM_VM(vm)->bedrock;
  mem = SCM_VM(vm)->mem;

  scm_vm_clean_eval_env(vm);
  scm_bedrock_clean(br);
  scm_mem_gc_start(mem);

  scm_mem_free_root(mem, vm);
  scm_mem_end(mem);
  scm_bedrock_end(br);
}

int
scm_vm_setup_system(ScmObj vm)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_init_eval_env(vm);
  if (rslt < 0) return -1;       /* [ERR]: [through] */

  rslt = scm_vm_load_builtin_modules(vm);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  int op;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  scm_vmsr_clear(SCM_VM(vm)->stack);

  SCM_VM(vm)->reg.sp = scm_vmsr_base(SCM_VM(vm)->stack);
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.pefp = NULL;
  SCM_VM(vm)->reg.flags = 0;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val[0], SCM_VM(vm)->cnsts.undef);
  SCM_VM(vm)->reg.vc = 1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp,
                scm_capi_make_closure(iseq, SCM_OBJ_NULL, 0));
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);

  while (!scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_HALT)) {
    if (scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_RAISE)) {
      int rslt;
      scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);
      rslt = scm_vm_setup_to_call_exception_handler(vm);
      if (rslt < 0 && scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_RAISE)) {
        scm_capi_fatal("can not handle exception");
        break;
      }
    }

    SCM_CAPI_INST_FETCH_OP(SCM_VM(vm)->reg.ip, op);

    switch(op) {
    case SCM_OPCODE_NOP:
      /* nothing to do */
      break;
    case SCM_OPCODE_HALT:
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
      break;
    case SCM_OPCODE_UNDEF:
      scm_vm_op_undef(vm, op);
      break;
    case SCM_OPCODE_CALL:       /* fall through */
    case SCM_OPCODE_TAIL_CALL:
      scm_vm_op_call(vm, op);
      break;
    case SCM_OPCODE_APPLY:      /* fall through */
    case SCM_OPCODE_TAIL_APPLY:
      scm_vm_op_apply(vm, op);
      break;
    case SCM_OPCODE_RETURN:
      scm_vm_op_return(vm, op);
      break;
    case SCM_OPCODE_FRAME:
      scm_vm_op_frame(vm, op);
      break;
    case SCM_OPCODE_CFRAME:
      scm_vm_op_cframe(vm, op);
      break;
    case SCM_OPCODE_EFRAME:
      scm_vm_op_eframe(vm, op);
      break;
    case SCM_OPCODE_ECOMMIT:
      scm_vm_op_ecommit(vm, op);
      break;
    case SCM_OPCODE_EPOP:
      scm_vm_op_epop(vm, op);
      break;
    case SCM_OPCODE_EREBIND:
      scm_vm_op_erebind(vm, op);
      break;
    case SCM_OPCODE_IMMVAL:
      scm_vm_op_immval(vm, op);
      break;
    case SCM_OPCODE_PUSH:
      scm_vm_op_push(vm, op);
      break;
    case SCM_OPCODE_MVPUSH:
      scm_vm_op_mvpush(vm, op);
      break;
    case SCM_OPCODE_GREF:
      scm_vm_op_gref(vm, op);
      break;
    case SCM_OPCODE_GDEF:
      scm_vm_op_gdef(vm, op);
      break;
    case SCM_OPCODE_GSET:
      scm_vm_op_gset(vm, op);
      break;
    case SCM_OPCODE_SREF:
      scm_vm_op_sref(vm, op);
      break;
    case SCM_OPCODE_SSET:
      scm_vm_op_sset(vm, op);
      break;
    case SCM_OPCODE_JMP:
      scm_vm_op_jmp(vm, op);
      break;
    case SCM_OPCODE_JMPT:
      scm_vm_op_jmpt(vm, op);
      break;
    case SCM_OPCODE_JMPF:
      scm_vm_op_jmpf(vm, op);
      break;
    case SCM_OPCODE_RAISE:
      scm_vm_op_raise(vm, op);
      break;
    case SCM_OPCODE_BOX:
      scm_vm_op_box(vm, op);
      break;
    case SCM_OPCODE_CLOSE:
      scm_vm_op_close(vm, op);
      break;
    case SCM_OPCODE_DEMINE:
      scm_vm_op_demine(vm, op);
      break;
    case SCM_OPCODE_EMINE:
      scm_vm_op_emine(vm, op);
      break;
    case SCM_OPCODE_EDEMINE:
      scm_vm_op_edemine(vm, op);
      break;
    case SCM_OPCODE_ARITY:
      scm_vm_op_arity(vm, op);
      break;
    default:
      /* TODO: error handling */
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
      break;
    }
  }
}

int
scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc)
{
  ScmObj vec = SCM_OBJ_INIT, rslt = SCM_OBJ_INIT;
  int n, rest;

  SCM_STACK_FRAME_PUSH(&vm,
                       &vec, &rslt);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(vc == 0 || val != NULL);
  scm_assert(vc >= 0);

  n = (vc <= SCM_VM_NR_VAL_REG) ? vc : SCM_VM_NR_VAL_REG - 1;
  for (int i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmVM, vm, reg.val[i], val[i]);

  rest = vc - (SCM_VM_NR_VAL_REG - 1);
  if (rest > 1) {
    vec = scm_capi_make_vector((size_t)rest, SCM_OBJ_NULL);
    if (scm_obj_null_p(vec)) return -1;

    for (int i = 0; i < rest; i++) {
      rslt = scm_capi_vector_set(vec, (size_t)i,
                                 val[SCM_VM_NR_VAL_REG - 1 + i]);
      if (scm_obj_null_p(rslt)) return -1;
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

  scm_contcap_cap(cc, stack,
                  SCM_VM(vm)->reg.cfp, SCM_VM(vm)->reg.efp,
                  SCM_VM(vm)->reg.pefp, SCM_VM(vm)->reg.cp,
                  SCM_VM(vm)->reg.ip, SCM_VM(vm)->reg.val,
                  SCM_VM(vm)->reg.vc, SCM_VM(vm)->reg.flags);

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

  SCM_VM(vm)->reg.cfp = scm_contcap_cfp(cc);
  SCM_VM(vm)->reg.efp = scm_contcap_efp(cc);
  SCM_VM(vm)->reg.pefp = scm_contcap_pefp(cc);
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_contcap_cp(cc));
  SCM_VM(vm)->reg.ip = scm_contcap_ip(cc);

  SCM_VM(vm)->reg.flags = scm_contcap_flags(cc);

  return 0;
}

int
scm_vm_setup_stat_trmp(ScmObj vm, ScmObj target, ScmObj args,
                       ScmSubrFunc callback)
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT;
  ScmObj cb_subr = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  scm_byte_t *ip;
  int rslt;

  SCM_STACK_FRAME_PUSH(&target, &args,
                       &trmp_code, &trmp_clsr,
                       &cb_subr, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_closure_p(target));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));

  if (callback != NULL) {
    cb_subr = scm_capi_make_subrutine(callback, 1, 0);
    if (scm_obj_null_p(target)) return -1; /* [ERR]: [through] */
  }
  else {
    cb_subr = SCM_OBJ_NULL;
  }

  trmp_code = scm_vm_make_trampolining_code(vm, target, args, cb_subr);
  if (scm_obj_null_p(trmp_code)) return -1; /* [ERR]: [through] */

  env = SCM_OBJ_NULL;
  if (scm_capi_closure_p(SCM_VM(vm)->reg.cp)) {
    rslt = scm_capi_closure_env(SCM_VM(vm)->reg.cp, SCM_CSETTER_L(env));
    if (rslt < 0) return -1;
  }

  trmp_clsr = scm_capi_make_closure(trmp_code, env, 0);
  if (scm_obj_null_p(trmp_clsr)) return -1; /* [ERR]: [through] */


  ip = scm_capi_iseq_to_ip(trmp_code);
  if (ip == NULL) return -1;

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            trmp_clsr);
  if (rslt < 0) return -1;

  rslt = scm_vm_commit_cframe(vm, ip);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_vm_setup_stat_halt(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
}

int
scm_vm_setup_stat_raised(ScmObj vm, ScmObj obj)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmVM, vm, ge.excpt.raised, obj);
  scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_RAISE);

  return 0;
}

int
scm_vm_clear_stat_raised(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;
  scm_vm_ctrl_flg_clr(vm, SCM_VM_CTRL_FLG_RAISE);

  return 0;
}

bool
scm_vm_raised_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return scm_vm_ctrl_flg_set_p(vm, SCM_VM_CTRL_FLG_RAISE);
}

int
scm_vm_push_exception_handler(ScmObj vm, ScmObj hndlr)
{
  ScmObj lst = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &hndlr, &lst);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_subrutine_p(hndlr)
             || scm_capi_closure_p(hndlr));

  lst = scm_api_cons(hndlr, SCM_VM(vm)->ge.excpt.hndlr);
  if (scm_obj_null_p(lst)) return -1; /* [ERR]: [thorugh] */

  SCM_SLOT_SETQ(ScmVM, vm, ge.excpt.hndlr, lst);

  return 0;
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_VM(obj)->mem = SCM_MEM(mem);

  SCM_VM(obj)->ge.symtbl = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.err = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.curio.out = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.excpt.raised = SCM_OBJ_NULL;
  SCM_VM(obj)->stack = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.sp = NULL;
  SCM_VM(obj)->reg.cfp = NULL;
  SCM_VM(obj)->reg.efp = NULL;
  SCM_VM(obj)->reg.pefp = NULL;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.vc = 0;
  SCM_VM(obj)->reg.flags = 0;
  SCM_VM(obj)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_false = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.undef = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.landmine = SCM_OBJ_NULL;
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

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->ge.stdio.in, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->ge.stdio.out, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->ge.stdio.err, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->ge.curio.in, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->ge.curio.out, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_VM(obj)->ge.excpt.hndlr, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                 SCM_VM(obj)->ge.excpt.raised, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  n = ((SCM_VM(obj)->reg.vc <= SCM_VM_NR_VAL_REG) ?
       SCM_VM(obj)->reg.vc : SCM_VM_NR_VAL_REG);
  for (int i = 0; i < n; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.cp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_stack(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_ref_stack_gc_accept(SCM_VM(obj)->bedrock->ref_stack,
                                 obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
