#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <stdio.h>
#include <assert.h>

#include "vm.h"
#include "memory.h"
#include "reference.h"
#include "object.h"
#include "string.h"
#include "symbol.h"
#include "gloc.h"
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


/***************************************************************************/
/*  ScmEnvFrameBox                                                         */
/***************************************************************************/

ScmTypeInfo SCM_EFBOX_TYPE_INFO = {
  .name = "efbox",
  .flags = SCM_TYPE_FLG_MMO,
  .pp_func = NULL,
  .obj_size = sizeof(ScmEFBox),
  .gc_ini_func = scm_efbox_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_efbox_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

scm_local_inline ScmObj
scm_efbox_efp_to_efbox(ScmEnvFrame *efp)
{
  if (efp == NULL)
    return SCM_OBJ_NULL;
  else
    return SCM_OBJ((uint8_t *)efp - offsetof(ScmEFBox, frame));
}

int
scm_efbox_initialize(ScmObj efb, ScmEnvFrame *ef)
{
  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert(!scm_vm_eframe_is_in_heap_p(scm_vm_current_vm(), ef));

  memcpy(&SCM_EFBOX(efb)->frame, ef, sizeof(*ef) + sizeof(ScmObj) * ef->len);

  if (!scm_vm_eframe_is_in_heap_p(scm_vm_current_vm(), ef->out))
    SCM_EFBOX(efb)->frame.out = NULL;

  return 0;
}

ScmObj
scm_efbox_new(SCM_MEM_TYPE_T mtype, ScmEnvFrame *ef)
{
  ScmObj efb = SCM_OBJ_INIT;
  int rslt;

  scm_assert(!scm_vm_eframe_is_in_heap_p(scm_vm_current_vm(), ef));

  efb = scm_capi_mem_alloc(&SCM_EFBOX_TYPE_INFO,
                           sizeof(ScmObj) * ef->len, mtype);
  if (scm_obj_null_p(efb)) return SCM_OBJ_NULL;

  rslt = scm_efbox_initialize(efb, ef);
  if (rslt < 0) return SCM_OBJ_NULL;

  return efb;
}

void
scm_efbox_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);

  SCM_EFBOX(obj)->frame.out = NULL;
  SCM_EFBOX(obj)->frame.len = 0;
}

int
scm_efbox_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj outer = SCM_OBJ_INIT;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_EFBOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  /* XXX: EFBox 内の frame.out は必ずボックス化された frame を指す */
  /*      (VM stack 上の enrioment frame を指すことはない)         */
  outer = scm_efbox_efp_to_efbox(SCM_EFBOX(obj)->frame.out);
  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, outer, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  SCM_EFBOX(obj)->frame.out = scm_efbox_to_efp(outer);

  for (size_t i = 0; i < SCM_EFBOX(obj)->frame.len; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler,
                                   obj, SCM_EFBOX(obj)->frame.arg[i], mem);
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

  SCM_SLOT_SETQ(ScmVM,vm, ge.gloctbl, scm_gloctbl_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->ge.gloctbl))
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

  if (scm_obj_null_p(SCM_VM(vm)->ge.gloctbl))
    scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->ge.gloctbl);

  SCM_VM(vm)->ge.symtbl = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.gloctbl = SCM_OBJ_NULL;

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

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_STACK_FRAME_PUSH(&vm, &in, &out, &err);

  scm_symtbl_clean(SCM_VM(vm)->ge.symtbl);
  scm_gloctbl_clean(SCM_VM(vm)->ge.gloctbl);

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

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.icfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.iefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.flags = 0;

  return 0;
}

scm_local_func void
scm_vm_clean_eval_env(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_symtbl_clean(SCM_VM(vm)->ge.symtbl);
  scm_gloctbl_clean(SCM_VM(vm)->ge.gloctbl);

  SCM_VM(vm)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.stdio.err = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.curio.out = SCM_OBJ_NULL;

  SCM_VM(vm)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(vm)->ge.excpt.raised = SCM_OBJ_NULL;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.icfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.iefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.flags = 0;
}

scm_local_func int
scm_vm_stack_push(ScmObj vm, ScmObj elm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp + sizeof(ScmObj)
      > SCM_VM(vm)->stack + SCM_VM(vm)->stack_size) {
    scm_capi_fatal("VM stack overflow");
    return -1; /* stack overflow; TODO: handle stack overflow error  */
  }

  SCM_WB_SETQ(vm, *(ScmObj *)SCM_VM(vm)->reg.sp, elm);
  SCM_VM(vm)->reg.sp += sizeof(ScmObj);

  return 0;
}

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

scm_local_inline int
scm_vm_update_ief_len_if_needed(ScmObj vm)
{
  ssize_t n;

  if (!scm_vm_eframe_is_in_stack_p(vm, SCM_VM(vm)->reg.iefp))
    return 0;

  if ((void *)SCM_VM(vm)->reg.iefp <= (void *)SCM_VM(vm)->reg.icfp
      || (void *)SCM_VM(vm)->reg.iefp <= (void *)SCM_VM(vm)->reg.cfp)
    return 0;

  if (scm_vm_eframe_is_in_stack_p(vm, SCM_VM(vm)->reg.efp))
    if ((void *)SCM_VM(vm)->reg.iefp <= (void *)SCM_VM(vm)->reg.efp)
      return 0;


  n = SCM_VM(vm)->reg.sp - (uint8_t *)SCM_VM(vm)->reg.iefp;
  n -= (ssize_t)sizeof(ScmEnvFrame);
  n /= (ssize_t)sizeof(ScmObj);

  SCM_VM(vm)->reg.iefp->len = (size_t)n;

  return 0;
}

scm_local_func int
scm_vm_make_cframe(ScmObj vm, ScmEnvFrame *efp,
                   ScmObj cp, ScmObj isp, uint8_t *ip)
{
  ScmCntFrame *icfp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_update_ief_len_if_needed(vm);
  if (rslt < 0) return -1;

  icfp = SCM_VM(vm)->reg.icfp;
  SCM_VM(vm)->reg.icfp = (ScmCntFrame *)SCM_VM(vm)->reg.sp;

  SCM_VM(vm)->reg.icfp->cfp = icfp;
  SCM_VM(vm)->reg.icfp->efp = efp;
  SCM_SLOT_SETQ(ScmVM, vm, reg.icfp->cp, cp);
  SCM_SLOT_SETQ(ScmVM, vm, reg.icfp->isp, isp);
  SCM_VM(vm)->reg.icfp->ip = ip;

  SCM_VM(vm)->reg.sp += sizeof(ScmCntFrame);

  return 0;
}

scm_local_func int
scm_vm_commit_cframe(ScmObj vm, ScmCntFrame *cfp, uint8_t *ip)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.icfp == NULL) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete continuatin frame is not pushed", 0);
    return -1;
  }

  SCM_VM(vm)->reg.cfp = SCM_VM(vm)->reg.icfp;
  SCM_VM(vm)->reg.icfp = SCM_VM(vm)->reg.icfp->cfp;

  SCM_VM(vm)->reg.cfp->cfp = cfp;
  SCM_VM(vm)->reg.cfp->ip = ip;

  return 0;
}

scm_local_func int
scm_vm_make_eframe(ScmObj vm, size_t nr_arg)
{
  ScmEnvFrame *iefp;
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  rslt = scm_vm_update_ief_len_if_needed(vm);
  if (rslt < 0) return -1;

  iefp = SCM_VM(vm)->reg.iefp;
  SCM_VM(vm)->reg.iefp = (ScmEnvFrame *)SCM_VM(vm)->reg.sp;

  SCM_VM(vm)->reg.iefp->out = iefp;
  SCM_VM(vm)->reg.iefp->len = nr_arg;

  SCM_VM(vm)->reg.sp += sizeof(ScmEnvFrame);

  return 0;
}

scm_local_func int
scm_vm_commit_eframe(ScmObj vm, ScmEnvFrame *efp, size_t nr_arg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* TODO: use scm_vm_eframe_is_in_stack_p() */
  if (SCM_VM(vm)->reg.iefp == NULL) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete environment frame is not pushed", 0);
    return -1;
  }

  SCM_VM(vm)->reg.efp = SCM_VM(vm)->reg.iefp;
  SCM_VM(vm)->reg.iefp = SCM_VM(vm)->reg.iefp->out;

  SCM_VM(vm)->reg.efp->out = efp;
  SCM_VM(vm)->reg.efp->len = nr_arg;

  return 0;
}

scm_local_func int
scm_vm_cancel_eframe(ScmObj vm)
{
  ScmEnvFrame *iefp;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  iefp = SCM_VM(vm)->reg.iefp;

  if (!scm_vm_eframe_is_in_stack_p(vm, iefp)) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete environment frame is not pushed", 0);
    return -1;
  }

  if (scm_vm_eframe_is_in_stack_p(vm, SCM_VM(vm)->reg.efp)
      && SCM_VM(vm)->reg.efp > iefp) {
    scm_capi_error("invalid operation of VM stack: "
                   "enviromnet frame link will be broken", 0);
    return -1;
  }

  if ((uint8_t *)SCM_VM(vm)->reg.icfp > (uint8_t *)iefp
      || (uint8_t *)SCM_VM(vm)->reg.cfp > (uint8_t *)iefp) {
    scm_capi_error("invalid operation of VM stack: "
                   "(incomplete) continuation frame link will be broken", 0);
    return -1;
  }

  SCM_VM(vm)->reg.iefp = iefp->out;
  SCM_VM(vm)->reg.sp = (uint8_t *)iefp;

  return 0;
}

scm_local_func int
scm_vm_box_eframe(ScmObj vm, ScmEnvFrame *efp, size_t depth, scm_csetter_t *box)
{
  ScmObj efb = SCM_OBJ_INIT, prev = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&efb, &prev);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (depth == 0) {
    scm_csetter_setq(box, SCM_OBJ_NULL);
    return 0;
  };

  if (efp == NULL) {
    scm_capi_error("invalid access to envrionment frame: out of range", 0);
    return -1;
  }

  if (scm_vm_eframe_is_in_heap_p(vm, efp))
    efb = scm_efbox_efp_to_efbox(efp);
  else
    efb = scm_efbox_new(SCM_MEM_HEAP, efp);

  if (scm_obj_null_p(efb)) return -1;

  scm_csetter_setq(box, efb);

  prev = efb;
  efp = efp->out;
  for (size_t i = 1; i < depth; i++) {
    if (efp == NULL) {
      scm_capi_error("invalid access to envrionment frame: out of range", 0);
      return -1;
    }

    if (scm_vm_eframe_is_in_heap_p(vm, efp))
      return 0;

    efb = scm_efbox_new(SCM_MEM_HEAP, efp);
    if (scm_obj_null_p(efb)) return -1;

    scm_efbox_update_outer(prev, efb);

    prev = efb;
    efp = efp->out;
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
       i++, efp = efp->out)
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

scm_local_func void
scm_vm_return_to_caller(ScmObj vm)
{
  ScmCntFrame *cfp;

  cfp = SCM_VM(vm)->reg.cfp;

  if ((uint8_t *)SCM_VM(vm)->reg.iefp >= (uint8_t *)cfp
      || (uint8_t *)SCM_VM(vm)->reg.icfp >= (uint8_t *)cfp) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete stack frame link will be broken", 0);
    return;
  }

  SCM_VM(vm)->reg.cfp = cfp->cfp;
  SCM_VM(vm)->reg.efp = cfp->efp;
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, cfp->cp);
  SCM_SLOT_SETQ(ScmVM, vm, reg.isp, cfp->isp);
  SCM_VM(vm)->reg.ip = cfp->ip;

  SCM_VM(vm)->reg.sp = (uint8_t *)cfp;
}

scm_local_func ScmObj
scm_vm_make_trampolining_code(ScmObj vm, ScmObj clsr,
                              ScmObj args, ScmObj callback)
{
  ScmObj iseq = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t rslt;
  uint32_t argc;

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

  if (!scm_obj_null_p(callback)) {
    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_FRAME);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  for (cur = args, argc = 0;
       !scm_obj_null_p(cur) && !scm_capi_nil_p(cur);
       cur = scm_api_cdr(cur), argc++) {
    arg = scm_api_car(cur);
    if (scm_obj_null_p(arg)) return SCM_OBJ_NULL; /* [ERR: [through] */

    rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, arg);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  if (argc > INT32_MAX) {
    scm_capi_error("", 0);      /* TODO: error message */
    return SCM_OBJ_NULL;
  }

  if (scm_obj_null_p(cur)) return SCM_OBJ_NULL; /* [ERR: [through] */

  rslt = scm_capi_iseq_push_opfmt_obj(iseq, SCM_OPCODE_IMMVAL, clsr);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_obj_null_p(callback)) {
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_TAIL_CALL,
                                       (int32_t)argc);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else {
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_CALL, (int32_t)argc);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_TAIL_CALL,
                                       (int32_t)argc);
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

  clsr = scm_capi_make_closure(iseq, env);
  if (scm_obj_null_p(clsr)) return -1;

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, clsr);
  SCM_SLOT_SETQ(ScmVM, vm, reg.isp, iseq);
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);

  return 0;
}

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

scm_local_func int
scm_vm_do_op_call(ScmObj vm, SCM_OPCODE_T op, uint32_t argc, bool tail_p)
{
  ScmObj val = SCM_OBJ_INIT, efb = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &val, &efb);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(argc <= INT32_MAX);

  if (argc > 0) {
    rslt = scm_vm_commit_eframe(vm, NULL, argc);
    if (rslt < 0) return -1;
  }

  if (tail_p) {
    ScmEnvFrame *ef_dst = (ScmEnvFrame *)((uint8_t *)SCM_VM(vm)->reg.cfp
                                          + sizeof(ScmCntFrame));

    if (SCM_VM(vm)->reg.iefp >= ef_dst
        || (uint8_t *)SCM_VM(vm)->reg.icfp >= (uint8_t *)ef_dst) {
      scm_capi_error("invalid operation of VM stack: "
                     "incomplete stack frame link will be broken", 0);
      return -1;
    }

    if (argc > 0) {
      if (SCM_VM(vm)->reg.efp > ef_dst) {
        size_t ef_sz = sizeof(ScmEnvFrame) + sizeof(ScmObj) * argc;
        memmove(ef_dst, SCM_VM(vm)->reg.efp, ef_sz);
        SCM_VM(vm)->reg.efp = ef_dst;
        SCM_VM(vm)->reg.sp = (uint8_t *)ef_dst + ef_sz;
      }
    }
    else {
      SCM_VM(vm)->reg.sp = (uint8_t *)ef_dst;
    }
  }

  if (!tail_p) {
    rslt = scm_vm_commit_cframe(vm, SCM_VM(vm)->reg.cfp, SCM_VM(vm)->reg.ip);
    if (rslt < 0) return -1;
  }

  if (scm_capi_subrutine_p(SCM_VM(vm)->reg.val)) {
    SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
    SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
    SCM_VM(vm)->reg.ip = NULL;

    if (argc > 0)
      val = scm_api_call_subrutine(SCM_VM(vm)->reg.val,
                                   (int)argc, SCM_VM(vm)->reg.efp->arg);
    else
      val = scm_api_call_subrutine(SCM_VM(vm)->reg.val, 0, NULL);

    if (scm_obj_null_p(val)) return -1;               /* [ERR]: [through] */

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);

    scm_vm_return_to_caller(vm);
  }
  else if (scm_capi_closure_p(SCM_VM(vm)->reg.val)) {
    ScmEnvFrame *efp;

    rslt = scm_capi_closure_env(SCM_VM(vm)->reg.val, SCM_CSETTER_L(efb));
    if (rslt < 0) return -1;

    efp = scm_efbox_to_efp(efb);
    if (argc > 0)
      SCM_WB_EXP(vm, SCM_VM(vm)->reg.efp->out = efp);
    else
      SCM_WB_EXP(vm, SCM_VM(vm)->reg.efp = efp);

    SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->reg.val);
    SCM_SLOT_SETQ(ScmVM, vm, reg.isp,
                  scm_capi_closure_to_iseq(SCM_VM(vm)->reg.val));
    SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(SCM_VM(vm)->reg.isp);
  }
  else {
    scm_capi_error("object is not applicable", 1, SCM_VM(vm)->reg.val);
  }

  return 0;
}

scm_local_func int
scm_vm_do_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* TODO: check return value */
  scm_vm_stack_push(vm, SCM_VM(vm)->reg.val);

  return 0;
}

scm_local_func int
scm_vm_do_op_frame(ScmObj vm, SCM_OPCODE_T op)
{
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* instraction pointer は FRAME インストラクション段階ではダミー */
  /* 値(SCM_OBJ_NULL)をプッシュする。本当の値は CALL 時に設定する */
  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            SCM_VM(vm)->reg.cp,
                            SCM_VM(vm)->reg.isp,
                            NULL);
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
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_VM(vm)->cnsts.undef);
}

/* 関数呼出のためのインストラクション */
scm_local_func void
scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op)
{
  int32_t argc;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si(SCM_VM(vm)->reg.ip, &argc);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (argc < 0) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  SCM_VM(vm)->reg.ip = ip;
  scm_vm_do_op_call(vm, op, (uint32_t)argc, (op == SCM_OPCODE_TAIL_CALL));
}

scm_local_func void
scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  size_t immv_idx;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_VM(vm)->reg.isp,
                                      &immv_idx, SCM_CSETTER_L(val));
  if (ip == NULL) return;       /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip;
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
}

scm_local_func void
scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_push(vm, op);
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
                     SCM_VM(vm)->reg.cp,
                     SCM_VM(vm)->reg.isp,
                     NULL);
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
  int32_t argc;
  uint8_t *ip;

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
  ScmEnvFrame *efp;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  efp = SCM_VM(vm)->reg.efp;

  if (!scm_vm_eframe_is_in_stack_p(vm, efp)) {
    scm_capi_error("invalid operation of VM stack: "
                   "environment frame is not pushed", 0);
    return;
  }

  if ((uint8_t *)SCM_VM(vm)->reg.iefp >= (uint8_t *)efp
      || (uint8_t *)SCM_VM(vm)->reg.icfp >= (uint8_t *)efp) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete stack frame link will be broken", 0);
    return;
  }

  SCM_VM(vm)->reg.efp = efp->out;
  SCM_VM(vm)->reg.sp = (uint8_t *)efp;
}

scm_local_func void
scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op) /* GC OK */
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_return_to_caller(vm);
}

scm_local_func void
scm_vm_op_gref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_VM(vm)->reg.isp,
                                      &immv_idx, SCM_CSETTER_L(arg));
  if (ip == NULL) return;              /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->ge.gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0) return;             /* [ERR]: [through] */

    if (scm_obj_null_p(gloc)) {
      scm_capi_error("unbound variable", 1, arg);
      return;
    }

    rslt = scm_capi_iseq_set_obj(SCM_VM(vm)->reg.isp,
                                 immv_idx, gloc);
    if (rslt < 0) return;      /* [ERR]: [through] */

    val = scm_gloc_value(gloc);
    if (scm_obj_null_p(val)) {
      scm_capi_error("unbound variable", 1, arg);
      return;
    }

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    val = scm_gloc_value(arg);
    if (scm_obj_null_p(val)) {
      scm_capi_error("unbound variable", 1, scm_gloc_symbol(arg));
      return;
    }

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
  }
  else {
    scm_assert(0);
  }
}

scm_local_func void
scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_VM(vm)->reg.isp,
                                      &immv_idx, SCM_CSETTER_L(arg));
  if (ip == NULL) return;              /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    gloc = scm_gloctbl_bind(SCM_VM(vm)->ge.gloctbl, arg, SCM_VM(vm)->reg.val);
    if (scm_obj_null_p(gloc)) return;  /* [ERR]: [through] */

    rslt = scm_capi_iseq_set_obj(SCM_VM(vm)->reg.isp, immv_idx, gloc);
    if (rslt < 0) return;             /* [ERR]: [through] */
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, SCM_VM(vm)->reg.val);
  }
  else {
    scm_assert(0);
  }
}

scm_local_func void
scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;
  uint8_t *ip;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_obj(SCM_VM(vm)->reg.ip, SCM_VM(vm)->reg.isp,
                                      &immv_idx, SCM_CSETTER_L(arg));
  if (ip == NULL) return;              /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip;
  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->ge.gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0) return;      /* [ERR]: [through] */

    if (scm_obj_null_p(gloc)) {
      scm_capi_error("unbound variable", 1, arg);
      return;
    }

    rslt = scm_capi_iseq_set_obj(SCM_VM(vm)->reg.isp, immv_idx, gloc);
    if (rslt < 0) return;      /* [ERR]: [through] */

    scm_gloc_bind(gloc, SCM_VM(vm)->reg.val);
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, SCM_VM(vm)->reg.val);
  }
  else {
    scm_assert(0);
  }
}

scm_local_func void
scm_vm_op_sref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  int32_t idx, layer;
  uint8_t *ip;

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

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
}

scm_local_func void
scm_vm_op_sset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT, o = SCM_OBJ_INIT;
  int32_t idx, layer;
  uint8_t *ip;

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

  scm_box_update(val, SCM_VM(vm)->reg.val);
}

scm_local_func void
scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op)
{
  int32_t dst;
  uint8_t *ip;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  SCM_VM(vm)->reg.ip = ip + dst;
}

scm_local_func void
scm_vm_op_jmpt(ScmObj vm, SCM_OPCODE_T op)
{
  int32_t dst;
  uint8_t *ip;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (!scm_capi_false_object_p(SCM_VM(vm)->reg.val))
    SCM_VM(vm)->reg.ip = ip + dst;
  else
    SCM_VM(vm)->reg.ip = ip;
}

scm_local_func void
scm_vm_op_jmpf(ScmObj vm, SCM_OPCODE_T op)
{
  int32_t dst;
  uint8_t *ip;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_iof(SCM_VM(vm)->reg.ip, &dst);
  if (ip == NULL) return;       /* [ERR]: [through] */

  if (scm_capi_false_object_p(SCM_VM(vm)->reg.val))
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

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_VM(vm)->ge.excpt.raised);

  rslt = scm_vm_do_op_push(vm, op);
  if (rslt < 0) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, hndlr);
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
  int32_t idx, layer;
  uint8_t *ip;

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
  if ((uint8_t *)efp < SCM_VM(vm)->stack
      || (uint8_t *)SCM_VM(vm)->reg.sp <= (uint8_t *)efp) {
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
  size_t idx;
  int32_t nr_env;
  uint8_t *ip;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm,
                       &clsr, &iseq, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  ip = scm_capi_inst_fetch_oprand_si_obj(SCM_VM(vm)->reg.ip,
                                         SCM_VM(vm)->reg.isp,
                                         &nr_env, &idx, SCM_CSETTER_L(iseq));
  if (ip == NULL) return;

  SCM_VM(vm)->reg.ip = ip;

  if (nr_env < 0) {
    scm_capi_error("invalid access to VM Stack: out of range", 0);
    return;
  }

  rslt = scm_vm_box_eframe(vm, SCM_VM(vm)->reg.efp,
                           (size_t)nr_env, SCM_CSETTER_L(env));
  if (rslt < 0) return;

  clsr = scm_capi_make_closure(iseq, env);
  if (scm_obj_null_p(clsr)) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, clsr);
}

scm_local_func void
scm_vm_op_demine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val;
  int32_t idx, layer;
  uint8_t *ip;

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

  scm_box_update(val, SCM_VM(vm)->reg.val);
}

scm_local_func void
scm_vm_op_emine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj box = SCM_OBJ_INIT;
  int32_t len;
  uint8_t *ip;
  int rslt;

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

  /* TODO: use scm_vm_do_op_eframe */
  rslt = scm_vm_do_op_eframe(vm, op);
  if (rslt < 0) return;

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_VM(vm)->cnsts.landmine);

  for (int i = 0; i < len; i++) {
    box = scm_box_new(SCM_MEM_HEAP, SCM_VM(vm)->cnsts.landmine);
    if (scm_obj_null_p(box)) return;

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, box);
    rslt = scm_vm_do_op_push(vm, op);
    if (rslt < 0) return;
  }

  scm_vm_do_op_ecommit(vm, op, (size_t)len);
}

scm_local_func void
scm_vm_op_edemine(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val;
  ScmEnvFrame *efp, *iefp;
  size_t n;
  int32_t argc, layer;
  uint8_t *ip;

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

  iefp = SCM_VM(vm)->reg.iefp;
  if (!scm_vm_eframe_is_in_stack_p(vm, iefp)) {
    scm_capi_error("invalid operation of VM stack: "
                   "incomplete environment frame is not pushed", 0);
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

    scm_box_update(efp->arg[i], iefp->arg[i]);
  }

  scm_vm_cancel_eframe(vm);
}

void
scm_vm_initialize(ScmObj vm,  ScmBedrock *bedrock)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(bedrock != NULL);

  SCM_VM(vm)->bedrock = bedrock;

  SCM_VM(vm)->stack = scm_capi_malloc(SCM_VM_STACK_INIT_SIZE);
  if (SCM_VM(vm)->stack == NULL) goto err;

  SCM_VM(vm)->stack_size = SCM_VM_STACK_INIT_SIZE;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.icfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.iefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  /* TODO: undefined オブジェクトみたいなものを初期値にする */
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.flags = SCM_OBJ_NULL;

  rslt = scm_vm_setup_singletons(vm);
  if (rslt < 0) goto err;

  rslt = scm_vm_setup_global_env(vm);
  if (rslt < 0) goto err;

  return;

 err:
  if (SCM_VM(vm)->stack != NULL) {
    SCM_VM(vm)->stack = scm_capi_free(SCM_VM(vm)->stack);
    SCM_VM(vm)->stack_size = 0;
  }

  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_beadrock_bind_vm(SCM_VM(vm)->bedrock, SCM_OBJ_NULL);

  scm_vm_clean_global_env(vm);
  scm_vm_clean_singletons(vm);

  SCM_VM(vm)->stack = scm_capi_free(SCM_VM(vm)->stack);
  SCM_VM(vm)->stack_size = 0;
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.icfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.iefp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;
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

  scm_core_subr_system_setup();
  scm_core_syntx_system_setup();

  return 0;
}

void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  uint8_t op;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.cfp = NULL;
  SCM_VM(vm)->reg.icfp = NULL;
  SCM_VM(vm)->reg.efp = NULL;
  SCM_VM(vm)->reg.iefp = NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_capi_make_closure(iseq, SCM_OBJ_NULL));
  SCM_SLOT_SETQ(ScmVM, vm, reg.isp, iseq);
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_VM(vm)->cnsts.undef);
  SCM_VM(vm)->reg.flags = 0;

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
    case SCM_OPCODE_CALL:
      scm_vm_op_call(vm, op);
      break;
    case SCM_OPCODE_TAIL_CALL:
      scm_vm_op_call(vm, op);
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
    case SCM_OPCODE_IMMVAL:
      scm_vm_op_immval(vm, op);
      break;
    case SCM_OPCODE_PUSH:
      scm_vm_op_push(vm, op);
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
    default:
      /* TODO: error handling */
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
      break;
    }
  }
}

int
scm_vm_setup_stat_trmp(ScmObj vm, ScmObj target, ScmObj args,
                       ScmObj (*callback)(int argc, ScmObj *argv))
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT;
  ScmObj cb_subr = SCM_OBJ_INIT, env = SCM_OBJ_INIT;
  uint8_t *ip;
  int rslt;

  SCM_STACK_FRAME_PUSH(&target, &args,
                       &trmp_code, &trmp_clsr,
                       &cb_subr, &env);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_closure_p(target) || scm_capi_iseq_p(target));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));

  if (scm_capi_iseq_p(target)) {
    target = scm_capi_make_closure(target, SCM_OBJ_NULL);
    if (scm_obj_null_p(target)) return -1; /* [ERR]: [through] */
  }

  if (callback != NULL) {
    cb_subr = scm_capi_make_subrutine(callback);
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

  trmp_clsr = scm_capi_make_closure(trmp_code, env);
  if (scm_obj_null_p(trmp_clsr)) return -1; /* [ERR]: [through] */


  ip = scm_capi_iseq_to_ip(trmp_code);
  if (ip == NULL) return -1;

  rslt = scm_vm_make_cframe(vm,
                            SCM_VM(vm)->reg.efp,
                            trmp_clsr,
                            trmp_code,
                            ip);
  if (rslt < 0) return -1;

  rslt = scm_vm_commit_cframe(vm, SCM_VM(vm)->reg.cfp, ip);
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

bool
scm_vm_eframe_is_in_stack_p(ScmObj vm, ScmEnvFrame *efp)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (efp == NULL)
    return false;
  else if ((uint8_t *)efp < SCM_VM(vm)->stack
           || SCM_VM(vm)->reg.sp <= (uint8_t *)efp)
    return false;
  else
    return true;
}

extern inline bool
scm_vm_eframe_is_in_heap_p(ScmObj vm, ScmEnvFrame *ef)
{
  /* XXX: continuation を実装した場合、continuation が保持しているスタックを */
  /*      指している場合も false を返すようにする */
  return !scm_vm_eframe_is_in_stack_p(vm, ef);
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_VM(obj)->mem = SCM_MEM(mem);

  SCM_VM(obj)->ge.symtbl = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.gloctbl = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.in = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.out = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.stdio.err = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.curio.in = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.curio.out = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.excpt.hndlr = SCM_OBJ_NULL;
  SCM_VM(obj)->ge.excpt.raised = SCM_OBJ_NULL;
  SCM_VM(obj)->stack = NULL;
  SCM_VM(obj)->stack_size = 0;
  SCM_VM(obj)->reg.sp = NULL;
  SCM_VM(obj)->reg.cfp = NULL;
  SCM_VM(obj)->reg.efp = NULL;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.val = SCM_OBJ_NULL;
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
scm_vm_gc_accept_eframe(ScmObj vm, ScmEnvFrame **efp,
                        ScmObj mem, ScmGCRefHandlerFunc handler)
{
  ScmObj efb = SCM_OBJ_INIT;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(efp != NULL);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  while (scm_vm_eframe_is_in_stack_p(vm, *efp)) {
    for (size_t i = 0; i < (*efp)->len; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, (*efp)->arg[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }

    efp = &(*efp)->out;
  }

  if (scm_vm_eframe_is_in_heap_p(vm, *efp)) {
    efb = scm_efbox_efp_to_efbox(*efp);
    rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, efb, mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    *efp = scm_efbox_to_efp(efb);
  }

  return rslt;
}

scm_local_func int
scm_vm_gc_accept_cframe(ScmObj vm, ScmCntFrame *cfp,
                        ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  while (cfp != NULL) {
    rslt = scm_vm_gc_accept_eframe(vm, &cfp->efp, mem, handler);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, cfp->cp, mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, cfp->isp, mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

    cfp = cfp->cfp;
  }

  return rslt;
}

scm_local_func int
scm_vm_gc_accept_stack(ScmObj vm, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;
  uint8_t *top;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (SCM_VM(vm)->stack == NULL) return rslt;

  top = SCM_VM(vm)->stack;

  if ((uint8_t *)SCM_VM(vm)->reg.cfp >= top)
    top = (uint8_t *)SCM_VM(vm)->reg.cfp + sizeof(ScmCntFrame);

  if ((uint8_t *)SCM_VM(vm)->reg.icfp >= top)
    top = (uint8_t *)SCM_VM(vm)->reg.icfp + sizeof(ScmCntFrame);

  if ((uint8_t *)SCM_VM(vm)->reg.efp >= top)
    top = (uint8_t *)SCM_VM(vm)->reg.efp
      + sizeof(ScmEnvFrame) + sizeof(ScmObj) * SCM_VM(vm)->reg.efp->len;

  if ((uint8_t *)SCM_VM(vm)->reg.iefp >= top)
    top = (uint8_t *)SCM_VM(vm)->reg.iefp
      + sizeof(ScmEnvFrame) + sizeof(ScmObj) * SCM_VM(vm)->reg.iefp->len;

  for (ScmObj *p = (ScmObj *)top; p < (ScmObj *)SCM_VM(vm)->reg.sp; p++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, vm, *p, mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  rslt = scm_vm_gc_accept_cframe(vm, SCM_VM(vm)->reg.cfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_cframe(vm, SCM_VM(vm)->reg.icfp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_eframe(vm, &SCM_VM(vm)->reg.efp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_eframe(vm, &SCM_VM(vm)->reg.iefp, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

int
scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

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

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.isp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.cp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_vm_gc_accept_stack(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = scm_ref_stack_gc_accept(SCM_VM(obj)->bedrock->ref_stack,
                                 obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}
