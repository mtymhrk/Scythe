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

  box = scm_capi_mem_alloc(&SCM_BOX_TYPE_INFO, mtype);
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
/*  ScmVM                                                                  */
/***************************************************************************/

#define SCM_VM_STACK_INIT_SIZE 2048

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

  SCM_VM(vm)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_false = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.undef = SCM_OBJ_NULL;
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
  SCM_VM(vm)->reg.fp = NULL;
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
  SCM_VM(vm)->reg.fp = NULL;
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

  if (SCM_VM(vm)->reg.sp > SCM_VM(vm)->stack + SCM_VM(vm)->stack_size) {
    scm_capi_fatal("VM stack overflow");
    return -1; /* stack overflow; TODO: handle stack overflow error  */
  }

  SCM_SLOT_REF_SETQ(ScmVM, vm, reg.sp, elm);
  SCM_VM(vm)->reg.sp++;

  return 0;
}

scm_local_func ScmObj
scm_vm_stack_pop(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp == SCM_VM(vm)->stack) {
    scm_capi_fatal("VM stack underflow");
    /* stack underflow; TODO; handle stack underflow error */
    return SCM_OBJ_NULL;
  }

  SCM_VM(vm)->reg.sp--;

  return *SCM_VM(vm)->reg.sp;
}

scm_local_func void
scm_vm_stack_shorten(ScmObj vm, size_t n)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if ((size_t)(SCM_VM(vm)->reg.sp - SCM_VM(vm)->stack) < n)
    SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  else
    SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.sp - n;
}

scm_local_func int
scm_vm_stack_shift(ScmObj vm, size_t nelm, size_t nshift)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp - nelm - nshift < SCM_VM(vm)->stack) {
    scm_capi_fatal("VM stack underflow");
    return -1;
  }

  memmove(SCM_VM(vm)->reg.sp - nelm - nshift, SCM_VM(vm)->reg.sp - nelm, nelm);
  SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.sp - nshift;

  return 0;
}

scm_local_inline ScmObj *
scm_vm_cur_frame_argv(ScmObj vm, int argc)
{
  return SCM_VM(vm)->reg.fp - argc;
}

scm_local_func int
scm_vm_make_stack_frame(ScmObj vm, ScmObj fp, ScmObj cp, ScmObj isp, ScmObj ip)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* push frame pointer */
  rslt = scm_vm_stack_push(vm, fp);
  if (rslt < 0) return -1;

  /* push closure pointer */
  rslt = scm_vm_stack_push(vm, cp);
  if (rslt < 0) return -1;

  /* push ScmISeq object */
  rslt = scm_vm_stack_push(vm, isp);
  if (rslt < 0) return -1;

  /* push instraction pointer */
  rslt = scm_vm_stack_push(vm, ip);
  if (rslt < 0) return -1;

  return 0;
}

scm_local_func void
scm_vm_return_to_caller(ScmObj vm, uint32_t nr_arg)
{
  ScmObj fp_fn = SCM_OBJ_INIT, ip_fn = SCM_OBJ_INIT;
  ScmObj *fp;

  SCM_STACK_FRAME_PUSH(&vm, &fp_fn, &ip_fn);

  scm_assert(nr_arg <= INT32_MAX);

  fp = SCM_VM(vm)->reg.fp;

  fp_fn = fp[-((int32_t)nr_arg + 4)];
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_OBJ(fp[-((int32_t)nr_arg + 3)]));
  SCM_SLOT_SETQ(ScmVM, vm, reg.isp, SCM_OBJ(fp[-((int32_t)nr_arg + 2)]));
  ip_fn = fp[-((int32_t)nr_arg + 1)];

  SCM_VM(vm)->reg.fp = scm_capi_fixnum_to_frame_ptr(fp_fn);
  SCM_VM(vm)->reg.ip = scm_capi_fixnum_to_inst_ptr(ip_fn);

  scm_vm_stack_shorten(vm, nr_arg + 4); /* 4 :=  fp, cp, iseq, ip */
}

scm_local_func ScmObj
scm_vm_make_trampolining_code(ScmObj vm, ScmObj clsr,
                              ScmObj args, uint32_t nr_arg_cf, ScmObj callback)
{
  ScmObj iseq = SCM_OBJ_INIT, cur = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t rslt;
  uint32_t argc;

  SCM_STACK_FRAME_PUSH(&vm, &clsr, &args, &callback, &iseq, &cur, &arg);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_closure_p(clsr));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));
  scm_assert(nr_arg_cf <= INT32_MAX);
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
    rslt = scm_capi_iseq_push_opfmt_si_si(iseq, SCM_OPCODE_TAIL_CALL,
                                          (int32_t)argc, (int32_t)nr_arg_cf);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else {
    rslt = scm_capi_iseq_push_opfmt_si(iseq, SCM_OPCODE_CALL, (int32_t)argc);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_noarg(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_opfmt_si_si(iseq, SCM_OPCODE_TAIL_CALL,
                                          (int32_t)argc, (int32_t)nr_arg_cf);
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
  ScmObj iseq = SCM_OBJ_INIT, clsr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &iseq, &clsr);

  iseq = scm_vm_make_exception_handler_code(vm);
  if (scm_obj_null_p(iseq)) return -1;

  clsr = scm_capi_iseq_to_closure(iseq);
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
scm_vm_do_op_call(ScmObj vm, SCM_OPCODE_T op,
                  uint32_t nr_arg, uint32_t nr_arg_cf, bool tail_p)
{
  ScmObj val = SCM_OBJ_INIT, ip_fn = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &val, &ip_fn);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(nr_arg <= INT32_MAX);
  scm_assert(nr_arg <= INT32_MAX);

  if (tail_p) {
    rslt = scm_vm_stack_shift(vm, nr_arg, nr_arg_cf);
    if (rslt < 0) return -1;
  }

  SCM_VM(vm)->reg.fp = SCM_VM(vm)->reg.sp;

  if (!tail_p) {
    /* FRAME インストラクションでダミー値を設定していたものを実際の値に変更
       する */
    ip_fn = scm_capi_inst_ptr_to_fixnum(SCM_VM(vm)->reg.ip);
    SCM_SLOT_SETQ(ScmVM, vm,
                  reg.fp[-((int32_t)nr_arg + 1)], ip_fn);
  }

  if (scm_capi_subrutine_p(SCM_VM(vm)->reg.val)) {
    val = scm_api_call_subrutine(SCM_VM(vm)->reg.val,
                                 (int)nr_arg,
                                 scm_vm_cur_frame_argv(vm, (int)nr_arg));
    if (scm_obj_null_p(val)) return -1;               /* [ERR]: [through] */

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);

    scm_vm_return_to_caller(vm, nr_arg);
  }
  else if (scm_capi_closure_p(SCM_VM(vm)->reg.val)) {
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

  scm_vm_stack_push(vm, SCM_VM(vm)->reg.val);

  return 0;
}

scm_local_func int
scm_vm_do_op_frame(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj fp_fn = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  fp_fn = scm_capi_frame_ptr_to_fixnum(SCM_VM(vm)->reg.fp);

  scm_vm_make_stack_frame(vm,
                          fp_fn,
                          SCM_VM(vm)->reg.cp,
                          SCM_VM(vm)->reg.isp,
                          SCM_OBJ_NULL);
  /* instraction pointer は FRAME インストラクション段階ではダミー */
  /* 値(SCM_OBJ_NULL)をプッシュする。本当の値は CALL 時に設定する */

  return 0;
}

/* 関数呼出のためのインストラクション */
scm_local_func void
scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op)
{
  uint32_t nr_arg, nr_arg_cf;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg);
  if (nr_arg > INT32_MAX) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  nr_arg_cf = 0;
  if (op == SCM_OPCODE_TAIL_CALL) {
    SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg_cf);
    if (nr_arg_cf > INT32_MAX) {
      scm_capi_error("bytecode format error", 0);
      return;
    }
  }

  scm_vm_do_op_call(vm, op, nr_arg, nr_arg_cf, (op == SCM_OPCODE_TAIL_CALL));

  return;
}

scm_local_func void
scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj val = SCM_OBJ_INIT;
  size_t immv_idx;

  SCM_STACK_FRAME_PUSH(&vm, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);

  val = scm_capi_iseq_ref_obj(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(val)) return; /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
}

scm_local_func void
scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_push(vm, op);
}


/* 関数呼出のためのスタックフレームを作成するインストラクション。
 * フレームポインタとインストラクションポインタをスタックにプッシュする。
 * このインストラクションの後、引数と引数の数をプッシュする必要がある。
 */
scm_local_func void
scm_vm_op_frame(ScmObj vm, SCM_OPCODE_T op) /* GC OK */
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_do_op_frame(vm, op);
}

/* 関数の呼び出しから戻るインストラクション。
 */
scm_local_func void
scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op) /* GC OK */
{
  uint32_t nr_arg;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg);
  if (nr_arg > INT32_MAX) {
    scm_capi_error("bytecode format error", 0);
    return;
  }

  scm_vm_return_to_caller(vm, nr_arg);
}

/* グローバル変数を参照するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し、その GLoc からシンボ
 * ルを束縛している値を得て、その値を val レジスタに設定する。またインストラク
 * ションの Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その Gloc からシンボルを束縛している値を得て、そ
 * の値を val レジスタに設定する。
 */
scm_local_func void
scm_vm_op_gref(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);

  arg = scm_capi_iseq_ref_obj(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg)) return; /* [ERR]: [through] */

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

/* グローバル変数を作成するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し(検索の結果存在しない
 * 場合は GLoc を作成し)、その GLoc を使用してシンボルを val レジスタの値で束
 * 縛する。またインストラクションの Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その GLoc を使用してシンボルを val レジスタの値で
 * 束縛する。
 */
scm_local_func void
scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);

  arg = scm_capi_iseq_ref_obj(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg)) return; /* [ERR]: [through] */

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

/* グローバル変数を更新するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し、その GLoc を使用して
 * グローバル変数の値を val レジスタで更新する。またインストラクションの
 * Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その Gloc その GLoc を使用してグローバル変数の値
 * を val レジスタで更新する。
 */
scm_local_func void
scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  size_t immv_idx;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);

  arg = scm_capi_iseq_ref_obj(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg)) return; /* [ERR]: [through] */

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

/* 無条件 JUMP 命令
 */
scm_local_func void
scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op)
{
  int32_t dst;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_CAPI_INST_FETCH_INT32(SCM_VM(vm)->reg.ip, dst);
  SCM_VM(vm)->reg.ip += dst;
}


/* exception handler リストにある先頭の handler を val レジスタに設定し、
 * exception handler リストから先頭の handler を取り除く
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

  rslt = scm_vm_do_op_call(vm, op, 1, 0, false);
  if (rslt < 0) return;

  scm_capi_raise(SCM_VM(vm)->ge.excpt.raised);
}

void
scm_vm_initialize(ScmObj vm,  ScmBedrock *bedrock)
{
  int rslt;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(bedrock != NULL);

  SCM_VM(vm)->bedrock = bedrock;

  SCM_VM(vm)->stack = scm_capi_malloc(sizeof(ScmObj) * SCM_VM_STACK_INIT_SIZE);
  if (SCM_VM(vm)->stack == NULL) goto err;

  SCM_VM(vm)->stack_size = SCM_VM_STACK_INIT_SIZE;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.fp = NULL;
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
  SCM_VM(vm)->reg.fp = NULL;
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

  vm = scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO);
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
  SCM_VM(vm)->reg.fp = NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_capi_iseq_to_closure(iseq));
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
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
      break;
    case SCM_OPCODE_SSET:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
      break;
    case SCM_OPCODE_CREF:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
      break;
    case SCM_OPCODE_CSET:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
      break;
    case SCM_OPCODE_JMP:
      scm_vm_op_jmp(vm, op);
      break;
    case SCM_OPCODE_JMPF:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
      break;
    case SCM_OPCODE_RAISE:
      scm_vm_op_raise(vm, op);
      break;
    case SCM_OPCODE_BOX:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
    case SCM_OPCODE_UNBOX:
      /* TODO: write me */
      scm_capi_error("not impremented opcode", 0);
    default:
      /* TODO: error handling */
      scm_vm_ctrl_flg_set(vm, SCM_VM_CTRL_FLG_HALT);
      break;
    }
  }
}

int
scm_vm_setup_stat_trmp(ScmObj vm, ScmObj target,
                       ScmObj args, int nr_arg_cf,
                       ScmObj (*callback)(int argc, ScmObj *argv))
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT;
  ScmObj cb_subr = SCM_OBJ_INIT, fp_fn = SCM_OBJ_INIT, ip_fn = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&target, &args,
                       &trmp_code, &trmp_clsr, &cb_subr, &fp_fn, &ip_fn);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_closure_p(target) || scm_capi_iseq_p(target));
  scm_assert(scm_capi_nil_p(args) || scm_capi_pair_p(args));
  scm_assert(0 <= nr_arg_cf && nr_arg_cf <= INT32_MAX);

  if (scm_capi_iseq_p(target)) {
    target = scm_capi_iseq_to_closure(target);
    if (scm_obj_null_p(target)) return -1; /* [ERR]: [through] */
  }

  if (callback != NULL) {
    cb_subr = scm_capi_make_subrutine(callback);
    if (scm_obj_null_p(target)) return -1; /* [ERR]: [through] */
  }
  else {
    cb_subr = SCM_OBJ_NULL;
  }

  trmp_code = scm_vm_make_trampolining_code(vm, target,
                                            args, (uint32_t)nr_arg_cf, cb_subr);
  if (scm_obj_null_p(trmp_code)) return -1; /* [ERR]: [through] */

  trmp_clsr = scm_capi_iseq_to_closure(trmp_code);
  if (scm_obj_null_p(trmp_clsr)) return -1; /* [ERR]: [through] */

  fp_fn = scm_capi_frame_ptr_to_fixnum(SCM_VM(vm)->reg.fp);
  ip_fn = scm_capi_inst_ptr_to_fixnum(scm_capi_iseq_to_ip(trmp_code));

  rslt = scm_vm_make_stack_frame(vm,
                                 fp_fn,
                                 trmp_clsr,
                                 trmp_code,
                                 ip_fn);
  if (rslt < 0) return -1;

  for (int i = 0; i < nr_arg_cf; i++) {
    rslt = scm_vm_stack_push(vm, SCM_OBJ_NULL);
    if (rslt < 0) return -1;
  }

  SCM_VM(vm)->reg.fp = SCM_VM(vm)->reg.sp;

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
  SCM_VM(obj)->reg.fp = NULL;
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
}

void
scm_vm_gc_finalize(ScmObj obj)
{
  scm_vm_finalize(obj);
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

  if (SCM_VM(obj)->stack != NULL) {
    for (ScmObj *p = SCM_VM(obj)->stack;
         p != SCM_VM(obj)->reg.sp;
         p++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *p, mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  rslt = scm_ref_stack_gc_accept(SCM_VM(obj)->bedrock->ref_stack,
                                 obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

