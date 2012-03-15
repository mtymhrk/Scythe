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
/*  ScmVM                                                                  */
/***************************************************************************/

#define SCM_VM_STACK_INIT_SIZE 1024
#define SCM_VM_STACK_MAX_SIZE 10240

#define SCM_VM_SYMTBL_SIZE 256


ScmTypeInfo SCM_VM_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

ScmObj scm_vm__current_vm = SCM_OBJ_INIT;


scm_local_func void
scm_vm_setup_singletons(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, symtbl, scm_symtbl_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->symtbl))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM,vm, gloctbl, scm_gloctbl_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->gloctbl))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.nil, scm_nil_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.nil))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.eof, scm_eof_new(SCM_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.eof))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_true,
                scm_bool_new(SCM_MEM_ROOT, true));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_true))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_false,
                scm_bool_new(SCM_MEM_ROOT, false));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_false))
    ;                           /* TODO: error handling */

}

scm_local_func void
scm_vm_clean_singletons(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->symtbl);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->gloctbl);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.nil);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.eof);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_true);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_false);

  SCM_VM(vm)->symtbl = SCM_OBJ_NULL;
  SCM_VM(vm)->gloctbl = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(vm)->cnsts.b_false = SCM_OBJ_NULL;
}

scm_local_func void
scm_vm_clean_eval_env(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_symtbl_clean(SCM_VM(vm)->symtbl);
  scm_gloctbl_clean(SCM_VM(vm)->gloctbl);

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.fp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;
}

scm_local_func void
scm_vm_stack_push(ScmObj vm, ScmObj elm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp > SCM_VM(vm)->stack + SCM_VM(vm)->stack_size)
    return; /* stack overflow; TODO: handle stack overflow error  */

  SCM_SLOT_REF_SETQ(ScmVM, vm, reg.sp, elm);
  SCM_VM(vm)->reg.sp++;
}

scm_local_func ScmObj
scm_vm_stack_pop(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp == SCM_VM(vm)->stack)
    /* stack underflow; TODO; handle stack underflow error */
    return SCM_OBJ_NULL;

  SCM_VM(vm)->reg.sp--;

  return *SCM_VM(vm)->reg.sp;
}

scm_local_func void
scm_vm_stack_shorten(ScmObj vm, size_t n)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if ((size_t)(SCM_VM(vm)->reg.sp - SCM_VM(vm)->stack) < n)
    /* stack underflow; TODO; handle stack underflow error */
    return;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.sp - n;
}

scm_local_func void
scm_vm_stack_shift(ScmObj vm, size_t nelm, size_t nshift)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  memmove(SCM_VM(vm)->reg.sp - nelm - nshift, SCM_VM(vm)->reg.sp - nelm, nelm);
  SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.sp - nshift;
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

  scm_vm_stack_shorten(vm, nr_arg + 3); /* 3 :=  fp, iseq, ip */
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
    rslt = scm_capi_iseq_push_op(iseq, SCM_OPCODE_FRAME);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  for (cur = args, argc = 0;
       !scm_obj_null_p(cur) && !scm_capi_nil_p(cur);
       cur = scm_api_cdr(cur), argc++) {
    arg = scm_api_car(cur);
    if (scm_obj_null_p(arg)) return SCM_OBJ_NULL; /* [ERR: [through] */

    rslt = scm_capi_iseq_push_op_immval(iseq, SCM_OPCODE_IMMVAL, arg);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_op(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  if (scm_obj_null_p(cur)) return SCM_OBJ_NULL; /* [ERR: [through] */

  rslt = scm_capi_iseq_push_op_immval(iseq, SCM_OPCODE_IMMVAL, clsr);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_obj_null_p(callback)) {
    rslt = scm_capi_iseq_push_op_cval_cval(iseq, SCM_OPCODE_TAIL_CALL,
                                           argc, nr_arg_cf);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else {
    rslt = scm_capi_iseq_push_op_cval(iseq, SCM_OPCODE_CALL, argc);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_op(iseq, SCM_OPCODE_PUSH);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

    rslt = scm_capi_iseq_push_op_cval_cval(iseq, SCM_OPCODE_TAIL_CALL,
                                           argc, nr_arg_cf);
    if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }

  return iseq;
}

/* 関数呼出のためのインストラクション */
scm_local_func void
scm_vm_op_call(ScmObj vm, uint32_t nr_arg, uint32_t nr_arg_cf, bool tail_p)
{
  ScmObj val = SCM_OBJ_INIT, ip_fn = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &val, &ip_fn);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (nr_arg > INT32_MAX) return; /* [ERR]:  */
  if (nr_arg_cf > INT32_MAX) return; /* [ERR]:  */

  if (tail_p)
    scm_vm_stack_shift(vm, nr_arg, nr_arg_cf);

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
                                 (int)nr_arg, SCM_VM(vm)->reg.fp - nr_arg);
    if (scm_obj_not_null_p(val))
      SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);

    if (scm_obj_null_p(SCM_VM(vm)->trmp.code)) {
      scm_vm_return_to_caller(vm, nr_arg);
    }
    else {
      SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->trmp.code);
      SCM_SLOT_SETQ(ScmVM, vm, reg.isp,
                    scm_capi_closure_to_iseq(SCM_VM(vm)->reg.cp));
      SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(SCM_VM(vm)->reg.isp);
      SCM_VM(vm)->trmp.code = SCM_OBJ_NULL;
    }
  }
  else if (scm_capi_closure_p(SCM_VM(vm)->reg.val)) {
    SCM_SLOT_SETQ(ScmVM, vm, reg.cp, SCM_VM(vm)->trmp.code);
    SCM_SLOT_SETQ(ScmVM, vm, reg.isp,
                  scm_capi_closure_to_iseq(SCM_VM(vm)->reg.cp));
    SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(SCM_VM(vm)->reg.isp);
  }
  else {
    ;                           /* TODO: error handling */
  }
}

scm_local_func void
scm_vm_op_immval(ScmObj vm, size_t immv_idx)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  val = scm_capi_iseq_ref_immval(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(val))
    ;    /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
}

scm_local_func void
scm_vm_op_push(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, SCM_VM(vm)->reg.val);
}


/* 関数呼出のためのスタックフレームを作成するインストラクション。
 * フレームポインタとインストラクションポインタをスタックにプッシュする。
 * このインストラクションの後、引数と引数の数をプッシュする必要がある。
 */
scm_local_func void
scm_vm_op_frame(ScmObj vm) /* GC OK */
{
  ScmObj fp_fn = SCM_OBJ_INIT;

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  fp_fn = scm_capi_frame_ptr_to_fixnum(SCM_VM(vm)->reg.fp);

  /* push frame pointer */
  scm_vm_stack_push(vm, fp_fn);

  /* push closure pointer */
  scm_vm_stack_push(vm, SCM_VM(vm)->reg.cp);

  /* push ScmISeq object */
  scm_vm_stack_push(vm, SCM_VM(vm)->reg.isp);

  /* push instraction pointer (FRAME インストラクション段階ではダミー
     値をプッシュする。本当の値は CALL 時に設定する) */
  scm_vm_stack_push(vm, SCM_OBJ_NULL);
}

/* 関数の呼び出しから戻るインストラクション。
 */
scm_local_func void
scm_vm_op_return(ScmObj vm, uint32_t nr_arg) /* GC OK */
{
  if (nr_arg > INT32_MAX) return; /* [ERR]: */

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
scm_vm_op_gref(ScmObj vm, size_t immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  arg = scm_capi_iseq_ref_immval(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg))
    ;  /* TODO: error handling */

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (scm_obj_null_p(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_capi_iseq_set_immval(SCM_VM(vm)->reg.isp,
                                           immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    val = scm_gloc_value(gloc);
    if (scm_obj_null_p(val))
      ; /* TODO: error handling (reference of unbound variable) */

    SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    val = scm_gloc_value(gloc);
    if (scm_obj_null_p(val))
      ; /* TODO: error handling (reference of unbound variable) */

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
scm_vm_op_gdef(ScmObj vm, size_t immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  arg = scm_capi_iseq_ref_immval(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg))
    ;  /* TODO: error handling */

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    gloc = scm_gloctbl_bind(SCM_VM(vm)->gloctbl, arg, SCM_VM(vm)->reg.val);
    if (scm_obj_null_p(gloc))
      ;                           /* TODO: error handling */

    rslt = scm_capi_iseq_set_immval(SCM_VM(vm)->reg.isp, immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */
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
scm_vm_op_gset(ScmObj vm, size_t immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT, arg = SCM_OBJ_INIT;
  ssize_t rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(arg));

  arg = scm_capi_iseq_ref_immval(SCM_VM(vm)->reg.isp, immv_idx);
  if (scm_obj_null_p(arg))
    ;  /* TODO: error handling */

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (scm_obj_null_p(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_capi_iseq_set_immval(SCM_VM(vm)->reg.isp, immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    scm_gloc_bind(gloc, SCM_VM(vm)->reg.val);
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, SCM_VM(vm)->reg.val);
  }
  else {
    scm_assert(0);
  }
}

void
scm_vm_initialize(ScmObj vm,  ScmBedrock *bedrock)
{
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

  SCM_VM(vm)->trmp.code = SCM_OBJ_NULL;

  scm_vm_setup_singletons(vm);

  return;

 err:
  if (SCM_VM(vm)->stack != NULL) {
    SCM_VM(vm)->stack = scm_capi_free(SCM_VM(vm)->stack);
    SCM_VM(vm)->stack_size = 0;
  }
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.fp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_VM(vm)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;

  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

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

  vm = scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO);
  if (scm_obj_null_p(vm)) goto err;

  scm_vm__current_vm = vm;

  scm_vm_initialize(vm, bedrock);

  return vm;

 err:
  if (mem != NULL) scm_mem_end(mem);
  if (bedrock != NULL) scm_bedrock_end(bedrock);
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

void
scm_vm_setup_system(ScmObj vm)
{
  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_core_subr_system_setup();
}

void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  bool stop_flag;
  uint8_t op;
  uint32_t immv_idx, nr_arg, nr_arg_cf;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_capi_iseq_p(iseq));

  SCM_SLOT_SETQ(ScmVM, vm, reg.cp, scm_capi_iseq_to_closure(iseq));
  SCM_SLOT_SETQ(ScmVM, vm, reg.isp, iseq);
  SCM_VM(vm)->reg.ip = scm_capi_iseq_to_ip(iseq);
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_OBJ_NULL);
    /* TODO: undefined オブジェクトのようなものを初期値にする */

  stop_flag = false;
  while (!stop_flag) {
    SCM_CAPI_INST_FETCH_OP(SCM_VM(vm)->reg.ip, op);

    switch(op) {
    case SCM_OPCODE_NOP:
      /* nothing to do */
      break;
    case SCM_OPCODE_STOP:
      stop_flag = true;
      break;
    case SCM_OPCODE_CALL:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg);
      scm_vm_op_call(vm, nr_arg, 0, false);
      break;
    case SCM_OPCODE_TAIL_CALL:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg);
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg_cf);
      scm_vm_op_call(vm, nr_arg, nr_arg_cf, true);
      break;
    case SCM_OPCODE_RETURN:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, nr_arg);
      scm_vm_op_return(vm, nr_arg);
      break;
    case SCM_OPCODE_FRAME:
      scm_vm_op_frame(vm);
      break;
    case SCM_OPCODE_IMMVAL:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);
      scm_vm_op_immval(vm, immv_idx);
      break;
    case SCM_OPCODE_PUSH:
      scm_vm_op_push(vm);
      break;
    case SCM_OPCODE_GREF:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);
      scm_vm_op_gref(vm, immv_idx);
      break;
    case SCM_OPCODE_GDEF:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);
      scm_vm_op_gdef(vm, immv_idx);
      break;
    case SCM_OPCODE_GSET:
      SCM_CAPI_INST_FETCH_UINT32(SCM_VM(vm)->reg.ip, immv_idx);
      scm_vm_op_gset(vm, immv_idx);
      break;
    default:
      /* TODO: error handling */
      stop_flag = true;
      break;
    }
  }
}

int
scm_vm_setup_trampolining(ScmObj vm, ScmObj target,
                          ScmObj args, int nr_arg_cf,
                          ScmObj (*callback)(int argc, ScmObj *argv))
{
  ScmObj trmp_code = SCM_OBJ_INIT, trmp_clsr = SCM_OBJ_INIT;
  ScmObj cb_subr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&target, &args, &cb_subr, &trmp_code, &trmp_clsr);

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

  SCM_SLOT_SETQ(ScmVM, vm, trmp.code, trmp_clsr);

  return 0;
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_VM(obj)->mem = SCM_MEM(mem);

  SCM_VM(obj)->symtbl = SCM_OBJ_NULL;
  SCM_VM(obj)->stack = NULL;
  SCM_VM(obj)->stack_size = 0;
  SCM_VM(obj)->reg.sp = NULL;
  SCM_VM(obj)->reg.fp = NULL;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.cp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.isp = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.val = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_false = SCM_OBJ_NULL;
  SCM_VM(obj)->trmp.code = SCM_OBJ_NULL;
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

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->symtbl, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->gloctbl, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.isp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.cp, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->trmp.code, mem);
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

