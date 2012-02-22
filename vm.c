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
#include "iseq.h"
#include "procedure.h"
#include "core_subr.h"
#include "miscobjects.h"
#include "impl_utils.h"

#define SCM_VM_STACK_INIT_SIZE 1024
#define SCM_VM_STACK_MAX_SIZE 10240
#define SCM_VM_STACK_OBJMAP_SIZE                                        \
  (SCM_VM_STACK_INIT_SIZE / (sizeof(unsigned int) * CHAR_BIT)           \
   + ((SCM_VM_STACK_INIT_SIZE % (sizeof(unsigned int) * CHAR_BIT) == 0) ? 0 : 1))
#define SCM_VM_REF_STACK_INIT_SIZE 512
#define SCM_VM_SYMTBL_SIZE 256
#define SCM_VM_ERR_MSG_SIZE 256

ScmTypeInfo SCM_VM_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

ScmObj scm_vm__current_vm;


scm_local_inline size_t
scm_vm_stack_objmap_sp2idx(ScmObj vm, scm_vm_stack_val_t *sp)
{
  return ((scm_uword_t)((sp) - SCM_VM(vm)->stack)
          / (sizeof(SCM_VM(vm)->stack_objmap[0]) * CHAR_BIT));
}

scm_local_inline unsigned int
scm_vm_stack_objmap_sp2mask(ScmObj vm, scm_vm_stack_val_t *sp)
{
  return 1u << ((scm_uword_t)((sp) - SCM_VM(vm)->stack)
                % (sizeof(SCM_VM(vm)->stack_objmap[0]) * CHAR_BIT));
}

scm_local_inline void
scm_vm_stack_objmap_set(ScmObj vm , scm_vm_stack_val_t *sp)
{
  scm_assert((sp) < SCM_VM(vm)->reg.sp);
  SCM_VM(vm)->stack_objmap[scm_vm_stack_objmap_sp2idx(vm, sp)]
    |= scm_vm_stack_objmap_sp2mask(vm, sp);
}

scm_local_inline void
scm_vm_stack_objmap_unset(ScmObj vm, scm_vm_stack_val_t *sp)
{
  scm_assert((sp) < SCM_VM(vm)->reg.sp);
  SCM_VM(vm)->stack_objmap[scm_vm_stack_objmap_sp2idx(vm, sp)]
    &= ~scm_vm_stack_objmap_sp2mask(vm, sp);
}

scm_local_inline bool
scm_vm_stack_objmap_is_scmobj(ScmObj vm, scm_vm_stack_val_t *sp)
{
  scm_assert((sp) < SCM_VM(vm)->reg.sp);
  return ((SCM_VM(vm)->stack_objmap[scm_vm_stack_objmap_sp2idx(vm, sp)]
          & scm_vm_stack_objmap_sp2mask(vm, sp)) ?
          true : false);
}

scm_local_inline scm_iword_t
scm_vm_inst_fetch(ScmObj vm)
{
  return *(SCM_VM(vm)->reg.ip++);
}

scm_local_func void
scm_vm_setup_root(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, symtbl, scm_symtbl_new(SCM_CAPI_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->symtbl))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM,vm, gloctbl, scm_gloctbl_new(SCM_CAPI_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->gloctbl))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.nil, scm_nil_new(SCM_CAPI_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.nil))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.eof, scm_eof_new(SCM_CAPI_MEM_ROOT));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.eof))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_true,
                scm_bool_new(SCM_CAPI_MEM_ROOT, true));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_true))
    ;                           /* TODO: error handling */

  SCM_SLOT_SETQ(ScmVM, vm, cnsts.b_false,
                scm_bool_new(SCM_CAPI_MEM_ROOT, false));
  if (scm_obj_null_p(SCM_VM(vm)->cnsts.b_false))
    ;                           /* TODO: error handling */

}

scm_local_func void
scm_vm_clean_root(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->symtbl);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.nil);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.eof);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_true);
  scm_mem_free_root(SCM_VM(vm)->mem, SCM_VM(vm)->cnsts.b_false);
}

scm_local_func void
scm_vm_stack_push(ScmObj vm, scm_vm_stack_val_t elm, bool scmobj_p)
{
  scm_vm_stack_val_t *sp;

  SCM_STACK_FRAME_PUSH(&vm);
  if (scmobj_p) SCM_STACK_PUSH(&elm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp > SCM_VM(vm)->stack + SCM_VM(vm)->stack_size)
    return; /* stack overflow; TODO: handle stack overflow error  */

  if (scmobj_p)
    SCM_SLOT_REF_SETQ(ScmVM, vm, reg.sp, elm);
  else
    *SCM_VM(vm)->reg.sp = elm;

  sp = SCM_VM(vm)->reg.sp++;

  if (scmobj_p)
    scm_vm_stack_objmap_set(vm, sp);
  else
    scm_vm_stack_objmap_unset(vm, sp);
}

scm_local_func scm_vm_stack_val_t
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
scm_vm_stack_shorten(ScmObj vm, int n)
{
  SCM_STACK_FRAME_PUSH(&vm);
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM(vm)->reg.sp - SCM_VM(vm)->stack < n)
    /* stack underflow; TODO; handle stack underflow error */
    return;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->reg.sp - n;
}


/* 現在のスタックフレームにある引数の数を返す */
scm_local_func int
scm_vm_frame_argc(ScmObj vm)
{
  scm_uword_t argc;

  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(SCM_VM(vm)->reg.fp != NULL);

  argc = (scm_uword_t)SCM_VM(vm)->reg.fp[-1];

  scm_assert(argc <= INT_MAX);

  return (int)argc;
}

/* 現在のスタックフレームにある nth 番目の引数を返す (0 origin) */
scm_local_func ScmObj
scm_vm_frame_argv(ScmObj vm, int nth)
{
  SCM_STACK_FRAME_PUSH(&vm);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (nth >= scm_vm_frame_argc(vm))
    return SCM_OBJ_NULL;    /* 存在しない引数を参照。とりあえず NULL を返す */

  return SCM_OBJ(SCM_VM(vm)->reg.fp[-(nth + 2)]);
}

/* 関数呼出のためのインストラクション */
scm_local_func void
scm_vm_op_call(ScmObj vm)
{
  ScmObj val = SCM_OBJ_INIT;
  int argc;

  SCM_STACK_FRAME_PUSH(&val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  if (scm_obj_type_p(SCM_VM(vm)->reg.val, &SCM_SUBRUTINE_TYPE_INFO)) {
    SCM_VM(vm)->reg.fp = SCM_VM(vm)->reg.sp;
    argc = scm_vm_frame_argc(vm);

    /* FRAME インストラクションでダミー値を設定していたものを実際の値に変更
       する */
    SCM_SLOT_SETQ(ScmVM, vm, reg.fp[-(argc + 3)], SCM_VM(vm)->reg.iseq);
    SCM_VM(vm)->reg.fp[-(argc + 2)] = (scm_vm_stack_val_t)SCM_VM(vm)->reg.ip;

    val = scm_subrutine_call(SCM_VM(vm)->reg.val);
    if (scm_obj_not_null_p(val))
      SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);

    scm_vm_return_to_caller(vm);
  }
  /* TODO:  val レジスタがクロージャのケースの実装 */
  else
    ;                           /* TODO: error handling */
}

scm_local_func void
scm_vm_op_immval(ScmObj vm, ScmObj val)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(val));

  SCM_SLOT_SETQ(ScmVM, vm, reg.val, val);
}

scm_local_func void
scm_vm_op_push(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM(vm)->reg.val, true);
}

scm_local_func void
scm_vm_op_push_primval(ScmObj vm, scm_sword_t val)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)val, false);
}

/* 関数呼出のためのスタックフレームを作成するインストラクション。
 * フレームポインタとインストラクションポインタをスタックにプッシュする。
 * このインストラクションの後、引数と引数の数をプッシュする必要がある。
 */
scm_local_func void
scm_vm_op_frame(ScmObj vm) /* GC OK */
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  /* push frame pointer */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM(vm)->reg.fp, false);

  /* push ScmISeq object (FRAME インストラクション段階ではダミー値を
     プッシュする。本当の値は CALL 時に設定する) */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_OBJ_NULL, true);

  /* push instraction pointer (FRAME インストラクション段階ではダミー
     値をプッシュする。本当の値は CALL 時に設定する) */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)NULL, false);
}

/* 関数の呼び出しから戻るインストラクション。
 */
scm_local_func void
scm_vm_op_return(ScmObj vm) /* GC OK */
{
  scm_vm_return_to_caller(vm);
}

/* グローバル変数を参照するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し、その GLoc からシンボ
 * ルを束縛している値を得て、その値を val レジスタに設定する。またインストラク
 * ションの Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その Gloc からシンボルを束縛している値を得て、そ
 * の値を val レジスタに設定する。
 */
scm_local_func void
scm_vm_op_gref(ScmObj vm, ScmObj arg, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc, &val);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(arg));
  scm_assert(immv_idx >= 0);

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (scm_obj_null_p(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_iseq_update_immval(SCM_VM(vm)->reg.iseq, immv_idx, gloc);
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
 * 場合は GLoc を作成し)、その GLoc を使用してシンボルを val の値で束縛する。
 * またインストラクションの Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その GLoc を使用してシンボルを val の値で束縛す
 * る。
 */
scm_local_func void
scm_vm_op_gdef(ScmObj vm, ScmObj arg, ScmObj val, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &val, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(arg));
  scm_assert(scm_obj_not_null_p(val));

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    gloc = scm_gloctbl_bind(SCM_VM(vm)->gloctbl, arg, val);
    if (scm_obj_null_p(gloc))
      ;                           /* TODO: error handling */

    rslt = scm_iseq_update_immval(SCM_VM(vm)->reg.iseq, immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, val);
  }
  else {
    scm_assert(0);
  }
}

/* グローバル変数を更新するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し、その GLoc を使用して
 * グローバル変数の値を引数 val で更新する。またインストラクションの Symbol を
 * その GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その Gloc その GLoc を使用してグローバル変数の値
 * を引数 val で更新する。
 */
scm_local_func void
scm_vm_op_gset(ScmObj vm, ScmObj arg, ScmObj val, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &val, &gloc);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(arg));
  scm_assert(immv_idx >= 0);

  if (scm_obj_type_p(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM(vm)->gloctbl, arg, SCM_CSETTER_L(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (scm_obj_null_p(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_iseq_update_immval(SCM_VM(vm)->reg.iseq, immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    scm_gloc_bind(gloc, val);
  }
  else if (scm_obj_type_p(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, val);
  }
  else {
    scm_assert(0);
  }
}

void
scm_vm_initialize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_obj_init(SCM_OBJ(vm), &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->ref_stack = scm_ref_stack_new(SCM_VM_REF_STACK_INIT_SIZE);
  if (SCM_VM(vm)->ref_stack == NULL) goto err;

  SCM_VM(vm)->stack = scm_capi_malloc(sizeof(scm_vm_stack_val_t)
                                 * SCM_VM_STACK_INIT_SIZE);
  if (SCM_VM(vm)->stack == NULL) goto err;

  SCM_VM(vm)->stack_objmap = scm_capi_malloc(sizeof(unsigned int)
                                       * SCM_VM_STACK_OBJMAP_SIZE);
  if (SCM_VM(vm)->stack_objmap == NULL) goto err;

  SCM_VM(vm)->stack_size = SCM_VM_STACK_INIT_SIZE;

  SCM_VM(vm)->reg.sp = SCM_VM(vm)->stack;
  SCM_VM(vm)->reg.fp = NULL;
  SCM_VM(vm)->reg.ip = NULL;

  /* TODO: undefined オブジェクトみたいなものを初期値にする */
  SCM_VM(vm)->reg.val = SCM_OBJ_NULL;

  SCM_VM(vm)->err.type = SCM_VM_ERR_NONE;
  SCM_VM(vm)->err.message = scm_capi_malloc(SCM_VM_ERR_MSG_SIZE);
  if (SCM_VM(vm)->err.message == NULL) goto err;

  return;

 err:
  if (SCM_VM(vm)->err.message == NULL)
    scm_capi_free(SCM_VM(vm)->err.message);

  if (scm_obj_null_p(SCM_VM(vm)->reg.iseq))
    SCM_SLOT_SETQ(ScmVM, vm, reg.iseq, SCM_OBJ_NULL);

  if (SCM_VM(vm)->stack != NULL) {
    SCM_VM(vm)->stack = scm_capi_free(SCM_VM(vm)->stack);
    SCM_VM(vm)->stack_size = 0;
  }
  if (SCM_VM(vm)->stack_objmap != NULL) {
    SCM_VM(vm)->stack_objmap = scm_capi_free(SCM_VM(vm)->stack_objmap);
  }
  if (SCM_VM(vm)->ref_stack != NULL) {
    scm_ref_stack_end(SCM_VM(vm)->ref_stack);
    SCM_VM(vm)->ref_stack = NULL;
  }
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.fp = NULL;

  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->stack = scm_capi_free(SCM_VM(vm)->stack);
  SCM_VM(vm)->stack_size = 0;
  SCM_VM(vm)->stack_objmap = scm_capi_free(SCM_VM(vm)->stack_objmap);
  scm_ref_stack_end(SCM_VM(vm)->ref_stack);
  SCM_VM(vm)->reg.sp = NULL;
  SCM_VM(vm)->reg.fp = NULL;
  SCM_VM(vm)->reg.ip = NULL;
  SCM_SLOT_SETQ(ScmVM, vm, reg.iseq, SCM_OBJ_NULL);
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_OBJ_NULL);
  SCM_VM(vm)->ref_stack = NULL;
  SCM_VM(vm)->err.message = scm_capi_free(SCM_VM(vm)->err.message);
}

ScmObj
scm_vm_new(void)
{
  ScmMem *mem;
  ScmObj vm;

  mem = scm_mem_new();
  if (mem == NULL) return SCM_OBJ_NULL;

  vm = scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO);
  if (scm_obj_null_p(vm)) goto err;

  scm_vm__current_vm = vm;

  scm_vm_initialize(vm);
  scm_vm_setup_root(vm);

  return vm;

 err:
  scm_mem_end(mem);
  return SCM_OBJ_NULL;
}

void
scm_vm_end(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  scm_vm_clean_root(vm);

  scm_mem_end(SCM_VM(vm)->mem);
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
  scm_inst_t code1, code2;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  SCM_SLOT_SETQ(ScmVM, vm, reg.iseq, iseq);
  SCM_VM(vm)->reg.ip = SCM_ISEQ_SEQ(iseq);
  SCM_SLOT_SETQ(ScmVM, vm, reg.val, SCM_OBJ_NULL);
    /* TODO: undefined オブジェクトのようなものを初期値にする */

  stop_flag = false;
  while (!stop_flag) {
    code1.iword = scm_vm_inst_fetch(vm);

    switch(code1.plain.op) {
    case SCM_OPCODE_NOP:
      /* nothing to do */
      break;
    case SCM_OPCODE_STOP:
      stop_flag = true;
      break;
    case SCM_OPCODE_CALL:
      scm_vm_op_call(vm);
      break;
    case SCM_OPCODE_RETURN:
      scm_vm_op_return(vm);
      break;
    case SCM_OPCODE_FRAME:
      scm_vm_op_frame(vm);
      break;
    case SCM_OPCODE_IMMVAL:
      scm_vm_op_immval(vm,
                       scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                           code1.immv1.imm_idx));
      break;
    case SCM_OPCODE_PUSH:
      scm_vm_op_push(vm);
      break;
    case SCM_OPCODE_PUSH_PRIMVAL:
      scm_vm_op_push_primval(vm, code1.primv.primval);
      break;
    case SCM_OPCODE_GREF:
      scm_vm_op_gref(vm,
                     scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                         code1.immv1.imm_idx),
                     code1.immv1.imm_idx);
      break;
    case SCM_OPCODE_GDEF:
      code2.iword = scm_vm_inst_fetch(vm);
      scm_vm_op_gdef(vm,
                     scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                         code1.immv1.imm_idx),
                     scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                         code2.immv2.imm_idx),
                     code1.immv1.imm_idx);;
      break;
    case SCM_OPCODE_GSET:
      code2.iword = scm_vm_inst_fetch(vm);
      scm_vm_op_gset(vm,
                     scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                         code1.immv1.imm_idx),
                     scm_iseq_get_immval(SCM_VM(vm)->reg.iseq,
                                         code2.immv2.imm_idx),
                     code1.immv1.imm_idx);;
      break;
    default:
      /* TODO: error handling */
      stop_flag = true;
      break;
    }
  }
}

int
scm_vm_nr_local_var(ScmObj vm)
{
  return scm_vm_frame_argc(vm);
}

/* 束縛変数を参照する。 box されている場合は unbox する */
ScmObj
scm_vm_refer_local_var(ScmObj vm, int nth)
{
  /* box/unbox is not implemented */
  return scm_vm_frame_argv(vm, nth);
}

void
scm_vm_return_to_caller(ScmObj vm)
{
  int argc;
  scm_vm_stack_val_t *fp;

  SCM_STACK_FRAME_PUSH(vm);

  argc = scm_vm_frame_argc(vm);
  fp = SCM_VM(vm)->reg.fp;

  SCM_VM(vm)->reg.fp = (scm_vm_stack_val_t *)fp[-(argc + 4)];
  SCM_SLOT_SETQ(ScmVM, vm, reg.iseq, SCM_OBJ(fp[-(argc + 3)]));
  SCM_VM(vm)->reg.ip = (scm_iword_t*)fp[-(argc + 2)];

  scm_vm_stack_shorten(vm, argc + 4); /* 3 := argc, fp, iseq, ip */
}

void
scm_vm_fatal(ScmObj vm, const char *msg)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->err.type = SCM_VM_ERR_FATAL;
  if (msg == NULL) {
    SCM_VM(vm)->err.message[0] = '\0';
  }
  else {
    size_t len = strlen(msg);
    if (len > SCM_VM_ERR_MSG_SIZE - 1) len = SCM_VM_ERR_MSG_SIZE - 1;
    memcpy(SCM_VM(vm)->err.message, msg, len);
    SCM_VM(vm)->err.message[len] = '\0';
  }
}

void
scm_vm_fatal_fmt(ScmObj vm, const char *msgfmt, va_list ap)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  SCM_VM(vm)->err.type = SCM_VM_ERR_FATAL;
  if (msgfmt == NULL) {
    SCM_VM(vm)->err.message[0] = '\0';
  }
  else {
    vsnprintf(SCM_VM(vm)->err.message, SCM_VM_ERR_MSG_SIZE, msgfmt, ap);
  }
}

bool
scm_vm_fatal_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return (SCM_VM(vm)->err.type == SCM_VM_ERR_FATAL) ? true : false;
}

bool
scm_vm_error_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return ((SCM_VM(vm)->err.type == SCM_VM_ERR_FATAL
           || SCM_VM(vm)->err.type == SCM_VM_ERR_ERROR) ?
          true : false);
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_VM_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_VM(obj)->mem = SCM_MEM(mem);

  SCM_VM(obj)->symtbl = SCM_OBJ_NULL;
  SCM_VM(obj)->stack = NULL;
  SCM_VM(obj)->stack_objmap = NULL;
  SCM_VM(obj)->stack_size = 0;
  SCM_VM(obj)->ref_stack = NULL;
  SCM_VM(obj)->reg.sp = NULL;
  SCM_VM(obj)->reg.fp = NULL;
  SCM_VM(obj)->reg.ip = NULL;
  SCM_VM(obj)->reg.iseq = SCM_OBJ_NULL;
  SCM_VM(obj)->reg.val = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.nil = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.eof = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_true = SCM_OBJ_NULL;
  SCM_VM(obj)->cnsts.b_false = SCM_OBJ_NULL;
  SCM_VM(obj)->err.message = NULL;
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

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.iseq, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM(obj)->reg.val, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  /* rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->cp, mem); */
  /* if (scm_gc_ref_handler_failure_p(rslt)) return rslt; */

  for (scm_vm_stack_val_t* p = SCM_VM(obj)->stack;
       p != SCM_VM(obj)->reg.sp;
       p++) {
    if (scm_vm_stack_objmap_is_scmobj(obj, p)) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *p, mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  rslt = scm_ref_stack_gc_accept(SCM_VM(obj)->ref_stack, obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  return rslt;
}

