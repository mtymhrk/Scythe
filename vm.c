#include <stdlib.h>
#include <stdint.h>
#include <limits.h>
#include <assert.h>
#include <unistd.h>

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

#define SCM_VM_STACK_INIT_SIZE 1024
#define SCM_VM_STACK_MAX_SIZE 10240
#define SCM_VM_STACK_OBJMAP_SIZE                                        \
  (SCM_VM_STACK_INIT_SIZE / (sizeof(unsigned int) * CHAR_BIT)           \
   + ((SCM_VM_STACK_INIT_SIZE % (sizeof(unsigned int) * CHAR_BIT) == 0) ? 0 : 1))
#define SCM_VM_REF_STACK_INIT_SIZE 512

#define SCM_VM_SYMTBL_SIZE 256

ScmTypeInfo SCM_VM_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

ScmObj scm_vm__current_vm;

void
scm_vm_initialize(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_obj_init(SCM_OBJ(vm), &SCM_VM_TYPE_INFO);

  SCM_VM_REF_STACK(vm) = scm_ref_stack_new(SCM_VM_REF_STACK_INIT_SIZE);
  if (SCM_VM_REF_STACK(vm) == NULL) goto err;

  SCM_VM_STACK(vm) = scm_memory_allocate(sizeof(scm_vm_stack_val_t)
                                         * SCM_VM_STACK_INIT_SIZE);
  if (SCM_VM_STACK(vm) == NULL) goto err;

  SCM_VM_STACK_OBJMAP(vm) = scm_memory_allocate(sizeof(unsigned int)
                                                * SCM_VM_STACK_OBJMAP_SIZE);
  if (SCM_VM_STACK_OBJMAP(vm) == NULL) goto err;

  SCM_VM_STACK_SIZE(vm) = SCM_VM_STACK_INIT_SIZE;

  SCM_VM_SP(vm) = SCM_VM_STACK(vm);
  SCM_VM_FP(vm) = NULL;
  SCM_VM_IP(vm) = NULL;

  /* TODO: undefined オブジェクトみたいなものを初期値にする */
  SCM_VM_VAL(vm) = SCM_OBJ_NULL;

  SCM_VM_ISEQ_SETQ(vm, scm_iseq_new(SCM_MEM_ALLOC_HEAP));
  if(SCM_OBJ_NULL_P(SCM_VM_ISEQ(vm))) goto err;

  return;

 err:
  if (SCM_OBJ_NULL_P(SCM_VM_ISEQ(vm)))
    SCM_VM_ISEQ_SETQ(vm, SCM_OBJ_NULL);

  if (SCM_VM_STACK(vm) != NULL) {
    SCM_VM_STACK(vm) = scm_memory_release(SCM_VM_STACK(vm));
    SCM_VM_STACK_SIZE(vm) = 0;
  }
  if (SCM_VM_STACK_OBJMAP(vm) != NULL) {
    SCM_VM_STACK_OBJMAP(vm) = scm_memory_release(SCM_VM_STACK_OBJMAP(vm));
  }
  if (SCM_VM_REF_STACK(vm) != NULL) {
    scm_ref_stack_end(SCM_VM_REF_STACK(vm));
    SCM_VM_REF_STACK(vm) = NULL;
  }
  SCM_VM_SP(vm) = NULL;
  SCM_VM_FP(vm) = NULL;

  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  SCM_VM_STACK(vm) = scm_memory_release(SCM_VM_STACK(vm));
  SCM_VM_STACK_SIZE(vm) = 0;
  SCM_VM_STACK_OBJMAP(vm) = scm_memory_release(SCM_VM_STACK_OBJMAP(vm));
  scm_ref_stack_end(SCM_VM_REF_STACK(vm));
  SCM_VM_SP(vm) = NULL;
  SCM_VM_FP(vm) = NULL;
  SCM_VM_IP(vm) = NULL;
  SCM_VM_ISEQ_SETQ(vm, SCM_OBJ_NULL);
  SCM_VM_VAL_SETQ(vm, SCM_OBJ_NULL);
  SCM_VM_REF_STACK(vm) = NULL;
}

void
scm_vm_setup_root(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  SCM_SETQ(SCM_VM_SYMTBL(vm), scm_symtbl_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_NULL_P(SCM_VM_SYMTBL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_GLOCTBL(vm), scm_gloctbl_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_NULL_P(SCM_VM_GLOCTBL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_NIL(vm), scm_nil_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_NULL_P(SCM_VM_CONST_NIL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_EOF(vm), scm_eof_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_NULL_P(SCM_VM_CONST_EOF(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_TRUE(vm), scm_bool_new(SCM_MEM_ALLOC_ROOT, true));
  if (SCM_OBJ_NULL_P(SCM_VM_CONST_TRUE(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_FALSE(vm),
           scm_bool_new(SCM_MEM_ALLOC_ROOT, false));
  if (SCM_OBJ_NULL_P(SCM_VM_CONST_FALSE(vm)))
    ;                           /* TODO: error handling */

}

void
scm_vm_clean_root(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_SYMTBL(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_CONST_NIL(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_CONST_EOF(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_CONST_TRUE(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_CONST_FALSE(vm));
}

ScmObj
scm_vm_new(void)
{
  ScmMem *mem;
  ScmObj vm;

  mem = scm_mem_new();
  if (mem == NULL) return SCM_OBJ_NULL;

  scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO, SCM_REF_MAKE(vm));
  if (SCM_OBJ_NULL_P(vm)) goto err;

  SCM_SETQ(scm_vm__current_vm, vm);

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
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_clean_root(vm);

  scm_mem_end(SCM_VM_MEM(vm));
}


void
scm_vm_setup_system(ScmObj vm)
{
  SCM_STACK_FRAME_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_core_subr_system_setup();
}


static inline scm_iword_t
scm_vm_inst_fetch(ScmObj vm)
{
  return *(SCM_VM_IP(vm)++);
}


void
scm_vm_run(ScmObj vm, ScmObj iseq)
{
  bool stop_flag;
  scm_inst_t code1, code2;

  SCM_STACK_FRAME_PUSH(&vm, &iseq);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(iseq, &SCM_ISEQ_TYPE_INFO);

  SCM_VM_ISEQ_SETQ(vm, iseq);
  SCM_VM_IP(vm) = SCM_ISEQ_SEQ(iseq);
  SCM_VM_VAL_SETQ(vm, SCM_OBJ_NULL);
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
                       scm_iseq_get_immval(SCM_VM_ISEQ(vm),
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
                     scm_iseq_get_immval(SCM_VM_ISEQ(vm), code1.immv1.imm_idx),
                     code1.immv1.imm_idx);
      break;
    case SCM_OPCODE_GDEF:
      code2.iword = scm_vm_inst_fetch(vm);
      scm_vm_op_gdef(vm,
                     scm_iseq_get_immval(SCM_VM_ISEQ(vm), code1.immv1.imm_idx),
                     scm_iseq_get_immval(SCM_VM_ISEQ(vm), code2.immv2.imm_idx),
                     code1.immv1.imm_idx);;
      break;
    case SCM_OPCODE_GSET:
      code2.iword = scm_vm_inst_fetch(vm);
      scm_vm_op_gset(vm,
                     scm_iseq_get_immval(SCM_VM_ISEQ(vm), code1.immv1.imm_idx),
                     scm_iseq_get_immval(SCM_VM_ISEQ(vm), code2.immv2.imm_idx),
                     code1.immv1.imm_idx);;
      break;
    default:
      /* TODO: error handling */
      stop_flag = true;
      break;
    }
  }
}



void
scm_vm_stack_push(ScmObj vm, scm_vm_stack_val_t elm, bool scmobj_p)
{
  scm_vm_stack_val_t *sp;

  SCM_STACK_FRAME_PUSH(&vm);
  if (scmobj_p) SCM_STACK_PUSH(&elm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM_SP(vm) > SCM_VM_STACK(vm) + SCM_VM_STACK_SIZE(vm))
    return; /* stack overflow; TODO: handle stack overflow error  */

  sp = SCM_VM_SP(vm);

  if (scmobj_p)
    SCM_SETQ(*sp, elm);
  else
    *SCM_VM_SP(vm) = elm;

  SCM_VM_SP_INC(vm);

  if (scmobj_p)
    SCM_VM_STACK_OBJMAP_SET(vm, sp);
  else
    SCM_VM_STACK_OBJMAP_UNSET(vm, sp);
}

scm_vm_stack_val_t
scm_vm_stack_pop(ScmObj vm)
{
  ScmObj elm = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vm, &elm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM_SP(vm) == SCM_VM_STACK(vm))
    /* stack underflow; TODO; handle stack underflow error */
    return SCM_OBJ_NULL;

  SCM_VM_SP_DEC(vm);

  SCM_SETQ(elm, *SCM_VM_SP(vm));

  return elm;
}

void
scm_vm_stack_shorten(ScmObj vm, int n)
{
  SCM_STACK_FRAME_PUSH(&vm);
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM_SP(vm) - SCM_VM_STACK(vm) < n)
    /* stack underflow; TODO; handle stack underflow error */
    return;

  SCM_VM_SP(vm) = SCM_VM_SP(vm) - n;
}


/* 現在のスタックフレームにある引数の数を返す */
int
scm_vm_frame_argc(ScmObj vm)
{
  scm_uword_t argc;

  SCM_STACK_FRAME_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_VM_FP(vm) != NULL);

  argc = (scm_uword_t)SCM_VM_FP(vm)[-1];

  assert(argc <= INT_MAX);

  return (int)argc;
}

/* 現在のスタックフレームにある nth 番目の引数を返す (0 origin) */
ScmObj
scm_vm_frame_argv(ScmObj vm, int nth)
{
  SCM_STACK_FRAME_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (nth >= scm_vm_frame_argc(vm))
    return SCM_OBJ_NULL;    /* 存在しない引数を参照。とりあえず NULL を返す */

  return SCM_OBJ(SCM_VM_FP(vm)[-(nth + 2)]);
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
  fp = SCM_VM_FP(vm);

  SCM_VM_FP(vm) = (scm_vm_stack_val_t *)fp[-(argc + 4)];
  SCM_VM_ISEQ_SETQ(vm, SCM_OBJ(fp[-(argc + 3)]));
  SCM_VM_IP(vm) = (scm_iword_t*)fp[-(argc + 2)];

  scm_vm_stack_shorten(vm, argc + 4); /* 3 := argc, fp, iseq, ip */
}


/* 関数呼出のためのインストラクション */
void
scm_vm_op_call(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_OBJ_IS_TYPE(SCM_VM_VAL(vm), &SCM_SUBRUTINE_TYPE_INFO)) {
    scm_vm_stack_val_t *fp = SCM_VM_FP(vm) = SCM_VM_SP(vm);
    int argc = scm_vm_frame_argc(vm);

    /* FRAME インストラクションでダミー値を設定していたものを実際の値に変更
       する */
    SCM_SETQ(fp[-(argc + 3)], SCM_VM_ISEQ(vm));
    fp[-(argc + 2)] = (scm_vm_stack_val_t)SCM_VM_IP(vm);

    scm_subrutine_call(SCM_VM_VAL(vm));
  }
  /* TODO:  val レジスタがクロージャのケースの実装 */
  else
    ;                           /* TODO: error handling */
}

void
scm_vm_op_immval(ScmObj vm, ScmObj val)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(val));

  SCM_VM_VAL_SETQ(vm, val);
}

void
scm_vm_op_push(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM_VAL(vm), true);
}

void
scm_vm_op_push_primval(ScmObj vm, scm_sword_t val)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)val, false);
}

/* 関数呼出のためのスタックフレームを作成するインストラクション。
 * フレームポインタとインストラクションポインタをスタックにプッシュする。
 * このインストラクションの後、引数と引数の数をプッシュする必要がある。
 */
void
scm_vm_op_frame(ScmObj vm) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  /* push frame pointer */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM_FP(vm), false);

  /* push ScmISeq object (FRAME インストラクション段階ではダミー値を
     プッシュする。本当の値は CALL 時に設定する) */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_OBJ_NULL, true);

  /* push instraction pointer (FRAME インストラクション段階ではダミー
     値をプッシュする。本当の値は CALL 時に設定する) */
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)NULL, false);
}

/* 関数の呼び出しから戻るインストラクション。
 */
void
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
void
scm_vm_op_gref(ScmObj vm, ScmObj arg, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  ScmObj val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &gloc, &val);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(arg));
  assert(immv_idx >= 0);

  if (SCM_OBJ_IS_TYPE(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM_GLOCTBL(vm), arg, SCM_REF_MAKE(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (SCM_OBJ_NULL_P(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_iseq_update_immval(SCM_VM_ISEQ(vm), immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    SCM_SETQ(val, scm_gloc_value(gloc));
    if (SCM_OBJ_NULL_P(val))
      ; /* TODO: error handling (reference of unbound variable) */

    SCM_VM_VAL_SETQ(vm, val);
  }
  else if (SCM_OBJ_IS_TYPE(arg, &SCM_GLOC_TYPE_INFO)) {
    SCM_SETQ(val, scm_gloc_value(gloc));
    if (SCM_OBJ_NULL_P(val))
      ; /* TODO: error handling (reference of unbound variable) */

    SCM_VM_VAL_SETQ(vm, val);
  }
  else {
    assert(0);
  }
}

/* グローバル変数を作成するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し(検索の結果存在しない
 * 場合は GLoc を作成し)、その GLoc を使用してシンボルを val の値で束縛する。
 * またインストラクションの Symbol をその GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その GLoc を使用してシンボルを val の値で束縛す
 * る。
 */
void
scm_vm_op_gdef(ScmObj vm, ScmObj arg, ScmObj val, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &val, &gloc);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(arg));
  assert(SCM_OBJ_IS_NOT_NULL(val));

  if (SCM_OBJ_IS_TYPE(arg, &SCM_SYMBOL_TYPE_INFO)) {
    SCM_SETQ(gloc, scm_gloctbl_bind(SCM_VM_GLOCTBL(vm), arg, val));
    if (SCM_OBJ_NULL_P(gloc))
      ;                           /* TODO: error handling */

    rslt = scm_iseq_update_immval(SCM_VM_ISEQ(vm), immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */
  }
  else if (SCM_OBJ_IS_TYPE(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, val);
  }
  else {
    assert(0);
  }
}

/* グローバル変数を更新するインストラクション。
 * 引数 arg が Symbol である場合、対応する GLoc を検索し、その GLoc を使用して
 * グローバル変数の値を引数 val で更新する。またインストラクションの Symbol を
 * その GLoc で置き換える。
 * 引数 arg  が GLoc の場合、その Gloc その GLoc を使用してグローバル変数の値
 * を引数 val で更新する。
 */
void
scm_vm_op_gset(ScmObj vm, ScmObj arg, ScmObj val, int immv_idx)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&vm, &arg, &val, &gloc);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(arg));
  assert(immv_idx >= 0);

  if (SCM_OBJ_IS_TYPE(arg, &SCM_SYMBOL_TYPE_INFO)) {
    rslt = scm_gloctbl_find(SCM_VM_GLOCTBL(vm), arg, SCM_REF_MAKE(gloc));
    if (rslt != 0)
      ;                           /* TODO: error handling */

    if (SCM_OBJ_NULL_P(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_iseq_update_immval(SCM_VM_ISEQ(vm), immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    scm_gloc_bind(gloc, val);
  }
  else if (SCM_OBJ_IS_TYPE(arg, &SCM_GLOC_TYPE_INFO)) {
    scm_gloc_bind(arg, val);
  }
  else {
    assert(0);
  }
}




void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));

  SCM_VM_MEM(obj) = SCM_MEM(mem);

  SCM_VM_SYMTBL(obj) = SCM_OBJ_NULL;
  SCM_VM_STACK(obj) = NULL;
  SCM_VM_STACK_OBJMAP(obj) = NULL;
  SCM_VM_STACK_SIZE(obj) = 0;
  SCM_VM_REF_STACK(obj) = NULL;
  SCM_VM_SP(obj) = NULL;
  SCM_VM_FP(obj) = NULL;
  SCM_VM_IP(obj) = NULL;
  SCM_VM_ISEQ(obj) = SCM_OBJ_NULL;
  SCM_VM_VAL(obj) = SCM_OBJ_NULL;
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

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_SYMTBL(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_ISEQ(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_VAL(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  /* rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->cp, mem); */
  /* if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt; */

  for (scm_vm_stack_val_t* p = SCM_VM_STACK(obj); p != SCM_VM_SP(obj); p++) {
    bool scmobj_p;
    SCM_VM_STACK_OBJMAP_IS_SCMOBJ(obj, p, scmobj_p);
    if (scmobj_p) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *p, mem);
      if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
    }
  }

  rslt = scm_ref_stack_gc_accept(SCM_VM_REF_STACK(obj), obj, mem, handler);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  return rslt;
}

