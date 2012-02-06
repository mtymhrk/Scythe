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
#include "miscobjects.h"

#define SCM_VM_STACK_INIT_SIZE 1024
#define SCM_VM_STACK_MAX_SIZE 10240
#define SCM_VM_STACK_OBJMAP_SIZE                                        \
  (SCM_VM_STACK_INIT_SIZE / (sizeof(unsigned int) * CHAR_BIT)           \
   + ((SCM_VM_STACK_INIT_SIZE % (sizeof(unsigned int) * CHAR_BIT) == 0) ? 0 : 1))
#define SCM_VM_REF_STACK_INIT_SIZE 512

#define SCM_VM_SYMTBL_SIZE 256

/* typedef enum { */
/*   SCM_VM_INST_CODE_NOOP, */
/*   SCM_VM_INST_CODE_PUSH, */
/*   SCM_VM_INST_CODE_POP, */
/*   SCM_VM_INST_CODE_CALL, */
/*   SCM_VM_INST_CODE_RET, */
/*   SCM_VM_INST_CODE_FRAME, */
/* } scm_vm_inst_code_t; */

/* struct ScmVMEnvRec { */
/*   ScmObj in_port;        /\* is not target of GC *\/ */
/*   ScmObj out_port;       /\* is not target of GC *\/ */
/*   ScmObj err_port;       /\* is not target of GC *\/ */
/*   ScmMem *mem; */
/* }; */

/* struct ScmVMInstRec { */
/*   scm_vm_inst_t *iseq; */
/*   size_t iseq_size; */
/* }; */

/* #define SCM_VM_INST_FETCH_CODE(iseq, ip, code) (code) = (*(ip++) & 0xff) */
/* #define SCM_VM_INST_FETCH_OBJ(iseq, ip, obj) (obj) = *(ScmObj *)(ip++) */



/* #define SCM_VM_CHECK_STACK_OVER_FLOW(vm) \ */
/*   ((size_t)((vm)->sp - (vm)->stack) < (vm)->stack_size - 1) */
/* #define SCM_VM_CHECK_STACK_UNDER_FLOW(vm) ((size_t)((vm)->sp - (vm)->stack) < 1) */
/* #define SCM_VM_PUSH_TO_STACK(vm, obj) (*((vm)->sp++) = obj) */
/* #define SCM_VM_POP_FROM_STACK(vm, obj) (obj = *(--(vm)->sp) ) */

ScmTypeInfo SCM_VM_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

/* static ScmVMEnv *global_env; */
ScmObj scm_vm__current_vm;

/* static void */
/* scm_vm_inst_call(ScmVM *vm) */
/* { */
/*   ; */
/* } */

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
  if(SCM_OBJ_IS_NULL(SCM_VM_ISEQ(vm))) goto err;

  return;

 err:
  if (SCM_OBJ_IS_NULL(SCM_VM_ISEQ(vm)))
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
  if (SCM_OBJ_IS_NULL(SCM_VM_SYMTBL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_GLOCTBL(vm), scm_gloctbl_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_GLOCTBL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_NIL(vm), scm_nil_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_CONST_NIL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_EOF(vm), scm_eof_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_CONST_EOF(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_TRUE(vm), scm_bool_new(SCM_MEM_ALLOC_ROOT, true));
  if (SCM_OBJ_IS_NULL(SCM_VM_CONST_TRUE(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_CONST_FALSE(vm),
           scm_bool_new(SCM_MEM_ALLOC_ROOT, false));
  if (SCM_OBJ_IS_NULL(SCM_VM_CONST_FALSE(vm)))
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
  if (SCM_OBJ_IS_NULL(vm)) goto err;

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
scm_vm_stack_push(ScmObj vm, scm_vm_stack_val_t elm, bool scmobj_p)
{
  scm_vm_stack_val_t *sp;

  SCM_STACK_PUSH(&vm, &elm);

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

  SCM_STACK_PUSH(&vm, &elm);

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
  SCM_STACK_PUSH(&vm);
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

  SCM_STACK_PUSH(&vm);

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
  SCM_STACK_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (nth >= scm_vm_frame_argc(vm))
    return SCM_OBJ_NULL;    /* 存在しない引数を参照。とりあえず NULL を返す */

  return SCM_OBJ(SCM_VM_FP(vm)[-(nth + 2)]);
}

/* 現在のスタックフレームに保存されている frame pointer を返す */
scm_vm_stack_val_t *
scm_vm_frame_outer_frame(ScmObj vm)
{
  int argc;

  SCM_STACK_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  argc = scm_vm_frame_argc(vm);

  return (scm_vm_stack_val_t *)(SCM_VM_FP(vm)[-(argc + 4)]);
}

/* 現在のスタックフレームに保存されている ISeq オブジェクトを返す */
ScmObj
scm_vm_frame_iseq(ScmObj vm)
{
  int argc;

  SCM_STACK_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  argc = scm_vm_frame_argc(vm);

  return SCM_OBJ((SCM_VM_FP(vm)[-(argc + 3)]));
}

/* 現在のスタックフレームに保存されている Instruction pointer を返す */
scm_iword_t *
scm_vm_frame_next_inst(ScmObj vm)
{
  int argc;

  SCM_STACK_PUSH(&vm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  argc = scm_vm_frame_argc(vm);

  return (scm_iword_t*)((SCM_VM_FP(vm)[-(argc + 2)]));
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


/* 関数呼出のためのスタックフレームを作成するインストラクション。
 * フレームポインタとインストラクションポインタをスタックにプッシュする。
 * このインストラクションの後、引数と引数の数をプッシュする必要がある。
 */
void
scm_vm_op_frame(ScmObj vm) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM_FP(vm), false);
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM_ISEQ(vm), true);
  scm_vm_stack_push(vm, (scm_vm_stack_val_t)SCM_VM_IP(vm), false);
}

/* 関数の呼び出しから戻るインストラクション。
 * 引数 val が SCM_OBJ_NULL ではない場合、 val レジスタを val の値で更新する。
 * 引数 val を使用するのは主に C 実装の関数呼出から戻る場合を想定。
 */
void
scm_vm_op_return(ScmObj vm, ScmObj val) /* GC OK */
{
  ScmObj *fp, iseq = SCM_OBJ_INIT;
  scm_iword_t *ip;
  int argc;

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  argc = scm_vm_frame_argc(vm);
  fp = scm_vm_frame_outer_frame(vm);
  SCM_SETQ(iseq, scm_vm_frame_iseq(vm));
  ip = scm_vm_frame_next_inst(vm);

  SCM_VM_FP(vm) = fp;
  SCM_VM_ISEQ_SETQ(vm, iseq);
  SCM_VM_IP(vm) = ip;

  if (SCM_OBJ_IS_NOT_NULL(val))
    SCM_VM_VAL_SETQ(vm, val);

  scm_vm_stack_shorten(vm, argc + 4); /* 3 := argc, fp, iseq, ip */
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

    if (SCM_OBJ_IS_NULL(gloc))
      ; /* TODO: error handling (reference of unbound variable) */

    rslt = scm_iseq_update_immval(SCM_VM_ISEQ(vm), immv_idx, gloc);
    if (rslt != 0)
      ;                           /* TODO: error handling */

    SCM_SETQ(val, scm_gloc_value(gloc));
    if (SCM_OBJ_IS_NULL(val))
      ; /* TODO: error handling (reference of unbound variable) */

    SCM_VM_VAL_SETQ(vm, val);
  }
  else if (SCM_OBJ_IS_TYPE(arg, &SCM_GLOC_TYPE_INFO)) {
    SCM_SETQ(val, scm_gloc_value(gloc));
    if (SCM_OBJ_IS_NULL(val))
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
    if (SCM_OBJ_IS_NULL(gloc))
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

    if (SCM_OBJ_IS_NULL(gloc))
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

