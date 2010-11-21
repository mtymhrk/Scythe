#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>

#include "vm.h"
#include "memory.h"
#include "reference.h"
#include "object.h"
#include "symbol.h"
#include "miscobjects.h"
#include "obuffer.h"

#define SCM_VM_STACK_INIT_SIZE 1024
#define SCM_VM_STACK_MAX_SIZE 10240
#define SCM_VM_REF_STACK_INIT_SIZE 512

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
  scm_vm_pretty_print,          /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

/* static ScmVMEnv *global_env; */
static ScmObj current_vm;

/* static void */
/* scm_vm_inst_call(ScmVM *vm) */
/* { */
/*   ; */
/* } */


static void
scm_vm_push_stack(ScmObj vm, ScmObj elm)
{
  SCM_STACK_PUSH(&vm, &elm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(elm));

  if (SCM_VM_SP(vm) > SCM_VM_STACK(vm) + SCM_VM_STACK_SIZE(vm))
    return; /* stack overflow; TODO: handle stack overflow error  */

  SCM_SETQ(*SCM_VM_SP(vm), elm);

  SCM_VM_SP_INC(vm);
}

static ScmObj
scm_vm_pop_stack(ScmObj vm)
{
  ScmObj elm = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&vm, &elm);

  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM_SP(vm) == SCM_VM_STACK(vm))
    /* stack underflow; TODO; handle stack underflow error */
    return SCM_OBJ_NULL;

  SCM_SETQ(elm, *SCM_VM_SP(vm));

  SCM_VM_SP_DEC(vm);

  return elm;
}

void
scm_vm_initialize(ScmObj vm, ScmObj parent)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE_ACCEPT_NULL(parent, &SCM_VM_TYPE_INFO);

  scm_obj_init(SCM_OBJ(vm), &SCM_VM_TYPE_INFO);

  SCM_VM_PARENT_VM(vm) = parent;
  SCM_VM_PREV_VM(vm) = SCM_OBJ_NULL;

  SCM_VM_STACK(vm) = scm_memory_allocate(sizeof(ScmObj) * SCM_VM_STACK_INIT_SIZE);
  if (SCM_VM_STACK(vm) == NULL) goto err;

  SCM_VM_STACK_SIZE(vm) = SCM_VM_STACK_INIT_SIZE;

  SCM_VM_REF_STACK(vm) = scm_ref_stack_new(SCM_VM_REF_STACK_INIT_SIZE);
  if (SCM_VM_REF_STACK(vm) == NULL) goto err;

  SCM_VM_SP(vm) = SCM_VM_STACK(vm);
  SCM_VM_FP(vm) = NULL;

  return;

 err:
  if (SCM_VM_STACK(vm) != NULL) {
    SCM_VM_STACK(vm) = scm_memory_release(SCM_VM_STACK(vm));
    SCM_VM_STACK_SIZE(vm) = 0;
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
  scm_ref_stack_end(SCM_VM_REF_STACK(vm));
  SCM_VM_SP(vm) = NULL;
  SCM_VM_FP(vm) = NULL;
  SCM_VM_REF_STACK(vm) = NULL;
}

void
scm_vm_setup_root(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_switch_vm(vm);

  SCM_SETQ(SCM_VM_SYMTBL(vm), scm_symtable_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_SYMTBL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_NIL(vm), scm_nil_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_NIL(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_EOF(vm), scm_eof_new(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_EOF(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_BOOL_TRUE(vm), scm_bool_new(SCM_MEM_ALLOC_ROOT, true));
  if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_TRUE(vm)))
    ;                           /* TODO: error handling */

  SCM_SETQ(SCM_VM_BOOL_FALSE(vm),
           scm_bool_new(SCM_MEM_ALLOC_ROOT, false));
  if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_FALSE(vm)))
    ;                           /* TODO: error handling */

  scm_vm_revert_vm();
}

void
scm_vm_clean_root(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_switch_vm(vm);

  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_SYMTBL(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_NIL(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_EOF(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_BOOL_TRUE(vm));
  scm_mem_free_root(SCM_VM_MEM(vm), SCM_VM_BOOL_FALSE(vm));

  scm_vm_revert_vm();

  scm_mem_end(SCM_VM_MEM(vm));
}

ScmObj
scm_vm_new(void)
{
  ScmMem *mem;
  ScmObj vm;

  mem = scm_mem_new();
  if (mem == NULL) return SCM_OBJ_NULL;

  if (scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(current_vm))
      == SCM_REF_NULL)
    goto err;

  scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO, SCM_REF_MAKE(vm));
  if (SCM_OBJ_IS_NULL(vm)) goto err;

  scm_vm_initialize(vm, SCM_OBJ_NULL);
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

  if (SCM_OBJ_IS_NULL(SCM_VM_PARENT_VM(vm))) /* root vm */
    scm_vm_clean_root(vm);
}

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

ScmObj
scm_vm_frame_argv(ScmObj vm, int nth)
{
  SCM_STACK_PUSH(&vm);

  if (nth < scm_vm_frame_argc(vm))
    return SCM_OBJ_NULL;    /* 存在しない引数を参照。とりあえず NULL を返す */

  return SCM_OBJ(SCM_VM_FP(vm)[-(nth - 1)]);
}



/* void */
/* scm_vm_run(ScmVM *vm) */
/* { */
/*   assert(vm != NULL); */

/*   while (1) { */
/*     scm_vm_inst_code_t code; */

/*     SCM_VM_INST_FETCH_CODE(vm->iseq, vm->ip, code); */
/*     switch (code) { */
/*     case SCM_VM_INST_CODE_NOOP: */
/*       break; */
/*     case SCM_VM_INST_CODE_PUSH: */
/*       if (SCM_VM_CHECK_STACK_OVER_FLOW(vm)) */
/*         SCM_VM_PUSH_TO_STACK(vm, vm->val); */
/*         ; /\* TODO: handling stack overflow *\/ */
/*       break; */
/*     case SCM_VM_INST_CODE_POP: */
/*       if (SCM_VM_CHECK_STACK_UNDER_FLOW(vm)) */
/*         SCM_VM_POP_FROM_STACK(vm, vm->val); */
/*         ; /\* TODO: handling stack underflow *\/ */
/*       break; */
/*     case SCM_VM_INST_CODE_CALL: */
/*       scm_vm_inst_call(vm); */
/*       break; */
/*     case SCM_VM_INST_CODE_RET: */
/*       break; */
/*     case SCM_VM_INST_CODE_FRAME: */
/*       break; */
/*     }; */
/*   } */
/* } */

void
scm_vm_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  scm_obuffer_concatenate_string(obuffer, "#<VM>");
}

void
scm_vm_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_VM_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));

  SCM_VM_MEM(obj) = SCM_MEM(mem);

  /* vm->fp = NULL; */
  /* vm->cp = NULL; */
  /* vm->ip = NULL; */
  /* vm->val = NULL; */
  /* vm->iseq = NULL; */
  /* vm->stack = NULL; */
  SCM_VM_STACK(obj) = NULL;
  SCM_VM_STACK_SIZE(obj) = 0;
  SCM_VM_SP(obj) = NULL;
  SCM_VM_FP(obj) = NULL;
  SCM_VM_REF_STACK(obj) = NULL;
  SCM_VM_SYMTBL(obj) = SCM_OBJ_NULL;
  SCM_VM_PARENT_VM(obj) = SCM_OBJ_NULL;
  SCM_VM_PREV_VM(obj) = SCM_OBJ_NULL;
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

  /* if (vm->fp != NULL) { */
  /*   rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *vm->fp, mem); */
  /*   if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt; */
  /* } */

  /* rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->cp, mem); */
  /* if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt; */

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_SYMTBL(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_PARENT_VM(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_VM_PREV_VM(obj), mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  /* TODO: write call handler for vm->iseq */

  /* if (vm->stack != NULL) { */
  /*   ScmObj *p; */
  /*   for (p = vm->stack; p != vm->sp; p++) { */
  /*     rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *p, mem); */
  /*     if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt; */
  /*   } */
  /* } */

  rslt = scm_ref_stack_gc_accept(SCM_VM_REF_STACK(obj), obj, mem, handler);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  return rslt;
}

void
scm_vm_switch_vm(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  SCM_SETQ(SCM_VM_PREV_VM(vm), current_vm);
  SCM_SETQ(current_vm, vm);
}

void
scm_vm_revert_vm(void)
{
  if (SCM_OBJ_IS_NOT_NULL(current_vm))
    SCM_SETQ(current_vm, SCM_VM_PREV_VM(current_vm));
}

/* TODO: to inline */
ScmObj
scm_vm_current_vm(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return current_vm;
}

/* TODO: to inline */
ScmMem *
scm_vm_current_mm(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_MEM(current_vm);
}

/* TODO: to inline */
ScmRefStack *
scm_vm_current_ref_stack(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_REF_STACK(current_vm);
}

/* TODO: to inline */
ScmObj
scm_vm_current_symtbl(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_SYMTBL(current_vm);
}

/* TODO: to inline */
ScmObj
scm_vm_nil_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_NIL(current_vm);
}

/* TODO: to inline */
ScmObj
scm_vm_eof_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_EOF(current_vm);
}

/* TODO: to inline */
ScmObj
scm_vm_bool_true_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_BOOL_TRUE(current_vm);
}

/* TODO: to inline */
ScmObj
scm_vm_bool_false_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_BOOL_FALSE(current_vm);
}

/* 束縛変数の数を返す */
/* TODO: to inline */
int
scm_vm_nr_local_var(void)
{
  return scm_vm_frame_argc(scm_vm_current_vm());
}

/* 束縛変数を参照する。 box されている場合は unbox する */
/* TODO: to inline */
ScmObj
scm_vm_refe_local_var(int nth)
{
  /* box/unbox is not implemented */
  return scm_vm_frame_argv(scm_vm_current_vm(), nth);
}
