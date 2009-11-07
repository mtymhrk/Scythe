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

/* #define SCM_VM_STACK_SIZE 1024 */
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

void
scm_vm_initialize(ScmObj vm, ScmObj parent)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE_ACCEPT_NULL(parent, &SCM_VM_TYPE_INFO);

  scm_obj_init(SCM_OBJ(vm), &SCM_VM_TYPE_INFO);

  SCM_VM_PARENT_VM(vm) = parent;
  SCM_VM_PREV_VM(vm) = SCM_OBJ_NULL;

  /* vm->stack = scm_memory_allocate(sizeof(ScmObj) * SCM_VM_STACK_SIZE); */
  /* if (vm->stack == NULL) goto err; */

  /* vm->stack_size = SCM_VM_STACK_SIZE; */
  /* vm->sp = vm->stack; */

  SCM_VM_REF_STACK(vm) = scm_ref_stack_construct(SCM_VM_REF_STACK_INIT_SIZE);
  if (SCM_VM_REF_STACK(vm) == NULL) goto err;

  return;

 err:
  /* if (vm->stack != NULL) { */
  /*   vm->stack = scm_memory_release(vm->stack); */
  /*   vm->sp = NULL; */
  /* } */
  if (SCM_VM_REF_STACK(vm) != NULL) {
    scm_ref_stack_destruct(SCM_VM_REF_STACK(vm));
    SCM_VM_REF_STACK(vm) = NULL;
  }
  return;
}

void
scm_vm_finalize(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  /* vm->stack = scm_memory_release(vm->stack); */
  scm_ref_stack_destruct(SCM_VM_REF_STACK(vm));
  SCM_VM_REF_STACK(vm) = NULL;
}

void
scm_vm_setup_root(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  scm_vm_switch_vm(vm);

  SCM_SETQ(SCM_VM_SYMTBL(vm), scm_symtable_construct(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_SYMTBL(vm)))
    ;                           /* TODO: error handling */

  /* scm_mem_alloc_root(SCM_VM(vm)->mem, &SCM_SYMTABLE_TYPE_INFO, */
  /*                    SCM_REF_MAKE(SCM_VM_SYMTBL(vm))); */
  /* if (SCM_OBJ_IS_NULL(SCM_VM_SYMTBL(vm))) */
  /*   ;                           /\* TODO: error handling *\/ */

  /* scm_symtable_initialize(SCM_VM(vm)->symtbl); */

  SCM_SETQ(SCM_VM_NIL(vm), scm_nil_construct(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_NIL(vm)))
    ;                           /* TODO: error handling */

  /* scm_mem_alloc_root(SCM_VM(vm)->mem, &SCM_NIL_TYPE_INFO, */
  /*                    SCM_REF_MAKE(SCM_VM_NIL(vm))); */
  /* if (SCM_OBJ_IS_NULL(SCM_VM_NIL(vm))) */
  /*   ;                           /\* TODO: error handling *\/ */

  /* scm_nil_initialize(SCM_VM_NIL(vm)); */

  SCM_SETQ(SCM_VM_EOF(vm), scm_eof_construct(SCM_MEM_ALLOC_ROOT));
  if (SCM_OBJ_IS_NULL(SCM_VM_EOF(vm)))
    ;                           /* TODO: error handling */

  /* scm_mem_alloc_root(SCM_VM_MEM(vm), &SCM_EOF_TYPE_INFO, */
  /*                    SCM_REF_MAKE(SCM_VM_EOF(vm))); */
  /* if (SCM_OBJ_IS_NULL(SCM_VM_EOF(vm))) */
  /*   ;                           /\* TODO: error handling *\/ */

  /* scm_eof_initialize(SCM_VM_EOF(vm)); */


  SCM_SETQ(SCM_VM_BOOL_TRUE(vm), scm_bool_construct(SCM_MEM_ALLOC_ROOT, true));
  if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_TRUE(vm)))
    ;                           /* TODO: error handling */

  /* scm_mem_alloc_root(SCM_VM_MEM(vm), &SCM_BOOL_TYPE_INFO, */
  /*                    SCM_REF_MAKE(SCM_VM_BOOL_TRUE(vm))); */
  /* if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_TRUE(vm))) */
  /*   ;                           /\* TODO: error handling *\/ */

  /* scm_bool_initialize(SCM_VM_BOOL_TRUE(vm), true); */

  SCM_SETQ(SCM_VM_BOOL_FALSE(vm),
           scm_bool_construct(SCM_MEM_ALLOC_ROOT, false));
  if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_FALSE(vm)))
    ;                           /* TODO: error handling */

  /* scm_mem_alloc_root(SCM_VM_MEM(vm), &SCM_BOOL_TYPE_INFO, */
  /*                    SCM_REF_MAKE(SCM_VM_BOOL_FALSE(vm))); */
  /* if (SCM_OBJ_IS_NULL(SCM_VM_BOOL_FALSE(vm))) */
  /*   ;                           /\* TODO: error handling *\/ */

  /* scm_bool_initialize(SCM_VM_BOOL_FALSE(vm), false); */


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

  scm_mem_destruct(SCM_VM_MEM(vm));
}

ScmObj
scm_vm_construct(void)
{
  ScmMem *mem;
  ScmObj vm;

  mem = scm_mem_construct();
  if (mem == NULL) return NULL;

  if (scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(current_vm))
      == SCM_REF_NULL)
    goto err;

  scm_mem_alloc_root(mem, &SCM_VM_TYPE_INFO, SCM_REF_MAKE(vm));
  if (vm == NULL) goto err;

  scm_vm_initialize(vm, NULL);
  scm_vm_setup_root(vm);

  return vm;

 err:
  scm_mem_destruct(mem);
  return NULL;
}

void
scm_vm_destruct(ScmObj vm)
{
  SCM_OBJ_ASSERT_TYPE(vm, &SCM_VM_TYPE_INFO);

  if (SCM_VM_PARENT_VM(vm) == NULL) /* root vm */
    scm_vm_clean_root(vm);
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

  /* vm->sp = NULL; */
  /* vm->fp = NULL; */
  /* vm->cp = NULL; */
  /* vm->ip = NULL; */
  /* vm->val = NULL; */
  /* vm->iseq = NULL; */
  /* vm->stack = NULL; */
  SCM_VM_REF_STACK(obj) = NULL;
  SCM_VM_SYMTBL(obj) = NULL;
  SCM_VM_PARENT_VM(obj) = NULL;
  SCM_VM_PREV_VM(obj) = NULL;
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

ScmObj
scm_vm_current_vm(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return current_vm;
}

ScmMem *
scm_vm_current_mm(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_MEM(current_vm);
}

ScmRefStack *
scm_vm_current_ref_stack(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_REF_STACK(current_vm);
}

ScmObj
scm_vm_current_symtbl(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_SYMTBL(current_vm);
}

ScmObj
scm_vm_nil_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_NIL(current_vm);
}

ScmObj
scm_vm_eof_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_EOF(current_vm);
}

ScmObj
scm_vm_bool_true_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_BOOL_TRUE(current_vm);
}

ScmObj
scm_vm_bool_false_instance(void)
{
  SCM_OBJ_ASSERT_TYPE(current_vm, &SCM_VM_TYPE_INFO);
  return SCM_VM_BOOL_FALSE(current_vm);
}
