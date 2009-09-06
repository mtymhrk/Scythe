#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>

#include "vm.h"
#include "memory.h"
#include "reference.h"
#include "object.h"
#include "obuffer.h"

#define SCM_VM_STACK_SIZE 1024
#define SCM_VM_REF_STACK_INIT_SIZE 512

typedef enum {
  SCM_VM_INST_CODE_NOOP,
  SCM_VM_INST_CODE_PUSH,
  SCM_VM_INST_CODE_POP,
  SCM_VM_INST_CODE_CALL,
  SCM_VM_INST_CODE_RET,
  SCM_VM_INST_CODE_FRAME,
} scm_vm_inst_code_t;

struct ScmVMEnvRec {
  ScmObj in_port;        /* is not target of GC */
  ScmObj out_port;       /* is not target of GC */
  ScmObj err_port;       /* is not target of GC */
  ScmMem *mem;
};

struct ScmVMInstRec {
  scm_vm_inst_t *iseq;
  size_t iseq_size;
};

#define SCM_VM_INST_FETCH_CODE(iseq, ip, code) (code) = (*(ip++) & 0xff)
#define SCM_VM_INST_FETCH_OBJ(iseq, ip, obj) (obj) = *(ScmObj *)(ip++)

struct ScmVMRec {
  ScmObjHeader header;
  ScmObj *stack;                /* stack */
  size_t stack_size;            /* stack size */
  ScmObj *sp;                   /* stack pointer */
  ScmObj *fp;                    /* frame pointer */
  ScmObj cp;                    /* closure pointer */
  scm_vm_inst_t *ip;            /* instruction pointer */
  ScmObj val;                   /* value register */
  ScmVMInst *iseq;
  ScmMem *mem;
  ScmRefStack *ref_stack;
  ScmVM *parent;
  ScmVM *prev_vm;
};

#define SCM_VM_CHECK_STACK_OVER_FLOW(vm) \
  ((size_t)((vm)->sp - (vm)->stack) < (vm)->stack_size - 1)
#define SCM_VM_CHECK_STACK_UNDER_FLOW(vm) ((size_t)((vm)->sp - (vm)->stack) < 1)
#define SCM_VM_PUSH_TO_STACK(vm, obj) (*((vm)->sp++) = obj)
#define SCM_VM_POP_FROM_STACK(vm, obj) (obj = *(--(vm)->sp) )

const ScmTypeInfo SCM_VM_TYPE_INFO = {
  SCM_OBJ_TYPE_STRING,          /* type                 */
  scm_vm_pretty_print,          /* pp_func              */
  sizeof(ScmVM),                /* obj_size             */
  scm_vm_gc_initialize,         /* gc_ini_func          */
  scm_vm_gc_finalize,           /* gc_fin_func          */
  scm_vm_gc_accept,             /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

static ScmVMEnv *global_env;
static ScmVM *current_vm;

static void
scm_vm_inst_call(ScmVM *vm)
{
  ;
}

ScmVM *
scm_vm_initialize(ScmVM *vm, ScmVM *parent)
{
  assert(vm != NULL);

  scm_obj_init(SCM_OBJ(vm), SCM_OBJ_TYPE_VM);

  vm->sp = NULL;
  vm->fp = NULL;
  vm->cp = NULL;
  vm->ip = NULL;
  vm->val = NULL;
  vm->iseq = NULL;
  vm->stack = NULL;
  vm->parent = parent;
  vm->prev_vm = NULL;

  vm->stack = scm_memory_allocate(sizeof(ScmObj) * SCM_VM_STACK_SIZE);
  if (vm->stack == NULL) goto err;

  vm->stack_size = SCM_VM_STACK_SIZE;  
  vm->sp = vm->stack;

  vm->ref_stack = scm_ref_stack_construct(SCM_VM_REF_STACK_INIT_SIZE);
  if (vm->ref_stack == NULL) goto err;
  
  return vm;

 err:
  if (vm->stack != NULL) {
    vm->stack = scm_memory_release(vm->stack);
    vm->sp = NULL;
  }
  if (vm->ref_stack != NULL) {
    scm_ref_stack_destruct(vm->ref_stack);
    vm->ref_stack = NULL;
  }
  return NULL;
}

ScmVM *
scm_vm_finalize(ScmVM *vm)
{
  vm->stack = scm_memory_release(vm->stack);
  scm_ref_stack_destruct(vm->ref_stack);
  vm->ref_stack = NULL;

  return vm;
}

ScmVM *
scm_vm_construct(void)
{
  ScmMem *mem;
  ScmObj vm;

  mem = scm_mem_construct();
  if (mem == NULL) return NULL;

  if (scm_mem_register_extra_rfrn(mem, SCM_REF_MAKE(current_vm))
      != SCM_REF_NULL)
    goto err;

  scm_mem_alloc_root(mem, SCM_OBJ_TYPE_VM, SCM_REF_MAKE(vm));
  if (vm == NULL) goto err;

  if (scm_vm_initialize(SCM_VM(vm), NULL) == NULL)
    goto err;

  return SCM_VM(vm);

 err:
  scm_mem_destruct(mem);
  return NULL;
}

void
scm_vm_destruct(ScmVM *vm)
{
  ScmMem *mem;

  assert(vm != NULL);

  scm_vm_finalize(vm);

  if (vm->parent == NULL)
    mem = vm->mem;
  else
    mem = NULL;

  scm_mem_free_root(vm->mem, SCM_OBJ(vm));

  if (mem != NULL)
    scm_mem_destruct(vm->mem);
}

void
scm_vm_run(ScmVM *vm)
{
  assert(vm != NULL);

  while (1) {
    scm_vm_inst_code_t code;

    SCM_VM_INST_FETCH_CODE(vm->iseq, vm->ip, code);
    switch (code) {
    case SCM_VM_INST_CODE_NOOP:
      break;
    case SCM_VM_INST_CODE_PUSH:
      if (SCM_VM_CHECK_STACK_OVER_FLOW(vm))
        SCM_VM_PUSH_TO_STACK(vm, vm->val);
        ; /* TODO: handling stack overflow */
      break;
    case SCM_VM_INST_CODE_POP:
      if (SCM_VM_CHECK_STACK_UNDER_FLOW(vm))
        SCM_VM_POP_FROM_STACK(vm, vm->val);
        ; /* TODO: handling stack underflow */
      break;
    case SCM_VM_INST_CODE_CALL:
      scm_vm_inst_call(vm);
      break;
    case SCM_VM_INST_CODE_RET:
      break;
    case SCM_VM_INST_CODE_FRAME:
      break;
    };
  }
}

void
scm_vm_pretty_print(ScmObj obj, ScmOBuffer *obuffer)
{
  scm_obuffer_concatenate_string(obuffer, "#<VM>");
}

void
scm_vm_gc_initialize(ScmObj obj, ScmMem *mem)
{
  ScmVM *vm;

  assert(obj != NULL);
  assert(mem != NULL);

  vm = SCM_VM(obj);
  vm->mem = mem;

  vm->sp = NULL;
  vm->fp = NULL;
  vm->cp = NULL;
  vm->ip = NULL;
  vm->val = NULL;
  vm->iseq = NULL;
  vm->stack = NULL;
  vm->parent = NULL;
  vm->prev_vm = NULL;
}

void
scm_vm_gc_finalize(ScmObj obj)
{
  scm_vm_finalize(SCM_VM(obj));
}

int
scm_vm_gc_accept(ScmObj obj, ScmMem *mem, ScmGCRefHandlerFunc handler)
{
  ScmVM *vm;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  assert(obj != NULL);
  assert(mem != NULL);
  assert(handler != NULL);

  vm = SCM_VM(obj);

  if (vm->fp != NULL) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *vm->fp, mem);
    if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
  }

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->cp, mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->parent, mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, vm->prev_vm, mem);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  /* TODO: write call handler for vm->iseq */

  if (vm->stack != NULL) {
    ScmObj *p;
    for (p = vm->stack; p != vm->sp; p++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *p, mem);
      if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
    }
  }

  rslt = scm_ref_stack_gc_accept(vm->ref_stack, obj, mem, handler);
  if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;

  return rslt;
}

void
scm_vm_switch_vm(ScmVM *vm)
{
  assert(vm != NULL);
  vm->prev_vm = current_vm;
  current_vm = vm;
}

void
scm_vm_revert_vm(void)
{
  if (current_vm != NULL)
    current_vm = current_vm->prev_vm;
}

ScmVM *
scm_vm_current_vm(void)
{
  return current_vm;
}

ScmMem *
scm_vm_current_mm(void)
{
  return (global_env == NULL) ? NULL : global_env->mem;
}

ScmRefStack *
scm_vm_current_ref_stack(void)
{
  return (current_vm == NULL) ? NULL : current_vm->ref_stack;
}
