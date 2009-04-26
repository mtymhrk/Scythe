#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>

#include "vm.h"
#include "memory.h"
#include "reference.h"
#include "object.h"

#define SCM_VM_STACK_SIZE 1024

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
  ScmObj *stack;                /* stack */
  size_t stack_size;            /* stack size */
  ScmObj *sp;                   /* stack pointer */
  ScmObj *fp;                   /* frame pointer */
  ScmObj *cp;                   /* closure pointer */
  scm_vm_inst_t *ip;            /* instruction pointer */
  ScmObj val;                   /* value register */
  ScmVMInst *iseq;
  ScmMem *mem;
  ScmRefStack *ref_stack;
};

#define SCM_VM_CHECK_STACK_OVER_FLOW(vm) \
  ((size_t)((vm)->sp - (vm)->stack) < (vm)->stack_size - 1)
#define SCM_VM_CHECK_STACK_UNDER_FLOW(vm) ((size_t)((vm)->sp - (vm)->stack) < 1)
#define SCM_VM_PUSH_TO_STACK(vm, obj) (*((vm)->sp++) = obj)
#define SCM_VM_POP_FROM_STACK(vm, obj) (obj = *(--(vm)->sp) )

static ScmVMEnv *global_env;
static ScmVM *current_vm;

static void
scm_vm_inst_call(ScmVM *vm)
{
  ;
}

static ScmVM *
scm_vm_initialize(ScmVM *vm, ScmVM *parent)
{
  assert(vm != NULL);

  vm->stack = malloc(sizeof(ScmObj) * SCM_VM_STACK_SIZE);
  if (vm->stack == NULL) return NULL;

  vm->stack_size = SCM_VM_STACK_SIZE;
  vm->sp = vm->stack;
  vm->fp = NULL;
  vm->cp = NULL;
  vm->ip = NULL;
  vm->val = NULL;
  vm->iseq = NULL;
  if (scm_mem_attach_vm(global_env->mem, vm) == NULL) goto err;

  return vm;

 err:
  free(vm->stack);
  return NULL;
}

ScmVM *
scm_vm_construct(ScmVMEnv *env)
{
  ScmVM *vm;

  vm = malloc(sizeof(ScmVM));
  if (vm == NULL) return NULL;

  if (scm_vm_initialize(vm, NULL) == NULL) {
    free(vm);
    return NULL;
  }

  return vm;
}

void
scm_vm_destruct(ScmVM *vm)
{
  assert(vm != NULL);

  //  scm_mem_clean(vm->mem);

  free(vm->stack);
  free(vm);
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

ScmVM *
scm_vm_current_vm(void)
{
  return current_vm;
}

ScmVM *
scm_vm_set_current_vm(ScmVM *vm)
{
  return (current_vm = vm);
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
