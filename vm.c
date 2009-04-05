#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <unistd.h>

#include "vm.h"
#include "memory.h"

#define SCM_VM_STACK_SIZE 1024

struct ScmVMInstRec {
  scm_vm_inst_t *iseq;
  size_t iseq_size;
};

#define SCM_VM_INST_FETCH_CODE(iseq, ip, code) (code) = (*(ip++) & 0xff)
#define SCM_VM_INST_FETCH_OBJ(iseq, ip, obj) (obj) = *(ScmObj *)(ip++)

struct ScmVMRec {
  ScmObj *stack;                /* stack */
  ScmObj *sp;                   /* stack pointer */
  ScmObj *fp;                   /* frame pointer */
  ScmObj *cp;                   /* closure */
  scm_vm_inst_t *ip;            /* instruction pointer */

  ScmMem *mem;
};

static ScmVM *
scm_vm_initialize(ScmVM *vm, ScmVM *parent, ScmMem *mem)
{
  assert(vm != NULL);
  assert(mem != NULL);

  vm->stack = malloc(sizeof(ScmObj) * SCM_VM_STACK_SIZE);
  if (vm->stack == NULL) return NULL;

  vm->sp = vm->stack;
  vm->fp = NULL;
  vm->cp = NULL;
  vm->ip = NULL;
  vm->mem = mem;
  if (scm_mem_attach_vm(mem, vm) == NULL) goto err;

  return vm;

 err:
  free(vm->stack);
  return NULL;
}

ScmVM *
scm_vm_construct(ScmVM *parent, ScmMem *mem)
{
  ScmVM *vm;

  vm = malloc(sizeof(ScmVM));
  if (vm == NULL) return NULL;

  if (scm_vm_initialize(vm, parent, mem) == NULL) {
    free(vm);
    return NULL;
  }

  return vm;
}

void
scm_vm_destruct(ScmVM *vm)
{
  assert(vm != NULL);

  scm_mem_clean(vm->mem);

  free(vm->stack);
  free(vm);
}
