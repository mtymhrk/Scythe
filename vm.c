#include <unistd.h>
#include <stdlib.h>

#include "vm.h"

void *
scm_vm_mem_allocate(ScmVM *vm, size_t size)
{
  return malloc(size);
}

void *
scm_vm_mem_release(ScmVM *vm, void *mem)
{
  free(mem);
  return NULL;
}
