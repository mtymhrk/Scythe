#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef uintptr_t scm_vm_inst_t;
typedef struct ScmVMInstRec ScmVMInst;
typedef struct ScmVMEnvRec ScmVMEnv;
typedef struct ScmVMRec ScmVM;

#include "object.h"
#include "memory.h"

ScmVM *scm_vm_current_vm(void);
ScmVM *scm_vm_set_current_vm(ScmVM *vm);
ScmMem *scm_vm_current_mm(void);

#endif /* INCLUDE_VM_H__ */
