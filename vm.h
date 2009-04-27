#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef uintptr_t scm_vm_inst_t;
typedef struct ScmVMInstRec ScmVMInst;
typedef struct ScmVMEnvRec ScmVMEnv;
typedef struct ScmVMRec ScmVM;

#define SCM_VM(obj) ((ScmVM *)obj)

#include "object.h"
#include "memory.h"
#include "reference.h"

ScmVM *scm_vm_initialize(ScmVM *vm, ScmVM *parent);
ScmVM *scm_vm_finalize(ScmVM *vm);
ScmVM *scm_vm_construct(void);
void scm_vm_destruct(ScmVM *vm);

ScmVM *scm_vm_current_vm(void);
ScmVM *scm_vm_set_current_vm(ScmVM *vm);
ScmMem *scm_vm_current_mm(void);
ScmRefStack *scm_vm_current_ref_stack(void);

#endif /* INCLUDE_VM_H__ */
