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
#include "obuffer.h"

const ScmTypeInfo SCM_VM_TYPE_INFO;

ScmVM *scm_vm_initialize(ScmVM *vm, ScmVM *parent);
ScmVM *scm_vm_finalize(ScmVM *vm);
ScmVM *scm_vm_construct(void);
void scm_vm_destruct(ScmVM *vm);
void scm_vm_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_vm_gc_initialize(ScmObj obj, ScmMem *mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmMem *mem, ScmGCRefHandlerFunc handler);

void scm_vm_switch_vm(ScmVM *vm);
void scm_vm_revert_vm(void);
ScmVM *scm_vm_current_vm(void);
ScmMem *scm_vm_current_mm(void);
ScmRefStack *scm_vm_current_ref_stack(void);

#endif /* INCLUDE_VM_H__ */
