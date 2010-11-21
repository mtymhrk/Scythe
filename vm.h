#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef uintptr_t scm_vm_inst_t;
typedef struct ScmVMInstRec ScmVMInst;
typedef struct ScmVMEnvRec ScmVMEnv;
typedef struct ScmVMRec ScmVM;

#define SCM_VM(obj) ((ScmVM *)(obj))

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "obuffer.h"

extern ScmTypeInfo SCM_VM_TYPE_INFO;

struct ScmVMRec {
  ScmObjHeader header;
  ScmObj *stack;                /* stack */
  size_t stack_size;            /* stack size */
  ScmObj *sp;                   /* stack pointer */
  ScmObj *fp;                    /* frame pointer */
  /* ScmObj cp;                    /\* closure pointer *\/ */
  /* scm_vm_inst_t *ip;            /\* instruction pointer *\/ */
  /* ScmObj val;                   /\* value register *\/ */
  /* ScmVMInst *iseq; */
  ScmMem *mem;
  ScmRefStack *ref_stack;
  ScmObj symtbl;
  ScmObj nil;
  ScmObj eof;
  ScmObj bool_true;
  ScmObj bool_false;
  ScmObj parent_vm;
  ScmObj prev_vm;
};

#define SCM_VM_MEM(obj) (SCM_VM(obj)->mem)
#define SCM_VM_STACK(obj) (SCM_VM(obj)->stack)
#define SCM_VM_STACK_SIZE(obj) (SCM_VM(obj)->stack_size)
#define SCM_VM_SP(obj) (SCM_VM(obj)->sp)
#define SCM_VM_SP_INC(obj) (SCM_VM_SP(obj)++)
#define SCM_VM_SP_DEC(obj) (SCM_VM_SP(obj)--)
#define SCM_VM_FP(obj) (SCM_VM(obj)->fp)
#define SCM_VM_REF_STACK(obj) (SCM_VM(obj)->ref_stack)
#define SCM_VM_SYMTBL(obj) (SCM_VM(obj)->symtbl)
#define SCM_VM_NIL(obj) (SCM_VM(obj)->nil)
#define SCM_VM_EOF(obj) (SCM_VM(obj)->eof)
#define SCM_VM_BOOL_TRUE(obj) (SCM_VM(obj)->bool_true)
#define SCM_VM_BOOL_FALSE(obj) (SCM_VM(obj)->bool_false)
#define SCM_VM_PARENT_VM(obj) (SCM_VM(obj)->parent_vm)
#define SCM_VM_PREV_VM(obj) (SCM_VM(obj)->prev_vm)

void scm_vm_initialize(ScmObj vm, ScmObj parent);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);
void scm_vm_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_vm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

void scm_vm_switch_vm(ScmObj vm);
void scm_vm_revert_vm(void);
ScmObj scm_vm_current_vm(void);
ScmMem *scm_vm_current_mm(void);
ScmRefStack *scm_vm_current_ref_stack(void);
ScmObj scm_vm_current_symtbl(void);
ScmObj scm_vm_nil_instance(void);
ScmObj scm_vm_eof_instance(void);
ScmObj scm_vm_bool_true_instance(void);
ScmObj scm_vm_bool_false_instance(void);

#endif /* INCLUDE_VM_H__ */
