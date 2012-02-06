#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef struct ScmVMInstRec ScmVMInst;
typedef struct ScmVMEnvRec ScmVMEnv;
typedef struct ScmVMRec ScmVM;

#define SCM_VM(obj) ((ScmVM *)(obj))

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "instractions.h"

typedef scm_uword_t scm_vm_inst_t;
typedef scm_uword_t scm_vm_stack_val_t;

extern ScmTypeInfo SCM_VM_TYPE_INFO;
extern ScmObj scm_vm__current_vm;
  /* vm.c の外部が scm_vm__current_vm を直接参照するのは禁止。
     scm_vm_current_vm() 経由で取得すること。 */

struct ScmVMRec {
  ScmObjHeader header;

  ScmMem *mem;
  ScmObj symtbl;                /* Symbol Table */
  ScmObj gloctbl;

  /*** VM Stack ***/
  scm_vm_stack_val_t *stack;
  unsigned int *stack_objmap;
  size_t stack_size;

  /*** C Lang Stack ***/
  ScmRefStack *ref_stack;

  /*** VM Registers ***/
  scm_vm_stack_val_t *sp;                   /* stack pointer */
  scm_vm_stack_val_t *fp;                    /* frame pointer */
  /* ScmObj cp;                    /\* closure pointer *\/ */
  scm_iword_t *ip;            /* instruction pointer */
  ScmObj iseq;                  /* instruction sequence object */
  ScmObj val;                   /* value register */

  /*** Constant Values ***/
  struct {
    ScmObj nil;
    ScmObj eof;
    ScmObj b_true;
    ScmObj b_false;
  } cnsts;
};

#define SCM_VM_MEM(obj) (SCM_VM(obj)->mem)
#define SCM_VM_SYMTBL(obj) (SCM_VM(obj)->symtbl)
#define SCM_VM_GLOCTBL(obj) (SCM_VM(obj)->gloctbl)
#define SCM_VM_STACK(obj) (SCM_VM(obj)->stack)
#define SCM_VM_STACK_OBJMAP(obj) (SCM_VM(obj)->stack_objmap)
#define SCM_VM_STACK_SIZE(obj) (SCM_VM(obj)->stack_size)
#define SCM_VM_REF_STACK(obj) (SCM_VM(obj)->ref_stack)
#define SCM_VM_SP(obj) (SCM_VM(obj)->sp)
#define SCM_VM_FP(obj) (SCM_VM(obj)->fp)
#define SCM_VM_IP(obj) (SCM_VM(obj)->ip)
#define SCM_VM_ISEQ(obj) (SCM_VM(obj)->iseq)
#define SCM_VM_ISEQ_SETQ(obj, v) SCM_SETQ(SCM_VM_ISEQ(obj), v)
#define SCM_VM_VAL(obj) (SCM_VM(obj)->val)
#define SCM_VM_VAL_SETQ(obj, v) SCM_SETQ(SCM_VM_VAL(vm), v)
#define SCM_VM_CONST_NIL(obj) (SCM_VM(obj)->cnsts.nil)
#define SCM_VM_CONST_EOF(obj) (SCM_VM(obj)->cnsts.eof)
#define SCM_VM_CONST_TRUE(obj) (SCM_VM(obj)->cnsts.b_true)
#define SCM_VM_CONST_FALSE(obj) (SCM_VM(obj)->cnsts.b_false)

#define SCM_VM_SP_INC(obj) (SCM_VM_SP(obj)++)
#define SCM_VM_SP_DEC(obj) (SCM_VM_SP(obj)--)

#define SCM_VM_STACk_OBJMAP_SP2IDX(vm, sp) \
  ((scm_uword_t)((sp) - SCM_VM_STACK(vm)) / (sizeof(*SCM_VM_STACK_OBJMAP(vm)) * CHAR_BIT))
#define SCM_VM_STACK_OBJMAP_SP2MASK(vm, sp) \
  (1u << (scm_uword_t)((sp) - SCM_VM_STACK(vm)) % (sizeof(*SCM_VM_STACK_OBJMAP(vm)) * CHAR_BIT))

#define SCM_VM_STACK_OBJMAP_SET(vm, sp)                          \
  do {                                                           \
    assert((sp) < SCM_VM_SP(vm));                                \
    SCM_VM_STACK_OBJMAP(vm)[SCM_VM_STACk_OBJMAP_SP2IDX(vm, sp)]  \
      |= SCM_VM_STACK_OBJMAP_SP2MASK(vm, sp);                    \
  } while(0)

#define SCM_VM_STACK_OBJMAP_UNSET(vm, sp)                        \
  do {                                                           \
    assert((sp) < SCM_VM_SP(vm));                                \
    SCM_VM_STACK_OBJMAP(vm)[SCM_VM_STACk_OBJMAP_SP2IDX(vm, sp)]  \
      &= ~SCM_VM_STACK_OBJMAP_SP2MASK(vm, sp);                   \
  } while(0)

#define SCM_VM_STACK_OBJMAP_IS_SCMOBJ(vm, sp, rslt)  \
  do {                                        \
    assert((sp) < SCM_VM_SP(vm));             \
    (rslt) = SCM_VM_STACK_OBJMAP(vm)[SCM_VM_STACk_OBJMAP_SP2IDX(vm, sp)] & SCM_VM_STACK_OBJMAP_SP2MASK(vm, sp) ? true : false; \
  } while(0)

void scm_vm_initialize(ScmObj vm);
int scm_vm_init_scmobjs(ScmObj vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);

void scm_vm_stack_push(ScmObj vm, scm_vm_stack_val_t elm, bool scmobj_p);
ScmObj scm_vm_stack_pop(ScmObj vm);
void scm_vm_stack_shorten(ScmObj vm, int n);

int scm_vm_frame_argc(ScmObj vm);
ScmObj scm_vm_frame_argv(ScmObj vm, int nth);
ScmObj *scm_vm_frame_outer_frame(ScmObj vm);
ScmObj scm_vm_frame_iseq(ScmObj vm);
scm_iword_t *scm_vm_frame_next_inst(ScmObj vm);

void scm_vm_op_frame(ScmObj vm);
void scm_vm_op_return(ScmObj vm, ScmObj val);


int scm_vm_nr_local_var(ScmObj vm);
ScmObj scm_vm_refer_local_var(ScmObj vm, int nth);

void scm_vm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


static inline ScmObj
scm_vm_current_vm(void)
{
  return scm_vm__current_vm;
}

static inline ScmMem *
scm_vm_current_mm(void)
{
  return SCM_VM_MEM(scm_vm__current_vm);
}

static inline ScmRefStack *
scm_vm_current_ref_stack(void)
{
  return SCM_VM_REF_STACK(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_current_symtbl(void)
{
  return SCM_VM_SYMTBL(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_current_gloctbl(void)
{
  return SCM_VM_GLOCTBL(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_nil_instance(void)
{
  return SCM_VM_CONST_NIL(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_eof_instance(void)
{
  return SCM_VM_CONST_EOF(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_bool_true_instance(void)
{
  return SCM_VM_CONST_TRUE(scm_vm__current_vm);
}

static inline ScmObj
scm_vm_bool_false_instance(void)
{
  return SCM_VM_CONST_FALSE(scm_vm__current_vm);
}

#endif /* INCLUDE_VM_H__ */
