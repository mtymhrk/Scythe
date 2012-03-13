#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef struct ScmBedrockRec ScmBedrock;
typedef struct ScmVMRec ScmVM;

#define SCM_VM(obj) ((ScmVM *)(obj))

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "api.h"

/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

typedef enum {
  SCM_BEDROCK_ERR_NONE,
  SCM_BEDROCK_ERR_FATAL,
  SCM_BEDROCK_ERR_ERROR,
} SCM_BEDROCK_ERROR_TYPE_T;

extern ScmBedrock *scm_bedrock__current_br;

struct ScmBedrockRec {
  /*** C Lang Stack ***/
  ScmRefStack *ref_stack;

  /*** Error Status ***/
  struct {
    SCM_BEDROCK_ERROR_TYPE_T type;
    char *message;
  } err;
};

ScmBedrock *scm_bedrock_new(void);
void scm_bedrock_end(ScmBedrock *br);
void scm_bedrock_clean(ScmBedrock *br);
void scm_bedrock_fatal(ScmBedrock *br, const char *msg);
void scm_bedrock_fatal_fmt(ScmBedrock *br, const char *msgfmt, va_list ap);
bool scm_bedrock_fatal_p(ScmBedrock *br);
bool scm_bedrock_error_p(ScmBedrock *br);

inline ScmBedrock *
scm_bedrock_current_br(void)
{
  return scm_bedrock__current_br;
}

/***************************************************************************/
/*  ScmVM                                                                  */
/***************************************************************************/

typedef scm_uword_t scm_vm_inst_t;

extern ScmTypeInfo SCM_VM_TYPE_INFO;
extern ScmObj scm_vm__current_vm;
  /* vm.c の外部が scm_vm__current_vm を直接参照するのは禁止。
     scm_vm_current_vm() 経由で取得すること。 */

struct ScmVMRec {
  ScmObjHeader header;

  ScmBedrock *bedrock;

  ScmMem *mem;
  ScmObj symtbl;                /* Symbol Table */
  ScmObj gloctbl;

  /*** VM Stack ***/
  scm_vm_stack_val_t *stack;
  unsigned int *stack_objmap;
  size_t stack_size;

  /*** VM Registers ***/
  struct {
    scm_vm_stack_val_t *sp;                   /* stack pointer */
    scm_vm_stack_val_t *fp;                    /* frame pointer */
    /* ScmObj cp;                    /\* closure pointer *\/ */
    scm_iword_t *ip;            /* instruction pointer */
    ScmObj iseq;                  /* instruction sequence object */
    ScmObj val;                   /* value register */
  } reg;

  /*** Constant Values ***/
  struct {
    ScmObj nil;
    ScmObj eof;
    ScmObj b_true;
    ScmObj b_false;
  } cnsts;

  /*** Trampolining ***/
  struct {
    ScmObj code;
  } trmp;
};

/* private functions ******************************************************/

#ifdef SCM_UNIT_TEST

size_t scm_vm_stack_objmap_sp2idx(ScmObj vm, scm_vm_stack_val_t *sp);
unsigned int scm_vm_stack_objmap_sp2mask(ScmObj vm, scm_vm_stack_val_t *sp);
void scm_vm_stack_objmap_set(ScmObj vm , scm_vm_stack_val_t *sp);
void scm_vm_stack_objmap_unset(ScmObj vm, scm_vm_stack_val_t *sp);
bool scm_vm_stack_objmap_is_scmobj(ScmObj vm, scm_vm_stack_val_t *sp);

scm_iword_t scm_vm_inst_fetch(ScmObj vm);

void scm_vm_setup_singletons(ScmObj vm);
void scm_vm_clean_singletons(ScmObj vm);
scm_local_func void scm_vm_clean_eval_env(ScmObj vm);

void scm_vm_stack_push(ScmObj vm, scm_vm_stack_val_t elm, bool scmobj_p);
ScmObj scm_vm_stack_pop(ScmObj vm);
void scm_vm_stack_shorten(ScmObj vm, int n);

int scm_vm_frame_argc(ScmObj vm);
ScmObj scm_vm_frame_argv(ScmObj vm, int nth);
void scm_vm_return_to_caller(ScmObj vm);

void scm_vm_op_call(ScmObj vm);
void scm_vm_op_immval(ScmObj vm, size_t imm_idx);
void scm_vm_op_push(ScmObj vm);
void scm_vm_op_push_immval(ScmObj vm, ScmObj val);
void scm_vm_op_push_primval(ScmObj vm, scm_sword_t val);
void scm_vm_op_frame(ScmObj vm);
void scm_vm_op_return(ScmObj vm);
void scm_vm_op_gref(ScmObj vm, size_t imm_idx);
void scm_vm_op_gdef(ScmObj vm, size_t imm_idx);
void scm_vm_op_gset(ScmObj vm, size_t imm_idx);

#endif

/* public functions ******************************************************/

void scm_vm_initialize(ScmObj vm, ScmBedrock *bedrock);
int scm_vm_init_scmobjs(ScmObj vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);

void scm_vm_setup_system(ScmObj vm);
void scm_vm_run(ScmObj vm, ScmObj iseq);

int scm_vm_nr_local_var(ScmObj vm);
ScmObj scm_vm_refer_local_var(ScmObj vm, int nth);

int scm_vm_setup_trampolining(ScmObj vm, ScmObj target, ScmObj args,
                              ScmObj (*callback)(void));

void scm_vm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


inline ScmObj
scm_vm_current_vm(void)
{
  return scm_vm__current_vm;
}

inline ScmMem *
scm_vm_current_mm(void)
{
  return SCM_VM(scm_vm__current_vm)->mem;
}

inline ScmObj
scm_vm_current_symtbl(void)
{
  return SCM_VM(scm_vm__current_vm)->symtbl;
}

inline ScmObj
scm_vm_current_gloctbl(void)
{
  return SCM_VM(scm_vm__current_vm)->gloctbl;
}

inline ScmObj
scm_vm_nil_instance(void)
{
  return SCM_VM(scm_vm__current_vm)->cnsts.nil;
}

inline ScmObj
scm_vm_eof_instance(void)
{
  return SCM_VM(scm_vm__current_vm)->cnsts.eof;
}

inline ScmObj
scm_vm_bool_true_instance(void)
{
  return SCM_VM(scm_vm__current_vm)->cnsts.b_true;
}

inline ScmObj
scm_vm_bool_false_instance(void)
{
  return SCM_VM(scm_vm__current_vm)->cnsts.b_false;
}

#endif /* INCLUDE_VM_H__ */
