#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef struct ScmVMIntTblEntryRec ScmVMIntTblEntry;
typedef struct ScmVMIntTableRec ScmVMIntTable;
typedef struct ScmVMRegRec ScmVMReg;
typedef struct ScmContCapRec ScmContCap;
typedef struct ScmVMRec ScmVM;

#define SCM_CONTCAP(obj) ((ScmContCap *)(obj))
#define SCM_VM(obj) ((ScmVM *)(obj))

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/vmstack.h"


/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

typedef enum {
  SCM_BEDROCK_ERR_NONE,
  SCM_BEDROCK_ERR_FATAL,
  SCM_BEDROCK_ERR_ERROR,
} SCM_BEDROCK_ERROR_TYPE_T;



struct ScmBedrockRec {
  FILE *output;

  /*** Error Status ***/
  SCM_BEDROCK_ERROR_TYPE_T err_type;

  /* Exit Status */
  int exit_stat;

  /*** Constant Values ***/
  struct {
    ScmObj nil;
    ScmObj eof;
    ScmObj b_true;
    ScmObj b_false;
    ScmObj undef;
    ScmObj landmine;
  } cnsts;

  /*** Memory Manager ***/
  ScmMem *mem;

  /*** Symbol Table ***/
  ScmObj symtbl;

  /*** Module Table ***/
  ScmObj modtree;

  /*** Subrutines ***/
  struct {
    ScmObj exc_hndlr_caller;
    ScmObj exc_hndlr_caller_cont;
    ScmObj exc_hndlr_caller_post;
    ScmObj trmp_apply;
  } subr;

  /*** Global Variables  ***/
  ScmObj gv[SCM_CACHED_GV_NR];

  /*** Configurations ***/
  ScmEncoding *encoding;
};

int scm_bedrock_setup(ScmBedrock *br);
int scm_bedrock_cleanup(ScmBedrock *br);
void scm_bedrock_set_mem(ScmBedrock *br, ScmMem *mem);
int scm_bedrock_initialize(ScmBedrock *br);
void scm_bedrock_finalize(ScmBedrock *br);
ScmBedrock *scm_bedrock_new(void);
void scm_bedrock_end(ScmBedrock *br);
void scm_bedrock_fatal(ScmBedrock *br, const char *msg);
void scm_bedrock_error(ScmBedrock *br, const char *msg);
bool scm_bedrock_fatal_p(ScmBedrock *br);
bool scm_bedrock_error_p(ScmBedrock *br);
int scm_bedrock_cached_gv(ScmBedrock *br, int kind, scm_csetter_t *gloc);

static inline ScmObj
scm_bedrock_nil(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.nil;
}

static inline ScmObj
scm_bedrock_eof(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.eof;
}

static inline ScmObj
scm_bedrock_true(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_true;
}

static inline ScmObj
scm_bedrock_false(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_false;
}

static inline ScmObj
scm_bedrock_undef(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.undef;
}

static inline ScmObj
scm_bedrock_landmine(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.landmine;
}

static inline ScmMem *
scm_bedrock_mem(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->mem;
}

static inline ScmObj
scm_bedrock_symtbl(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->symtbl;
}

static inline ScmObj
scm_bedrock_modtree(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->modtree;
}

static inline ScmObj
scm_bedrock_exc_hndlr_caller(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller;
}

static inline ScmObj
scm_bedrock_exc_hndlr_caller_cont(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller_cont;
}

static inline ScmObj
scm_bedrock_exc_hndlr_caller_post(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller_post;
}

static inline ScmObj
scm_bedrock_trmp_apply(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.trmp_apply;
}

static inline ScmEncoding *
scm_bedrock_encoding(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->encoding;
}


/*******************************************************************/
/*  VM Interruptions                                               */
/*******************************************************************/

enum {
  SCM_VM_INT_GC = 0,
  SCM_VM_INT_HALT,
  SCM_VM_INT_RAISE,
  SCM_VM_INT_RAISE_CONT,
  SCM_VM_INT_RETURN,
};

#define SCM_VM_NR_INTERRUPTIONS 5

struct ScmVMIntTblEntryRec {
  int (*func)(ScmObj vm);
  scm_byte_t *save;
};

struct ScmVMIntTableRec {
  ScmVMIntTblEntry table[SCM_VM_NR_INTERRUPTIONS];
  unsigned int activated;
};


/*******************************************************************/
/*  VM Registers                                                   */
/*******************************************************************/

#define SCM_VM_NR_VAL_REG 10

struct ScmVMRegRec {
  scm_byte_t *sp;                 /* stack pointer */
  ScmCntFrame *cfp;               /* continuation frame pointer */
  ScmEnvFrame *efp;               /* environment frame pointer */
  int partial;                    /* partial environment frame */
  ScmObj cp;                      /* closure pointer */
  scm_byte_t *ip;                 /* instruction pointer */
  ScmObj val[SCM_VM_NR_VAL_REG];  /* value register */
  int vc;                         /* value count */
  ScmObj prm;                     /* dynamic bindings */
  ScmObj exc;                     /* raised object */
  ScmObj hndlr;                   /* exception handler */
  unsigned int flags;
};


/*******************************************************************/
/*  VM Continuation Capture                                        */
/*******************************************************************/


extern ScmTypeInfo SCM_CONTCAP_TYPE_INFO;

struct ScmContCapRec {
  ScmObjHeader header;
  ScmObj stack;
  struct {
    ScmObj cp;
    scm_byte_t *ip;
    ScmObj val[SCM_VM_NR_VAL_REG];
    int vc;
    ScmObj prm;
    ScmObj exc;
    ScmObj hndlr;
    unsigned int flags;
  } reg;
};

ScmObj scm_contcap_new(SCM_MEM_TYPE_T mtype);
void scm_contcap_cap(ScmObj cc,  ScmObj stack, const ScmVMReg *regs);
void scm_contcap_replace_val(ScmObj cc, const ScmObj *val, int vc);
void scm_contcap_replace_ip(ScmObj cc, scm_byte_t *ip, ScmObj cp);
void scm_contcap_gc_initialize(ScmObj obj, ScmObj mem);
int scm_contcap_gc_accepct(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmObj
scm_contcap_stack(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->stack;
}

static inline ScmObj
scm_contcap_cp(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.cp;
}

static inline scm_byte_t *
scm_contcap_ip(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.ip;
}

static inline const ScmObj *
scm_contcap_val(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.val;
}

static inline int
scm_contcap_vc(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.vc;
}

static inline ScmObj
scm_contcap_prm(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.prm;
}

static inline ScmObj
scm_contcap_exc(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.exc;
}

static inline ScmObj
scm_contcap_hndlr(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.hndlr;
}

static inline uint
scm_contcap_flags(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.flags;
}


/***************************************************************************/
/*  ScmVM                                                                  */
/***************************************************************************/

extern ScmTypeInfo SCM_VM_TYPE_INFO;

typedef enum {
  SCM_VM_CTRL_FLG_RAISE = 0x00000001,
  SCM_VM_CTRL_FLG_UCF   = 0x00000002, /* cfp レジスタが、対応する call 令を実
                                         行していないフレームを指している場合
                                         セットする */
  SCM_VM_CTRL_FLG_CCF   = 0x00000004, /* cfp レジスタがキャプチャされたスタッ
                                         クセグメント上のフレームを指している
                                         場合セットする */
} SCM_VM_CTRL_FLG_T;


struct ScmVMRec {
  ScmObjHeader header;
  ScmObj main;
  ScmObj stack;
  ScmVMReg reg;
  ScmVMIntTable inttbl;
};

int scm_vm_subr_trmp_apply(ScmObj subr, int argc, const ScmObj *argv);

int scm_vm_bootup(void);
void scm_vm_shutdown(void);
int scm_vm_initialize(ScmObj vm, ScmObj main_vm);
int scm_vm_init_scmobjs(ScmObj vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_clone(ScmObj parent);

void scm_vm_run(ScmObj vm, ScmObj iseq);
ScmObj scm_vm_apply(ScmObj vm, ScmObj proc, ScmObj args);
ScmObj scm_vm_run_cloned(ScmObj vm, ScmObj iseq);

int scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc);

ScmObj scm_vm_capture_cont(ScmObj vm);
int scm_vm_reinstatement_cont(ScmObj vm, ScmObj cc, const ScmObj *val, int vc);
ScmObj scm_vm_parameter_value(ScmObj vm, ScmObj var);
int scm_vm_setup_stat_trmp(ScmObj vm, ScmObj proc, ScmObj args,
                           ScmObj postproc, ScmObj handover, bool tail);
int scm_vm_setup_stat_halt(ScmObj vm);
int scm_vm_setup_stat_raise(ScmObj vm, ScmObj obj, bool continuable);
int scm_vm_setup_stat_return(ScmObj vm);
int scm_vm_setup_stat_call_exc_hndlr(ScmObj vm);
int scm_vm_setup_stat_call_exc_hndlr_cont(ScmObj vm);
int scm_vm_push_exc_handler(ScmObj vm, ScmObj hndlr);
int scm_vm_pop_exc_handler(ScmObj vm);
int scm_vm_exc_handler(ScmObj vm, scm_csetter_t *hndlr);
int scm_vm_subr_exc_hndlr_caller(ScmObj subr, int argc, const ScmObj *argv);
int scm_vm_subr_exc_hndlr_caller_cont(ScmObj subr,
                                      int argc, const ScmObj *argv);
int scm_vm_subr_exc_hndlr_caller_post(ScmObj subr,
                                      int argc, const ScmObj *argv);
void scm_vm_disposal_unhandled_exc(ScmObj vm);
void scm_vm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

static inline ScmObj
scm_vm_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return SCM_VM(vm)->reg.exc;
}

static inline bool
scm_vm_raised_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return scm_obj_not_null_p(SCM_VM(vm)->reg.exc);
}

static inline void
scm_vm_discard_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  SCM_VM(vm)->reg.exc = SCM_OBJ_NULL;
}


#endif /* INCLUDE_VM_H__ */
