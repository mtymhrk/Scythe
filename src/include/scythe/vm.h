#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/vminst.h"
#include "scythe/vmstack.h"
#include "scythe/memory.h"

typedef struct ScmBedrockRec ScmBedrock;

extern ScmBedrock *scm__current_br;
extern ScmObj scm__current_vm;
extern ScmObj scm__current_ref_stack;

static inline ScmBedrock *
scm_current_br(void)
{
  return scm__current_br;
}

static inline ScmObj
scm_current_vm(void)
{
  return scm__current_vm;
}

static inline ScmObj
scm_current_ref_stack(void)
{
  return scm__current_ref_stack;
}

static inline void
scm_chg_current_br(ScmBedrock *br)
{
  scm__current_br = br;
}

static inline void
scm_chg_current_vm(ScmObj vm)
{
  scm__current_vm = vm;
}

static inline void
scm_chg_current_ref_stack(ScmObj stack)
{
  scm__current_ref_stack = stack;
}


/***************************************************************************/
/*  ScmBedrock                                                             */
/***************************************************************************/

typedef enum scm_bedrock_err_type scm_bedrock_err_type_t;

enum scm_bedrock_err_type {
  SCM_BEDROCK_ERR_NONE,
  SCM_BEDROCK_ERR_FATAL,
  SCM_BEDROCK_ERR_ERROR,
};

enum {
  SCM_CACHED_GV_COMPILE = 0,
  SCM_CACHED_GV_EVAL,
  SCM_CACHED_GV_CURRENT_INPUT_PORT,
  SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
  SCM_CACHED_GV_LOAD_PATH,
};

#define SCM_CACHED_GV_NR 5

enum {
  SCM_CACHED_SYM_QUOTE = 0,
  SCM_CACHED_SYM_QUASIQUOTE,
  SCM_CACHED_SYM_UNQUOTE,
  SCM_CACHED_SYM_UNQUOTE_SPLICING,
};

#define SCM_CACHED_SYM_NR 4

struct ScmBedrockRec {
  FILE *output;

  /*** Error Status ***/
  scm_bedrock_err_type_t err_type;

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

  /*** Symbols  ***/
  ScmObj sym[SCM_CACHED_SYM_NR];

  /*** Configurations ***/
  ScmEncoding *encoding;
};

int scm_bedrock_setup(ScmBedrock *br);
void scm_bedrock_cleanup(ScmBedrock *br);
int scm_bedrock_create_mem(ScmBedrock *br);
void scm_bedrock_delete_mem(ScmBedrock *br);
int scm_bedrock_initialize(ScmBedrock *br);
void scm_bedrock_finalize(ScmBedrock *br);
ScmBedrock *scm_bedrock_new(void);
void scm_bedrock_end(ScmBedrock *br);
void scm_bedrock_fatal(ScmBedrock *br, const char *msg);
void scm_bedrock_error(ScmBedrock *br, const char *msg);
bool scm_bedrock_fatal_p(ScmBedrock *br);
bool scm_bedrock_error_p(ScmBedrock *br);
int scm_bedrock_cached_gv(ScmBedrock *br, int kind, scm_csetter_t *gloc);
ScmObj scm_bedrock_cached_sym(ScmBedrock *br, int kind);

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

typedef struct ScmVMIntTblEntryRec ScmVMIntTblEntry;
typedef struct ScmVMIntTableRec ScmVMIntTable;

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

typedef struct ScmVMRegRec ScmVMReg;

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
  struct {
    ScmObj obj;                     /* raised object */
    ScmObj hndlr;                   /* exception handler */
  } exc;
  struct {
    ScmObj hndlr;                    /* dynamic wind handler*/
    size_t n;                        /* number of handlers */
  } dw;
  unsigned int flags;
};


/*******************************************************************/
/*  VM Continuation Capture                                        */
/*******************************************************************/

typedef struct ScmContCapRec ScmContCap;

struct ScmContCapRec {
  ScmObjHeader header;
  ScmObj stack;
  struct {
    ScmObj cp;
    scm_byte_t *ip;
    ScmObj val[SCM_VM_NR_VAL_REG];
    int vc;
    ScmObj prm;
    struct {
      ScmObj obj;
      ScmObj hndlr;
    } exc;
    struct {
      ScmObj hndlr;
      size_t n;
    } dw;
    unsigned int flags;
  } reg;
};

#define SCM_CONTCAP(obj) ((ScmContCap *)(obj))

extern ScmTypeInfo SCM_CONTCAP_TYPE_INFO;

ScmObj scm_contcap_new(scm_mem_type_t mtype);
void scm_contcap_cap(ScmObj cc,  ScmObj stack, const ScmVMReg *regs);
void scm_contcap_gc_initialize(ScmObj obj);
int scm_contcap_gc_accept(ScmObj obj, ScmGCRefHandler handler);

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
scm_contcap_exc_obj(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.exc.obj;
}

static inline ScmObj
scm_contcap_exc_hndlr(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.exc.hndlr;
}

static inline ScmObj
scm_contcap_dw_hndlr(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.dw.hndlr;
}

static inline size_t
scm_contcap_dw_num(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.dw.n;
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

typedef struct ScmVMRec ScmVM;
typedef enum scm_vm_ctrl_flg scm_vm_ctrl_flg_t;

enum scm_vm_ctrl_flg {
  SCM_VM_CTRL_FLG_RAISE = 0x00000001,
  SCM_VM_CTRL_FLG_UCF   = 0x00000002, /* cfp レジスタが、対応する call 令を実
                                         行していないフレームを指している場合
                                         セットする */
  SCM_VM_CTRL_FLG_CCF   = 0x00000004, /* cfp レジスタがキャプチャされたスタッ
                                         クセグメント上のフレームを指している
                                         場合セットする */
};

struct ScmVMRec {
  ScmObjHeader header;
  ScmObj main;
  ScmObj stack;
  ScmVMReg reg;
  ScmVMIntTable inttbl;
};

#define SCM_VM(obj) ((ScmVM *)(obj))

extern ScmTypeInfo SCM_VM_TYPE_INFO;

int scm_vm_subr_trmp_apply(ScmObj subr, int argc, const ScmObj *argv);

int scm_vm_initialize(ScmObj vm, ScmObj main_vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);
ScmObj scm_vm_clone(ScmObj parent);

void scm_vm_run(ScmObj vm, ScmObj iseq);
ScmObj scm_vm_apply(ScmObj vm, ScmObj proc, ScmObj args);
ScmObj scm_vm_run_cloned(ScmObj vm, ScmObj iseq);

int scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc);

ScmObj scm_vm_capture_cont(ScmObj vm);
int scm_vm_reinstatement_cont(ScmObj vm, ScmObj cc);
int scm_vm_push_dynamic_bindings(ScmObj vm, ScmObj alist);
void scm_vm_pop_dynamic_bindings(ScmObj vm);
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
int scm_vm_push_dw_handler(ScmObj vm, ScmObj before, ScmObj after);
int scm_vm_pop_dw_handler(ScmObj vm);
ScmObj scm_vm_collect_dw_handler(ScmObj vm, ScmObj contcap);

const void *scm_vm_opcode2ptr(scm_opcode_t op);
scm_opcode_t scm_vm_ptr2opcode(const void *ptr);

void scm_vm_gc_initialize(ScmObj obj);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_vm_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_VM_TYPE_INFO);
}

static inline ScmObj
scm_vm_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return SCM_VM(vm)->reg.exc.obj;
}

static inline bool
scm_vm_raised_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return scm_obj_not_null_p(SCM_VM(vm)->reg.exc.obj);
}

static inline void
scm_vm_discard_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  SCM_VM(vm)->reg.exc.obj = SCM_OBJ_NULL;
}


/***************************************************************************/
/*  Facade                                                                 */
/***************************************************************************/

#define SCM_NIL_OBJ (scm_bedrock_nil(scm_current_br()))
#define SCM_TRUE_OBJ (scm_bedrock_true(scm_current_br()))
#define SCM_FALSE_OBJ (scm_bedrock_false(scm_current_br()))
#define SCM_EOF_OBJ (scm_bedrock_eof(scm_current_br()))
#define SCM_UNDEF_OBJ (scm_bedrock_undef(scm_current_br()))
#define SCM_LANDMINE_OBJ (scm_bedrock_landmine(scm_current_br()))
#define SCM_UNINIT_OBJ SCM_LANDMINE_OBJ

int scm_cached_global_var_ref(int kind, scm_csetter_t *val);
int scm_cached_global_var_set(int kind, ScmObj val);

int scm_load_iseq(ScmObj iseq);

static inline int
scm_halt(void)
{
  return scm_vm_setup_stat_halt(scm_current_vm());
}

static inline void
scm_fatal(const char *msg)
{
  scm_bedrock_fatal(scm_current_br(), msg);
}

static inline ScmObj
scm_cached_symbol(int kind)
{
  return scm_bedrock_cached_sym(scm_current_br(), kind);
}

static inline ScmEncoding *
scm_system_encoding(void)
{
  return scm_bedrock_encoding(scm_current_br());
}

static inline void *
scm_current_memory_manager(void)
{
  return scm_bedrock_mem(scm_current_br());
}

static inline ScmObj
scm_current_symbol_table(void)
{
  return scm_bedrock_symtbl(scm_current_br());
}

static inline ScmObj
scm_current_module_tree(void)
{
  return scm_bedrock_modtree(scm_current_br());
}

static inline int
scm_return_val(const ScmObj *val, int vc)
{
  return scm_vm_set_val_reg(scm_current_vm(), val, vc);
}

static inline ScmObj
scm_capture_continuation(void)
{
  return scm_vm_capture_cont(scm_current_vm());
}

static inline int
scm_reinstantemnet_continuation(ScmObj cc)
{
  return scm_vm_reinstatement_cont(scm_current_vm(), cc);
}

static inline int
scm_push_dynamic_bindings(ScmObj alist)
{
  return scm_vm_push_dynamic_bindings(scm_current_vm(), alist);
}

static inline void
scm_pop_dynamic_bindings(void)
{
  scm_vm_pop_dynamic_bindings(scm_current_vm());
}

static inline ScmObj
scm_parameter_value(ScmObj var)
{
  return scm_vm_parameter_value(scm_current_vm(), var);
}

static inline int
scm_trampolining(ScmObj proc, ScmObj args, ScmObj postproc, ScmObj handover)
{
  return scm_vm_setup_stat_trmp(scm_current_vm(), proc, args,
                                postproc, handover, true);
}

static inline void
scm_exit(ScmObj obj)
{
  /* TODO: obj の内容に応じた VM の終了ステータスの設定*/

  scm_vm_setup_stat_halt(scm_current_vm());
}

static inline int
scm_raise(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_current_vm(), obj, false);
}

static inline int
scm_raise_continuable(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_current_vm(), obj, true);
}

static inline bool
scm_raised_p(void)
{
  return scm_vm_raised_p(scm_current_vm());
}

static inline ScmObj
scm_raised_obj(void)
{
  return scm_vm_raised_obj(scm_current_vm());
}

static inline void
scm_discard_raised_obj(void)
{
  scm_vm_discard_raised_obj(scm_current_vm());
}

static inline int
scm_push_exception_handler(ScmObj handler)
{
  return scm_vm_push_exc_handler(scm_current_vm(), handler);
}

static inline int
scm_pop_exception_handler(void)
{
  return scm_vm_pop_exc_handler(scm_current_vm());
}

static inline int
scm_push_dynamic_wind_handler(ScmObj before, ScmObj after)
{
  return scm_vm_push_dw_handler(scm_current_vm(), before, after);
}

static inline int
scm_pop_dynamic_wind_handler(void)
{
  return scm_vm_pop_dw_handler(scm_current_vm());
}

static inline ScmObj
scm_collect_dynamic_wind_handler(ScmObj contcap)
{
  return scm_vm_collect_dw_handler(scm_current_vm(), contcap);
}

static inline void
scm_disposal_unhandled_exec(void)
{
  scm_vm_disposal_unhandled_exc(scm_current_vm());
}

static inline scm_opcode_t
scm_internal_opcode(scm_opcode_t op)
{
  return (scm_opcode_t)scm_vm_opcode2ptr(op);
}

static inline scm_opcode_t
scm_external_opcode(scm_opcode_t op)
{
  return scm_vm_ptr2opcode((const void *)op);
}


#endif /* INCLUDE_VM_H__ */
