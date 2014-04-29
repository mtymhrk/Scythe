#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef struct ScmBedrockRec ScmBedrock;
typedef struct ScmBoxRec ScmBox;
typedef struct ScmVMRegRec ScmVMReg;
typedef struct ScmContCapRec ScmContCap;
typedef struct ScmVMRec ScmVM;

#define SCM_BOX(obj) ((ScmBox *)(obj))
#define SCM_CONTCAP(obj) ((ScmContCap *)(obj))
#define SCM_VM(obj) ((ScmVM *)(obj))

#include "object.h"
#include "memory.h"
#include "encoding.h"
#include "api_enum.h"
#include "vmstack.h"


/* vm.c の外部が scm__current_XXX を直接参照するのは禁止。
   scm_vm_current_XXX() 経由で取得すること。 */
extern ScmBedrock *scm__current_br;
extern ScmObj scm__current_vm;
extern ScmObj scm__current_ref_stack;


inline ScmBedrock *
scm_vm_current_br(void)
{
  return scm__current_br;
}

inline ScmObj
scm_vm_current_vm(void)
{
  return scm__current_vm;
}

inline ScmObj
scm_vm_current_ref_stack(void)
{
  return scm__current_ref_stack;
}

inline void
scm_vm_chg_current_br(ScmBedrock *br)
{
  scm__current_br = br;
}

inline void
scm_vm_chg_current_vm(ScmObj vm)
{
  scm__current_vm = vm;
}

inline void
scm_vm_chg_current_ref_stack(ScmObj stack)
{
  scm__current_ref_stack = stack;
}


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
  } subr;

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

inline ScmObj
scm_bedrock_nil(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.nil;
}

inline ScmObj
scm_bedrock_eof(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.eof;
}

inline ScmObj
scm_bedrock_true(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_true;
}

inline ScmObj
scm_bedrock_false(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.b_false;
}

inline ScmObj
scm_bedrock_undef(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.undef;
}

inline ScmObj
scm_bedrock_landmine(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->cnsts.landmine;
}

inline ScmMem *
scm_bedrock_mem(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->mem;
}

inline ScmObj
scm_bedrock_symtbl(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->symtbl;
}

inline ScmObj
scm_bedrock_modtree(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->modtree;
}

inline ScmObj
scm_bedrock_exc_hndlr_caller(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller;
}

inline ScmObj
scm_bedrock_exc_hndlr_caller_cont(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller_cont;
}

inline ScmObj
scm_bedrock_exc_hndlr_caller_post(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->subr.exc_hndlr_caller_post;
}

inline ScmEncoding *
scm_bedrock_encoding(ScmBedrock *br)
{
  scm_assert(br != NULL);
  return br->encoding;
}


/***************************************************************************/
/*  ScmBox                                                                 */
/***************************************************************************/

extern ScmTypeInfo SCM_BOX_TYPE_INFO;

struct ScmBoxRec {
  ScmObjHeader header;
  ScmObj obj;
};

void scm_box_gc_initialize(ScmObj obj, ScmObj mem);
int scm_box_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_box_unbox(ScmObj box)
{
  scm_assert_obj_type(box, &SCM_BOX_TYPE_INFO);
  return SCM_BOX(box)->obj;
}

inline void
scm_box_update(ScmObj box, ScmObj obj)
{
  scm_assert_obj_type(box, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmBox, box, obj, obj);
}


/*******************************************************************/
/*  VM Registers                                                   */
/*******************************************************************/

#define SCM_VM_NR_VAL_REG 10

struct ScmVMRegRec {
  scm_byte_t *sp;                 /* stack pointer */
  ScmCntFrame *cfp;               /* continuation frame pointer */
  ScmEnvFrame *efp;               /* environment frame pointer */
  ScmEnvFrame *pefp;              /* partial environment frame pointer */
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

inline ScmObj
scm_contcap_stack(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->stack;
}

inline ScmObj
scm_contcap_cp(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.cp;
}

inline scm_byte_t *
scm_contcap_ip(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.ip;
}

inline const ScmObj *
scm_contcap_val(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.val;
}

inline int
scm_contcap_vc(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.vc;
}

inline ScmObj
scm_contcap_prm(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.prm;
}

inline ScmObj
scm_contcap_exc(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.exc;
}

inline ScmObj
scm_contcap_hndlr(ScmObj cc)
{
  scm_assert_obj_type(cc, &SCM_CONTCAP_TYPE_INFO);

  return SCM_CONTCAP(cc)->reg.hndlr;
}

inline uint
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
  SCM_VM_CTRL_FLG_HALT  = 0x00000001,
  SCM_VM_CTRL_FLG_RAISE = 0x00000002,
  SCM_VM_CTRL_FLG_PCF   = 0x00000004, /* cfp レジスタが作りかけ(対応する call
                                         命令を実行していない)フレームを指して
                                         いる場合セットする */
  SCM_VM_CTRL_FLG_PEF   = 0x00000008, /* pefp レジスタが指すフレームが efp や
                                         cfp レジスタが指すフレームよりもス
                                         タックの上にある場合セットする */
  SCM_VM_CTRL_FLG_CCF   = 0x00000010, /* cfp レジスタがキャプチャされたスタッ
                                         クセグメント上のフレームを指している
                                         場合セットする */
} SCM_VM_CTRL_FLG_T;


struct ScmVMRec {
  ScmObjHeader header;
  ScmObj stack;
  ScmVMReg reg;
};

/* private functions ******************************************************/

#ifdef SCM_UNIT_TEST

/* int scm_vm_stack_push(ScmObj vm, ScmObj elm); */
/* ScmObj scm_vm_stack_pop(ScmObj vm); */

void scm_vm_ctrl_flg_set(ScmObj vm, SCM_VM_CTRL_FLG_T flg);
void scm_vm_ctrl_flg_clr(ScmObj vm, SCM_VM_CTRL_FLG_T flg);
bool scm_vm_ctrl_flg_set_p(ScmObj vm, SCM_VM_CTRL_FLG_T flg);

int scm_vm_update_pef_len_if_needed(ScmObj vm);
int scm_vm_copy_pef_to_top_of_stack_if_needed(ScmObj vm);
ScmObj scm_vm_capture_stack(ScmObj vm);
int scm_vm_restore_stack(ScmObj vm, ScmObj stack);
int scm_vm_handle_stack_overflow(ScmObj vm);
int scm_vm_handle_stack_underflow(ScmObj vm);

int scm_vm_make_cframe(ScmObj vm, ScmEnvFrame *efp, ScmObj cp);
int scm_vm_commit_cframe(ScmObj vm, scm_byte_t *ip);
int scm_vm_pop_cframe(ScmObj vm);
int scm_vm_make_eframe(ScmObj vm, size_t nr_arg);
int scm_vm_commit_eframe(ScmObj vm, ScmEnvFrame *efp, size_t nr_arg);
int scm_vm_cancel_eframe(ScmObj vm);
int scm_vm_pop_eframe(ScmObj vm);

int scm_vm_box_eframe(ScmObj vm, ScmEnvFrame *efp,
                      size_t depth, scm_csetter_t *box);
ScmEnvFrame *scm_vm_eframe_list_ref(ScmEnvFrame *efp_list, size_t n);
ScmObj scm_vm_eframe_arg_ref(ScmEnvFrame *efp_list,
                             size_t idx, size_t layer, ScmEnvFrame **efp);

int scm_vm_push_dynamic_bindings(ScmObj vm, ScmObj *param, size_t n);
int scm_vm_pop_dynamic_bindings(ScmObj vm);

ScmObj scm_vm_make_trampolining_code(ScmObj vm, ScmObj proc, ScmObj args,
                                     ScmObj postproc, ScmObj handover);

int scm_vm_cmp_arity(int argc, int arity, bool unwished);
int scm_vm_adjust_arg_to_arity(ScmObj vm, int argc, ScmObj proc, int *adjusted);
int scm_vm_do_op_return(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_call(ScmObj vm, SCM_OPCODE_T op, int argc, bool tail_p);
int scm_vm_do_op_push(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_mvpush(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_frame(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_eframe(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_ecommit(ScmObj vm, SCM_OPCODE_T op, size_t argc);

int scm_vm_op_undef(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_apply(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_frame(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_mvpush(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_cframe(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_eframe(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_ecommit(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_epop(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_erebind(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_gref(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_sref(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_sset(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_cref(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_cset(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_jmpt(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_jmpf(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_box(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_close(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_demine(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_emine(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_edemine(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_op_arity(ScmObj vm, SCM_OPCODE_T op);

int scm_vm_gc_accept_stack(ScmObj vm, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_vm_register_val(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->reg.val[0];
}

#endif

/* public functions ******************************************************/

int scm_vm_bootup(void);
void scm_vm_shutdown(void);
int scm_vm_initialize(ScmObj vm);
int scm_vm_init_scmobjs(ScmObj vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);

void scm_vm_run(ScmObj vm, ScmObj iseq);

int scm_vm_set_val_reg(ScmObj vm, const ScmObj *val, int vc);

ScmObj scm_vm_capture_cont(ScmObj vm);
int scm_vm_reinstatement_cont(ScmObj vm, ScmObj cc, const ScmObj *val, int vc);
ScmObj scm_vm_parameter_value(ScmObj vm, ScmObj var);
int scm_vm_setup_stat_trmp(ScmObj vm, ScmObj proc, ScmObj args,
                           ScmObj postproc, ScmObj handover);
void scm_vm_setup_stat_halt(ScmObj vm);
int scm_vm_setup_stat_raise(ScmObj vm, ScmObj obj);
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

inline ScmObj
scm_vm_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return SCM_VM(vm)->reg.exc;
}

inline bool
scm_vm_raised_p(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  return scm_obj_not_null_p(SCM_VM(vm)->reg.exc);
}

inline void
scm_vm_discard_raised_obj(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);
  SCM_VM(vm)->reg.exc = SCM_OBJ_NULL;
}


#endif /* INCLUDE_VM_H__ */
