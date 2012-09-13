#ifndef INCLUDE_VM_H__
#define INCLUDE_VM_H__

#include <stdint.h>

typedef struct ScmBedrockRec ScmBedrock;
typedef struct ScmBoxRec ScmBox;
typedef struct ScmEFBoxRec ScmEFBox;
typedef struct ScmVMRec ScmVM;

#define SCM_BOX(obj) ((ScmBox *)(obj))
#define SCM_EFBOX(obj) ((ScmEFBox *)(obj))
#define SCM_VM(obj) ((ScmVM *)(obj))

#include "object.h"
#include "memory.h"
#include "reference.h"
#include "api_enum.h"
#include "api_type.h"


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

  SCM_ENC_T encoding;

  ScmObj vm;
};

ScmBedrock *scm_bedrock_new(void);
void scm_bedrock_end(ScmBedrock *br);
void scm_bedrock_clean(ScmBedrock *br);
void scm_bedrock_fatal(ScmBedrock *br, const char *msg);
void scm_bedrock_fatal_fmt(ScmBedrock *br, const char *msgfmt, va_list ap);
bool scm_bedrock_fatal_p(ScmBedrock *br);
bool scm_bedrock_error_p(ScmBedrock *br);

inline void
scm_beadrock_bind_vm(ScmBedrock *br, ScmObj vm)
{
  br->vm = vm;
}

inline ScmObj
scm_bedrock_vm(ScmBedrock *br)
{
  return br->vm;
}

inline ScmBedrock *
scm_bedrock_current_br(void)
{
  return scm_bedrock__current_br;
}

inline void
scm_bedrock_change_current_br(ScmBedrock *br)
{
  scm_bedrock__current_br = br;
}

inline SCM_ENC_T
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


/***************************************************************************/
/*  ScmEnvFrameBox                                                         */
/***************************************************************************/

extern ScmTypeInfo SCM_EFBOX_TYPE_INFO;

struct ScmEFBoxRec {
  ScmObjHeader header;
  ScmEnvFrame frame;
};

int scm_efbox_initialize(ScmObj efb, ScmEnvFrame *ef);
ScmObj scm_efbox_new(SCM_MEM_TYPE_T mtype, ScmEnvFrame *ef);
void scm_efbox_gc_initialize(ScmObj obj, ScmObj mem);
int scm_efbox_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmEnvFrame *
scm_efbox_to_efp(ScmObj efb)
{
  scm_assert_obj_type_accept_null(efb, &SCM_EFBOX_TYPE_INFO);

  if (scm_obj_null_p(efb))
    return NULL;
  else
    return &(SCM_EFBOX(efb)->frame);
}

inline void
scm_efbox_update_outer(ScmObj efb, ScmObj outer)
{
  scm_assert_obj_type(efb, &SCM_EFBOX_TYPE_INFO);
  scm_assert_obj_type_accept_null(outer, &SCM_EFBOX_TYPE_INFO);

  if (scm_obj_null_p(outer))
    SCM_EFBOX(efb)->frame.out = NULL;
  else
    SCM_WB_EXP(efb, SCM_EFBOX(efb)->frame.out = scm_efbox_to_efp(outer));
}


/***************************************************************************/
/*  ScmVM                                                                  */
/***************************************************************************/

extern ScmTypeInfo SCM_VM_TYPE_INFO;
extern ScmObj scm_vm__current_vm;
extern ScmMem *scm_vm__current_mm;
  /* vm.c の外部が scm_vm__current_vm を直接参照するのは禁止。
     scm_vm_current_vm() 経由で取得すること。 */

typedef enum {
  SCM_VM_CTRL_FLG_HALT  = 0x00000001,
  SCM_VM_CTRL_FLG_RAISE = 0x00000002,
  SCM_VM_CTRL_FLG_PCF   = 0x00000004,
} SCM_VM_CTRL_FLG_T;


struct ScmVMRec {
  ScmObjHeader header;

  ScmBedrock *bedrock;
  ScmMem *mem;

  struct {
    ScmObj symtbl;                /* Symbol Table */
    ScmObj gloctbl;

    struct {
      ScmObj in;
      ScmObj out;
      ScmObj err;
    } stdio;

    struct {
      ScmObj in;
      ScmObj out;
    } curio;

    struct {
      ScmObj hndlr;
      ScmObj raised;
    } excpt;
  } ge;


  /*** VM Stack ***/
  uint8_t *stack;
  size_t stack_size;

  /*** VM Registers ***/
  struct {
    uint8_t *sp;                  /* stack pointer */
    ScmCntFrame *cfp;             /* continuation frame pointer */
    ScmEnvFrame *efp;             /* environment frame pointer */
    ScmEnvFrame *pefp;            /* partial environment frame pointer */
    ScmObj cp;                    /* closure pointer */
    uint8_t *ip;                  /* instruction pointer */
    ScmObj val;                   /* value register */
    uint32_t flags;
  } reg;

  /*** Constant Values ***/
  struct {
    ScmObj nil;
    ScmObj eof;
    ScmObj b_true;
    ScmObj b_false;
    ScmObj undef;
    ScmObj landmine;
  } cnsts;
};

/* private functions ******************************************************/

#ifdef SCM_UNIT_TEST

int scm_vm_setup_singletons(ScmObj vm);
void scm_vm_clean_singletons(ScmObj vm);
int scm_vm_setup_global_env(ScmObj vm);
void scm_vm_clean_global_env(ScmObj vm);
void scm_vm_clean_eval_env(ScmObj vm);

int scm_vm_stack_push(ScmObj vm, ScmObj elm);
/* ScmObj scm_vm_stack_pop(ScmObj vm); */

void scm_vm_ctrl_flg_set(ScmObj vm, SCM_VM_CTRL_FLG_T flg);
void scm_vm_ctrl_flg_clr(ScmObj vm, SCM_VM_CTRL_FLG_T flg);
bool scm_vm_ctrl_flg_set_p(ScmObj vm, SCM_VM_CTRL_FLG_T flg);

int scm_vm_update_ief_len_if_needed(ScmObj vm);
ScmCntFrame *scm_vm_next_cfp(ScmCntFrame *cfp);
bool scm_vm_next_cfp_partial_p(ScmCntFrame *cfp);
ptrdiff_t scm_vm_calc_cframe_cfp_val(ScmObj vm, ScmCntFrame *new_cfp);
int scm_vm_make_cframe(ScmObj vm, ScmEnvFrame * efp, ScmObj cp);
int scm_vm_commit_cframe(ScmObj vm, uint8_t *ip);
int scm_vm_make_eframe(ScmObj vm, size_t nr_arg);
int scm_vm_commit_eframe(ScmObj vm, ScmEnvFrame *efp, size_t nr_arg);
int scm_vm_cancel_eframe(ScmObj vm);
int scm_vm_box_eframe(ScmObj vm, ScmEnvFrame *efp,
                      size_t depth, scm_csetter_t *box);
ScmEnvFrame *scm_vm_eframe_list_ref(ScmEnvFrame *efp_list, size_t n);
ScmObj scm_vm_eframe_arg_ref(ScmEnvFrame *efp_list,
                             size_t idx, size_t layer, ScmEnvFrame **efp);
void scm_vm_return_to_caller(ScmObj vm);

ScmObj scm_vm_make_trampolining_code(ScmObj vm, ScmObj clsr, ScmObj args,
                                     ScmObj callback);
ScmObj scm_vm_make_exception_handler_code(ScmObj vm);
int scm_vm_setup_to_call_exception_handler(ScmObj vm);

int scm_vm_do_op_call(ScmObj vm, SCM_OPCODE_T op,
                      uint32_t argc, bool tail_p);
int scm_vm_do_op_push(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_frame(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_eframe(ScmObj vm, SCM_OPCODE_T op);
int scm_vm_do_op_ecommit(ScmObj vm, SCM_OPCODE_T op, size_t argc);

void scm_vm_op_undef(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_call(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_immval(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_push(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_frame(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_cframe(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_eframe(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_ecommit(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_epop(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_erebind(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_return(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_gref(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_gdef(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_gset(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_sref(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_sset(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_cref(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_cset(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_jmp(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_jmpt(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_jmpf(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_raise(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_box(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_unbox(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_close(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_demine(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_emine(ScmObj vm, SCM_OPCODE_T op);
void scm_vm_op_edemine(ScmObj vm, SCM_OPCODE_T op);

int scm_vm_gc_accept_eframe(ScmObj vm, ScmEnvFrame **efp,
                            ScmObj mem, ScmGCRefHandlerFunc handler);
int scm_vm_gc_accept_cframe(ScmObj vm, ScmCntFrame *cfp,
                            ScmObj mem, ScmGCRefHandlerFunc handler);
int scm_vm_gc_accept_stack(ScmObj vm, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_vm_register_val(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->reg.val;
}

#endif

/* public functions ******************************************************/

void scm_vm_initialize(ScmObj vm, ScmBedrock *bedrock);
int scm_vm_init_scmobjs(ScmObj vm);
void scm_vm_finalize(ScmObj vm);
ScmObj scm_vm_new(void);
void scm_vm_end(ScmObj vm);

int scm_vm_setup_system(ScmObj vm);
void scm_vm_run(ScmObj vm, ScmObj iseq);

int scm_vm_setup_stat_trmp(ScmObj vm, ScmObj target, ScmObj args,
                           ScmObj (*callback)(int argc, ScmObj *argv));
void scm_vm_setup_stat_halt(ScmObj vm);
int scm_vm_setup_stat_raised(ScmObj vm, ScmObj obj);
int scm_vm_clear_stat_raised(ScmObj vm);
bool scm_vm_raised_p(ScmObj vm);
int scm_vm_push_exception_handler(ScmObj vm, ScmObj hndlr);
bool scm_vm_eframe_is_in_stack_p(ScmObj vm, ScmEnvFrame *efp);
bool scm_vm_eframe_is_in_heap_p(ScmObj vm, ScmEnvFrame *ef);

void scm_vm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vm_gc_finalize(ScmObj obj);
int scm_vm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

inline ScmObj
scm_vm_symtbl(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.symtbl;
}

inline ScmObj
scm_vm_gloctbl(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.gloctbl;
}

inline ScmObj
scm_vm_standard_input_port(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.stdio.in;
}

inline ScmObj
scm_vm_standard_output_port(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.stdio.out;
}

inline ScmObj
scm_vm_standard_error_port(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.stdio.err;
}

inline ScmObj
scm_vm_current_input_port(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.curio.in;
}

inline ScmObj
scm_vm_current_output_port(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->ge.curio.out;
}

inline ScmObj
scm_vm_nil(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->cnsts.nil;
}

inline ScmObj
scm_vm_eof(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->cnsts.eof;
}

inline ScmObj
scm_vm_true(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->cnsts.b_true;
}

inline ScmObj
scm_vm_false(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->cnsts.b_false;
}

inline ScmObj
scm_vm_undef(ScmObj vm)
{
  scm_assert_obj_type(vm, &SCM_VM_TYPE_INFO);

  return SCM_VM(vm)->cnsts.undef;
}


inline ScmObj
scm_vm_current_vm(void)
{
  return scm_vm__current_vm;
}

inline ScmMem *
scm_vm_current_mm(void)
{
  return scm_vm__current_mm;
}

inline void
scm_vm_change_current_vm(ScmObj vm)
{
  scm_vm__current_vm = vm;
  scm_vm__current_mm = SCM_VM(vm)->mem;
  if (scm_obj_not_null_p(vm))
    scm_bedrock_change_current_br(SCM_VM(vm)->bedrock);
}


#endif /* INCLUDE_VM_H__ */
