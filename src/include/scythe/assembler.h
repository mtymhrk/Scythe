#ifndef INCLUDE_ASSEMBLER_H__
#define INCLUDE_ASSEMBLER_H__

#include <sys/types.h>
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/earray.h"
#include "scythe/memory.h"

#define SCM_ASM_PI_START ((INT_MAX >> 1) + 1)

enum {
  SCM_ASM_PI_LABEL = SCM_ASM_PI_START,  /* define a label */
};

#define SCM_ASM_NR_PI 1
#define SCM_ASM_NR_OPD_MAX 3


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

typedef struct ScmAsmLabelDeclRec ScmAsmLabelDecl;
typedef struct ScmAsmLabelRefRec ScmAsmLabelRef;
typedef struct ScmAssemblerRec ScmAssembler;

struct ScmAsmLabelDeclRec {
  ssize_t offset;
};

struct ScmAsmLabelRefRec{
  size_t offset;
  size_t label_id;
};

struct ScmAssemblerRec {
  ScmObjHeader header;
  ScmObj iseq;
  EArray label_decl;
  EArray label_ref;
};

#define SCM_ASSEMBLER(obj) ((ScmAssembler *)(obj))
#define SCM_ASSEMBLER_ISEQ(a) (SCM_ASSEMBLER(a)->iseq)
#define SCM_ASSEMBLER_LABEL_DECL(a) (&SCM_ASSEMBLER(a)->label_decl)
#define SCM_ASSEMBLER_LABEL_REF(a) (&SCM_ASSEMBLER(a)->label_ref)

extern ScmTypeInfo SCM_ASSEMBLER_TYPE_INFO;

int scm_asm_initialize(ScmObj asmb, ScmObj iseq);
void scm_asm_finalize(ScmObj obj);
ScmObj scm_asm_new(scm_mem_type_t mtype, ScmObj iseq);
ssize_t scm_asm_assign_label_id(ScmObj asmb);
int scm_asm_register_label_id(ScmObj asmb, size_t id);
int scm_asm_push_inst_noopd(ScmObj asmb, scm_opcode_t op);
int scm_asm_push_inst_obj(ScmObj asmb, scm_opcode_t op, ScmObj obj);
int scm_asm_push_inst_obj_obj(ScmObj asmb,
                              scm_opcode_t op, ScmObj obj1, ScmObj obj2);
int scm_asm_push_inst_si(ScmObj asmb, scm_opcode_t op, int si);
int scm_asm_push_inst_si_si(ScmObj asmb, scm_opcode_t op, int si1, int si2);
int scm_asm_push_inst_si_si_obj(ScmObj asmb,
                                scm_opcode_t op, int si1, int si2, ScmObj obj);
int scm_asm_push_inst_iof(ScmObj asmb, scm_opcode_t op, bool label, ...);
int scm_asm_push_inst_rlid(ScmObj asmb, scm_opcode_t op, size_t id);
int scm_asm_push_pinst_label(ScmObj asmb, scm_opcode_t op, size_t id);
int scm_asm_push_va(ScmObj asmb, scm_opcode_t op, va_list operands);
int scm_asm_push(ScmObj asmb, scm_opcode_t op, ...);
int scm_asm_resolve_label_ref(ScmObj asmb);
void scm_asm_clear_labels(ScmObj asmb);
int scm_asm_commit(ScmObj asmb);
void scm_asm_gc_initialize(ScmObj obj);
void scm_asm_gc_finalize(ScmObj obj);
int scm_asm_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_assembler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_ASSEMBLER_TYPE_INFO);
}

static inline bool
scm_asm_uncommited_p(ScmObj asmb)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  return (EARY_SIZE(SCM_ASSEMBLER_LABEL_DECL(asmb)) > 0
          || EARY_SIZE(SCM_ASSEMBLER_LABEL_REF(asmb)) > 0);
}

static inline ScmObj
scm_asm_iseq(ScmObj asmb)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  return SCM_ASSEMBLER_ISEQ(asmb);
}

static inline ScmObj
scm_make_assembler(ScmObj iseq)
{
  return scm_asm_new(SCM_MEM_HEAP, iseq);
}


/**************************************************************************/
/* Disassembler                                                           */
/**************************************************************************/

enum {
  SCM_DISASM_TK_INST,
  SCM_DISASM_TK_LABEL,
  SCM_DISASM_TK_END,
};

typedef struct ScmDisasmTokenRec ScmDisasmToken;
typedef struct ScmDisassemblerRec ScmDisassembler;

struct ScmDisasmTokenRec {
  int type;
  struct {
    int fmt;
    union {
      scm_opcode_t op;
      struct scm_vm_inst_noopd     noopd;
      struct scm_vm_inst_obj       obj;
      struct scm_vm_inst_obj_obj   obj_obj;
      struct scm_vm_inst_si        si;
      struct scm_vm_inst_si_si     si_si;
      struct scm_vm_inst_si_si_obj si_si_obj;
      struct scm_vm_inst_iof       iof;
    } i;
  } inst;
  size_t label_id;
};

struct ScmDisassemblerRec {
  ScmObjHeader header;
  ScmObj iseq;
  EArray label_decl;
  scm_byte_t *ip;
  size_t decl_idx;
  ScmDisasmToken *token;
};

#define SCM_DISASSEMBLER(obj) ((ScmDisassembler *)(obj))
#define SCM_DISASSEMBLER_ISEQ(d) (SCM_DISASSEMBLER(d)->iseq)
#define SCM_DISASSEMBLER_LABEL_DECL(d) (&SCM_DISASSEMBLER(d)->label_decl)
#define SCM_DISASSEMBLER_IP(d) (SCM_DISASSEMBLER(d)->ip)
#define SCM_DISASSEMBLER_DECL_IDX(d) (SCM_DISASSEMBLER(d)->decl_idx)
#define SCM_DISASSEMBLER_TOKEN(d) (SCM_DISASSEMBLER(d)->token)

#define SCM_DISASSEMBLER_SET_IP(d, v) (SCM_DISASSEMBLER(d)->ip = (v))
#define SCM_DISASSEMBLER_ADD_IP(d, a) (SCM_DISASSEMBLER(d)->ip += (a))
#define SCM_DISASSEMBLER_SET_DECL_IDX(d, v) (SCM_DISASSEMBLER(d)->decl_idx = (v))
#define SCM_DISASSEMBLER_INC_DECL_IDX(d) (SCM_DISASSEMBLER(d)->decl_idx++)
#define SCM_DISASSEMBLER_SET_TOKEN(d, v) (SCM_DISASSEMBLER(d)->token = (v))

extern ScmTypeInfo SCM_DISASSEMBLER_TYPE_INFO;

int scm_disasm_initialize(ScmObj disasm, ScmObj iseq);
void scm_disasm_finalize(ScmObj disasm);
ScmObj scm_disasm_new(scm_mem_type_t mtype, ScmObj iseq);
const ScmDisasmToken *scm_disasm_token(ScmObj disasm);
int scm_disasm_next(ScmObj disasm);
void scm_disasm_rewind(ScmObj disasm);
int scm_disasm_cnv_to_marshalable(ScmObj disasm);
void scm_disasm_gc_initialize(ScmObj obj);
void scm_disasm_gc_finalize(ScmObj obj);
int scm_disasm_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_disassembler_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_DISASSEMBLER_TYPE_INFO);
}

static inline ScmObj
scm_make_disassembler(ScmObj iseq)
{
  return scm_disasm_new(SCM_MEM_HEAP, iseq);
}


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

int scm_asm_assemble_1inst_cv(ScmObj asmb, const ScmObj *inst, size_t n);
int scm_asm_assemble_1inst(ScmObj asmb, ScmObj inst);
int scm_asm_assemble(ScmObj asmb, ScmObj lst);
ScmObj scm_disasm_disassemble(ScmObj disasm);

ScmObj scm_assemble_1inst_cv(const ScmObj *inst, size_t n, ScmObj acc);
ScmObj scm_assemble_1inst(ScmObj inst, ScmObj acc);
ScmObj scm_assemble(ScmObj lst, ScmObj acc);
ScmObj scm_disassemble(ScmObj obj);


/**************************************************************************/
/* Printable/Unprintable                                                  */
/**************************************************************************/

ScmObj scm_unprintable_asm_inst(ScmObj inst);
ScmObj scm_printable_asm_inst(ScmObj inst);
ScmObj scm_unprintable_asm(ScmObj lst);
ScmObj scm_printable_asm(ScmObj lst);


/**************************************************************************/
/* etc                                                                    */
/**************************************************************************/

int scm_asm_mnemonic2opcode(const char *mne);
const char *scm_asm_opcode2mnemonic(scm_opcode_t code);
ScmObj scm_asm_mnemonic(scm_opcode_t opcode);
void scm_update_vminst_opd_iof(ScmObj iseq, scm_byte_t *ip, int iof);
int scm_update_vminst_opd_obj_obj_1(ScmObj clsr, scm_byte_t *ip, ScmObj obj);


#endif /* INCLUDE_ASSEMBLER_H__ */
