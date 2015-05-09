#ifndef INCLUDE_ASSEMBLER_H__
#define INCLUDE_ASSEMBLER_H__

#include <stdint.h>
#include <stdbool.h>

typedef struct ScmAssemblerRec ScmAssembler;
typedef struct ScmDisassemblerRec ScmDisassembler;

#define SCM_ASSEMBLER(obj) ((ScmAssembler *)(obj))
#define SCM_DISASSEMBLER(obj) ((ScmDisassembler *)(obj))

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/earray.h"
#include "scythe/fcd_type.h"


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

typedef struct {
  ssize_t offset;
} ScmAsmLabelDecl;

typedef struct {
  size_t offset;
  size_t label_id;
} ScmAsmLabelRef;

extern ScmTypeInfo SCM_ASSEMBLER_TYPE_INFO;

struct ScmAssemblerRec {
  ScmObjHeader header;
  ScmObj iseq;
  EArray label_decl;
  EArray label_ref;
};

#define SCM_ASSEMBLER_ISEQ(a) (SCM_ASSEMBLER(a)->iseq)
#define SCM_ASSEMBLER_LABEL_DECL(a) (&SCM_ASSEMBLER(a)->label_decl)
#define SCM_ASSEMBLER_LABEL_REF(a) (&SCM_ASSEMBLER(a)->label_ref)

int scm_asm_initialize(ScmObj asmb, ScmObj iseq);
void scm_asm_finalize(ScmObj obj);
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
int scm_asm_resolve_label_ref(ScmObj asmb);
void scm_asm_clear_labels(ScmObj asmb);
void scm_asm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_asm_gc_finalize(ScmObj obj);
int scm_asm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

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


/**************************************************************************/
/* Disassembler                                                           */
/**************************************************************************/

extern ScmTypeInfo SCM_DISASSEMBLER_TYPE_INFO;

struct ScmDisassemblerRec {
  ScmObjHeader header;
  ScmObj iseq;
  EArray label_decl;
  scm_byte_t *ip;
  size_t decl_idx;
  ScmDisasmToken *token;
};

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


int scm_disasm_initialize(ScmObj disasm, ScmObj iseq);
void scm_disasm_finalize(ScmObj disasm);
const ScmDisasmToken *scm_disasm_token(ScmObj disasm);
int scm_disasm_next(ScmObj disasm);
void scm_disasm_rewind(ScmObj disasm);
int scm_disasm_cnv_to_marshalable(ScmObj disasm);
void scm_disasm_gc_initialize(ScmObj obj, ScmObj mem);
void scm_disasm_gc_finalize(ScmObj obj);
int scm_disasm_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

int scm_asm_assemble_1inst_cv(ScmObj asmb, const ScmObj *inst, size_t n);
int scm_asm_assemble_1inst(ScmObj asmb, ScmObj inst);
int scm_asm_assemble(ScmObj asmb, ScmObj lst);
ScmObj scm_asm_disassemble(ScmObj asmb);

ScmObj scm_asm_unprintable_inst(ScmObj inst);
ScmObj scm_asm_printable_inst(ScmObj inst);
ScmObj scm_asm_unprintable(ScmObj lst);
ScmObj scm_asm_printable(ScmObj lst);

int scm_asm_mnemonic2opcode(const char *mne);
const char *scm_asm_opcode2mnemonic(scm_opcode_t code);
ScmObj scm_asm_mnemonic(scm_opcode_t opcode);


#endif /* INCLUDE_ASSEMBLER_H__ */
