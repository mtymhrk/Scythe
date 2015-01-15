#ifndef INCLUDE_ASSEMBLER_H__
#define INCLUDE_ASSEMBLER_H__

#include <stdint.h>
#include <stdbool.h>

typedef struct ScmAssemblerRec ScmAssembler;

#define SCM_ASSEMBLER(obj) ((ScmAssembler *)(obj))

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/earray.h"


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

static inline ScmObj
scm_asm_iseq(ScmObj asmb)
{
  scm_assert_obj_type(asmb, &SCM_ASSEMBLER_TYPE_INFO);
  return SCM_ASSEMBLER_ISEQ(asmb);
}


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

#define SCM_ASM_PI_START 0x10000

enum {
  SCM_ASM_PI_LABEL = SCM_ASM_PI_START,  /* define a label */
};

int scm_asm_assemble(ScmObj asmb, ScmObj lst);

int scm_asm_mnemonic2opcode(const char *mne);
const char *scm_asm_opcode2mnemonic(scm_opcode_t code);
ScmObj scm_asm_mnemonic(scm_opcode_t opcode);


#endif /* INCLUDE_ASSEMBLER_H__ */
