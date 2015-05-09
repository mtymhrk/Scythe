#ifndef INCLUDE_FCD_ASSEMBLER_H__
#define INCLUDE_FCD_ASSEMBLER_H__

#include <limits.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/fcd_type.h"


#define SCM_ASM_PI_START ((INT_MAX >> 1) + 1)

enum {
  SCM_ASM_PI_LABEL = SCM_ASM_PI_START,  /* define a label */
  SCM_ASM_PI_UNDEF,                     /* (immval #<undef>) */
  SCM_ASM_PI_UNINIT,                    /* (immval #<landmine>) */
  SCM_ASM_PI_QQTEMPLATE,                /* (immval #<qq-template>) */
};

#define SCM_ASM_NR_PI 3

#define SCM_ASM_NR_OPD_MAX 3


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

bool scm_fcd_assembler_p(ScmObj obj);
ScmObj scm_fcd_assembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq);
ScmObj scm_fcd_make_assembler(ScmObj iseq);
ssize_t scm_fcd_assembler_assign_label_id(ScmObj asmb);
int scm_fcd_assembler_register_label_id(ScmObj asmb, size_t id);
int scm_fcd_assembler_push_va(ScmObj asmb, scm_opcode_t op, va_list operands);
int scm_fcd_assembler_push(ScmObj asmb, scm_opcode_t op, ...);
int scm_fcd_assembler_commit(ScmObj asmb);
ScmObj scm_fcd_assembler_iseq(ScmObj asmb);


/**************************************************************************/
/* Disassembler                                                           */
/**************************************************************************/

bool scm_fcd_disassembler_p(ScmObj obj);
ScmObj scm_fcd_disassembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq);
ScmObj scm_fcd_make_disassembler(ScmObj iseq);
const ScmDisasmToken *scm_fcd_disassembler_token(ScmObj disasm);
int scm_fcd_disassembler_next(ScmObj disasm);
void scm_fcd_disassembler_rewind(ScmObj disasm);
int scm_fcd_disassembler_cnv_to_marshalable(ScmObj disasm);


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

ScmObj scm_fcd_assemble_1inst_cv(const ScmObj *inst, size_t n, ScmObj acc);
ScmObj scm_fcd_assemble_1inst(ScmObj inst, ScmObj acc);
ScmObj scm_fcd_assemble(ScmObj lst, ScmObj acc);
ScmObj scm_fcd_disassemble(ScmObj obj);

ScmObj scm_fcd_unprintable_assembler_1inst(ScmObj inst);
ScmObj scm_fcd_printable_assembler_1inst(ScmObj inst);
ScmObj scm_fcd_unprintable_assembler(ScmObj lst);
ScmObj scm_fcd_printable_assembler(ScmObj lst);

void scm_fcd_update_vminst_opd_iof(ScmObj iseq, scm_byte_t *ip, int iof);
int scm_fcd_update_vminst_opd_obj_obj_1(ScmObj iseq,
                                        scm_byte_t *ip, ScmObj obj);

#endif  /* INCLUDE_FCD_ASSEMBLER_H__ */
