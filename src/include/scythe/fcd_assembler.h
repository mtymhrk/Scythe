#ifndef INCLUDE_FCD_ASSEMBLER_H__
#define INCLUDE_FCD_ASSEMBLER_H__

#include "scythe/object.h"
#include "scythe/vminst.h"
#include "scythe/fcd_type.h"


/**************************************************************************/
/* Assembler                                                              */
/**************************************************************************/

bool scm_fcd_assembler_p(ScmObj obj);
ScmObj scm_fcd_assembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq);
ScmObj scm_fcd_make_assembler(ScmObj iseq);
int scm_fcd_assembler_commit(ScmObj asmb);


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
int scm_fcd_disassembler_cnv_to_printable(ScmObj disasm);


/**************************************************************************/
/* Assemble/Disassemble                                                   */
/**************************************************************************/

ScmObj scm_fcd_assemble(ScmObj lst, ScmObj acc);
ScmObj scm_fcd_disassemble(ScmObj obj);


#endif  /* INCLUDE_FCD_ASSEMBLER_H__ */
