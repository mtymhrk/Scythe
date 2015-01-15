#ifndef INCLUDE_FCD_ASSEMBLER_H__
#define INCLUDE_FCD_ASSEMBLER_H__

#include "scythe/object.h"
#include "scythe/fcd_type.h"

bool scm_fcd_assembler_p(ScmObj obj);
ScmObj scm_fcd_assembler_new(SCM_MEM_TYPE_T mtype, ScmObj iseq);
ScmObj scm_fcd_make_assembler(ScmObj iseq);
int scm_fcd_assembler_commit(ScmObj asmb);

ScmObj scm_fcd_assemble(ScmObj lst, ScmObj acc);


#endif  /* INCLUDE_FCD_ASSEMBLER_H__ */
