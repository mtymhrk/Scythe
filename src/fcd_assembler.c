#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/assembler.h"

ScmObj
scm_fcd_assemble(ScmObj lst, ScmObj iseq)
{
  return scm_asm_assemble(lst, iseq);
}
