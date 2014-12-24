#include "scythe/fcd.h"
#include "scythe/reference.h"

void
scm_fcd_ref_stack_push(ScmRefStackBlock *block)
{
  scm_ref_stack_push(scm_fcd_current_ref_stack(), block);
}

void
scm_fcd_ref_stack_save(ScmRefStackInfo *info)
{
  scm_ref_stack_save(scm_fcd_current_ref_stack(), info);
}

void
scm_fcd_ref_stack_restore(ScmRefStackInfo *info)
{
  scm_ref_stack_restore(scm_fcd_current_ref_stack(), info);
}
