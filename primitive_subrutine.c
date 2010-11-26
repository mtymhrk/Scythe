#include "vm.h"
#include "procedure.h"

#include "pair.h"

ScmObj
scm_subr_func_cons(void)
{
  if (scm_vm_nr_local_var(scm_vm_current_vm()) != 2)
    ;                           /* TODO: error handling */

  return scm_pair_new(SCM_MEM_ALLOC_HEAP,
                      scm_vm_refer_local_var(scm_vm_current_vm(), 0),
                      scm_vm_refer_local_var(scm_vm_current_vm(), 1));
}

ScmObj
scm_subr_func_car(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&pair);

  if (scm_vm_nr_local_var(scm_vm_current_vm()) != 1)
    ;                           /* TODO: error handling */


  SCM_SETQ(pair, scm_vm_refer_local_var(scm_vm_current_vm(), 0));

  if (!scm_pair_is_pair(pair))
    ;                           /* TODO: err handling  */

  return scm_pair_car(pair);
}

ScmObj
scm_subr_func_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&pair);

  if (scm_vm_nr_local_var(scm_vm_current_vm()) != 1)
    ;                           /* TODO: error handling */


  SCM_SETQ(pair, scm_vm_refer_local_var(scm_vm_current_vm(), 0));

  if (!scm_pair_is_pair(pair))
    ;                           /* TODO: err handling  */

  return scm_pair_car(pair);
}
