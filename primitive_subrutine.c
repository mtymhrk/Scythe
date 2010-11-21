#include "vm.h"
#include "procedure.h"

#include "pair.h"

void
scm_subr_func_cons(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&pair, &car, &cdr);

  // TODO: retrive arguments from the vm stack
  // TODO: test arguments

  pair = scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr);

  // TODO: set return value of subrutine to vm
}

void
scm_subr_func_car(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&pair, &car);



  // TODO: retrive arguments from the vm stack
  // TODO: test arguments

  car = scm_pair_car(pair);

  // TODO: set return value of subrutine to vm
}

void
scm_subr_func_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_PUSH(&pair, &cdr);

  // TODO: retrive arguments from the vm stack
  // TODO: test arguments

  cdr = scm_pair_cdr(pair);

  // TODO: set return value of subrutine to vm
}
