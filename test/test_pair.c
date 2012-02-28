#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "pair.h"


static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  vm = scm_vm_new();
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}

void
test_scm_pair_new(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  /* preprocess */
  car = scm_api_nil();
  cdr = scm_api_nil();

  /* action */
  pair = scm_pair_new(SCM_MEM_HEAP, car, cdr);;

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(pair));
  cut_assert_true(scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO));
}

void
test_scm_pair_car(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj actual_car = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr,&actual_car);

  /* preprocess */
  car = scm_api_nil();
  cdr = scm_api_eof();
  pair = scm_pair_new(SCM_MEM_HEAP, car, cdr);;

  /* action  */
  actual_car = scm_pair_car(pair);

  /* postcondition check */
  cut_assert_true(scm_obj_same_instance_p(car, actual_car));
}

void
test_scm_pair_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj actual_cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr, &actual_cdr);

  /* preprocess */
  car = scm_api_nil();
  cdr = scm_api_eof();
  pair = scm_pair_new(SCM_MEM_HEAP, car, cdr);;

  /* action */
  actual_cdr = scm_pair_cdr(pair);

  /* postcondition check */
  cut_assert_true(scm_obj_same_instance_p(cdr, actual_cdr));
}









