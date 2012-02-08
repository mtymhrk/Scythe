#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "miscobjects.h"
#include "pair.h"


static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  SCM_SETQ_PRIM(vm, scm_vm_new());
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
  SCM_SETQ(car, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(cdr, SCM_OBJ(scm_nil_instance()));

  /* action */
  SCM_SETQ(pair, scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr));;

  /* postcondition check */
  cut_assert_true(SCM_OBJ_NOT_NULL_P(pair));
  cut_assert_true(SCM_OBJ_IS_TYPE(pair, &SCM_PAIR_TYPE_INFO));
}

void
test_scm_pair_is_pair(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  /* preprocess */
  SCM_SETQ(car, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(cdr, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(pair, scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr));;

  /* action and postcondition check */
  cut_assert_true(scm_pair_is_pair(SCM_OBJ(pair)));
  cut_assert_false(scm_pair_is_pair(SCM_OBJ(car)));
}

void
test_scm_pair_car(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  /* preprocess */
  SCM_SETQ(car, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(cdr, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(pair, scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr));;

  /* action and postcondition check */
  cut_assert_true(scm_obj_same_instance_p(car, scm_pair_car(pair)));
}

void
test_scm_pair_cdr(void)
{
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&pair, &car, &cdr);

  /* preprocess */
  SCM_SETQ(car, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(cdr, SCM_OBJ(scm_nil_instance()));
  SCM_SETQ(pair, scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr));;

  /* action and postcondition check */
  cut_assert_true(scm_obj_same_instance_p(cdr, scm_pair_cdr(pair)));
}
