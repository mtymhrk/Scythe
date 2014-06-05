#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "string.h"
#include "symbol.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}



void
test_scm_symbol_new(void)
{
  ScmObj sym = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &str);

  /* preprocess */
  str = scm_string_new(SCM_MEM_HEAP,
                                "foo", sizeof("foo") - 1, SCM_ENC_ASCII);

  /* action */
  sym = scm_symbol_new(SCM_MEM_HEAP, str);

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(sym));
}

void
test_scm_symbol_string__string_derived_from_a_symbol_should_be_equal_to_the_string_from_which_the_symbol_was_derived(void)
{
  ScmObj sym = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &expected, &actual);

  /* preprocess */
  expected = scm_string_new(SCM_MEM_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENC_ASCII);
  sym = scm_symbol_new(SCM_MEM_HEAP, expected);

  /* action */
  actual = scm_symbol_string(sym);

  /* postcondition check */
  cut_assert_true(scm_string_is_equal(expected, actual));
}

void
test_scm_symbol_string__string_derived_from_a_symbol_should_not_be_same_instance_with_the_string_from_which_the_symbol_was_derived(void)
{
  ScmObj sym = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &expected, &actual);

  /* preprocess */
  expected = scm_string_new(SCM_MEM_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENC_ASCII);
  sym = scm_symbol_new(SCM_MEM_HEAP, expected);

  /* action */
  actual = scm_symbol_string(sym);

  /* postcondition check */
  cut_assert_false(scm_obj_same_instance_p(expected, actual));
}
