#include <cutter.h>

#include "object.h"
#include "reference.h"
#include "miscobjects.h"
#include "string.h"
#include "symbol.h"

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
test_scm_symbol_new(void)
{
  ScmObj sym = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &str);

  /* preprocess */
  SCM_SETQ(str,
           scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo") - 1, SCM_ENCODING_ASCII));

  /* action */
  SCM_SETQ(sym, scm_symbol_new(SCM_MEM_ALLOC_HEAP, str));

  /* postcondition check */
  cut_assert_true(scm_obj_not_null_p(sym));
}

void
test_scm_symbol_is_symbol__object_which_is_not_symbol_should_not_be_symbol(void)
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&nil);

  /* preprocess */
  SCM_SETQ(nil, SCM_OBJ(scm_nil_instance()));

  /* action and postcondition check */
  cut_assert_false(scm_symbol_is_symbol(nil));
}

void
test_scm_symbol_string__string_derived_from_a_symbol_should_be_equal_to_the_string_from_which_the_symbol_was_derived(void)
{
  ScmObj sym = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &expected, &actual);

  /* preprocess */
  SCM_SETQ(expected,
           scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_new(SCM_MEM_ALLOC_HEAP, expected));

  /* action */
  SCM_SETQ(actual, scm_symbol_string(sym));

  /* postcondition check */
  cut_assert_true(scm_string_is_equal(expected, actual));
}

void
test_scm_symbol_string__string_derived_from_a_symbol_should_not_be_same_instance_with_the_string_from_which_the_symbol_was_derived(void)
{
  ScmObj sym = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &expected, &actual);

  /* preprocess */
  SCM_SETQ(expected,
           scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_new(SCM_MEM_ALLOC_HEAP, expected));

  /* action */
  SCM_SETQ(actual, scm_symbol_string(sym));

  /* postcondition check */
  cut_assert_false(scm_obj_same_instance_p(expected, actual));
}
