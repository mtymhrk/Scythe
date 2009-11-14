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
  SCM_SETQ_PRIM(vm, scm_vm_construct());
  scm_vm_switch_vm(vm);
}

void
cut_shutdown(void)
{
  scm_vm_revert_vm();
  scm_vm_destruct(vm);
}



void
test_scm_symbol_construct(void)
{
  ScmObj sym = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &str);

  /* preprocess */
  SCM_SETQ(str,
           scm_string_construct(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo") - 1, SCM_ENCODING_ASCII));

  /* action */
  SCM_SETQ(sym, scm_symbol_construct(SCM_MEM_ALLOC_HEAP, str));

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(sym));
}

void
test_scm_symbol_instance__2_symbols_derived_from_same_string_shuld_be_same_instance(void)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2, &str);

  /* preprocess */
  SCM_SETQ(str,
           scm_string_construct(SCM_MEM_ALLOC_HEAP,
                                "bar", sizeof("bar") - 1, SCM_ENCODING_ASCII));

  /* action */
  SCM_SETQ(sym1, scm_symbol_instance(str));
  SCM_SETQ(sym2, scm_symbol_instance(str));

  /* postcondition check */
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(sym1));
  cut_assert_true(SCM_OBJ_IS_NOT_NULL(sym2));
  cut_assert(SCM_OBJ_IS_SAME_INSTANCE(sym1, sym2));
}

void
test_scm_symbol_is_symbol__symbol_object_shuld_be_symbol(void)
{
  ScmObj sym = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &str);

  /* preprocess */
  SCM_SETQ(str,
           scm_string_construct(SCM_MEM_ALLOC_HEAP,
                                "bar", sizeof("bar") - 1, SCM_ENCODING_ASCII));

  SCM_SETQ(sym, scm_symbol_instance(str));

  /* action and postcondition check */
  cut_assert_true(scm_symbol_is_symbol(sym));
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
           scm_string_construct(SCM_MEM_ALLOC_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_instance(expected));

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
           scm_string_construct(SCM_MEM_ALLOC_HEAP,
                                "baz", sizeof("baz") - 1, SCM_ENCODING_ASCII));
  SCM_SETQ(sym, scm_symbol_instance(expected));

  /* action */
  SCM_SETQ(actual, scm_symbol_string(sym));

  /* postcondition check */
  cut_assert_false(SCM_OBJ_IS_SAME_INSTANCE(expected, actual));
}
