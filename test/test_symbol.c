#include <CUnit/CUnit.h>

#include "object.h"
#include "nil.h"
#include "symbol.h"

void
test_scm_symbol_construct(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  CU_ASSERT_PTR_NOT_NULL(symbol);
}

void
test_scm_symbol_instance(void)
{
  ScmSymbol *symbol1 = scm_symbol_instance("foo");
  ScmSymbol *symbol2 = scm_symbol_instance("foo");

  CU_ASSERT_PTR_NOT_NULL(symbol1);
  CU_ASSERT_PTR_NOT_NULL(symbol2);
  CU_ASSERT_PTR_EQUAL(symbol1, symbol2);
}

void
test_scm_symbol_is_symbol(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");
  ScmNil *nil = scm_nil_construct();

  CU_ASSERT_TRUE(scm_symbol_is_symbol(SCM_OBJ(symbol)));
  CU_ASSERT_FALSE(scm_symbol_is_symbol(SCM_OBJ(nil)));
}

void
test_scm_symbol_name(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  CU_ASSERT_STRING_EQUAL("foo", scm_symbol_name(symbol));
}

void
test_scm_symbol_length(void)
{
  ScmSymbol *symbol = scm_symbol_construct("foo");

  CU_ASSERT_EQUAL(3, scm_symbol_length(symbol));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    TEST_CASE(test_scm_symbol_construct),
    TEST_CASE(test_scm_symbol_instance),
    TEST_CASE(test_scm_symbol_is_symbol),
    TEST_CASE(test_scm_symbol_name),
    TEST_CASE(test_scm_symbol_length),
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmSymbol", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
