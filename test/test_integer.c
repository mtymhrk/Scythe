#include <CUnit/CUnit.h>

#include "object.h"
#include "nil.h"
#include "integer.h"

void
test_scm_integer_construct(void)
{
  ScmInteger *integer = scm_integer_construct(0);

  CU_ASSERT_PTR_NOT_NULL(integer);
}

void
test_scm_integer_is_integer_pass(void)
{
  ScmInteger *integer = scm_integer_construct(0);

  CU_ASSERT_TRUE(scm_integer_is_integer(SCM_OBJ(integer)));
}

void
test_scm_integer_is_integer_failure(void)
{
  ScmNil *nil = scm_nil_instance();

  CU_ASSERT_FALSE(scm_integer_is_integer(SCM_OBJ(nil)));
}

void
test_scm_integer_value(void)
{
  ScmInteger *integer = scm_integer_construct(100);

  CU_ASSERT_EQUAL(100, scm_integer_value(integer));
}

void
test_scm_integer_plus(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  CU_ASSERT_EQUAL(300,
		  scm_integer_value(scm_integer_plus(integer1, integer2)));
}

void
test_scm_integer_minus(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  CU_ASSERT_EQUAL(100,
		  scm_integer_value(scm_integer_minus(integer1, integer2)));
}

void
test_scm_integer_multiple(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  CU_ASSERT_EQUAL(20000,
		  scm_integer_value(scm_integer_multiply(integer1, integer2)));
}

void
test_scm_integer_divide(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(100);

  CU_ASSERT_EQUAL(2,
		  scm_integer_value(scm_integer_divide(integer1, integer2)));
}

void
test_scm_integer_reminder(void)
{
  ScmInteger *integer1 = scm_integer_construct(200);
  ScmInteger *integer2 = scm_integer_construct(3);

  CU_ASSERT_EQUAL(2,
		  scm_integer_value(scm_integer_reminder(integer1, integer2)));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    TEST_CASE(test_scm_integer_construct),
    TEST_CASE(test_scm_integer_is_integer_pass),
    TEST_CASE(test_scm_integer_is_integer_failure),
    TEST_CASE(test_scm_integer_value),
    TEST_CASE(test_scm_integer_plus),
    TEST_CASE(test_scm_integer_minus),
    TEST_CASE(test_scm_integer_multiple),
    TEST_CASE(test_scm_integer_divide),
    TEST_CASE(test_scm_integer_reminder),
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmInteger", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
