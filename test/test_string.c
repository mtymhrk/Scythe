#include <CUnit/CUnit.h>

#include "object.h"
#include "nil.h"
#include "string.h"

void
test_scm_string_construct(void)
{
  ScmString *string = scm_string_construct("foo");

  CU_ASSERT_PTR_NOT_NULL(string);
}

void
test_scm_string_is_string(void)
{
  ScmString *string = scm_string_construct("foo");
  ScmNil *nil = scm_nil_construct();

  CU_ASSERT_TRUE(scm_string_is_string(SCM_OBJ(string)));
  CU_ASSERT_FALSE(scm_string_is_string(SCM_OBJ(nil)));
}

void
test_scm_string_string(void)
{
  ScmString *string = scm_string_construct("foo");

  CU_ASSERT_STRING_EQUAL("foo", scm_string_string(string));
}

void
test_scm_string_length(void)
{
  ScmString *string = scm_string_construct("foo");

  CU_ASSERT_EQUAL(3, scm_string_length(string));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    TEST_CASE(test_scm_string_construct),
    TEST_CASE(test_scm_string_is_string),
    TEST_CASE(test_scm_string_string),
    TEST_CASE(test_scm_string_length),
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmString", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
