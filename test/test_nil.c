#include <CUnit/CUnit.h>

#include "object.h"
#include "pair.h"
#include "nil.h"

void
test_scm_nil_construct(void)
{
  ScmNil *nil = scm_nil_construct();

  CU_ASSERT_PTR_NOT_NULL(nil);
}

void
test_scm_nil_instance(void)
{
  ScmNil *nil1 = scm_nil_instance();
  ScmNil *nil2 = scm_nil_instance();

  CU_ASSERT_PTR_NOT_NULL(nil1);
  CU_ASSERT_PTR_NOT_NULL(nil2);
  CU_ASSERT_PTR_EQUAL(nil1, nil2);
}

void
test_scm_nil_is_nil(void)
{
  ScmNil *nil = scm_nil_construct();
  ScmPair *pair = scm_pair_construct(SCM_OBJ(nil), SCM_OBJ(nil));

  CU_ASSERT_TRUE(scm_nil_is_nil(SCM_OBJ(nil)));
  CU_ASSERT_FALSE(scm_nil_is_nil(SCM_OBJ(pair)));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    TEST_CASE(test_scm_nil_construct),
    TEST_CASE(test_scm_nil_instance),
    TEST_CASE(test_scm_nil_is_nil),
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmNil", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
