#include <CUnit/CUnit.h>

#include "object.h"
#include "nil.h"
#include "pair.h"

void
test_scm_pair_construct(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  CU_ASSERT_PTR_NOT_NULL(pair);
}

void
test_scm_pair_is_pair(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  CU_ASSERT_TRUE(scm_pair_is_pair(SCM_OBJ(pair)));
  CU_ASSERT_FALSE(scm_pair_is_pair(SCM_OBJ(car)));
}

void
test_scm_pair_car(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  CU_ASSERT_PTR_EQUAL(car, scm_pair_car(pair));
}

void
test_scm_pair_cdr(void)
{
  ScmNil *car = scm_nil_construct();
  ScmNil *cdr = scm_nil_construct();

  ScmPair *pair = scm_pair_construct(SCM_OBJ(car), SCM_OBJ(cdr));

  CU_ASSERT_PTR_EQUAL(cdr, scm_pair_cdr(pair));
}

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    {"test_scm_pair_construct", test_scm_pair_construct},
    {"test_scm_pair_is_pair", test_scm_pair_is_pair},
    {"test_scm_pair_car", test_scm_pair_car},
    {"test_scm_pair_cdr", test_scm_pair_cdr},
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmPair", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
