#include <CUnit/CUnit.h>

#include "basiclist.h"

void
test_scm_basic_list_construct(void)
{
  ScmBasicList *list = scm_basic_list_construct();
  
  CU_ASSERT_PTR_NOT_NULL(list);
}

void
test_scm_basic_list_push(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(100));
  CU_ASSERT_EQUAL(1, scm_basic_list_length(list));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_tail(list)));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_head(list)));


  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(200));
  CU_ASSERT_EQUAL(2, scm_basic_list_length(list));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(200),
		  scm_basic_list_entry_value(scm_basic_list_tail(list)));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_head(list)));

}

void
test_scm_basic_list_pop(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(100));
  scm_basic_list_pop(list);

  CU_ASSERT_EQUAL(0, scm_basic_list_length(list));
  CU_ASSERT_PTR_NULL(scm_basic_list_head(list));
  CU_ASSERT_PTR_NULL(scm_basic_list_tail(list));
}

void
test_scm_basic_list_unshift(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(100));
  
  CU_ASSERT_EQUAL(1, scm_basic_list_length(list));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_head(list)));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_tail(list)));

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(200));
  CU_ASSERT_EQUAL(2, scm_basic_list_length(list));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(200),
		  scm_basic_list_entry_value(scm_basic_list_head(list)));
  CU_ASSERT_EQUAL(SCM_BASIC_LIST_VALUE(100),
		  scm_basic_list_entry_value(scm_basic_list_tail(list)));

}

void
test_scm_basic_list_shift(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(100));
  scm_basic_list_shift(list);

  CU_ASSERT_EQUAL(0, scm_basic_list_length(list));
  CU_ASSERT_PTR_NULL(scm_basic_list_head(list));
  CU_ASSERT_PTR_NULL(scm_basic_list_tail(list));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    { "Constructor", test_scm_basic_list_construct },
    { "Push value", test_scm_basic_list_push },
    { "Pop value", test_scm_basic_list_pop },
    { "Unshift value", test_scm_basic_list_unshift },
    { "Shift value", test_scm_basic_list_shift },
    CU_TEST_INFO_NULL
  };

  CU_SuiteInfo suites[] = {
    {"ScmBasicList", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };

  return  CU_register_suites(suites);
}

