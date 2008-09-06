#include <cutter.h>

#include "basiclist.h"

void
test_scm_basic_list_construct(void)
{
  ScmBasicList *list = scm_basic_list_construct();
  
  cut_assert_not_null(list);
  cut_assert_null(scm_basic_list_head(list));
  cut_assert_null(scm_basic_list_tail(list));
  cut_assert_equal_int(0, scm_basic_list_length(list));
}

void
test_scm_basic_list_push(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(100));
  cut_assert_equal_int(1, scm_basic_list_length(list));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_tail(list)));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_head(list)));


  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(200));
  cut_assert_equal_int(2, scm_basic_list_length(list));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(200),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_tail(list)));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_head(list)));

}

void
test_scm_basic_list_pop(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_push(list, SCM_BASIC_LIST_VALUE(100));
  scm_basic_list_pop(list);

  cut_assert_equal_int(0, scm_basic_list_length(list));
  cut_assert_null(scm_basic_list_head(list));
  cut_assert_null(scm_basic_list_tail(list));
}

void
test_scm_basic_list_unshift(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(100));
  
  cut_assert_equal_int(1, scm_basic_list_length(list));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_head(list)));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_tail(list)));

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(200));
  cut_assert_equal_int(2, scm_basic_list_length(list));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(200),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_head(list)));
  cut_assert_equal_int(SCM_BASIC_LIST_VALUE(100),
                       SCM_BASIC_LIST_ENTRY_VALUE(scm_basic_list_tail(list)));

}

void
test_scm_basic_list_shift(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(100));
  scm_basic_list_shift(list);

  cut_assert_equal_int(0, scm_basic_list_length(list));
  cut_assert_null(scm_basic_list_head(list));
  cut_assert_null(scm_basic_list_tail(list));
}

void
test_scm_basic_list_cleanup(void)
{
  ScmBasicList *list = scm_basic_list_construct();

  scm_basic_list_unshift(list, SCM_BASIC_LIST_VALUE(100));
  scm_basic_list_shift(list);

  scm_basic_list_cleanup(list);

  cut_assert_null(scm_basic_list_head(list));
  cut_assert_null(scm_basic_list_tail(list));
  cut_assert_equal_int(0, scm_basic_list_length(list));
}
