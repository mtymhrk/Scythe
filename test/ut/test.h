#ifndef INCLUDE_TEST_HELPER_H__
#define INCLUDE_TEST_HELPER_H__

#include "unity_fixture.h"

#include "object.h"
#include "api.h"

#define TEST_ASSERT_SCM_NULL(obj) \
  TEST_ASSERT_TRUE(scm_obj_null_p(obj))

#define TEST_ASSERT_SCM_TRUE(obj) \
  TEST_ASSERT_TRUE(scm_capi_true_object_p(obj))

#define TEST_ASSERT_SCM_FALSE(obj) \
  TEST_ASSERT_TRUE(scm_capi_false_object_p(obj))

#define TEST_ASSERT_SCM_UNDEF(obj) \
  TEST_ASSERT_TRUE(scm_capi_undef_object_p(obj))

#define TEST_ASSERT_SCM_NIL(obj) \
  TEST_ASSERT_TRUE(scm_capi_nil_p(obj))

#define TEST_ASSERT_SCM_EQ(expected, actual) \
  TEST_ASSERT_TRUE(scm_capi_eq_p(expected, actual))

#define TEST_ASSERT_SCM_EQV(expected, actual) \
  TEST_ASSERT_SCM_TRUE(scm_api_eqv_P(expected, actual))

#define TEST_ASSERT_SCM_EQUAL(expected, actual) \
  TEST_ASSERT_SCM_TRUE(scm_api_equal_P(expected, actual))


ScmObj read_cstr(const char *str);

ScmObj scm_capi_ut_compile(ScmEvaluator *ev, ScmObj exp);
ScmObj scm_capi_ut_eval(ScmEvaluator *ev, ScmObj exp);
void scm_capi_ut_disposal_unhandled_exc(ScmEvaluator *ev);


#endif  /* INCLUDE_TEST_HELPER_H__ */
