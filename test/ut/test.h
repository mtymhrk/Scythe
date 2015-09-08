#ifndef INCLUDE_TEST_HELPER_H__
#define INCLUDE_TEST_HELPER_H__

#include "scythe/object.h"
#include "scythe/api.h"

#include "unity_fixture.h"

#define TEST_ASSERT_SCM_NULL(obj) \
  TEST_ASSERT_TRUE(scm_obj_null_p(obj))

#define TEST_ASSERT_SCM_TRUE(obj) \
  TEST_ASSERT_TRUE(scm_fcd_true_object_p(obj))

#define TEST_ASSERT_SCM_FALSE(obj) \
  TEST_ASSERT_TRUE(scm_fcd_false_object_p(obj))

#define TEST_ASSERT_SCM_UNDEF(obj) \
  TEST_ASSERT_TRUE(scm_fcd_undef_object_p(obj))

#define TEST_ASSERT_SCM_NIL(obj) \
  TEST_ASSERT_TRUE(scm_fcd_nil_p(obj))

#define TEST_ASSERT_SCM_EQ(expected, actual) \
  TEST_ASSERT_TRUE(scm_fcd_eq_p(expected, actual))

#define TEST_ASSERT_SCM_EQV(expected, actual) \
  TEST_ASSERT_SCM_TRUE(scm_fcd_eqv_P(expected, actual))

#define TEST_ASSERT_SCM_EQUAL(expected, actual) \
  TEST_ASSERT_SCM_TRUE(scm_fcd_equal_P(expected, actual))


ScmObj ut_read_cstr(const char *str);

ScmScythe *ut_scythe_setup(bool load);
void ut_scythe_tear_down(ScmScythe *scy);
ScmObj ut_compile(ScmObj exp);
ScmObj ut_precompile(ScmObj exp);
ScmObj ut_eval(ScmObj exp);
void ut_disposal_unhandled_exc(void);


#endif  /* INCLUDE_TEST_HELPER_H__ */
