#include "scythe/object.h"
#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_equivalence);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(api_equivalence)
{
  scy = ut_scythe_setup(false);
  scm_fcd_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(api_equivalence)
{
  scm_fcd_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(api_equivalence, api_eq_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_api_eq_P(sym1, sym2));
}

TEST(api_equivalence, api_eq_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_api_eq_P(sym1, sym2));
}

TEST(api_equivalence, api_eqv_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_api_eqv_P(sym1, sym2));
}

TEST(api_equivalence, api_eqv_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_api_eqv_P(sym1, sym2));
}

TEST(api_equivalence, api_equal_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_EQUAL(sym1, sym2);
}

TEST(api_equivalence, api_equal_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_api_equal_P(sym1, sym2));
}
