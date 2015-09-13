#include "equivalence.c"

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/pair.h"
#include "scythe/vector.h"
#include "scythe/equivalence.h"

#include "test.h"

TEST_GROUP(equivalence);

static ScmScythe *scy;
static ScmRefStackInfo rsi;

TEST_SETUP(equivalence)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(equivalence)
{
  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(equivalence, eq_p__symbol_return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_eq_p(sym1, sym2));
}

TEST(equivalence, eq_p__symbol_return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_FALSE(scm_eq_p(sym1, sym2));
}

TEST(equivalence, eq_p__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = lst1;

  TEST_ASSERT_TRUE(scm_eq_p(lst1, lst2));
}

TEST(equivalence, eq_p__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = ut_read_cstr("(a b c)");

  TEST_ASSERT_FALSE(scm_eq_p(lst1, lst2));
}

TEST(equivalence, eq_p__empty_list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("()");
  lst2 = ut_read_cstr("()");

  TEST_ASSERT_TRUE(scm_eq_p(lst1, lst2));
}

TEST(equivalence, eq_p__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = str1;

  TEST_ASSERT_TRUE(scm_eq_p(str1, str2));
}

TEST(equivalence, eq_p__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = ut_read_cstr("\"abc\"");

  TEST_ASSERT_FALSE(scm_eq_p(str1, str2));
}

TEST(equivalence, eq_p__empty_string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"\"");
  str2 = ut_read_cstr("\"\"");

  TEST_ASSERT_FALSE(scm_eq_p(str1, str2));
}

TEST(equivalence, eq_p__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("123");

  TEST_ASSERT_TRUE(scm_eq_p(num1, num2));
}

TEST(equivalence, eq_p__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("321");

  TEST_ASSERT_FALSE(scm_eq_p(num1, num2));
}

TEST(equivalence, eq_p__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = num1;

  TEST_ASSERT_TRUE(scm_eq_p(num1, num2));
}

TEST(equivalence, eq_p__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = ut_read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_FALSE(scm_eq_p(num1, num2));
}

TEST(equivalence, eq_p__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = chr1;

  TEST_ASSERT_TRUE(scm_eq_p(chr1, chr2));
}

TEST(equivalence, eq_p__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = ut_read_cstr("#\\a");

  TEST_ASSERT_FALSE(scm_eq_p(chr1, chr2));
}

TEST(equivalence, eq_p__SCM_OBJ_NULL__return_false)
{
  TEST_ASSERT_FALSE(scm_eq_p(SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(equivalence, eq_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_eq_P(sym1, sym2));
}

TEST(equivalence, eq_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_eq_P(sym1, sym2));
}

TEST(equivalence, eqv__symbol__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__symbol__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = lst1;

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = ut_read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(lst1, lst2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__empty_list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("()");
  lst2 = ut_read_cstr("()");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = str1;

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(str1, str2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = ut_read_cstr("\"abc\"");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__empty_string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"\"");
  str2 = ut_read_cstr("\"\"");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("123");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("321");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = ut_read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = ut_read_cstr("88888888888888888888888888888888888");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(chr1, chr2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, eqv__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(chr1, chr2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv__different_obj_type__return_false)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(SCM_FALSE_OBJ, SCM_NIL_OBJ, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, eqv_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_TRUE(scm_eqv_P(sym1, sym2));
}

TEST(equivalence, eqv_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_eqv_P(sym1, sym2));
}

TEST(equivalence, equal__symbol__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__symbol__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = ut_read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = ut_read_cstr("(a b z)");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(lst1, lst2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__circularly_linked_list__return_true_1)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT, tail = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2, &tail);

  lst1 = ut_read_cstr("(a b c)");
  lst2 = ut_read_cstr("(a b c)");

  tail = scm_list_tail(lst1, 2);
  scm_set_cdr(tail, lst1);

  tail = scm_list_tail(lst2, 2);
  scm_set_cdr(tail, lst2);

  /* lst1 ;=>  #1=(a b c . #1#) */
  /* lst2 ;=>  #1=(a b c . #1#) */

  TEST_ASSERT_EQUAL_INT(0, scm_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

/* 循環構造が異なるリストについても真を返さなければならない (r7rs-draft-9) が、
 * 現状、偽を返す。また循環リストの read が未実装なのでテストケースを実行でき
 * ない。
 */
IGNORE_TEST(equivalence, equal__circularly_linked_list__return_true_2)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&lst1, &lst2);

  lst1 = ut_read_cstr("#1=(a b . #1#)");
  lst2 = ut_read_cstr("#1=(a b a b . #1#)");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__vector__return_true)
{
  ScmObj vec1 = SCM_OBJ_INIT, vec2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&vec1, &vec2);

  vec1 = ut_read_cstr("#(a b c)");
  vec2 = ut_read_cstr("#(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(vec1, vec2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__vector__return_false)
{
  ScmObj vec1 = SCM_OBJ_INIT, vec2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&vec1, &vec2);

  vec1 = ut_read_cstr("#(a b c)");
  vec2 = ut_read_cstr("#(a b z)");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(vec1, vec2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__circularly_linked_vector__return_true_1)
{
  ScmObj vec1 = SCM_OBJ_INIT, vec2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&vec1, &vec2);

  vec1 = ut_read_cstr("#(a b c)");
  vec2 = ut_read_cstr("#(a b c)");

  scm_vector_set(vec1, 2, vec2);
  scm_vector_set(vec2, 2, vec1);

  TEST_ASSERT_EQUAL_INT(0, scm_equal(vec1, vec2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = ut_read_cstr("\"abc\"");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(str1, str2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&str1, &str2);

  str1 = ut_read_cstr("\"abc\"");
  str2 = ut_read_cstr("\"abz\"");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("123");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("123");
  num2 = ut_read_cstr("321");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = ut_read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&num1, &num2);

  num1 = ut_read_cstr("99999999999999999999999999999999999");
  num2 = ut_read_cstr("88888888888888888888888888888888888");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = ut_read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(chr1, chr2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, equal__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_REFSTK_INIT_REG(&chr1, &chr2);

  chr1 = ut_read_cstr("#\\a");
  chr2 = ut_read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_equal(chr1, chr2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal__different_obj_type__return_false)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_eqv(SCM_FALSE_OBJ, SCM_NIL_OBJ, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, equal_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("aaa");

  TEST_ASSERT_SCM_EQUAL(sym1, sym2);
}

TEST(equivalence, equal_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&sym1, &sym2);

  sym1 = ut_read_cstr("aaa");
  sym2 = ut_read_cstr("bbb");

  TEST_ASSERT_SCM_FALSE(scm_equal_P(sym1, sym2));
}

