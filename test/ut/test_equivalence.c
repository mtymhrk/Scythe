#include "unity_fixture.h"

#include "object.h"
#include "api.h"

TEST_GROUP(equivalence);

static ScmEvaluator *ev;

TEST_SETUP(equivalence)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

TEST_TEAR_DOWN(equivalence)
{
  scm_capi_evaluator_end(ev);
}

static ScmObj
read_cstr(const char *str)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_capi_open_input_string_from_cstr(str, SCM_ENC_ASCII);
  return scm_api_read(port);
}

static void
debug_print_obj(ScmObj obj)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj,
                       &port);

  port = scm_api_standard_output_port();
  scm_api_write(obj, port);
  scm_api_newline(port);
}

TEST(equivalence, capi_eq_p__symbol_return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_capi_eq_p(sym1, sym2));
}

TEST(equivalence, capi_eq_p__symbol_return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_FALSE(scm_capi_eq_p(sym1, sym2));
}

TEST(equivalence, capi_eq_p__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = lst1;

  TEST_ASSERT_TRUE(scm_capi_eq_p(lst1, lst2));
}

TEST(equivalence, capi_eq_p__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = read_cstr("(a b c)");

  TEST_ASSERT_FALSE(scm_capi_eq_p(lst1, lst2));
}

TEST(equivalence, capi_eq_p__empty_list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("()");
  lst2 = read_cstr("()");

  TEST_ASSERT_TRUE(scm_capi_eq_p(lst1, lst2));
}

TEST(equivalence, capi_eq_p__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = str1;

  TEST_ASSERT_TRUE(scm_capi_eq_p(str1, str2));
}

TEST(equivalence, capi_eq_p__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = read_cstr("\"abc\"");

  TEST_ASSERT_FALSE(scm_capi_eq_p(str1, str2));
}

TEST(equivalence, capi_eq_p__empty_string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"\"");
  str2 = read_cstr("\"\"");

  TEST_ASSERT_FALSE(scm_capi_eq_p(str1, str2));
}

TEST(equivalence, capi_eq_p__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("123");

  TEST_ASSERT_TRUE(scm_capi_eq_p(num1, num2));
}

TEST(equivalence, capi_eq_p__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("321");

  TEST_ASSERT_FALSE(scm_capi_eq_p(num1, num2));
}

TEST(equivalence, capi_eq_p__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = num1;

  TEST_ASSERT_TRUE(scm_capi_eq_p(num1, num2));
}

TEST(equivalence, capi_eq_p__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_FALSE(scm_capi_eq_p(num1, num2));
}

TEST(equivalence, capi_eq_p__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = chr1;

  TEST_ASSERT_TRUE(scm_capi_eq_p(chr1, chr2));
}

TEST(equivalence, capi_eq_p__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = read_cstr("#\\a");

  TEST_ASSERT_FALSE(scm_capi_eq_p(chr1, chr2));
}

TEST(equivalence, capi_eq_p__SCM_OBJ_NULL__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_eq_p(SCM_OBJ_NULL, SCM_OBJ_NULL));
}

TEST(equivalence, capi_eq__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eq(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eq__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eq(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eq__SCM_OBJ_NULL__return_ERROR)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(-1, scm_capi_eq(SCM_OBJ_NULL, SCM_OBJ_NULL, &actual));
}

TEST(equivalence, api_eq_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_capi_true_object_p(scm_api_eq_P(sym1, sym2)));
}

TEST(equivalence, api_eq_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_TRUE(scm_capi_false_object_p(scm_api_eq_P(sym1, sym2)));
}

TEST(equivalence, api_eq_P__SCM_OBJ_NULL__return_ERROR)
{
  TEST_ASSERT_TRUE(scm_obj_null_p(scm_api_eq_P(SCM_OBJ_NULL, SCM_OBJ_NULL)));
}

TEST(equivalence, capi_eqv__symbol__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__symbol__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = lst1;

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, api_eqv__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(lst1, lst2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__empty_list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("()");
  lst2 = read_cstr("()");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = str1;

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(str1, str2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = read_cstr("\"abc\"");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__empty_string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"\"");
  str2 = read_cstr("\"\"");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("123");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("321");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = read_cstr("88888888888888888888888888888888888");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(chr1, chr2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_eqv__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(chr1, chr2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__different_obj_type__return_false)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(SCM_FALSE_OBJ, SCM_NIL_OBJ, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_eqv__SCM_OBJ_NULL__return_ERROR)
{
  bool actual;
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_eqv(SCM_OBJ_NULL, SCM_OBJ_NULL, &actual));
}

TEST(equivalence, api_eqv_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_capi_true_object_p(scm_api_eqv_P(sym1, sym2)));
}

TEST(equivalence, api_eqv_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_TRUE(scm_capi_false_object_p(scm_api_eqv_P(sym1, sym2)));
}

TEST(equivalence, api_eqv_P__SCM_OBJ_NULL__return_ERROR)
{
  TEST_ASSERT_TRUE(scm_obj_null_p(scm_api_eqv_P(SCM_OBJ_NULL, SCM_OBJ_NULL)));
}

TEST(equivalence, capi_equal__symbol__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(sym1, sym2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__symbol__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(sym1, sym2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__list__return_true)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = read_cstr("(a b c)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, api_equal__list__return_false)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("(a b c)");
  lst2 = read_cstr("(a b z)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(lst1, lst2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, api_equal__circularly_linked_list__return_true_1)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT, tail = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2, &tail);

  lst1 = read_cstr("(a b c)");
  lst2 = read_cstr("(a b c)");

  tail = scm_capi_list_tail(lst1, 2);
  scm_capi_set_cdr_i(tail, lst1);

  tail = scm_capi_list_tail(lst2, 2);
  scm_capi_set_cdr_i(tail, lst2);

  /* lst1 ;=>  #1=(a b c . #1#) */
  /* lst2 ;=>  #1=(a b c . #1#) */

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

/* 循環構造が異なるリストについても真を返さなければならない (r7rs-draft-9) が、
 * 現状、偽を返す。また循環リストの read が未実装なのでテストケースを実行でき
 * ない。
 */
IGNORE_TEST(equivalence, api_equal__circularly_linked_list__return_true_2)
{
  ScmObj lst1 = SCM_OBJ_INIT, lst2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&lst1, &lst2);

  lst1 = read_cstr("#1=(a b . #1#)");
  lst2 = read_cstr("#1=(a b a b . #1#)");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(lst1, lst2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__string__return_true)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = read_cstr("\"abc\"");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(str1, str2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__string__return_false)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = read_cstr("\"abc\"");
  str2 = read_cstr("\"abz\"");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(str1, str2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__fixnum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("123");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__fixnum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("123");
  num2 = read_cstr("321");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__bignum__return_true)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = read_cstr("99999999999999999999999999999999999");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(num1, num2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__bignum__return_false)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&num1, &num2);

  num1 = read_cstr("99999999999999999999999999999999999");
  num2 = read_cstr("88888888888888888888888888888888888");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(num1, num2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__char__return_true)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = read_cstr("#\\a");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(chr1, chr2, &actual));
  TEST_ASSERT_TRUE(actual);
}

TEST(equivalence, capi_equal__char__return_false)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  bool actual;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  chr1 = read_cstr("#\\a");
  chr2 = read_cstr("#\\b");

  TEST_ASSERT_EQUAL_INT(0, scm_capi_equal(chr1, chr2, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__different_obj_type__return_false)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_capi_eqv(SCM_FALSE_OBJ, SCM_NIL_OBJ, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(equivalence, capi_equal__SCM_OBJ_NULL__return_ERROR)
{
  bool actual;
  TEST_ASSERT_EQUAL_INT(-1, scm_capi_equal(SCM_OBJ_NULL, SCM_OBJ_NULL, &actual));
}

TEST(equivalence, api_equal_P__return_true)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("aaa");

  TEST_ASSERT_TRUE(scm_capi_true_object_p(scm_api_equal_P(sym1, sym2)));
}

TEST(equivalence, api_equal_P__return_false)
{
  ScmObj sym1 = SCM_OBJ_INIT, sym2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym1, &sym2);

  sym1 = read_cstr("aaa");
  sym2 = read_cstr("bbb");

  TEST_ASSERT_TRUE(scm_capi_false_object_p(scm_api_equal_P(sym1, sym2)));
}

TEST(equivalence, api_equal_P__SCM_OBJ_NULL__return_ERROR)
{
  TEST_ASSERT_TRUE(scm_obj_null_p(scm_api_equal_P(SCM_OBJ_NULL, SCM_OBJ_NULL)));
}
