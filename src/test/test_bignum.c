#include <cutter.h>

#include "api.h"
#include "numeric.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_bignum_plus_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_sword(0);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775807",
                                          sizeof("-9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_plus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_plus_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_sword(-2);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775809",
                                          sizeof("-9223372036854775809") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_plus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_plus_3(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("3800310896464717741434767931388",
                                               sizeof("3800310896464717741434767931388") - 1);

  bn1 = scm_capi_make_number_from_literal("3800310896473941113471622707197",
                                          sizeof("3800310896473941113471622707197") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775809",
                                          sizeof("-9223372036854775809") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_plus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_plus_4(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("18448744075709555618",
                                               sizeof("18448744075709555618") - 1);

  bn1 = scm_capi_make_number_from_literal("9224372037854777809",
                                          sizeof("9224372037854777809") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9224372037854777809",
                                          sizeof("9224372037854777809") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_plus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_plus_5(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("21267647932558655147052533681896816638",
                                               sizeof("21267647932558655147052533681896816638") - 1);

  bn1 = scm_capi_make_number_from_literal("#b1111111111111111111111111111111111111111111111111111111111111111111111",
                                          sizeof("#b1111111111111111111111111111111111111111111111111111111111111111111111") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("21267647932558653966460912964485513215",
                                          sizeof("21267647932558653966460912964485513215") - 1);


  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_plus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_minus_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("0", sizeof("0") - 1);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_minus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(actual));

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_minus_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("-2", sizeof("-2") - 1);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9223372036854775809",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_minus(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_mul_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual, &expected);

  expected = scm_capi_make_number_from_literal("-85070591730234615847396907784232501249", sizeof("-85070591730234615847396907784232501249") - 1);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775807",
                                          sizeof("-9223372036854775807") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_mul(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_mul_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;
  bool equal;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual);

  expected = scm_capi_make_number_from_literal("25108406941546723055321890044898107009262369244174241234945",
                                               sizeof("25108406941546723055321890044898107009262369244174241234945") - 1);

  bn1 = scm_capi_make_number_from_literal("#b1111111111111111111111111111111111111111111111111111111111111111111111",
                                          sizeof("#b1111111111111111111111111111111111111111111111111111111111111111111111") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("21267647932558653966460912964485513215",
                                          sizeof("21267647932558653966460912964485513215") - 1);


  cut_assert_true(scm_capi_bignum_p(bn2));

  actual = scm_bignum_mul(bn1, bn2);

  scm_capi_num_eq(expected, actual, &equal);

  cut_assert_true(equal);
}

void
test_scm_bignum_div_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj actual_quo = SCM_OBJ_INIT, actual_rem = SCM_OBJ_INIT;
  ScmObj expected_quo = SCM_OBJ_INIT, expected_rem = SCM_OBJ_INIT;
  bool equal;
  int rslt;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &actual_quo, &actual_rem,
                       &expected_quo, &expected_rem);

  expected_quo = scm_capi_make_number_from_literal("1208925819614629174706175",
                                                   sizeof("1208925819614629174706175") - 1);
  expected_rem = scm_capi_make_number_from_literal("36893488147419103231",
                                                   sizeof("36893488147419103231") - 1);

  bn1 = scm_capi_make_number_from_literal("1427247692705959881057075899931747937215840256",
                                          sizeof("1427247692705959881057075899931747937215840256") - 1);
  cut_assert_true(scm_capi_bignum_p(bn1));

  bn2 = scm_capi_make_number_from_literal("1180591620717411303423",
                                          sizeof("1180591620717411303423") - 1);
  cut_assert_true(scm_capi_bignum_p(bn2));

  rslt = scm_bignum_truncate_div(bn1, bn2,
                                 SCM_CSETTER_L(actual_quo),
                                 SCM_CSETTER_L(actual_rem));
  cut_assert_equal_int(rslt, 0);

  scm_capi_num_eq(expected_quo, actual_quo, &equal);
  cut_assert_true(equal);

  scm_capi_num_eq(expected_rem, actual_rem, &equal);
  cut_assert_true(equal);
}
