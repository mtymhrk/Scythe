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
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775807",
                                          sizeof("-9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_plus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 0: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_plus_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775809",
                                          sizeof("-9223372036854775809") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_plus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be -2: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_plus_3(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("3800310896473941113471622707197",
                                          sizeof("3800310896473941113471622707197") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775809",
                                          sizeof("-9223372036854775809") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_plus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 3800310896464717741434767931388: ",
                      SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_plus_4(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9224372037854777809",
                                          sizeof("9224372037854777809") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9224372037854777809",
                                          sizeof("9224372037854777809") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_plus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 18448744075709555618: ",
                      SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_plus_5(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("#b1111111111111111111111111111111111111111111111111111111111111111111111",
                                          sizeof("#b1111111111111111111111111111111111111111111111111111111111111111111111") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("21267647932558653966460912964485513215",
                                          sizeof("21267647932558653966460912964485513215") - 1);


  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_plus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 21267647932558655147052533681896816638: ",
                      SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_minus_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_minus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 0: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_minus_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("9223372036854775809",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_minus(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be -2: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_mul_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("9223372036854775807",
                                          sizeof("9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("-9223372036854775807",
                                          sizeof("-9223372036854775807") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_mul(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be -85070591730234615847396907784232501249: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_mul_2(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj bn3 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &bn3);

  bn1 = scm_capi_make_number_from_literal("#b1111111111111111111111111111111111111111111111111111111111111111111111",
                                          sizeof("#b1111111111111111111111111111111111111111111111111111111111111111111111") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("21267647932558653966460912964485513215",
                                          sizeof("21267647932558653966460912964485513215") - 1);


  cut_assert_true(scm_obj_not_null_p(bn2));

  bn3 = scm_bignum_mul(bn1, bn2);
  cut_assert_true(scm_obj_not_null_p(bn3));

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("should be 25108406941546723055321890044898107009262369244174241234945: ",
                      SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn3, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);
  scm_api_flush_output_port(SCM_OBJ_NULL);
}

void
test_scm_bignum_div_1(void)
{
  ScmObj bn1 = SCM_OBJ_INIT, bn2 = SCM_OBJ_INIT;
  ScmObj quo = SCM_OBJ_INIT, rem = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&bn1, &bn2, &quo, &rem);

  bn1 = scm_capi_make_number_from_literal("1427247692705959881057075899931747937215840256",
                                          sizeof("1427247692705959881057075899931747937215840256") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  bn2 = scm_capi_make_number_from_literal("1180591620717411303423",
                                          sizeof("1180591620717411303423") - 1);
  cut_assert_true(scm_obj_not_null_p(bn2));

  rslt = scm_bignum_truncate_div(bn1, bn2,
                                 SCM_CSETTER_L(quo), SCM_CSETTER_L(rem));
  cut_assert_equal_int(rslt, 0);

  scm_capi_write_cstr("bn1 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn1, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("bn2 = ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(bn2, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("quo should be 1208925819614629174706175: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(quo, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_capi_write_cstr("rem should be 36893488147419103231: ", SCM_ENC_ASCII, SCM_OBJ_NULL);
  scm_api_write(rem, SCM_OBJ_NULL);
  scm_api_newline(SCM_OBJ_NULL);

  scm_api_flush_output_port(SCM_OBJ_NULL);
}
