#include <cutter.h>

#include "api.h"
#include "numeric.h"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_setup_current_vm(ev);
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

  /* bn1 = scm_capi_make_number_from_literal("21267647932558653966460912964485513215", */
  /*                                         sizeof("21267647932558653966460912964485513215") - 1); */
  bn1 = scm_capi_make_number_from_literal("#b1111111111111111111111111111111111111111111111111111111111111111111111",
                                          sizeof("#b1111111111111111111111111111111111111111111111111111111111111111111111") - 1);
  cut_assert_true(scm_obj_not_null_p(bn1));

  /* bn2 = scm_capi_make_number_from_literal("1180591620717411303423", */
  /*                                         sizeof("1180591620717411303423") - 1); */

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
