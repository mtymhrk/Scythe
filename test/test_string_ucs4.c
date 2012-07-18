#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "string.h"

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

size_t
utf8str_to_ucs4str(const void *utf8, size_t size, void *ucs4)
{
  const uint8_t *p8;
  uint32_t *p4;
  size_t i;

  p8 = utf8;
  p4 = ucs4;
  i = 0;
  while (i < size)
    i += (size_t)scm_enc_utf8_to_ucs4(p8 + i, size - i, p4++);

  return (size_t)((uint8_t *)p4 - (uint8_t *)ucs4);
}

void
test_scm_string_ucs4(void)
{
  char expected[256];
  size_t expected_len;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  expected_len = utf8str_to_ucs4str("テスト文字列", sizeof("テスト文字列") - 1,
                                    expected);

  str = scm_string_new(SCM_MEM_HEAP,
                                expected, expected_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(str));
  cut_assert_equal_uint(SCM_ENC_UCS4, scm_string_encoding(str));
  cut_assert_equal_uint(6u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_copy_ucs4(void)
{
  char expected[256];
  size_t expected_len;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  expected_len = utf8str_to_ucs4str("この文字列は誤りである",
                                    sizeof("この文字列は誤りである") - 1,
                                    expected);

  str = scm_string_new(SCM_MEM_HEAP,
                                expected, expected_len, SCM_ENC_UCS4);
  copy = scm_string_copy(str);

  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_dup_ucs4(void)
{
  char expected[256];
  size_t expected_len;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  expected_len = utf8str_to_ucs4str("この文字列は誤りである",
                                    sizeof("この文字列は誤りである") - 1,
                                    expected);

  str = scm_string_new(SCM_MEM_HEAP,
                                expected, expected_len, SCM_ENC_UCS4);
  copy = scm_string_dup(str);

  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}


void
test_scm_string_is_equal_compare_with_same_string_ucs4(void)
{
  char row[256];
  size_t row_len;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      row, row_len, SCM_ENC_UCS4);
  str2 = scm_string_new(SCM_MEM_HEAP,
                                      row, row_len, SCM_ENC_UCS4);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_different_string_ucs4(void)
{
  char row1[256], row2[256];
  size_t row1_len, row2_len;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  row1_len = utf8str_to_ucs4str("この文字列は誤りである",
                                sizeof("この文字列は誤りである") - 1,
                                row1);

  row2_len = utf8str_to_ucs4str("この文字列は誤りでない",
                                sizeof("この文字列は誤りでない") - 1,
                                row2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      row1, row1_len, SCM_ENC_UCS4);
  str2 = scm_string_new(SCM_MEM_HEAP,
                                      row2, row2_len, SCM_ENC_UCS4);

  cut_assert_false(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_copy_string_ucs4(void)
{
  char row[256];
  size_t row_len;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      row, row_len, SCM_ENC_UCS4);
  str2 = scm_string_copy(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_duplicate_string_ucs4(void)
{
  char row[256];
  size_t row_len;
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      row, row_len, SCM_ENC_UCS4);
  str2 = scm_string_dup(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_substr_ucs4(void)
{
  char row[256], expected[256];
  size_t row_len, expected_len;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, sub = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sub);

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  expected_len = utf8str_to_ucs4str("誤りである",
                                    sizeof("誤りである") - 1,
                                    expected);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);
  sub = scm_string_substr(str, 6, 5);

  cut_assert_true(scm_obj_not_null_p(sub));
  cut_assert_equal_uint(5u, scm_string_length(sub));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(sub));

  len = scm_string_dump(sub, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_push_ucs4(void)
{
  char row[256], expected[256];
  size_t row_len, expected_len;
  scm_char_t pushed;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  expected_len = utf8str_to_ucs4str("この文字列は誤りである。",
                                    sizeof("この文字列は誤りである。") - 1,
                                    expected);
  utf8str_to_ucs4str("。", sizeof("。") -1, &pushed);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(scm_string_push(str, pushed)));

  cut_assert_equal_uint(12u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_append_ucs4(void)
{
  char row1[256], row2[256], expected[256];
  size_t row1_len, row2_len, expected_len;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, apnd = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &apnd);

  row1_len = utf8str_to_ucs4str("次の文は正しい。",
                                sizeof("次の文は正しい。") - 1,
                                row1);

  row2_len = utf8str_to_ucs4str("前の文は誤りである。",
                                sizeof("前の文は誤りである。") - 1,
                                row2);
  expected_len = utf8str_to_ucs4str("次の文は正しい。前の文は誤りである。",
                                    sizeof("次の文は正しい。前の文は誤りである。") -1 ,
                                    expected);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row1, row1_len, SCM_ENC_UCS4);
  apnd = scm_string_new(SCM_MEM_HEAP,
                                      row2, row2_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(scm_string_append(str, apnd)));

  cut_assert_equal_uint(18u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_ref_ucs4(void)
{
  char row[256];
  size_t i, row_len;
  scm_char_t actual;
  const char *tmp[] = { "こ" , "の", "文", "字", "列", "は", "誤", "り", "で","あ",
                  "る", "" };
  scm_char_t expected[sizeof(tmp)/sizeof(tmp[1])];
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    utf8str_to_ucs4str(tmp[i], strlen(tmp[i]), expected + i);
  expected[11] = SCM_CHR_ZERO;

  row_len = utf8str_to_ucs4str("この文字列は誤りである",
                               sizeof("この文字列は誤りである") - 1,
                               row);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  actual = scm_string_ref(str, 0);
  cut_assert_equal_int(0, memcmp(expected + 0, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 1);
  cut_assert_equal_int(0, memcmp(expected + 1, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 2);
  cut_assert_equal_int(0, memcmp(expected + 2, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 3);
  cut_assert_equal_int(0, memcmp(expected + 3, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 4);
  cut_assert_equal_int(0, memcmp(expected + 4, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 5);
  cut_assert_equal_int(0, memcmp(expected + 5, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 6);
  cut_assert_equal_int(0, memcmp(expected + 6, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 7);
  cut_assert_equal_int(0, memcmp(expected + 7, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 8);
  cut_assert_equal_int(0, memcmp(expected + 8, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 9);
  cut_assert_equal_int(0, memcmp(expected + 9, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 10);
  cut_assert_equal_int(0, memcmp(expected + 10, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 11);
  cut_assert_equal_int(0, memcmp(expected + 11, &actual, sizeof(scm_char_t)));
}

void
test_scm_string_set_same_width_utf4(void)
{
  char row[256], expected[256];
  size_t row_len, expected_len;
  char actual[256];
  ssize_t len;
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("テスト", sizeof("テスト") - 1, row);
  expected_len = utf8str_to_ucs4str("テント", sizeof("テント") - 1, expected);
  utf8str_to_ucs4str("ン", sizeof("ン") - 1, &c);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(scm_string_set(str, 1, c)));

  cut_assert_equal_uint(3u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_ucs4(void)
{
  char row[256], expected[256];
  size_t row_len, expected_len;
  char actual[256];
  ssize_t len;
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                               sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                               row);

  expected_len = utf8str_to_ucs4str("この文字列は正しaaaの文は誤りである。",
                                    sizeof("この文字列は正しaaaの文は誤りである。") - 1,
                                    expected);
  utf8str_to_ucs4str("a", sizeof("a") - 1, &c);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(scm_string_fill(str, 8, 3, c)));

  cut_assert_equal_uint(20u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_append_ucs4(void)
{
  char row[256], expected[256];
  size_t row_len, expected_len;
  char actual[256];
  ssize_t len;
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                               sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                               row);

  expected_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りであaaaaa",
                                    sizeof("この文字列は正しい。前の文は誤りであaaaaa") - 1,
                                    expected);
  utf8str_to_ucs4str("a", sizeof("a") - 1, &c);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_true(scm_obj_not_null_p(scm_string_fill(str, 18, 5, c)));

  cut_assert_equal_uint(23u, scm_string_length(str));
  cut_assert_equal_uint(expected_len, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert(len >= 0);
  cut_assert_equal_uint(expected_len, (size_t)len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_find_chr_found_ucs4(void)
{
  char row[256];
  size_t row_len;
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                               sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                               row);
  utf8str_to_ucs4str("は", sizeof("は") - 1, &c);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_equal_int(5, scm_string_find_chr(str, c));
}

void
test_scm_string_find_chr_not_found_ucs4(void)
{
  char row[256];
  size_t row_len;
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  row_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                               sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                               row);
  utf8str_to_ucs4str("a", sizeof("a") - 1, &c);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row, row_len, SCM_ENC_UCS4);

  cut_assert_equal_int(-1, scm_string_find_chr(str, c));
}

void
test_scm_string_match_matched_ucs4(void)
{
  char row1[256], row2[256];
  size_t row1_len, row2_len;
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  row1_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                                sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                row1);

  row2_len = utf8str_to_ucs4str("しい。前の文", sizeof("しい。前の文") - 1, row2);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row1, row1_len, SCM_ENC_UCS4);
  pat = scm_string_new(SCM_MEM_HEAP,
                                     row2, row2_len, SCM_ENC_UCS4);

  cut_assert_equal_int(7, scm_string_match(str, pat));
}

void
test_scm_string_match_unmatched_ucs4(void)
{
  char row1[256], row2[256];
  size_t row1_len, row2_len;
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  row1_len = utf8str_to_ucs4str("この文字列は正しい。前の文は誤りである。",
                                sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                row1);

  row2_len = utf8str_to_ucs4str("しい、前の文", sizeof("しい、前の文") - 1, row2);

  str = scm_string_new(SCM_MEM_HEAP,
                                     row1, row1_len, SCM_ENC_UCS4);
  pat = scm_string_new(SCM_MEM_HEAP,
                                     row2, row2_len, SCM_ENC_UCS4);

  cut_assert_equal_int(-1, scm_string_match(str, pat));
}
