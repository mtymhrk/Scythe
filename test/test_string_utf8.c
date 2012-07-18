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

void
test_scm_string_utf8(void)
{
  char expected[] = "テスト文字列";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  str = scm_string_new(SCM_MEM_HEAP,
                                     expected, sizeof(expected) - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(str));
  cut_assert_equal_uint(SCM_ENC_UTF8, scm_string_encoding(str));
  cut_assert_equal_uint(6, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_copy_utf8(void)
{
  char expected[] = "この文字列は誤りである";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  str = scm_string_new(SCM_MEM_HEAP,
                                     expected,
                                     sizeof(expected) - 1,
                                     SCM_ENC_UTF8);
  copy = scm_string_copy(str);

  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_string_dup_utf8(void)
{
  char expected[] = "この文字列は誤りである";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  str = scm_string_new(SCM_MEM_HEAP,
                                     expected, sizeof(expected) - 1,
                                     SCM_ENC_UTF8);
  copy = scm_string_dup(str);


  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_is_equal_compare_with_same_string_utf8(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りである",
                                      sizeof("この文字列は誤りである") - 1,
                                      SCM_ENC_UTF8);
  str2 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りである",
                                      sizeof("この文字列は誤りである") - 1,
                                      SCM_ENC_UTF8);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_different_string_utf8(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りである",
                                      sizeof("この文字列は誤りである") - 1,
                                      SCM_ENC_UTF8);
  str2 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りでない",
                                      sizeof("この文字列は誤りでない") - 1,
                                      SCM_ENC_UTF8);

  cut_assert_false(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_copy_string_utf8(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りである",
                                      sizeof("この文字列は誤りである") - 1,
                                      SCM_ENC_UTF8);
  str2 = scm_string_copy(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_duplicate_string_utf8(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                                      "この文字列は誤りである",
                                      sizeof("この文字列は誤りである") - 1,
                                      SCM_ENC_UTF8);
  str2 = scm_string_dup(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_substr_utf8(void)
{
  char expected[] = "誤りである";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, sub = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sub);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は誤りである",
                                     sizeof("この文字列は誤りである") - 1,
                                     SCM_ENC_UTF8);
  sub = scm_string_substr(str, 6, 5);

  cut_assert_true(scm_obj_not_null_p(sub));
  cut_assert_equal_uint(5u, scm_string_length(sub));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(sub));

  len = scm_string_dump(sub, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_push_utf8(void)
{
  char expected[] = "この文字列は誤りである。";
  scm_char_t pushed;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&pushed, 0, sizeof(pushed));
  memcpy(&pushed, "。", 3);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は誤りである",
                                     sizeof("この文字列は誤りである") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_push(str, pushed)));

  cut_assert_equal_uint(12u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_append_utf8(void)
{
  char expected[] = "次の文は正しい。前の文は誤りである。";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, apnd = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &apnd);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "次の文は正しい。",
                                     sizeof("次の文は正しい。") - 1,
                                     SCM_ENC_UTF8);
  apnd = scm_string_new(SCM_MEM_HEAP,
                                      "前の文は誤りである。",
                                      sizeof("前の文は誤りである。") - 1,
                                      SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_append(str, apnd)));

  cut_assert_equal_uint(18u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_ref_utf8(void)
{
  unsigned int i;
  scm_char_t actual;
  const char *tmp[] = { "こ" , "の", "文", "字", "列", "は", "誤", "り", "で","あ",
                        "る", "" };
  scm_char_t expected[sizeof(tmp)/sizeof(tmp[1])];
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(expected, 0, sizeof(expected));
  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    memcpy(expected + i, tmp[i], strlen(tmp[i]));
  expected[11] = SCM_CHR_ZERO;

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は誤りである",
                                     sizeof("この文字列は誤りである") - 1,
                                     SCM_ENC_UTF8);

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
test_scm_string_set_less_width_utf8(void)
{
  char expected[] = "テaト";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "テスト", sizeof("テスト") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_set(str, 1, c)));

  cut_assert_equal_uint(3u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_set_same_width_utf8(void)
{
  char expected[] = "テント";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "ン", 3);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "テスト",
                                     sizeof("テスト") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_set(str, 1, c)));

  cut_assert_equal_uint(3u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_set_greater_width_utf8(void)
{
  char expected[] = "aあc";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "あ", 3);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "abc", sizeof("abc") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_set(str, 1, c)));

  cut_assert_equal_uint(3u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_utf8(void)
{
  char expected[] = "この文字列は正しaaaの文は誤りである。";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_fill(str, 8, 3, c)));

  cut_assert_equal_uint(20u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_append_utf8(void)
{
  char expected[] = "この文字列は正しい。前の文は誤りであaaaaa";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_true(scm_obj_not_null_p(scm_string_fill(str, 18, 5, c)));

  cut_assert_equal_uint(23u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_find_chr_found_utf8(void)
{
  scm_char_t c;
  ScmObj str;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "は", 3);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_equal_int(5, scm_string_find_chr(str, c));
}

void
test_scm_string_find_chr_not_found_utf8(void)
{
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_equal_int(-1, scm_string_find_chr(str, c));
}

void
test_scm_string_match_matched_utf8(void)
{
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);
  pat = scm_string_new(SCM_MEM_HEAP,
                                     "しい。前の文",
                                     sizeof("しい。前の文") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_equal_int(7, scm_string_match(str, pat));
}

void
test_scm_string_match_unmatched_utf8(void)
{
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  str = scm_string_new(SCM_MEM_HEAP,
                                     "この文字列は正しい。前の文は誤りである。",
                                     sizeof("この文字列は正しい。前の文は誤りである。") - 1,
                                     SCM_ENC_UTF8);
  pat = scm_string_new(SCM_MEM_HEAP,
                                     "しい、前の文",
                                     sizeof("しい、前の文") - 1,
                                     SCM_ENC_UTF8);

  cut_assert_equal_int(-1, scm_string_match(str, pat));
}
