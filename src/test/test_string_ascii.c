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
  scm_capi_evaluator_make_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_string_ascii(void)
{
  char expected[] = "test string";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  str = scm_string_new(SCM_MEM_HEAP,
                       expected, sizeof(expected) - 1,
                       SCM_ENC_ASCII);

  cut_assert_true(scm_obj_not_null_p(str));
  cut_assert_equal_pointer(SCM_ENC_ASCII, scm_string_encoding(str));
  cut_assert_equal_uint(11, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_copy_ascii(void)
{
  char expected[] = "this string is fault";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  str = scm_string_new(SCM_MEM_HEAP,
                       expected, sizeof(expected) - 1,
                       SCM_ENC_ASCII);

  copy = scm_string_copy(str);

  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_dup_ascii(void)
{
  char expected[] = "this string is fault";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, copy = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &copy);

  str = scm_string_new(SCM_MEM_HEAP,
                       expected, sizeof(expected) - 1,
                       SCM_ENC_ASCII);
  copy = scm_string_dup(str);


  cut_assert_equal_uint(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_uint(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_is_equal_compare_with_same_string_ascii(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                        "this string is fault",
                        sizeof("this string is fault") - 1,
                        SCM_ENC_ASCII);
  str2 = scm_string_new(SCM_MEM_HEAP,
                        "this string is fault",
                        sizeof("this string is fault") - 1,
                        SCM_ENC_ASCII);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_different_string_ascii(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                        "this string is fault",
                        sizeof("this string is fault") - 1,
                        SCM_ENC_ASCII);

  str2 = scm_string_new(SCM_MEM_HEAP,
                        "this string is not fault",
                        sizeof("this string is not fault") - 1,
                        SCM_ENC_ASCII);

  cut_assert_false(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_copy_string_ascii(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                        "this string is fault",
                        sizeof("this string is fault") - 1,
                        SCM_ENC_ASCII);

  str2 = scm_string_copy(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_is_equal_compare_with_duplicate_string_ascii(void)
{
  ScmObj str1 = SCM_OBJ_INIT, str2 = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str1, &str2);

  str1 = scm_string_new(SCM_MEM_HEAP,
                        "this string is fault",
                        sizeof("this string is fault") - 1,
                        SCM_ENC_ASCII);
  str2 = scm_string_dup(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));
}

void
test_scm_string_substr_ascii(void)
{
  char expected[] = "is fault";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, sub = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sub);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);
  sub = scm_string_substr(str, 12, 8);

  cut_assert_true(scm_obj_not_null_p(sub));
  cut_assert_equal_uint(8u, scm_string_length(sub));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(sub));

  len = scm_string_dump(sub, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_push_ascii(void)
{
  char expected[] = "this string is fault.";
  scm_char_t pushed;
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  memset(&pushed, 0, sizeof(pushed));
  memcpy(&pushed, ".", 3);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(0, scm_string_push(str, &pushed));

  cut_assert_equal_uint(21u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_append_ascii(void)
{
  char expected[] = "next sentence is right. previous sentence is fault";
  char actual[256];
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT, apnd = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &apnd);

  str = scm_string_new(SCM_MEM_HEAP,
                       "next sentence is right.",
                       sizeof("next sentence is right.") - 1,
                       SCM_ENC_ASCII);

  apnd = scm_string_new(SCM_MEM_HEAP,
                        " previous sentence is fault",
                        sizeof(" previous sentence is fault") - 1,
                        SCM_ENC_ASCII);

  cut_assert_equal_int(0, scm_string_append(str, apnd));

  cut_assert_equal_uint(50u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_ref_ascii(void)
{
  unsigned int i;
  scm_char_t actual;
  const char *tmp[] = { "t" , "h", "i", "s", " ", "s", "t", "r", "i","n", "g",
                        " ", "i", "s", " ", "f", "a", "u", "l", "t", "" };
  scm_char_t expected[sizeof(tmp)/sizeof(tmp[1])];
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(expected, 0, sizeof(expected));
  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    memcpy(expected + i, tmp[i], strlen(tmp[i]));

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(0, scm_string_ref(str, 0, &actual));
  cut_assert_equal_int(0, memcmp(expected + 0, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 1, &actual));
  cut_assert_equal_int(0, memcmp(expected + 1, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 2, &actual));
  cut_assert_equal_int(0, memcmp(expected + 2, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 3, &actual));
  cut_assert_equal_int(0, memcmp(expected + 3, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 4, &actual));
  cut_assert_equal_int(0, memcmp(expected + 4, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 5, &actual));
  cut_assert_equal_int(0, memcmp(expected + 5, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 6, &actual));
  cut_assert_equal_int(0, memcmp(expected + 6, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 7, &actual));
  cut_assert_equal_int(0, memcmp(expected + 7, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 8, &actual));
  cut_assert_equal_int(0, memcmp(expected + 8, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 9, &actual));
  cut_assert_equal_int(0, memcmp(expected + 9, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 10, &actual));
  cut_assert_equal_int(0, memcmp(expected + 10, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 11, &actual));
  cut_assert_equal_int(0, memcmp(expected + 11, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 12, &actual));
  cut_assert_equal_int(0, memcmp(expected + 12, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 13, &actual));
  cut_assert_equal_int(0, memcmp(expected + 13, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 14, &actual));
  cut_assert_equal_int(0, memcmp(expected + 14, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 15, &actual));
  cut_assert_equal_int(0, memcmp(expected + 15, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 16, &actual));
  cut_assert_equal_int(0, memcmp(expected + 16, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 17, &actual));
  cut_assert_equal_int(0, memcmp(expected + 17, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 18, &actual));
  cut_assert_equal_int(0, memcmp(expected + 18, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(0, scm_string_ref(str, 19, &actual));
  cut_assert_equal_int(0, memcmp(expected + 19, &actual, sizeof(scm_char_t)));

  cut_assert_equal_int(-1, scm_string_ref(str, 20, &actual));
}

void
test_scm_string_set_same_width_ascii(void)
{
  char expected[] = "adc";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "d", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                       "abc", sizeof("abc") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(0, scm_string_set(str, 1, &c));

  cut_assert_equal_uint(3u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_ascii(void)
{
  char expected[] = "next sentence is aaaaa. previous sentence is fault";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                       "next sentence is right. previous sentence is fault",
                       sizeof("next sentence is right. previous sentence is fault") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(0, scm_string_fill(str, 17, 5, &c));

  cut_assert_equal_uint(50u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_fill_append_ascii(void)
{
  char expected[] = "next sentence is right. previous sentence is fauaaaaa";
  char actual[256];
  scm_char_t c;
  ssize_t len;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                       "next sentence is right. previous sentence is fault",
                       sizeof("next sentence is right. previous sentence is fault") - 1,
                       SCM_ENC_ASCII);


  cut_assert_equal_int(0, scm_string_fill(str, 48, 5, &c));

  cut_assert_equal_uint(53u, scm_string_length(str));
  cut_assert_equal_uint(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, (size_t)len));
}

void
test_scm_string_find_chr_found_ascii(void)
{
  scm_char_t c;
  ScmObj str;

  SCM_STACK_FRAME_PUSH(&str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "r", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(7, scm_string_find_chr(str, c));
}

void
test_scm_string_find_chr_not_found_ascii(void)
{
  scm_char_t c;
  ScmObj str = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(str);

  memset(&c, 0, sizeof(c));
  memcpy(&c, "z", 1);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(-1, scm_string_find_chr(str, c));
}

void
test_scm_string_match_matched_ascii(void)
{
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);

  pat = scm_string_new(SCM_MEM_HEAP,
                       "g is f",
                       sizeof("g is f") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(10, scm_string_match(str, pat));
}

void
test_scm_string_match_unmatched_ascii(void)
{
  ScmObj str = SCM_OBJ_INIT, pat = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &pat);

  str = scm_string_new(SCM_MEM_HEAP,
                       "this string is fault",
                       sizeof("this string is fault") - 1,
                       SCM_ENC_ASCII);
  pat = scm_string_new(SCM_MEM_HEAP,
                       "g-is-f",
                       sizeof("g-is-f") - 1,
                       SCM_ENC_ASCII);

  cut_assert_equal_int(-1, scm_string_match(str, pat));
}
