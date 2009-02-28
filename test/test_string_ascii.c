#include <cutter.h>

#include "string.h"

void
test_scm_string_ascii(void)
{
  char expected[] = "test string";
  char actual[256];
  size_t len;

  ScmString *str = scm_string_construct_new(expected, sizeof(expected) - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_not_null(str);
  cut_assert_equal_int(11, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_copy_ascii(void)
{
  char expected[] = "this string is fault";
  
  ScmString *str = scm_string_construct_new(expected,
                                            sizeof(expected) - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *copy = scm_string_copy(str);
  char actual[256];
  size_t len;

  cut_assert_equal_int(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_int(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(copy);
}

void
test_scm_string_dup_ascii(void)
{
  char expected[] = "this string is fault";
  
  ScmString *str = scm_string_construct_new(expected, sizeof(expected) - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *copy = scm_string_dup(str);
  char actual[256];
  size_t len;

  cut_assert_equal_int(scm_string_length(str), scm_string_length(copy));
  cut_assert_equal_int(scm_string_bytesize(str), scm_string_bytesize(copy));

  len = scm_string_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(copy);
}

void
test_scm_string_is_equal_compare_with_same_string_ascii(void)
{
  ScmString *str1 = scm_string_construct_new("this string is fault",
                                             sizeof("this string is fault") - 1,
                                             SCM_ENCODING_ASCII);
  ScmString *str2 = scm_string_construct_new("this string is fault",
                                             sizeof("this string is fault") - 1,
                                             SCM_ENCODING_ASCII);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_different_string_ascii(void)
{
  ScmString *str1 = scm_string_construct_new("this string is fault",
                                             sizeof("this string is fault") - 1,
                                             SCM_ENCODING_ASCII);
  ScmString *str2 = scm_string_construct_new("this string is not fault",
                                             sizeof("this string is not fault") - 1,
                                             SCM_ENCODING_ASCII);

  cut_assert_false(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_copy_string_ascii(void)
{
  ScmString *str1 = scm_string_construct_new("this string is fault",
                                             sizeof("this string is fault") - 1,
                                             SCM_ENCODING_ASCII);
  ScmString *str2 = scm_string_copy(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_duplicate_string_ascii(void)
{
  ScmString *str1 = scm_string_construct_new("this string is fault",
                                             sizeof("this string is fault") - 1,
                                             SCM_ENCODING_ASCII);
  ScmString *str2 = scm_string_dup(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_substr_ascii(void)
{
  char expected[] = "is fault";
  char actual[256];
  int len;

  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *sub = scm_string_substr(str, 12, 8);

  cut_assert_not_null(sub);
  cut_assert_equal_int(8, scm_string_length(sub));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(sub));

  len = scm_string_dump(sub, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(sub);
}

void
test_scm_string_push_ascii(void)
{
  char expected[] = "this string is fault.";
  scm_char_t pushed;
  char actual[256];
  int len;

  memset(&pushed, 0, sizeof(pushed));
  memcpy(&pushed, ".", 3);

  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_not_null(scm_string_push(str, pushed));

  cut_assert_equal_int(21, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_append_ascii(void)
{
  char expected[] = "next sentence is right. previous sentence is fault";
  char actual[256];
  int len;

  ScmString *str = scm_string_construct_new("next sentence is right.",
                                            sizeof("next sentence is right.") - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *apnd = scm_string_construct_new(" previous sentence is fault",
                                             sizeof(" previous sentence is fault") - 1,
                                             SCM_ENCODING_ASCII);

  cut_assert_not_null(scm_string_append(str, apnd));

  cut_assert_equal_int(50, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(apnd);
}

void
test_scm_string_ref_ascii(void)
{
  int i;
  scm_char_t actual;
  char *tmp[] = { "t" , "h", "i", "s", " ", "s", "t", "r", "i","n", "g", " ",
                  "i", "s", " ", "f", "a", "u", "l", "t", "" };
  scm_char_t expected[sizeof(tmp)/sizeof(tmp[1])];

  memset(expected, 0, sizeof(expected));
  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    memcpy(expected + i, tmp[i], strlen(tmp[i]));
  expected[i - 1] = SCM_CHR_ZERO;

  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);

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

  actual = scm_string_ref(str, 12);
  cut_assert_equal_int(0, memcmp(expected + 12, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 13);
  cut_assert_equal_int(0, memcmp(expected + 13, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 14);
  cut_assert_equal_int(0, memcmp(expected + 14, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 15);
  cut_assert_equal_int(0, memcmp(expected + 15, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 16);
  cut_assert_equal_int(0, memcmp(expected + 16, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 17);
  cut_assert_equal_int(0, memcmp(expected + 17, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 18);
  cut_assert_equal_int(0, memcmp(expected + 18, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 19);
  cut_assert_equal_int(0, memcmp(expected + 19, &actual, sizeof(scm_char_t)));

  actual = scm_string_ref(str, 20);
  cut_assert_equal_int(0, memcmp(expected + 20, &actual, sizeof(scm_char_t)));
}

void
test_scm_string_set_same_width_ascii(void)
{
  char expected[] = "adc";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "d", 1);

  ScmString *str = scm_string_construct_new("abc",
                                            sizeof("abc") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_not_null(scm_string_set(str, 1, c));

  cut_assert_equal_int(3, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_fill_ascii(void)
{
  char expected[] = "next sentence is aaaaa. previous sentence is fault";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct_new("next sentence is right. previous sentence is fault",
                                            sizeof("next sentence is right. previous sentence is fault") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_not_null(scm_string_fill(str, 17, 5, c));

  cut_assert_equal_int(50, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_fill_append_ascii(void)
{
  char expected[] = "next sentence is right. previous sentence is fauaaaaa";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct_new("next sentence is right. previous sentence is fault",
                                            sizeof("next sentence is right. previous sentence is fault") - 1,
                                            SCM_ENCODING_ASCII);


  cut_assert_not_null(scm_string_fill(str, 48, 5, c));

  cut_assert_equal_int(53, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_find_chr_found_ascii(void)
{
  scm_char_t c;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "r", 1);

  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_equal_int(7, scm_string_find_chr(str, c));

  scm_string_destruct(str);
}

void
test_scm_string_find_chr_not_found_ascii(void)
{
  scm_char_t c;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "z", 1);

  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_equal_int(-1, scm_string_find_chr(str, c));

  scm_string_destruct(str);
}

void
test_scm_string_match_matched_ascii(void)
{
  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *pat = scm_string_construct_new("g is f",
                                            sizeof("g is f") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_equal_int(10, scm_string_match(str, pat));

  scm_string_destruct(str);
  scm_string_destruct(pat);
}

void
test_scm_string_match_unmatched_ascii(void)
{
  ScmString *str = scm_string_construct_new("this string is fault",
                                            sizeof("this string is fault") - 1,
                                            SCM_ENCODING_ASCII);
  ScmString *pat = scm_string_construct_new("g-is-f",
                                            sizeof("g-is-f") - 1,
                                            SCM_ENCODING_ASCII);

  cut_assert_equal_int(-1, scm_string_match(str, pat));

  scm_string_destruct(str);
  scm_string_destruct(pat);
}
