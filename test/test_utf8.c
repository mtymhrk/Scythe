#include <cutter.h>

#include "utf8.h"

void
test_utf8str(void)
{
  char expected[] = "テスト文字列";
  char actual[256];
  size_t len;

  Utf8String *str = utf8str(expected, sizeof(expected) - 1);

  cut_assert_not_null(str);
  cut_assert_equal_int(6, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
}

void
test_str8str_copy(void)
{
  char expected[] = "この文字列は誤りである";
  
  Utf8String *str = utf8str(expected, sizeof(expected) - 1);
  Utf8String *copy = utf8str_copy(str);
  char actual[256];
  size_t len;

  cut_assert_equal_int(utf8str_length(str), utf8str_length(copy));
  cut_assert_equal_int(utf8str_bytesize(str), utf8str_bytesize(copy));

  len = utf8str_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
  utf8str_destruct(copy);
}

void
test_str8str_dup(void)
{
  char expected[] = "この文字列は誤りである";
  
  Utf8String *str = utf8str(expected, sizeof(expected) - 1);
  Utf8String *copy = utf8str_dup(str);
  char actual[256];
  size_t len;

  cut_assert_equal_int(utf8str_length(str), utf8str_length(copy));
  cut_assert_equal_int(utf8str_bytesize(str), utf8str_bytesize(copy));

  len = utf8str_dump(copy, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
  utf8str_destruct(copy);
}

void
test_str8str_is_equal_compare_with_same_string(void)
{
  Utf8String *str1 = utf8str("この文字列は誤りである",
                             sizeof("この文字列は誤りである") - 1);
  Utf8String *str2 = utf8str("この文字列は誤りである",
                             sizeof("この文字列は誤りである") - 1);

  cut_assert_true(utf8str_is_equal(str1, str2));

  utf8str_destruct(str1);
  utf8str_destruct(str2);
}

void
test_str8str_is_equal_compare_with_different_string(void)
{
  Utf8String *str1 = utf8str("この文字列は誤りである",
                             sizeof("この文字列は誤りである") - 1);
  Utf8String *str2 = utf8str("この文字列は誤りでない",
                             sizeof("この文字列は誤りでない") - 1);

  cut_assert_false(utf8str_is_equal(str1, str2));

  utf8str_destruct(str1);
  utf8str_destruct(str2);
}

void
test_str8str_is_equal_compare_with_copy_string(void)
{
  Utf8String *str1 = utf8str("この文字列は誤りである",
                             sizeof("この文字列は誤りである") - 1);
  Utf8String *str2 = utf8str_copy(str1);

  cut_assert_true(utf8str_is_equal(str1, str2));

  utf8str_destruct(str1);
  utf8str_destruct(str2);
}

void
test_str8str_is_equal_compare_with_duplicate_string(void)
{
  Utf8String *str1 = utf8str("この文字列は誤りである",
                             sizeof("この文字列は誤りである") - 1);
  Utf8String *str2 = utf8str_dup(str1);

  cut_assert_true(utf8str_is_equal(str1, str2));

  utf8str_destruct(str1);
  utf8str_destruct(str2);
}

void
test_str8str_substr(void)
{
  char expected[] = "誤りである";
  char actual[256];
  int len;

  Utf8String *str = utf8str("この文字列は誤りである",
                            sizeof("この文字列は誤りである") - 1);
  Utf8String *sub = utf8str_substr(str, 6, 5);

  cut_assert_not_null(sub);
  cut_assert_equal_int(5, utf8str_length(sub));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(sub));

  len = utf8str_dump(sub, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
  utf8str_destruct(sub);
}

void
test_utf8str_push(void)
{
  char expected[] = "この文字列は誤りである。";
  char pushed[] = "。";
  char actual[256];
  int len;

  Utf8String *str = utf8str("この文字列は誤りである",
                            sizeof("この文字列は誤りである") - 1);

  cut_assert_not_null(utf8str_push(str, (Utf8Chr *)pushed));

  cut_assert_equal_int(12, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
}

void
test_utf8str_append(void)
{
  char expected[] = "この文字列は正しい。前の文は誤りである。";
  char actual[256];
  int len;

  Utf8String *str = utf8str("この文字列は正しい。",
                            sizeof("この文字列は正しい。") - 1);
  Utf8String *apnd = utf8str("前の文は誤りである。",
                             sizeof("前の文は誤りである。") - 1);

  cut_assert_not_null(utf8str_append(str, apnd));

  cut_assert_equal_int(20, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
  utf8str_destruct(apnd);
}

void
test_utf8str_get(void)
{
  int i;
  Utf8Chr actual;
  Utf8Chr expected[] = { { "こ" }, { "の" }, { "文" }, { "字" }, { "列" },
                         { "は" }, { "誤" }, { "り" }, { "で" }, { "あ" },
                         { "る" },  { "" } };
  Utf8Chr tmp[sizeof(expected)/sizeof(expected[1])];

  memset(tmp, 0, sizeof(tmp));
  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    strncpy((char *)tmp[i].chr, (char *)expected[i].chr, 4);


  Utf8String *str = utf8str("この文字列は誤りである",
                            sizeof("この文字列は誤りである") - 1);


  actual = utf8str_get(str, 0);
  cut_assert_equal_int(0, memcmp(tmp + 0, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 1);
  cut_assert_equal_int(0, memcmp(tmp + 1, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 2);
  cut_assert_equal_int(0, memcmp(tmp + 2, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 3);
  cut_assert_equal_int(0, memcmp(tmp + 3, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 4);
  cut_assert_equal_int(0, memcmp(tmp + 4, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 5);
  cut_assert_equal_int(0, memcmp(tmp + 5, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 6);
  cut_assert_equal_int(0, memcmp(tmp + 6, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 7);
  cut_assert_equal_int(0, memcmp(tmp + 7, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 8);
  cut_assert_equal_int(0, memcmp(tmp + 8, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 9);
  cut_assert_equal_int(0, memcmp(tmp + 9, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 10);
  cut_assert_equal_int(0, memcmp(tmp + 10, &actual, sizeof(Utf8Chr)));

  actual = utf8str_get(str, 11);
  cut_assert_equal_int(0, memcmp(tmp + 11, &actual, sizeof(Utf8Chr)));
}

void
test_utf8str_set(void)
{
  char expected[] = "テaト";
  char actual[256];
  Utf8Chr c = { "a" };
  int len;

  Utf8String *str = utf8str("テスト", sizeof("テスト") - 1);

  cut_assert_not_null(utf8str_set(str, 1, &c));

  cut_assert_equal_int(3, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
}

void
test_utf8str_fill(void)
{
  char expected[] = "この文字列は正しaaaの文は誤りである。";
  char actual[256];
  Utf8Chr c = { "a" };
  int len;

  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);

  cut_assert_not_null(utf8str_fill(str, 8, 3, &c));

  cut_assert_equal_int(20, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
}

void
test_utf8str_fill_append(void)
{
  char expected[] = "この文字列は正しい。前の文は誤りであaaaaa";
  char actual[256];
  Utf8Chr c = { "a" };
  int len;

  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);

  cut_assert_not_null(utf8str_fill(str, 18, 5, &c));

  cut_assert_equal_int(23, utf8str_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, utf8str_bytesize(str));

  len = utf8str_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  utf8str_destruct(str);
}

void
test_utf8str_find_chr_found(void)
{
  Utf8Chr c = { "は" };

  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);

  cut_assert_equal_int(5, utf8str_find_chr(str, &c));

  utf8str_destruct(str);
}

void
test_utf8str_find_chr_not_found(void)
{
  Utf8Chr c = { "a" };

  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);

  cut_assert_equal_int(-1, utf8str_find_chr(str, &c));

  utf8str_destruct(str);
}

void
test_utf8str_match_matched(void)
{
  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);
  Utf8String *pat = utf8str("しい。前の文",
                            sizeof("しい。前の文") - 1);

  cut_assert_equal_int(7, utf8str_match(str, pat));

  utf8str_destruct(str);
  utf8str_destruct(pat);
}

void
test_utf8str_match_unmatched(void)
{
  Utf8String *str = utf8str("この文字列は正しい。前の文は誤りである。",
                            sizeof("この文字列は正しい。前の文は誤りである。") - 1);
  Utf8String *pat = utf8str("しい、前の文",
                            sizeof("しい、前の文") - 1);

  cut_assert_equal_int(-1, utf8str_match(str, pat));

  utf8str_destruct(str);
  utf8str_destruct(pat);
}
