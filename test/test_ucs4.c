#include <cutter.h>

#include "ucs4.h"

void
test_ucs4str_from_ascii(void)
{
  Ucs4String *str;
  char ascii[256];

  str = ucs4str_from_ascii("hello, world");

  cut_assert_not_null(str);
  cut_assert_equal_int(strlen("hello, world"),
                       ucs4str_length(str));

  ucs4str_to_ascii(str, ascii, sizeof(ascii));
  cut_assert_equal_string("hello, world", ascii);

  ucs4str_destruct(str);
}

void
test_ucs4str_from_utf8(void)
{
  Ucs4String *str;
  utf8_t utf8[512];

  str = ucs4str_from_utf8((utf8_t *)"わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない");

  cut_assert_not_null(str);
  cut_assert_equal_int(72, ucs4str_length(str));

  cut_assert(ucs4str_to_utf8(str, utf8, sizeof(utf8)) >= 0);
  cut_assert_equal_string("わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない", (char *)utf8);

  ucs4str_destruct(str);
}

void
test_ucs4str_copy(void)
{
  Ucs4String *str1, *str2;

  str1 = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str1);

  str2 = ucs4str_copy(str1);

  cut_assert_not_null(str2);

  cut_assert_equal_int(30, ucs4str_length(str1));
  cut_assert_equal_int(30, ucs4str_length(str2));
  cut_assert_true(ucs4str_is_equal(str1, str2));

  ucs4str_destruct(str1);
  ucs4str_destruct(str2);
}

void
test_cus4str_copy_and_modify(void)
{
  Ucs4String *str1, *str2;
  utf8_t utf8[512];

  str1 = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str1);

  str2 = ucs4str_copy(str1);

  cut_assert_not_null(str2);

  cut_assert_not_null(ucs4str_append_utf8(str2,
                                          (utf8_t *)"、これを一般的な規則としてよい"));

  cut_assert(ucs4str_to_utf8(str1, utf8, sizeof(utf8)) >= 0);
  cut_assert_equal_string("わたしたちがきわめて明晰かつ判明に捉えることはすべて真である", (char *)utf8);

  cut_assert(ucs4str_to_utf8(str2, utf8, sizeof(utf8)) >= 0);
  cut_assert_equal_string("わたしたちがきわめて明晰かつ判明に捉えることはすべて真である、これを一般的な規則としてよい", (char *)utf8);

  cut_assert_false(ucs4str_is_equal(str1, str2));

  ucs4str_destruct(str1);
  ucs4str_destruct(str2);
}

void
test_ucs4str_dup(void)
{
  Ucs4String *str1, *str2;

  str1 = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str1);

  str2 = ucs4str_dup(str1);

  cut_assert_not_null(str2);

  cut_assert_equal_int(30, ucs4str_length(str1));
  cut_assert_equal_int(30, ucs4str_length(str2));
  cut_assert_true(ucs4str_is_equal(str1, str2));

  ucs4str_destruct(str1);
  ucs4str_destruct(str2);
}

void
test_cus4str_dup_and_modify(void)
{
  Ucs4String *str1, *str2;
  utf8_t utf8[512];

  str1 = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str1);

  str2 = ucs4str_dup(str1);

  cut_assert_not_null(str2);

  cut_assert_not_null(ucs4str_append_utf8(str2,
                                          (utf8_t *)"、これを一般的な規則としてよい"));

  cut_assert(ucs4str_to_utf8(str1, utf8, sizeof(utf8)) >= 0);
  cut_assert_equal_string("わたしたちがきわめて明晰かつ判明に捉えることはすべて真である", (char *)utf8);

  cut_assert(ucs4str_to_utf8(str2, utf8, sizeof(utf8)) >= 0);
  cut_assert_equal_string("わたしたちがきわめて明晰かつ判明に捉えることはすべて真である、これを一般的な規則としてよい", (char *)utf8);

  cut_assert_false(ucs4str_is_equal(str1, str2));

  ucs4str_destruct(str1);
  ucs4str_destruct(str2);
}
