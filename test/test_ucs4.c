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
test_ucs4str_convert_from_utf8_to_ucs4(void)
{
  Ucs4String *str;
  utf8_t *src = (utf8_t *)"わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない";
  ucs4chr_t expected[] = {
    0x308f, 0x305f, 0x3057, 0x306f, 0x4e00, 0x3064, 0x306e, 0x5b9f, 0x4f53,
    0x3067, 0x3042, 0x308a, 0x3001, 0x305d, 0x306e, 0x672c, 0x8cea, 0x306a,
    0x3044, 0x3057, 0x672c, 0x6027, 0x306f, 0x8003, 0x3048, 0x308b, 0x3068,
    0x3044, 0x3046, 0x3053, 0x3068, 0x3060, 0x3051, 0x306b, 0x3042, 0x3063,
    0x3066, 0x3001, 0x5b58, 0x5728, 0x3059, 0x308b, 0x305f, 0x3081, 0x306b,
    0x3069, 0x3093, 0x306a, 0x5834, 0x6240, 0x3082, 0x8981, 0x305b, 0x305a,
    0x3001, 0x3044, 0x304b, 0x306a, 0x308b, 0x7269, 0x8cea, 0x7684, 0x306a,
    0x3082, 0x306e, 0x306b, 0x3082, 0x4f9d, 0x5b58, 0x3057, 0x306a, 0x3044 };
  ucs4chr_t actual[72];

  str = ucs4str_from_utf8(src);

  cut_assert_not_null(str);

  cut_assert_equal_int(72 * sizeof(ucs4chr_t),
                       ucs4str_dump(str, actual, sizeof(actual)));
  cut_assert_equal_int(0, memcmp(expected, actual, sizeof(expected)));
}

void
test_ucs4str_get(void)
{
  Ucs4String *str;
  utf8_t *src = (utf8_t *)"わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない";
  ucs4chr_t expected[] = {
    0x308f, 0x305f, 0x3057, 0x306f, 0x4e00, 0x3064, 0x306e, 0x5b9f, 0x4f53,
    0x3067, 0x3042, 0x308a, 0x3001, 0x305d, 0x306e, 0x672c, 0x8cea, 0x306a,
    0x3044, 0x3057, 0x672c, 0x6027, 0x306f, 0x8003, 0x3048, 0x308b, 0x3068,
    0x3044, 0x3046, 0x3053, 0x3068, 0x3060, 0x3051, 0x306b, 0x3042, 0x3063,
    0x3066, 0x3001, 0x5b58, 0x5728, 0x3059, 0x308b, 0x305f, 0x3081, 0x306b,
    0x3069, 0x3093, 0x306a, 0x5834, 0x6240, 0x3082, 0x8981, 0x305b, 0x305a,
    0x3001, 0x3044, 0x304b, 0x306a, 0x308b, 0x7269, 0x8cea, 0x7684, 0x306a,
    0x3082, 0x306e, 0x306b, 0x3082, 0x4f9d, 0x5b58, 0x3057, 0x306a, 0x3044 };
  int i;

  str = ucs4str_from_utf8(src);

  for (i = 0; i < sizeof(expected)/sizeof(expected[0]); i++)
    cut_assert_equal_int(expected[i], ucs4str_get(str, i));
  cut_assert_equal_int(UCS4EOF, ucs4str_get(str, i));
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

