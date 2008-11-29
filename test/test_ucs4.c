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
  ucs4chr_t actual[256];

  str = ucs4str_from_utf8(src);

  cut_assert_not_null(str);

  cut_assert_equal_int(72 * sizeof(ucs4chr_t),
                       ucs4str_dump(str, actual, sizeof(actual)));
  cut_assert_equal_int(0, memcmp(expected, actual, sizeof(expected)));
}

void
test_ucs4str_convert_from_ascii_to_ucs4(void)
{
  Ucs4String *str;
  char src[] = {
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c,
    0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
    0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24,
    0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30,
    0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c,
    0x3d, 0x3e, 0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48,
    0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54,
    0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60,
    0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c,
    0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78,
    0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x00
  };
  ucs4chr_t expected[] = {
    0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 0x0008, 0x0009,
    0x000a, 0x000b, 0x000c, 0x000d, 0x000e, 0x000f, 0x0010, 0x0011, 0x0012,
    0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 0x0018, 0x0019, 0x001a, 0x001b,
    0x001c, 0x001d, 0x001e, 0x001f, 0x0020, 0x0021, 0x0022, 0x0023, 0x0024,
    0x0025, 0x0026, 0x0027, 0x0028, 0x0029, 0x002a, 0x002b, 0x002c, 0x002d,
    0x002e, 0x002f, 0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036,
    0x0037, 0x0038, 0x0039, 0x003a, 0x003b, 0x003c, 0x003d, 0x003e, 0x003f,
    0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 0x0048,
    0x0049, 0x004a, 0x004b, 0x004c, 0x004d, 0x004e, 0x004f, 0x0050, 0x0051,
    0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 0x0058, 0x0059, 0x005a,
    0x005b, 0x005c, 0x005d, 0x005e, 0x005f, 0x0060, 0x0061, 0x0062, 0x0063,
    0x0064, 0x0065, 0x0066, 0x0067, 0x0068, 0x0069, 0x006a, 0x006b, 0x006c,
    0x006d, 0x006e, 0x006f, 0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075,
    0x0076, 0x0077, 0x0078, 0x0079, 0x007a, 0x007b, 0x007c, 0x007d, 0x007e,
    0x007f
  };
  ucs4chr_t actual[256];

  str = ucs4str_from_ascii(src);

  cut_assert_not_null(str);

  cut_assert_equal_int((sizeof(src)/sizeof(src[0]) - 1) * sizeof(ucs4chr_t),
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
test_ucs4str_set(void)
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
  ucs4chr_t actual[256];
  int i;
  
  str = ucs4str_from_utf8(src);

  for (i = 0; i < sizeof(expected)/sizeof(expected[0]); i++) {
    ucs4chr_t tmp;
    tmp = expected[i];
    expected[i] = expected[sizeof(expected)/sizeof(expected[0]) - 1 - i];
    expected[sizeof(expected)/sizeof(expected[0]) - 1 - i] = tmp;
  }

  for (i = 0; i < ucs4str_length(str); i++) {
    ucs4chr_t tmp;
    tmp = ucs4str_get(str, i);
    ucs4str_set(str, i, ucs4str_get(str, ucs4str_length(str) - 1 - i));
    ucs4str_set(str, i, tmp);
  }

  cut_assert_equal_int(sizeof(expected),
                       ucs4str_dump(str, actual, sizeof(actual)));
  cut_assert_equal_int(0, memcmp(expected, actual, sizeof(expected)));
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
test_ucs4str_dup_and_modify(void)
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

void
test_ucs4str_push(void)
{
  Ucs4String *str;
  ucs4chr_t chr = 0x3002; /* "。"*/
  utf8_t *expected = (utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。";
  utf8_t actual[256];
  int i;

  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str);

  for (i = 0; i < 40; i++) 
    cut_assert_equal_pointer(str, ucs4str_push(str, chr));

  cut_assert_equal_int(70, ucs4str_length(str));

  cut_assert(ucs4str_to_utf8(str, actual, sizeof(actual)) >= 0);
  cut_assert_equal_string((char *)expected, (char *)actual);
}

void
test_ucs4str_fill(void)
{
  Ucs4String *str;
  ucs4chr_t chr = 0x3002; /* "。"*/
  utf8_t *expected = (utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。。";
  utf8_t actual[256];

  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_not_null(str);

  cut_assert_equal_pointer(str, ucs4str_fill(str, 30, 40, chr));

  cut_assert_equal_int(70, ucs4str_length(str));

  cut_assert(ucs4str_to_utf8(str, actual, sizeof(actual)) >= 0);
  cut_assert_equal_string((char *)expected, (char *)actual);
}

void
test_ucs4str_find_chr_fund(void)
{
  Ucs4String *str;
  ucs4chr_t exist = 0x3042; /* "あ" */
  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_equal_int(28, ucs4str_find_chr(str, exist));
  cut_assert_equal_int(exist, ucs4str_get(str, 28));
}

void
test_ucs4str_find_chr_not_found(void)
{
  Ucs4String *str;
  ucs4chr_t not_exist = 0x305a; /* "ず" */
  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_equal_int(-1, ucs4str_find_chr(str, not_exist));
}

void
test_ucs4str_match_match(void)
{
  Ucs4String *str;

  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_equal_int(9, ucs4str_match_utf8(str, (utf8_t *)"て明晰かつ判"));
}

void
test_ucs4str_match_unmatch(void)
{
  Ucs4String *str;

  str = ucs4str_from_utf8((utf8_t *)"わたしたちがきわめて明晰かつ判明に捉えることはすべて真である");

  cut_assert_equal_int(-1, ucs4str_match_utf8(str, (utf8_t *)"て明晰かつ明"));
}
