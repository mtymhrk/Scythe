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
}

void
test_ucs4str_from_utf8(void)
{
  Ucs4String *str;
  utf8_t utf8[512];

  str = ucs4str_from_utf8((utf8_t *)"わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない");

  cut_assert_not_null(str);
  cut_assert_equal_int(72, ucs4str_length(str));

  ucs4str_to_utf8(str, utf8, sizeof(utf8));
  cut_assert_equal_string("わたしは一つの実体であり、その本質ないし本性は考えるということだけにあって、存在するためにどんな場所も要せず、いかなる物質的なものにも依存しない", (char *)utf8);
}
