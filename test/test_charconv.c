#include <cutter.h>

#include "charconv.h"

void
test_charconv_construct(void)
{
  ScmCharConv *conv = scm_charconv_construct("EUC-JP", "UTF-8");

  cut_assert_not_null(conv);
  cut_assert_false(scm_charconv_ready(conv));
  cut_assert_false(scm_charconv_has_error(conv));

  scm_charconv_destruct(conv);
}

void
test_convert_eucjp_to_utf8_ascii(void)
{
  char input[] = "abcdefghijklmnopqrstuvwxyz";
  char output[sizeof(input)];
  size_t len, ret;
  ScmCharConv *conv = scm_charconv_construct("EUC-JP", "UTF-8");

  len = scm_charconv_convert(conv,
                             input, sizeof(input) - 1,
                             output, sizeof(output) - 1);

  cut_assert(len >= 0);

  ret = scm_charconv_terminate(conv, output + len, sizeof(output) - 1 - len);

  cut_assert(ret >= 0);

  output[len + ret] = '\0';

  cut_assert_equal_string(input, output);
  cut_assert_false(scm_charconv_has_error(conv));
  cut_assert_false(scm_charconv_ready(conv));

  scm_charconv_destruct(conv);
}

void
test_convert_utf8_to_jis_and_jis_to_utf8(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないことについては、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char jis[256];
  size_t ret, len_jis, len_utf8;

  ScmCharConv *conv_to_jis = scm_charconv_construct("UTF8","ISO-2022-JP");
  ScmCharConv *conv_to_utf8 = scm_charconv_construct("ISO-2022-JP", "UTF8");

  ret = scm_charconv_convert(conv_to_jis,
                             utf8_src, sizeof(utf8_src) - 1,
                             jis, sizeof(jis) - 1);

  cut_assert(ret >= 0);

  len_jis = ret;
  ret = scm_charconv_terminate(conv_to_jis,
                               jis + len_jis, sizeof(jis) - 1 - len_jis);

  cut_assert(ret >= 0);

  len_jis += ret;
  jis[len_jis] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_jis));
  cut_assert_false(scm_charconv_ready(conv_to_jis));
  
  ret = scm_charconv_convert(conv_to_utf8,
                             jis, len_jis, utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_ready(conv_to_utf8));

  cut_assert_equal_string(utf8_src, utf8_dst);

  scm_charconv_destruct(conv_to_jis);
  scm_charconv_destruct(conv_to_utf8);
}
