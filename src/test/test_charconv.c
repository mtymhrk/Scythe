#include <cutter.h>

#include "charconv.h"

void
test_charconv_new(void)
{
  ScmCharConv *conv = scm_charconv_new("EUC-JP", "UTF-8",
                                       SCM_CHARCONV_OMIT);

  cut_assert_not_null(conv);
  cut_assert_equal_string("EUC-JP", scm_charconv_src_encoding(conv));
  cut_assert_equal_string("UTF-8", scm_charconv_dst_encoding(conv));
  cut_assert_false(scm_charconv_is_ready(conv));
  cut_assert_false(scm_charconv_has_error(conv));

  scm_charconv_end(conv);
}

void
test_convert_eucjp_to_utf8_ascii(void)
{
  char input[] = "abcdefghijklmnopqrstuvwxyz";
  char output[sizeof(input)];
  ssize_t len, ret;
  ScmCharConv *conv = scm_charconv_new("EUC-JP", "UTF-8",
                                       SCM_CHARCONV_OMIT);

  len = scm_charconv_convert(conv,
                             input, sizeof(input) - 1,
                             output, sizeof(output) - 1);

  cut_assert(len >= 0);

  ret = scm_charconv_terminate(conv, output + len,
                               sizeof(output) - 1 - (size_t)len);

  cut_assert(ret >= 0);

  output[len + ret] = '\0';

  cut_assert_equal_string(input, output);
  cut_assert_false(scm_charconv_has_error(conv));
  cut_assert_false(scm_charconv_is_ready(conv));

  scm_charconv_end(conv);
}

void
test_convert_utf8_to_jis_and_jis_to_utf8(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないことについては、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char jis[256];
  ssize_t ret;
  size_t len_jis, len_utf8;

  ScmCharConv *conv_to_jis = scm_charconv_new("UTF8", "ISO-2022-JP",
                                                    SCM_CHARCONV_OMIT);
  ScmCharConv *conv_to_utf8 = scm_charconv_new("ISO-2022-JP", "UTF8",
                                                     SCM_CHARCONV_OMIT);

  ret = scm_charconv_convert(conv_to_jis,
                             utf8_src, sizeof(utf8_src) - 1,
                             jis, sizeof(jis) - 1);

  cut_assert(ret >= 0);

  len_jis = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_jis,
                               jis + len_jis, sizeof(jis) - 1 - len_jis);

  cut_assert(ret >= 0);

  len_jis += (size_t)ret;
  jis[len_jis] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_jis));
  cut_assert_false(scm_charconv_is_ready(conv_to_jis));

  ret = scm_charconv_convert(conv_to_utf8,
                             jis, (size_t)len_jis,
                             utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += (size_t)ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_is_ready(conv_to_utf8));

  cut_assert_equal_string(utf8_src, utf8_dst);

  scm_charconv_end(conv_to_jis);
  scm_charconv_end(conv_to_utf8);
}

void
test_convert_utf8_to_sjis_and_sjis_to_utf8(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないことについては、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char sjis[256];
  ssize_t ret;
  size_t len_sjis, len_utf8;

  ScmCharConv *conv_to_sjis = scm_charconv_new("UTF8", "SHIFT-JIS",
                                                     SCM_CHARCONV_OMIT);
  ScmCharConv *conv_to_utf8 = scm_charconv_new("SHIFT-JIS", "UTF8",
                                                     SCM_CHARCONV_OMIT);

  ret = scm_charconv_convert(conv_to_sjis,
                             utf8_src, sizeof(utf8_src) - 1,
                             sjis, sizeof(sjis) - 1);

  cut_assert(ret >= 0);

  len_sjis = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_sjis,
                               sjis + len_sjis, sizeof(sjis) - 1 - len_sjis);

  cut_assert(ret >= 0);

  len_sjis += (size_t)ret;
  sjis[len_sjis] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_sjis));
  cut_assert_false(scm_charconv_is_ready(conv_to_sjis));

  ret = scm_charconv_convert(conv_to_utf8,
                             sjis, len_sjis, utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += (size_t)ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_is_ready(conv_to_utf8));

  cut_assert_equal_string(utf8_src, utf8_dst);

  scm_charconv_end(conv_to_sjis);
  scm_charconv_end(conv_to_utf8);
}

void
test_convert_utf8_to_eucjp_and_eucjp_to_utf8(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないことについては、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char eucjp[256];
  ssize_t ret;
  size_t len_eucjp, len_utf8;

  ScmCharConv *conv_to_eucjp = scm_charconv_new("UTF8", "EUC-JP",
                                                      SCM_CHARCONV_OMIT);
  ScmCharConv *conv_to_utf8 = scm_charconv_new("EUC-JP", "UTF8",
                                                     SCM_CHARCONV_OMIT);

  ret = scm_charconv_convert(conv_to_eucjp,
                             utf8_src, sizeof(utf8_src) - 1,
                             eucjp, sizeof(eucjp) - 1);

  cut_assert(ret >= 0);

  len_eucjp = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_eucjp,
                               eucjp + len_eucjp, sizeof(eucjp) - 1 - len_eucjp);

  cut_assert(ret >= 0);

  len_eucjp += (size_t)ret;
  eucjp[len_eucjp] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_eucjp));
  cut_assert_false(scm_charconv_is_ready(conv_to_eucjp));

  ret = scm_charconv_convert(conv_to_utf8,
                             eucjp, len_eucjp, utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += (size_t)ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_is_ready(conv_to_utf8));

  cut_assert_equal_string(utf8_src, utf8_dst);

  scm_charconv_end(conv_to_eucjp);
  scm_charconv_end(conv_to_utf8);
}

void
test_convert_invalid_sequence_omit(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないこと\xe0\x80については、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char eucjp[256];
  ssize_t ret;
  size_t len_eucjp, len_utf8;

  ScmCharConv *conv_to_eucjp = scm_charconv_new("UTF8", "EUC-JP",
                                                      SCM_CHARCONV_OMIT);
  ScmCharConv *conv_to_utf8 = scm_charconv_new("EUC-JP", "UTF8",
                                                     SCM_CHARCONV_OMIT);

  ret = scm_charconv_convert(conv_to_eucjp,
                             utf8_src, sizeof(utf8_src) - 1,
                             eucjp, sizeof(eucjp) - 1);

  cut_assert(ret >= 0);

  len_eucjp = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_eucjp,
                               eucjp + len_eucjp, sizeof(eucjp) - 1 - len_eucjp);

  cut_assert(ret >= 0);

  len_eucjp += (size_t)ret;
  eucjp[len_eucjp] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_eucjp));
  cut_assert_false(scm_charconv_is_ready(conv_to_eucjp));
  
  ret = scm_charconv_convert(conv_to_utf8,
                             eucjp, len_eucjp, utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += (size_t)ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_is_ready(conv_to_utf8));

  cut_assert_equal_string("およそ語られうることは明晰に語られうる。そして、論じえないことについては、人は沈黙せねばならない", utf8_dst);

  scm_charconv_end(conv_to_eucjp);
  scm_charconv_end(conv_to_utf8);
}


void
test_convert_invalid_sequence_error(void)
{
  char utf8_src[] = "およそ語られうることは明晰に語られうる。そして、論じえないこと\xe0\x80については、人は沈黙せねばならない";
  char utf8_dst[sizeof(utf8_src)];
  char eucjp[256];
  ssize_t ret;
  size_t len_eucjp, len_utf8;

  ScmCharConv *conv_to_eucjp = scm_charconv_new("UTF8", "EUC-JP",
                                                      SCM_CHARCONV_ERROR);
  ScmCharConv *conv_to_utf8 = scm_charconv_new("EUC-JP", "UTF8",
                                                     SCM_CHARCONV_ERROR);

  ret = scm_charconv_convert(conv_to_eucjp,
                             utf8_src, sizeof(utf8_src) - 1,
                             eucjp, sizeof(eucjp) - 1);

  cut_assert(ret > 0);
  cut_assert_true(scm_charconv_has_error(conv_to_eucjp));
  cut_assert_equal_int(EILSEQ, scm_charconv_errorno(conv_to_eucjp));

  len_eucjp = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_eucjp,
                               eucjp + len_eucjp, sizeof(eucjp) - 1 - len_eucjp);

  cut_assert(ret >= 0);

  len_eucjp += (size_t)ret;
  eucjp[len_eucjp] = '\0';

  ret = scm_charconv_convert(conv_to_utf8,
                             eucjp, len_eucjp, utf8_dst, sizeof(utf8_dst) - 1);

  cut_assert(ret >= 0);

  len_utf8 = (size_t)ret;
  ret = scm_charconv_terminate(conv_to_utf8,
                               utf8_dst + len_utf8,
                               sizeof(utf8_dst) - 1 - len_utf8);

  cut_assert(ret >= 0);

  len_utf8 += (size_t)ret;
  utf8_dst[len_utf8] = '\0';

  cut_assert_false(scm_charconv_has_error(conv_to_utf8));
  cut_assert_false(scm_charconv_is_ready(conv_to_utf8));

  cut_assert_equal_string("およそ語られうることは明晰に語られうる。そして、論じえないこと", utf8_dst);

  scm_charconv_end(conv_to_eucjp);
  scm_charconv_end(conv_to_utf8);
}
