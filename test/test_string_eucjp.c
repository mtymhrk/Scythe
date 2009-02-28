#include <cutter.h>

#include "string.h"

void
test_scm_string_eucjp(void)
{
  char expected[] = "�ƥ���ʸ����";
  char actual[256];
  size_t len;

  ScmString *str = scm_string_construct(expected, sizeof(expected) - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(str);
  cut_assert_equal_int(SCM_ENCODING_EUCJP, scm_string_encoding(str));
  cut_assert_equal_int(6, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_copy_eucjp(void)
{
  char expected[] = "����ʸ����ϸ��Ǥ���";
  
  ScmString *str = scm_string_construct(expected,
                                        sizeof(expected) - 1,
                                        SCM_ENCODING_EUCJP);
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
test_string_dup_eucjp(void)
{
  char expected[] = "����ʸ����ϸ��Ǥ���";
  
  ScmString *str = scm_string_construct(expected, sizeof(expected) - 1,
                                        SCM_ENCODING_EUCJP);
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
test_scm_string_is_equal_compare_with_same_string_eucjp(void)
{
  ScmString *str1 = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                         sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                         SCM_ENCODING_EUCJP);
  ScmString *str2 = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                         sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                         SCM_ENCODING_EUCJP);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_different_string_eucjp(void)
{
  ScmString *str1 = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                         sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                         SCM_ENCODING_EUCJP);
  ScmString *str2 = scm_string_construct("����ʸ����ϸ��Ǥʤ�",
                                         sizeof("����ʸ����ϸ��Ǥʤ�") - 1,
                                         SCM_ENCODING_EUCJP);

  cut_assert_false(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_copy_string_eucjp(void)
{
  ScmString *str1 = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                         sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                         SCM_ENCODING_EUCJP);
  ScmString *str2 = scm_string_copy(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_is_equal_compare_with_duplicate_string_eucjp(void)
{
  ScmString *str1 = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                         sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                         SCM_ENCODING_EUCJP);
  ScmString *str2 = scm_string_dup(str1);

  cut_assert_true(scm_string_is_equal(str1, str2));

  scm_string_destruct(str1);
  scm_string_destruct(str2);
}

void
test_scm_string_substr_eucjp(void)
{
  char expected[] = "���Ǥ���";
  char actual[256];
  int len;

  ScmString *str = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                        sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                        SCM_ENCODING_EUCJP);
  ScmString *sub = scm_string_substr(str, 6, 5);

  cut_assert_not_null(sub);
  cut_assert_equal_int(5, scm_string_length(sub));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(sub));

  len = scm_string_dump(sub, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(sub);
}

void
test_scm_string_push_eucjp(void)
{
  char expected[] = "����ʸ����ϸ��Ǥ��롣";
  scm_char_t pushed;
  char actual[256];
  int len;

  memset(&pushed, 0, sizeof(pushed));
  memcpy(&pushed, "��", 3);

  ScmString *str = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                        sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_push(str, pushed));

  cut_assert_equal_int(12, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_append_eucjp(void)
{
  char expected[] = "����ʸ��������������ʸ�ϸ��Ǥ��롣";
  char actual[256];
  int len;

  ScmString *str = scm_string_construct("����ʸ����������",
                                        sizeof("����ʸ����������") - 1,
                                        SCM_ENCODING_EUCJP);
  ScmString *apnd = scm_string_construct("����ʸ�ϸ��Ǥ��롣",
                                         sizeof("����ʸ�ϸ��Ǥ��롣") - 1,
                                         SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_append(str, apnd));

  cut_assert_equal_int(18, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
  scm_string_destruct(apnd);
}

void
test_scm_string_ref_eucjp(void)
{
  int i;
  scm_char_t actual;
  char *tmp[] = { "��" , "��", "ʸ", "��", "��", "��", "��", "��", "��","��",
                  "��", "" };
  scm_char_t expected[sizeof(tmp)/sizeof(tmp[1])];

  memset(expected, 0, sizeof(expected));
  for (i = 0; i < sizeof(expected)/sizeof(expected[1]); i++)
    memcpy(expected + i, tmp[i], strlen(tmp[i]));
  expected[11] = SCM_CHR_ZERO;

  ScmString *str = scm_string_construct("����ʸ����ϸ��Ǥ���",
                                        sizeof("����ʸ����ϸ��Ǥ���") - 1,
                                        SCM_ENCODING_EUCJP);

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
test_scm_string_set_less_width_eucjp(void)
{
  char expected[] = "��a��";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct("�ƥ���", sizeof("�ƥ���") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_set(str, 1, c));

  cut_assert_equal_int(3, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_set_same_width_eucjp(void)
{
  char expected[] = "�ƥ��";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "��", 3);

  ScmString *str = scm_string_construct("�ƥ���",
                                        sizeof("�ƥ���") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_set(str, 1, c));

  cut_assert_equal_int(3, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_set_greater_width_eucjp(void)
{
  char expected[] = "a��c";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "��", 3);

  ScmString *str = scm_string_construct("abc", sizeof("abc") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_set(str, 1, c));

  cut_assert_equal_int(3, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_fill_eucjp(void)
{
  char expected[] = "����ʸ���������aaa��ʸ�ϸ��Ǥ��롣";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_fill(str, 8, 3, c));

  cut_assert_equal_int(20, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_fill_append_eucjp(void)
{
  char expected[] = "����ʸ�����������������ʸ�ϸ��Ǥ�aaaaa";
  char actual[256];
  scm_char_t c;
  int len;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_not_null(scm_string_fill(str, 18, 5, c));

  cut_assert_equal_int(23, scm_string_length(str));
  cut_assert_equal_int(sizeof(expected) - 1, scm_string_bytesize(str));

  len = scm_string_dump(str, actual, sizeof(actual));
  cut_assert_equal_int(sizeof(expected) - 1, len);
  cut_assert_equal_int(0, memcmp(expected, actual, len));

  scm_string_destruct(str);
}

void
test_scm_string_find_chr_found_eucjp(void)
{
  scm_char_t c;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "��", 3);

  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_equal_int(5, scm_string_find_chr(str, c));

  scm_string_destruct(str);
}

void
test_scm_string_find_chr_not_found_eucjp(void)
{
  scm_char_t c;

  memset(&c, 0, sizeof(c));
  memcpy(&c, "a", 1);

  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_equal_int(-1, scm_string_find_chr(str, c));

  scm_string_destruct(str);
}

void
test_scm_string_match_matched_eucjp(void)
{
  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);
  ScmString *pat = scm_string_construct("����������ʸ",
                                        sizeof("����������ʸ") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_equal_int(7, scm_string_match(str, pat));

  scm_string_destruct(str);
  scm_string_destruct(pat);
}

void
test_scm_string_match_unmatched_eucjp(void)
{
  ScmString *str = scm_string_construct("����ʸ�����������������ʸ�ϸ��Ǥ��롣",
                                        sizeof("����ʸ�����������������ʸ�ϸ��Ǥ��롣") - 1,
                                        SCM_ENCODING_EUCJP);
  ScmString *pat = scm_string_construct("����������ʸ",
                                        sizeof("����������ʸ") - 1,
                                        SCM_ENCODING_EUCJP);

  cut_assert_equal_int(-1, scm_string_match(str, pat));

  scm_string_destruct(str);
  scm_string_destruct(pat);
}
