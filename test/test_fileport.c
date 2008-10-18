#include <cutter.h>

#include <stdio.h>
#include <stdbool.h>

#include "port.h"

#define TEST_TEXT_FILE "test_fileport_input_tmp_text_file"
#define TEST_BIG_FILE "test_fileport_input_tmp_big_file"

void
startup(void)
{
  FILE *fp;
  int i;

  fp = fopen(TEST_TEXT_FILE, "w");
  fputs("hello, world\nhello, world!", fp);
  fclose(fp);

  fp = fopen(TEST_BIG_FILE, "w");
  for (i = 0; i < (1048576 / sizeof(i)); i++)
    fwrite(&i, sizeof(i), 1, fp);
  fclose(fp);
}

void
shutdown(void)
{
  remove(TEST_TEXT_FILE);
  remove(TEST_BIG_FILE);
}

void
xxx_test_scm_port_construct_input_file_port(ScmPort *port)
{
  cut_assert_not_null(port);
  cut_assert_true(scm_port_is_readable(port));
  cut_assert_false(scm_port_is_writable(port));
  cut_assert_true(scm_port_is_file_port(port));
  cut_assert_false(scm_port_is_string_port(port));
  cut_assert_false(scm_port_is_closed(port));
}

void
test_scm_port_construct_input_file_port_ful_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_FULL);

  xxx_test_scm_port_construct_input_file_port(port);
}

void
test_scm_port_construct_input_file_port_line_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_LINE);

  xxx_test_scm_port_construct_input_file_port(port);
}

void
test_scm_port_construct_input_file_port_none_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_NONE);

  xxx_test_scm_port_construct_input_file_port(port);
}

void
xxx_test_scm_port_read_per_bye(ScmPort *port)
{
  char expected_chars[] = "hello, world\nhello, world!";
  char byte;
  ssize_t ret;
  int i;

  for (i = 0; i < sizeof(expected_chars) - 1; i++) {
    ret = scm_port_read_prim(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
  }

  ret = scm_port_read_prim(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_eof(port));
}

void
test_scm_port_read_per_bye_full_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_FULL);
  xxx_test_scm_port_read_per_bye(port);
}

void
test_scm_port_read_per_bye_line_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_LINE);
  xxx_test_scm_port_read_per_bye(port);
}

void
test_scm_port_read_per_bye_none_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_NONE);
  xxx_test_scm_port_read_per_bye(port);
}

void
xxx_test_scm_port_interleave_read_and_seek(ScmPort *port)
{
  char expected_chars[] = "hello, world\nhello, world!";
  char byte;
  ssize_t ret;
  int i;

  i = 0;
  while (true) {
    ret = scm_port_read_prim(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }
  
  ret = scm_port_seek(port, 0, SEEK_SET);
  cut_assert_equal_int(0, ret);

  i = 0;
  while (true) {
    ret = scm_port_read_prim(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, -6, SEEK_CUR);
  cut_assert_equal_int(0, ret);
  cut_assert_false(scm_port_is_eof(port));

  i -= 6;
  while (true) {
    ret = scm_port_read_prim(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 13, SEEK_CUR);
  cut_assert_equal_int(0, ret);
  cut_assert_false(scm_port_is_eof(port));

  i += 13;

  ret = scm_port_read_prim(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_eof(port));

  ret = scm_port_seek(port, -13, SEEK_END);
  cut_assert_equal_int(0, ret);
  cut_assert_false(scm_port_is_eof(port));

  i -= 13;
  while (true) {
    ret = scm_port_read_prim(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 26) break;
  }

  ret = scm_port_read_prim(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_eof(port));
}

void
test_scm_port_interleave_read_and_seek_full_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_FULL);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
test_scm_port_interleave_read_and_seek_line_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_LINE);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
test_scm_port_interleave_read_and_seek_none_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_NONE);
  xxx_test_scm_port_interleave_read_and_seek(port);
}
