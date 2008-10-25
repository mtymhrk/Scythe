#include <cutter.h>

#include <stdio.h>
#include <stdbool.h>

#include "port.h"

#define TEST_TEXT_FILE "test_fileport_input_tmp_text_file"
#define TEST_TEXT_FILE_CONTENTS "hello, world\nhello, world!"
#define TEST_BIG_FILE "test_fileport_input_tmp_big_file"
#define TEST_BIG_FILE_SIZE 1048576 /* 1 MByte */ 
#define TEST_OUTPUT_FILE "test_fileport_output_tmp_file"

void
startup(void)
{
  FILE *fp;
  int i;

  fp = fopen(TEST_TEXT_FILE, "w");
  fputs(TEST_TEXT_FILE_CONTENTS, fp);
  fclose(fp);

  fp = fopen(TEST_BIG_FILE, "w");
  for (i = 0; i < (TEST_BIG_FILE_SIZE / sizeof(i)); i++)
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
teardown(void)
{
  remove(TEST_OUTPUT_FILE);
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
  char expected_chars[] = TEST_TEXT_FILE_CONTENTS;
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
  char expected_chars[] = TEST_TEXT_FILE_CONTENTS;
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

void
xxx_test_scm_port_read_big_file(ScmPort *port)
{
  int i, data, ret;

  for (i = 0; i < (TEST_BIG_FILE_SIZE / sizeof(i)); i++) {
    ret = scm_port_read_prim(port, &data, sizeof(data));
    cut_assert_equal_int(sizeof(data), ret);
    cut_assert_equal_int(i, data);
  }

  ret = scm_port_read_prim(port, &data, sizeof(data));
  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_eof(port));
}

void
test_scm_port_read_big_file_full_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_BIG_FILE,
                                                SCM_PORT_BUF_FULL);

  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_file_line_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_BIG_FILE,
                                                SCM_PORT_BUF_LINE);

  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_file_none_buffer(void)
{
  ScmPort *port = scm_port_construct_input_port(TEST_BIG_FILE,
                                                SCM_PORT_BUF_NONE);

  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_data(void)
{
  int data[TEST_BIG_FILE_SIZE / sizeof(int)];
  int i, ret;
  ScmPort *port = scm_port_construct_input_port(TEST_BIG_FILE,
                                                SCM_PORT_BUF_FULL);

  ret = scm_port_read_prim(port, data, TEST_BIG_FILE_SIZE);
  cut_assert_equal_int(TEST_BIG_FILE_SIZE, ret);

  for (i = 0; i < TEST_BIG_FILE_SIZE / sizeof(int); i++)
    cut_assert_equal_int(i, data[i]);
}

void
test_scm_port_close_input_port(void)
{
  int ret;
  ScmPort *port = scm_port_construct_input_port(TEST_TEXT_FILE,
                                                SCM_PORT_BUF_DEFAULT);

  cut_assert_false(scm_port_is_closed(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_closed(port));
}

bool
is_file_contents_same(const char *file, const void *contents, size_t size)
{
  FILE *fp;
  char data[size];
  int i;
  size_t n;

  memset(data, 0, size);

  fp = fopen(file, "rb");
  n = fread(data, size, 1, fp);
  fclose(fp);

  cut_assert_equal_int(1, n);

  for(i = 0; i < size; i++)
    cut_assert_equal_int(((char *)contents)[i], data[i]);

  return true;
}

void
xxx_test_scm_port_write_per_byte(ScmPort *port)
{
  char data[] = TEST_TEXT_FILE_CONTENTS;
  int i, ret;

  for (i = 0; i < sizeof(data); i++) {
    ret = scm_port_write_prim(port, data + i, sizeof(char));
    cut_assert_equal_int(sizeof(char), ret);
  }
  scm_port_flush(port);

  cut_assert_true(is_file_contents_same(TEST_OUTPUT_FILE,
                                        TEST_TEXT_FILE_CONTENTS,
                                        sizeof(TEST_TEXT_FILE_CONTENTS) - 1));
}

void
test_scm_port_write_per_byte_full_buffer(void)
{
  ScmPort *port = scm_port_construct_output_port(TEST_OUTPUT_FILE,
                                                 SCM_PORT_BUF_FULL);

  xxx_test_scm_port_write_per_byte(port);
}

void
test_scm_port_write_per_byte_line_buffer(void)
{
  ScmPort *port = scm_port_construct_output_port(TEST_OUTPUT_FILE,
                                                 SCM_PORT_BUF_LINE);

  xxx_test_scm_port_write_per_byte(port);
}

void
test_scm_port_write_per_byte_none_buffer(void)
{
  ScmPort *port = scm_port_construct_output_port(TEST_OUTPUT_FILE,
                                                 SCM_PORT_BUF_NONE);

  xxx_test_scm_port_write_per_byte(port);
}

void
xxx_test_scm_port_interleave_write_and_seek(ScmPort *port)
{
  char contents[] = TEST_TEXT_FILE_CONTENTS;
  ssize_t ret;
  int i;

  i = 0;
  while (true) {
    ret = scm_port_write_prim(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }
  
  ret = scm_port_seek(port, 0, SEEK_SET);
  cut_assert_equal_int(0, ret);

  i = 0;
  while (true) {
    ret = scm_port_write_prim(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, -6, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i -= 6;
  while (true) {
    ret = scm_port_write_prim(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 10, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i += 10;
  while (true) {
    ret = scm_port_write_prim(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 26) break;
  }

  ret = scm_port_seek(port, -13, SEEK_END);
  cut_assert_equal_int(0, ret);

  i -= 13;
  while (true) {
    ret = scm_port_write_prim(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 26) break;
  }

  scm_port_flush(port);

  cut_assert_true(is_file_contents_same(TEST_OUTPUT_FILE,
                                        TEST_TEXT_FILE_CONTENTS,
                                        sizeof(TEST_TEXT_FILE_CONTENTS) - 1));
}

void
test_scm_port_interleave_write_and_seek_full_buffer(void)
{
  ScmPort *port = scm_port_construct_output_port(TEST_OUTPUT_FILE,
                                                 SCM_PORT_BUF_FULL);

  xxx_test_scm_port_interleave_write_and_seek(port);
}
