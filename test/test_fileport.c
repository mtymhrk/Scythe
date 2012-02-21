#include <cutter.h>

#include <stdio.h>
#include <stdbool.h>

#include "vm.h"
#include "port.h"

#define TEST_TEXT_FILE "test_fileport_input_tmp_text_file"
#define TEST_TEXT_FILE_CONTENTS "hello, world\nhello, world!"
#define TEST_BIG_FILE "test_fileport_input_tmp_big_file"
#define TEST_BIG_FILE_SIZE 1048576 /* 1 MByte */ 
#define TEST_OUTPUT_FILE "test_fileport_output_tmp_file"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  FILE *fp;
  size_t n;

  fp = fopen(TEST_TEXT_FILE, "w");
  fputs(TEST_TEXT_FILE_CONTENTS, fp);
  fclose(fp);

  fp = fopen(TEST_BIG_FILE, "w");
  for (uint64_t i = 0; i < (TEST_BIG_FILE_SIZE / sizeof(i)); i++)
    n = fwrite(&i, sizeof(i), 1, fp);
  fclose(fp);

  vm = scm_vm_new();
}

void
cut_shutdown(void)
{
  remove(TEST_TEXT_FILE);
  remove(TEST_BIG_FILE);

  scm_vm_end(vm);
}

void
cut_teardown(void)
{
  remove(TEST_OUTPUT_FILE);
}

void
xxx_test_scm_port_new_input_file_port(ScmObj port)
{
  cut_assert_true(scm_obj_not_null_p(port));
  cut_assert_true(scm_port_readable_p(port));
  cut_assert_false(scm_port_writable_p(port));
  cut_assert_true(scm_port_file_port_p(port));
  cut_assert_false(scm_port_string_port_p(port));
  cut_assert_false(scm_port_closed_p(port));
}

void
test_scm_port_new_input_file_port_ful_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);

  xxx_test_scm_port_new_input_file_port(port);
}

void
test_scm_port_new_input_file_port_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;


  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);

  xxx_test_scm_port_new_input_file_port(port);
}

void
test_scm_port_new_input_file_port_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_new_input_file_port(port);
}

void
test_scm_port_new_input_file_port_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);

  xxx_test_scm_port_new_input_file_port(port);
}

void
xxx_test_scm_port_read_per_byte(ScmObj port)
{
  char expected_chars[] = TEST_TEXT_FILE_CONTENTS;
  char byte;
  ssize_t ret;

  for (size_t i = 0; i < sizeof(expected_chars) - 1; i++) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
  }

  ret = scm_port_read(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
}

void
test_scm_port_read_per_byte_full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);
  xxx_test_scm_port_read_per_byte(port);
}

void
test_scm_port_read_per_byte_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);
  xxx_test_scm_port_read_per_byte(port);
}

void
test_scm_port_read_per_byte_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);
  xxx_test_scm_port_read_per_byte(port);
}

void
test_scm_port_read_per_byte_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);
  xxx_test_scm_port_read_per_byte(port);
}

void
xxx_test_scm_port_interleave_read_and_seek(ScmObj port)
{
  char expected_chars[] = TEST_TEXT_FILE_CONTENTS;
  char byte;
  ssize_t ret;
  int i;

  i = 0;
  while (true) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 0, SEEK_SET);
  cut_assert_equal_int(0, ret);

  i = 0;
  while (true) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, -6, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i -= 6;
  while (true) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 13, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i += 13;

  ret = scm_port_read(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);

  ret = scm_port_seek(port, -13, SEEK_END);
  cut_assert_equal_int(0, ret);

  i -= 13;
  while (true) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
    i++;

    if (i >= 26) break;
  }

  ret = scm_port_read(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
}

void
test_scm_port_interleave_read_and_seek_full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
test_scm_port_interleave_read_and_seek_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
test_scm_port_interleave_read_and_seek_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
test_scm_port_interleave_read_and_seek_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);
  xxx_test_scm_port_interleave_read_and_seek(port);
}

void
xxx_test_scm_port_read_big_file(ScmObj port)
{
  ssize_t ret;
  uint64_t data;

  for (uint64_t i = 0; i < (TEST_BIG_FILE_SIZE / sizeof(i)); i++) {
    ret = scm_port_read(port, &data, sizeof(data));
    cut_assert_equal_int(sizeof(data), ret);
    cut_assert_equal_uint_fast64(i, data);
  }

  ret = scm_port_read(port, &data, sizeof(data));
  cut_assert_equal_int(0, ret);
}

void
test_scm_port_read_big_file_full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_BIG_FILE,
                                          SCM_PORT_BUF_FULL);
  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_file_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_BIG_FILE,
                                          SCM_PORT_BUF_LINE);
  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_file_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_BIG_FILE,
                                          SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_file_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_BIG_FILE,
                                          SCM_PORT_BUF_NONE);

  xxx_test_scm_port_read_big_file(port);
}

void
test_scm_port_read_big_data(void)
{
  uint64_t data[TEST_BIG_FILE_SIZE / sizeof(uint64_t)];
  ssize_t ret;
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_BIG_FILE,
                                          SCM_PORT_BUF_FULL);

  ret = scm_port_read(port, data, TEST_BIG_FILE_SIZE);
  cut_assert_equal_int(TEST_BIG_FILE_SIZE, ret);

  for (uint64_t i = 0; i < TEST_BIG_FILE_SIZE / sizeof(i); i++)
    cut_assert_equal_uint_fast64(i, data[i]);
}

void
test_scm_port_close_input_port(void)
{
  int ret, data;
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_DEFAULT);

  cut_assert_false(scm_port_closed_p(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_closed_p(port));

  cut_assert_equal_int(-1, scm_port_read(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}

void
xxx_test_scm_port_new_output_file_port(ScmObj port)
{
  cut_assert_true(scm_obj_not_null_p(port));
  cut_assert_false(scm_port_readable_p(port));
  cut_assert_true(scm_port_writable_p(port));
  cut_assert_true(scm_port_file_port_p(port));
  cut_assert_false(scm_port_string_port_p(port));
  cut_assert_false(scm_port_closed_p(port));
}

void
test_scm_port_new_output_file_port_ful_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_FULL);

  xxx_test_scm_port_new_output_file_port(port);
}

void
test_scm_port_new_output_file_port_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_LINE);

  xxx_test_scm_port_new_output_file_port(port);
}

void
test_scm_port_new_output_file_port_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_new_output_file_port(port);
}

void
test_scm_port_new_output_file_port_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_NONE);

  xxx_test_scm_port_new_output_file_port(port);
}

bool
is_file_contents_same(const char *file, const void *contents, size_t size)
{
  FILE *fp;
  char data[size];
  size_t n;

  memset(data, 0, size);

  fp = fopen(file, "rb");
  n = fread(data, size, 1, fp);
  fclose(fp);

  cut_assert_equal_int(1, (int)n);

  cut_assert_true(memcmp(contents, data, size) == 0);

  return true;
}

bool
is_file_contents_same2(const char *file1, const char *file2, size_t size)
{
  FILE *fp1, *fp2;
  char data1[size], data2[size];
  size_t n1, n2;


  fp1 = fopen(file1, "rb");
  fp2 = fopen(file2, "rb");
  n1 = fread(data1, size, 1, fp1);
  n2 = fread(data2, size, 1, fp2);
  fclose(fp1);
  fclose(fp2);

  cut_assert_equal_int(1, (int)n1);
  cut_assert_equal_int(1, (int)n2);

  cut_assert_true(memcmp(data1, data2, size) == 0);

  return true;
}

void
xxx_test_scm_port_write_per_byte(ScmObj port)
{
  char data[] = TEST_TEXT_FILE_CONTENTS;
  ssize_t ret;

  for (size_t i = 0; i < sizeof(data); i++) {
    ret = scm_port_write(port, data + i, sizeof(char));
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
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_FULL);

  xxx_test_scm_port_write_per_byte(port);
}

void
test_scm_port_write_per_byte_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_LINE);

  xxx_test_scm_port_write_per_byte(port);
}

void
test_scm_port_write_per_byte_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_write_per_byte(port);
}

void
test_scm_port_write_per_byte_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                          SCM_PORT_BUF_NONE);

  xxx_test_scm_port_write_per_byte(port);
}

void
xxx_test_scm_port_interleave_write_and_seek(ScmObj port)
{
  char contents[] = TEST_TEXT_FILE_CONTENTS;
  ssize_t ret;
  int i;

  i = 0;
  while (true) {
    ret = scm_port_write(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 0, SEEK_SET);
  cut_assert_equal_int(0, ret);

  i = 0;
  while (true) {
    ret = scm_port_write(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, -6, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i -= 6;
  while (true) {
    ret = scm_port_write(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 13) break;
  }

  ret = scm_port_seek(port, 10, SEEK_CUR);
  cut_assert_equal_int(0, ret);

  i += 10;
  while (true) {
    ret = scm_port_write(port, contents + i, sizeof(contents[i]));
    cut_assert_equal_int(sizeof(contents[i]), ret);
    i++;

    if (i >= 26) break;
  }

  ret = scm_port_seek(port, -13, SEEK_END);
  cut_assert_equal_int(0, ret);

  i -= 13;
  while (true) {
    ret = scm_port_write(port, contents + i, sizeof(contents[i]));
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
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_FULL);

  xxx_test_scm_port_interleave_write_and_seek(port);
}

void
test_scm_port_interleave_write_and_seek_line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_LINE);

  xxx_test_scm_port_interleave_write_and_seek(port);
}

void
test_scm_port_interleave_write_and_seek_modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_interleave_write_and_seek(port);
}

void
test_scm_port_interleave_write_and_seek_none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_LINE);

  xxx_test_scm_port_interleave_write_and_seek(port);
}

void
test_scm_port_write_big_data(void)
{
  ScmObj port = SCM_OBJ_INIT;


  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_FULL);


  for (uint64_t i = 0; i < (TEST_BIG_FILE_SIZE / sizeof(i)); i++)
    cut_assert_equal_int(sizeof(i),
                         scm_port_write(port, &i, sizeof(i)));

  cut_assert_equal_int(0, scm_port_close(port));

  cut_assert_true(is_file_contents_same2(TEST_BIG_FILE,
                                         TEST_OUTPUT_FILE,
                                         TEST_BIG_FILE_SIZE));
}

void
test_scm_port_close_output_port(void)
{
  int ret, data;
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_DEFAULT);

  cut_assert_false(scm_port_closed_p(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_closed_p(port));

  cut_assert_equal_int(-1, scm_port_write(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}

void
xxx_test_scm_port_read_line__read_up_to_lf(ScmObj port)
{
  const char *expected1 = "hello, world\n";
  const char *expected2 = "hello, world!";
  char actual1[256];
  ssize_t actual1_len;
  char actual2[256];
  ssize_t actual2_len;
  char actual3[256];
  ssize_t actual3_len;


  /* 改行まで読み込み */
  actual1_len = scm_port_read_line(port, actual1, sizeof(actual1));

  cut_assert_equal_int((int)strlen(expected1), (int)actual1_len);
  actual1[actual1_len] = '\0';
  cut_assert_equal_string(expected1, actual1);

  /* 改行以降を読み込み */
  actual2_len = scm_port_read_line(port, actual2, sizeof(actual2));

  cut_assert_equal_int((int)strlen(expected2), (int)actual2_len);
  actual2[actual2_len] = '\0';
  cut_assert_equal_string(expected2, actual2);


  /* EOF に到達 */
  actual3_len = scm_port_read_line(port, actual3, sizeof(actual3));

  cut_assert_equal_int(0, (int)actual3_len);
}

void
test_scm_port_read_line__read_up_to_lf__full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);
  xxx_test_scm_port_read_line__read_up_to_lf(port);
}

void
test_scm_port_read_line__read_up_to_lf__line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);
  xxx_test_scm_port_read_line__read_up_to_lf(port);
}

void
test_scm_port_read_line__read_up_to_lf__modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);
  xxx_test_scm_port_read_line__read_up_to_lf(port);
}

void
test_scm_port_read_line__read_up_to_lf__none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);
  xxx_test_scm_port_read_line__read_up_to_lf(port);
}

void
xxx_test_scm_port_read_line__read_up_to_buf_filled(ScmObj port)
{
  const char expected1[] = "hello";
  const char expected2[] = ", world\n";
  char actual1[sizeof(expected1)];
  ssize_t actual1_len;
  char actual2[256];
  ssize_t actual2_len;

  /* actual1 が一杯になるまで読み込み */
  actual1_len = scm_port_read_line(port, actual1, sizeof(actual1) - 1);

  cut_assert_equal_int((int)strlen(expected1), (int)actual1_len);
  actual1[actual1_len] = '\0';
  cut_assert_equal_string(expected1, actual1);

  /* 改行までを読み込み */
  actual2_len = scm_port_read_line(port, actual2, sizeof(actual2));

  cut_assert_equal_int((int)strlen(expected2), (int)actual2_len);
  actual2[actual2_len] = '\0';
  cut_assert_equal_string(expected2, actual2);
}

void
test_scm_port_read_line__read_up_to_buf_filled__full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);
  xxx_test_scm_port_read_line__read_up_to_buf_filled(port);
}

void
test_scm_port_read_line__read_up_to_buf_filled__line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);
  xxx_test_scm_port_read_line__read_up_to_buf_filled(port);
}

void
test_scm_port_read_line__read_up_to_buf_filled__modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);
  xxx_test_scm_port_read_line__read_up_to_buf_filled(port);
}

void
test_scm_port_read_line__read_up_to_buf_filled__none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);
  xxx_test_scm_port_read_line__read_up_to_buf_filled(port);
}

void
test_scm_port_write__line_buffer_should_flushed_up_to_lf(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_file(TEST_OUTPUT_FILE,
                                           SCM_PORT_BUF_LINE);

  scm_port_write(port,
                 TEST_TEXT_FILE_CONTENTS, sizeof(TEST_TEXT_FILE_CONTENTS) - 1);

  cut_assert_true(is_file_contents_same(TEST_OUTPUT_FILE,
                                        "hello, world\n",
                                        sizeof("hello, world\n") - 1));

  scm_port_close(port);

  cut_assert_true(is_file_contents_same(TEST_OUTPUT_FILE,
                                        TEST_TEXT_FILE_CONTENTS,
                                        sizeof(TEST_TEXT_FILE_CONTENTS) - 1));
}

void
xxx_test_scm_port_pushback__pushback(ScmObj port)
{
  char expected1 = 'h';
  char actual1;
  char actual2[256];
  ssize_t ret;

  ret = scm_port_read(port, &actual1, sizeof(actual1));
  cut_assert_equal_int((int)sizeof(actual1), ret);
  cut_assert_equal_char(expected1, actual1);

  ret = scm_port_pushback(port, &actual1, sizeof(actual1));
  cut_assert_equal_int((int)sizeof(actual1), ret);

  ret = scm_port_read(port, actual2, sizeof(actual2));
  cut_assert_equal_int(strlen(TEST_TEXT_FILE_CONTENTS), ret);
  actual2[ret] = '\0';
  cut_assert_equal_string(TEST_TEXT_FILE_CONTENTS, actual2);
}

void
test_scm_port_pushback__pushback__full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                           SCM_PORT_BUF_FULL);

  xxx_test_scm_port_pushback__pushback(port);
}

void
test_scm_port_pushback__pushback__line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                           SCM_PORT_BUF_LINE);

  xxx_test_scm_port_pushback__pushback(port);
}

void
test_scm_port_pushback__pushback__modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                           SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_pushback__pushback(port);
}

void
test_scm_port_pushback__pushback__none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                           SCM_PORT_BUF_NONE);

  xxx_test_scm_port_pushback__pushback(port);
}

void
xxx_test_scm_port_pushback__pushback_and_read_line(ScmObj port)
{
  const char pushbacked[] = "a\nb";
  const char expected1[] = "a\n";
  const char expected2[] = "bhello, world\n";
  char actual1[256];
  char actual2[256];
  ssize_t ret;

  ret = scm_port_pushback(port, pushbacked, strlen(pushbacked));
  cut_assert_equal_int(strlen(pushbacked), ret);

  ret = scm_port_read_line(port, actual1, sizeof(actual1) - 1);
  cut_assert_equal_int(strlen(expected1), ret);
  actual1[ret] = '\0';
  cut_assert_equal_string(expected1, actual1);

  ret = scm_port_read_line(port, actual2, sizeof(actual2) - 1);
  cut_assert_equal_int(strlen(expected2), ret);
  actual2[ret] = '\0';
  cut_assert_equal_string(expected2, actual2);
}

void
test_scm_port_pushback__pushback_and_read_line__full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);

  xxx_test_scm_port_pushback__pushback_and_read_line(port);
}

void
test_scm_port_pushback__pushback_and_read_line__line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);

  xxx_test_scm_port_pushback__pushback_and_read_line(port);
}

void
test_scm_port_pushback__pushback_and_read_line__modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_pushback__pushback_and_read_line(port);
}

void
test_scm_port_pushback__pushback_and_read_line__none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);

  xxx_test_scm_port_pushback__pushback_and_read_line(port);
}

void
xxx_test_scm_port_peek(ScmObj port)
{
  char expected1[] = "h";
  char expected2[] = "he";
  char expected3[] = TEST_TEXT_FILE_CONTENTS;
  char actual1[256];
  char actual2[256];
  char actual3[256];
  ssize_t ret;

  ret = scm_port_peek(port, actual1, 1);
  cut_assert_equal_int(1, ret);
  actual1[1] = '\0';
  cut_assert_equal_string(expected1, actual1);

  ret = scm_port_peek(port, actual2, 2);
  cut_assert_equal_int(2, ret);
  actual2[2] = '\0';
  cut_assert_equal_string(expected2, actual2);

  ret = scm_port_read(port, actual3, sizeof(actual3));
  cut_assert_equal_int((int)strlen(expected3), ret);
  actual3[ret] = '\0';
  cut_assert_equal_string(expected3, actual3);
}

void
test_scm_port_peek__full_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_FULL);

  xxx_test_scm_port_peek(port);
}

void
test_scm_port_peek__line_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_LINE);

  xxx_test_scm_port_peek(port);
}

void
test_scm_port_peek__modest_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_MODEST);

  xxx_test_scm_port_peek(port);
}

void
test_scm_port_peek__none_buffer(void)
{
  ScmObj port = SCM_OBJ_INIT;;

  port = scm_port_open_input_file(TEST_TEXT_FILE,
                                          SCM_PORT_BUF_NONE);

  xxx_test_scm_port_peek(port);
}


