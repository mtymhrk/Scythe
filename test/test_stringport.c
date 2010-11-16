#include <cutter.h>

#include <stdio.h>
#include <stdbool.h>

#include "port.h"

#define TEST_TEXT_CONTENTS "hello, world\nhello, world!"

void
test_scm_string_port_new_input_port(void)
{
  ScmPort *port =
    scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                         sizeof(TEST_TEXT_CONTENTS) - 1);

  cut_assert_not_null(port);
  cut_assert_true(scm_port_is_readable(port));
  cut_assert_false(scm_port_is_writable(port));
  cut_assert_false(scm_port_is_file_port(port));
  cut_assert_true(scm_port_is_string_port(port));
  cut_assert_false(scm_port_is_closed(port));
}

void
test_scm_string_port_read_per_bye(void)
{
  char expected_chars[] = TEST_TEXT_CONTENTS;
  char byte;
  ssize_t ret;
  int i;

  ScmPort *port =
    scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                         sizeof(TEST_TEXT_CONTENTS) - 1);

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
test_scm_string_port_interleave_read_and_seek(void)
{
  char expected_chars[] = TEST_TEXT_CONTENTS;
  char byte;
  ssize_t ret;
  int i;

  ScmPort *port =
    scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                         sizeof(TEST_TEXT_CONTENTS) - 1);


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
  cut_assert_true(scm_port_is_eof(port));

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
test_scm_string_port_close_input_port(void)
{
  int ret, data;
  ScmPort *port =
    scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                         sizeof(TEST_TEXT_CONTENTS) - 1);

  cut_assert_false(scm_port_is_closed(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_closed(port));

  cut_assert_equal_int(-1, scm_port_read_prim(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}

void
test_scm_string_port_new_output_port(void)
{
  ScmPort *port = scm_port_open_output_string();

  cut_assert_not_null(port);
  cut_assert_false(scm_port_is_readable(port));
  cut_assert_true(scm_port_is_writable(port));
  cut_assert_false(scm_port_is_file_port(port));
  cut_assert_true(scm_port_is_string_port(port));
  cut_assert_false(scm_port_is_closed(port));
}

void
test_scm_string_port_write_per_byte(void)
{
  char data[] = TEST_TEXT_CONTENTS;
  int i, ret;

  ScmPort *port = scm_port_open_output_string();

  for (i = 0; i < sizeof(data) - 1; i++) {
    ret = scm_port_write_prim(port, data + i, sizeof(char));
    cut_assert_equal_int(sizeof(char), ret);
  }
  scm_port_flush(port);

  cut_assert_equal_int(sizeof(data) - 1, scm_port_string_buffer_length(port));

  cut_assert_equal_int(0, memcmp(data,
                                 scm_port_string_buffer(port),
                                 sizeof(data) - 1));
}

void
test_scm_string_port_interleave_write_and_seek(void)
{
  char contents[] = TEST_TEXT_CONTENTS;
  ssize_t ret;
  int i;

  ScmPort *port = scm_port_open_output_string();

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

  cut_assert_equal_int(sizeof(contents) - 1,
                       scm_port_string_buffer_length(port));

  cut_assert_equal_int(0, memcmp(contents,
                                 scm_port_string_buffer(port),
                                 sizeof(contents) - 1));
}

void
test_scm_string_port_close_output_port(void)
{
  int ret, data;
  ScmPort *port = scm_port_open_output_string();

  cut_assert_false(scm_port_is_closed(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_is_closed(port));

  cut_assert_equal_int(-1, scm_port_write_prim(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}
