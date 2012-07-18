#include <cutter.h>

#include <stdio.h>
#include <stdbool.h>

#include "vm.h"
#include "port.h"

#define TEST_TEXT_CONTENTS "hello, world\nhello, world!"

static ScmEvaluator *ev;

void
cut_startup(void)
{
  ev = scm_capi_evaluator();
  scm_capi_ut_setup_current_vm(ev);
}

void
cut_shutdown(void)
{
  scm_capi_evaluator_end(ev);
}

void
test_scm_string_port_new_input_port(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                    sizeof(TEST_TEXT_CONTENTS) - 1,
                                    SCM_ENC_ASCII);

  cut_assert_true(scm_obj_not_null_p(port));
  cut_assert_true(scm_port_readable_p(port));
  cut_assert_false(scm_port_writable_p(port));
  cut_assert_false(scm_port_file_port_p(port));
  cut_assert_true(scm_port_string_port_p(port));
  cut_assert_false(scm_port_closed_p(port));
}

void
test_scm_string_port_read_per_bye(void)
{
  char expected_chars[] = TEST_TEXT_CONTENTS;
  char byte;
  ssize_t ret;
  int i;

  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                    sizeof(TEST_TEXT_CONTENTS) - 1,
                                    SCM_ENC_ASCII);

  for (i = 0; i < (int)sizeof(expected_chars) - 1; i++) {
    ret = scm_port_read(port, &byte, sizeof(byte));
    cut_assert_equal_int(sizeof(byte), ret);
    cut_assert_equal_int(expected_chars[i], byte);
  }

  ret = scm_port_read(port, &byte, sizeof(byte));
  cut_assert_equal_int(0, ret);
}

void
test_scm_string_port_interleave_read_and_seek(void)
{
  char expected_chars[] = TEST_TEXT_CONTENTS;
  char byte;
  ssize_t ret;
  int i;

  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                    sizeof(TEST_TEXT_CONTENTS) - 1,
                                    SCM_ENC_ASCII);


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
test_scm_string_port_close_input_port(void)
{
  int ret, data;
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_input_string(TEST_TEXT_CONTENTS,
                                    sizeof(TEST_TEXT_CONTENTS) - 1,
                                    SCM_ENC_ASCII);

  cut_assert_false(scm_port_closed_p(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_closed_p(port));

  cut_assert_equal_int(-1, scm_port_read(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}

void
test_scm_string_port_new_output_port(void)
{
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_string(SCM_ENC_ASCII);

  cut_assert_true(scm_obj_not_null_p(port));
  cut_assert_false(scm_port_readable_p(port));
  cut_assert_true(scm_port_writable_p(port));
  cut_assert_false(scm_port_file_port_p(port));
  cut_assert_true(scm_port_string_port_p(port));
  cut_assert_false(scm_port_closed_p(port));
}

void
test_scm_string_port_write_per_byte(void)
{
  char data[] = TEST_TEXT_CONTENTS;
  int i;
  ssize_t ret;

  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_string(SCM_ENC_ASCII);

  for (i = 0; i < (int)sizeof(data) - 1; i++) {
    ret = scm_port_write(port, data + i, sizeof(char));
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

  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_string(SCM_ENC_ASCII);

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
  ScmObj port = SCM_OBJ_INIT;

  port = scm_port_open_output_string(SCM_ENC_ASCII);

  cut_assert_false(scm_port_closed_p(port));

  ret = scm_port_close(port);

  cut_assert_equal_int(0, ret);
  cut_assert_true(scm_port_closed_p(port));

  cut_assert_equal_int(-1, scm_port_write(port, &data, sizeof(data)));
  cut_assert_equal_int(-1, scm_port_seek(port, 0, SEEK_SET));
}
