#include <cutter.h>

#include <stdio.h>

#include "ibuffer.h"

#define TEST_DATA_FILE "test_ibuffer_input_tmp_file"
#define TEST_DATA_BIG_FILE "test_ibuffer_input_tmp_big_file"

static ScmPort *test_port;
static ScmPort *test_big_file_port;
static ScmPort *test_string_port;

void
startup(void)
{
  FILE *fp;
  int i, j;

  fp = fopen(TEST_DATA_FILE, "w");
  fputs("hello, world\n", fp);
  fclose(fp);

  fp = fopen(TEST_DATA_BIG_FILE, "w");
  for(i = 0; i < 4; i++) {
    for (j = 0; j < 63; j++) fputc('a', fp);
    fputc('\n', fp);
  } /* (63 + 1) * 4 = 256 is bigger than BUFFER_SIZE */
  
  fclose(fp);
}

void
shutdown(void)
{
  remove(TEST_DATA_FILE);
  remove(TEST_DATA_BIG_FILE);
}

void
setup(void)
{
  test_port = scm_port_construct_input_port(TEST_DATA_FILE,
                                           SCM_PORT_BUF_DEFAULT);
  test_big_file_port = scm_port_construct_input_port(TEST_DATA_BIG_FILE,
                                                    SCM_PORT_BUF_DEFAULT);
  test_string_port = scm_port_construct_input_string_port("hello, world\n", 13);
}

void
teardown(void)
{
  scm_port_destruct(test_port);
  scm_port_destruct(test_big_file_port);
  scm_port_destruct(test_string_port);
}


void
test_scm_ibuffer_construct(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_port);
  cut_assert_not_null(ibuffer);
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_column_num(ibuffer));
}

void
test_scm_ibuffer_construct_from_string(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_string_port);
  cut_assert_not_null(ibuffer);
}

void
test_scm_ibuffer_interleave_shift_and_head_char_for_file(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_port);

  cut_assert_equal_int('h', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('e', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(2, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(3, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(4, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(5, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(',', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(6, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(' ', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(7, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('w', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(8, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(9, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('r', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(10, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(11, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('d', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(12, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('\n', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(13, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(EOF, scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(2, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);
  cut_assert_true(scm_ibuffer_is_eof(ibuffer));
}

void
test_scm_ibuffer_interleave_shift_and_head_char_for_string(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_string_port);

  cut_assert_equal_int('h', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('e', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(2, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(3, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(4, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(5, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(',', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(6, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(' ', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(7, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('w', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(8, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(9, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('r', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(10, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(11, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int('d', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(12, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('\n', scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(13, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);

  cut_assert_equal_int(EOF, scm_ibuffer_head_char(ibuffer));
  cut_assert_equal_int(2, scm_ibuffer_current_line_num(ibuffer));
  cut_assert_equal_int(1, scm_ibuffer_current_column_num(ibuffer));

  scm_ibuffer_shift_char(ibuffer);
  cut_assert_true(scm_ibuffer_is_eof(ibuffer));
}

void
test_scm_ibuffer_forecast_for_file(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_port);

  cut_assert_equal_int('h', scm_ibuffer_forecast(ibuffer, 0));
  cut_assert_equal_int('e', scm_ibuffer_forecast(ibuffer, 1));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 2));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 3));
  cut_assert_equal_int('o', scm_ibuffer_forecast(ibuffer, 4));
  cut_assert_equal_int(',', scm_ibuffer_forecast(ibuffer, 5));
  cut_assert_equal_int(' ', scm_ibuffer_forecast(ibuffer, 6));
  cut_assert_equal_int('w', scm_ibuffer_forecast(ibuffer, 7));
  cut_assert_equal_int('o', scm_ibuffer_forecast(ibuffer, 8));
  cut_assert_equal_int('r', scm_ibuffer_forecast(ibuffer, 9));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 10));
  cut_assert_equal_int('d', scm_ibuffer_forecast(ibuffer, 11));
  cut_assert_equal_int('\n', scm_ibuffer_forecast(ibuffer, 12));
  cut_assert_equal_int(EOF, scm_ibuffer_forecast(ibuffer, 13));
}

void
test_scm_ibuffer_forecast_for_string(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_string_port);

  cut_assert_equal_int('h', scm_ibuffer_forecast(ibuffer, 0));
  cut_assert_equal_int('e', scm_ibuffer_forecast(ibuffer, 1));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 2));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 3));
  cut_assert_equal_int('o', scm_ibuffer_forecast(ibuffer, 4));
  cut_assert_equal_int(',', scm_ibuffer_forecast(ibuffer, 5));
  cut_assert_equal_int(' ', scm_ibuffer_forecast(ibuffer, 6));
  cut_assert_equal_int('w', scm_ibuffer_forecast(ibuffer, 7));
  cut_assert_equal_int('o', scm_ibuffer_forecast(ibuffer, 8));
  cut_assert_equal_int('r', scm_ibuffer_forecast(ibuffer, 9));
  cut_assert_equal_int('l', scm_ibuffer_forecast(ibuffer, 10));
  cut_assert_equal_int('d', scm_ibuffer_forecast(ibuffer, 11));
  cut_assert_equal_int('\n', scm_ibuffer_forecast(ibuffer, 12));
  cut_assert_equal_int(EOF, scm_ibuffer_forecast(ibuffer, 13));
}

void
test_scm_ibuffer_interleave_shift_and_forecast_for_file(void)
{
  FILE *fp;
  char content[256];
  size_t size;
  unsigned int shift, forecast;

  fp = fopen(TEST_DATA_FILE, "r");
  fgets(content, sizeof(content), fp);
  fclose(fp);
  size = strlen(content);

  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_port);

  for (shift = 0; shift < size; shift++) {
    for (forecast = 0; forecast < size - shift; forecast++) {
      cut_assert_equal_int(content[shift + forecast],
                                   scm_ibuffer_forecast(ibuffer, forecast));
    }
    cut_assert_equal_int(EOF, scm_ibuffer_forecast(ibuffer, forecast));
    scm_ibuffer_shift_char(ibuffer);
  }
}

void
test_scm_ibuffer_interleave_shift_and_forecast_for_string(void)
{
  char *content = "hello, world\n";
  size_t size;
  unsigned int shift, forecast;

  size = strlen(content);

  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_string_port);

  for (shift = 0; shift < size; shift++) {
    for (forecast = 0; forecast < size - shift; forecast++) {
      cut_assert_equal_int(content[shift + forecast],
                                   scm_ibuffer_forecast(ibuffer, forecast));
    }
    cut_assert_equal_int(EOF, scm_ibuffer_forecast(ibuffer, forecast));
    scm_ibuffer_shift_char(ibuffer);
  }
}

void
test_scm_ibuffer_long_forecast()
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_big_file_port);

  /* 191 is bigger than BUFFER_SIZE */
  cut_assert_equal_int('\n',
                       scm_ibuffer_forecast(ibuffer, 191));

}
