#include <cutter.h>

#include <stdio.h>

#include "ibuffer.h"

#define TEST_DATA_FILE "test_ibuffer_input_tmp_file"
#define TEST_DATA_BIG_FILE "test_ibuffer_input_tmp_big_file"

static FILE *test_file;
static FILE *test_big_file;

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
  test_file = fopen(TEST_DATA_FILE, "r");
  test_big_file = fopen(TEST_DATA_BIG_FILE, "r");
}

void
teardown(void)
{
  fclose(test_file);
  fclose(test_big_file);
}


void
test_scm_ibuffer_construct(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_file);
  cut_assert_not_null(ibuffer);
}

void
test_scm_ibuffer_construct_from_string(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct_from_string("hello, world\n");
  cut_assert_not_null(ibuffer);
}

void
test_scm_ibuffer_interleave_shift_and_head_char_for_file(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_file);

  cut_assert_equal_int('h', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('e', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(',', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(' ', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('w', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('r', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('d', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('\n', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(EOF, scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_true(scm_ibuffer_is_eof(ibuffer));
}

void
test_scm_ibuffer_interleave_shift_and_head_char_for_string(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct_from_string("hello, world\n");

  cut_assert_equal_int('h', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('e', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(',', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(' ', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('w', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('o', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('r', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('l', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('d', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int('\n', scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_equal_int(EOF, scm_ibuffer_head_char(ibuffer));
  scm_ibuffer_shift_char(ibuffer);
  cut_assert_true(scm_ibuffer_is_eof(ibuffer));
}

void
test_scm_ibuffer_forecast_for_file(void)
{
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_file);

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
  ScmIBuffer *ibuffer = scm_ibuffer_construct_from_string("hello, world\n");

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
  char content[256];
  size_t size;
  unsigned int shift, forecast;

  fgets(content, sizeof(content), test_file);
  rewind(test_file);
  size = strlen(content);

  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_file);

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

  ScmIBuffer *ibuffer = scm_ibuffer_construct_from_string("hello, world\n");

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
  ScmIBuffer *ibuffer = scm_ibuffer_construct(test_big_file);

  /* 191 is bigger than BUFFER_SIZE */
  cut_assert_equal_int('\n',
                       scm_ibuffer_forecast(ibuffer, 191));

}
