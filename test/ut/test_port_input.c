#include <string.h>

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/number.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"
#include "scythe/port.h"
#include "scythe/symbol.h"

#include"test.h"

TEST_GROUP(port_input);

#define TEST_FILE_PATH "test_api_input_test_file"
#define TEST_FILE_CONTENTS_1ST_LINE "(hello world)"
#define TEST_FILE_CONTENTS_2ND_LINE "(dlrow olleh)"
#define TEST_FILE_CONTENTS (TEST_FILE_CONTENTS_1ST_LINE "\n" TEST_FILE_CONTENTS_2ND_LINE)

#define TEST_FILE_NR_LINE 2
#define TEST_FILE_NR_S_EXPR 2

static ScmScythe *scy;
static ScmRefStackInfo rsi;
static ScmObj file_port = SCM_OBJ_INIT;
static ScmObj string_port = SCM_OBJ_INIT;

static void
make_test_file(void)
{
  FILE *fp = fopen(TEST_FILE_PATH, "w");
  fputs(TEST_FILE_CONTENTS, fp);
  fclose(fp);
}

static void
delete_test_file(void)
{
  remove(TEST_FILE_PATH);
}

TEST_SETUP(port_input)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);

  make_test_file();

  file_port = string_port = SCM_OBJ_NULL;
  scm_register_extra_rfrn(SCM_REF_MAKE(file_port));
  scm_register_extra_rfrn(SCM_REF_MAKE(string_port));
  file_port = scm_open_input_file(TEST_FILE_PATH, NULL);
  string_port = scm_open_input_string_cstr(TEST_FILE_CONTENTS,
                                           SCM_ENC_NAME_SRC);
}

TEST_TEAR_DOWN(port_input)
{
  delete_test_file();

  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

static void
test_read(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  const char *str[] = { "world", "hello", NULL };

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected, &sym);

  expected = SCM_NIL_OBJ;
  for (const char **p = str; *p != NULL; p++) {
    sym = scm_make_symbol_from_cstr(*p, SCM_ENC_SRC);
    expected = scm_cons(sym, expected);
  }

  actual = scm_read(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read__return_EOF(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  for (int i = 0; i < TEST_FILE_NR_S_EXPR; i++)
    scm_read(port);

  TEST_ASSERT_TRUE(scm_eof_object_p(scm_read(port)));
}

static void
test_read__specify_closed_port__return_ERROR(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_read(port));
}

static void
test_read_cchr(ScmObj port)
{
  scm_char_t actual, expected;
  ssize_t sz;

  SCM_REFSTK_INIT_REG(&port);

  expected.ascii = TEST_FILE_CONTENTS[0];

  sz = scm_read_cchr(&actual, port);

  TEST_ASSERT_EQUAL_INT(scm_enc_char_width(SCM_ENC_SRC,
                                           expected.bytes, sizeof(expected)),
                        sz);
  TEST_ASSERT_EQUAL_INT(0, memcmp(expected.bytes, actual.bytes, (size_t)sz));
}

static void
test_read_cchr__return_EOF(ScmObj port)
{
  scm_char_t actual;

  SCM_REFSTK_INIT_REG(&port);

  for (size_t i = 0; i < strlen(TEST_FILE_CONTENTS); i++)
    scm_read_cchr(&actual, port);

  TEST_ASSERT_EQUAL_INT(0, scm_read_cchr(&actual, port));
}

static void
test_read_cchr__specify_closed_port__return_ERROR(ScmObj port)
{
  scm_char_t actual;

  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_read_cchr(&actual, port));
}

static void
test_read_char(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  expected = scm_make_char(&(scm_char_t){ .ascii = TEST_FILE_CONTENTS[0] },
                           SCM_ENC_SRC);

  actual = scm_read_char(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read_char__return_EOF(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  for (size_t i = 0; i < strlen(TEST_FILE_CONTENTS); i++)
    scm_read_char(port);

  TEST_ASSERT_TRUE(scm_eof_object_p(scm_read_char(port)));
}

static void
test_read_char__specify_closed_port__return_ERROR(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_read_char(port));
}

static void
test_peek_cchr(ScmObj port)
{
  scm_char_t actual, expected;
  ssize_t sz;

  SCM_REFSTK_INIT_REG(&port);

  expected.ascii = TEST_FILE_CONTENTS[0];

  sz = scm_peek_cchr(&actual, port);

  TEST_ASSERT_EQUAL_INT(scm_enc_char_width(SCM_ENC_SRC,
                                           expected.bytes, sizeof(expected)),
                        sz);
  TEST_ASSERT_EQUAL_INT(0, memcmp(expected.bytes, actual.bytes, (size_t)sz));
}

static void
test_peek_cchr__return_same_char_with_preceding_peek_cchr(ScmObj port)
{
  scm_char_t actual, expected;
  ssize_t sz_a, sz_e;

  SCM_REFSTK_INIT_REG(&port);

  sz_e = scm_peek_cchr(&expected, port);
  sz_a = scm_peek_cchr(&actual, port);

  TEST_ASSERT_EQUAL_INT(sz_e, sz_a);
  TEST_ASSERT_EQUAL_INT(0, memcmp(expected.bytes, actual.bytes, (size_t)sz_e));
}

static void
test_peek_cchr__return_same_char_with_next_call_to_read_cchr(ScmObj port)
{
  scm_char_t actual, expected;
  ssize_t sz_a, sz_e;

  SCM_REFSTK_INIT_REG(&port);

  sz_a = scm_peek_cchr(&actual, port);
  sz_e = scm_read_cchr(&expected, port);

  TEST_ASSERT_EQUAL_INT(sz_e, sz_a);
  TEST_ASSERT_EQUAL_INT(0, memcmp(expected.bytes, actual.bytes, (size_t)sz_e));
}

static void
test_peek_cchr__return_EOF(ScmObj port)
{
  scm_char_t actual;

  SCM_REFSTK_INIT_REG(&port);

  for (size_t i = 0; i < strlen(TEST_FILE_CONTENTS); i++)
    scm_read_cchr(&actual, port);

  TEST_ASSERT_EQUAL_INT(0, scm_peek_cchr(&actual, port));
}

static void
test_peek_cchr__specify_closed_port__return_ERROR(ScmObj port)
{
  scm_char_t actual;

  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);

  TEST_ASSERT_EQUAL_INT(-1, scm_peek_cchr(&actual, port));
}

static void
test_peek_char(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  expected = scm_make_char(&(scm_char_t){ .ascii = '(' }, SCM_ENC_SRC);

  actual = scm_peek_char(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_peek_char__return_same_char_with_preceding_peek_char(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  expected = scm_peek_char(port);
  actual = scm_peek_char(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_peek_char__return_same_char_with_next_call_to_read_char(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  actual = scm_peek_char(port);
  expected = scm_read_char(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_peek_char__return_EOF(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  for (size_t i = 0; i < strlen(TEST_FILE_CONTENTS); i++)
    scm_read_char(port);

  TEST_ASSERT_TRUE(scm_eof_object_p(scm_peek_char(port)));
}

static void
test_peek_char__specify_closed_port__return_ERROR(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_peek_char(port));
}

static void
test_read_line__upt_to_EOL(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  expected = scm_make_string_from_cstr(TEST_FILE_CONTENTS_1ST_LINE,
                                       SCM_ENC_SRC);
  actual = scm_read_line(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read_line__upt_to_EOF(ScmObj port)
{
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  expected = scm_make_string_from_cstr(TEST_FILE_CONTENTS_2ND_LINE,
                                       SCM_ENC_SRC);
  scm_read_line(port);
  actual = scm_read_line(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read_line__return_EOF(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  for (int i = 0; i < TEST_FILE_NR_LINE; i++)
    scm_read_line(port);

  TEST_ASSERT_TRUE(scm_eof_object_p(scm_read_line(port)));
}

static void
test_read_line__specify_closed_port__return_ERROR(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_read_line(port));
}

static void
test_char_ready__return_TRUE(ScmObj port)
{
  bool actual;

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_char_ready(port, &actual));
  TEST_ASSERT_TRUE(actual);
}

static void
test_char_ready__specify_closed_port__return_ERROR(ScmObj port)
{
  bool actual;

  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);

  TEST_ASSERT_EQUAL_INT(-1, scm_char_ready(port, &actual));
}

static void
test_char_ready_P__return_TRUE(ScmObj port)
{
  TEST_ASSERT_SCM_TRUE(scm_char_ready_P(port));
}

static void
test_char_ready_P__specify_closed_port__return_ERROR(ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);

  TEST_ASSERT_SCM_NULL(scm_char_ready_P(port));
}

static void
test_read_string(ScmObj port)
{
  const size_t len = 9;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_NULL;
  char str[len + 1];

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected);

  strncpy(str, TEST_FILE_CONTENTS, len);
  str[len] = '\0';

  expected = scm_make_string_from_cstr(str, SCM_ENC_SRC);
  actual = scm_read_string(len, port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read_string__read_up_to_EOF(ScmObj port)
{
  const size_t len = strlen(TEST_FILE_CONTENTS) + 1;
  ScmObj actual = SCM_OBJ_INIT, expected = SCM_OBJ_NULL, n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expected, &n);

  n = scm_make_number_from_size_t(len);
  expected = scm_make_string_from_cstr(TEST_FILE_CONTENTS, SCM_ENC_SRC);
  actual = scm_read_string(n, port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

static void
test_read_string__return_EOF(ScmObj port)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &n);

  n = scm_make_number_from_size_t(strlen(TEST_FILE_CONTENTS));
  scm_read_string(n, port);

  TEST_ASSERT_TRUE(scm_eof_object_p(scm_read_string(n, port)));
}

static void
test_read_string__specify_closed_port__return_ERROR(ScmObj port)
{
  ScmObj n = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &n);

  n = scm_make_number_from_size_t(strlen(TEST_FILE_CONTENTS));

  scm_close_port(port);

  TEST_ASSERT_SCM_NULL(scm_read_string(n, port));
}

TEST(port_input, file_port__read)
{
  test_read(file_port);
}

TEST(port_input, string_port__read)
{
  test_read(string_port);
}

TEST(port_input, file_port__read__return_EOF)
{
  test_read__return_EOF(file_port);
}

TEST(port_input, string_port__read__return_EOF)
{
  test_read__return_EOF(string_port);
}

TEST(port_input, file_port__read__specify_closed_port__return_ERROR)
{
  test_read__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__read__specify_closed_port__return_ERROR)
{
  test_read__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__read_cchr)
{
  test_read_cchr(file_port);
}

TEST(port_input, string_port__read_cchr)
{
  test_read_cchr(string_port);
}

TEST(port_input, file_port__read_cchr__return_EOF)
{
  test_read_cchr__return_EOF(file_port);
}

TEST(port_input, string_port__read_cchr__return_EOF)
{
  test_read_cchr__return_EOF(string_port);
}

TEST(port_input, file_port__read_cchr__specify_closed_port__return_ERROR)
{
  test_read_cchr__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__read_cchr__specify_closed_port__return_ERROR)
{
  test_read_cchr__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__read_char)
{
  test_read_char(file_port);
}

TEST(port_input, string_port__read_char)
{
  test_read_char(string_port);
}

TEST(port_input, file_port__read_char__return_EOF)
{
  test_read_char__return_EOF(file_port);
}

TEST(port_input, string_port__read_char__return_EOF)
{
  test_read_char__return_EOF(string_port);
}

TEST(port_input, file_port__read_char__specify_closed_port__return_ERROR)
{
  test_read_char__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__read_char__specify_closed_port__return_ERROR)
{
  test_read_char__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__peek_cchr)
{
  test_peek_cchr(file_port);
}

TEST(port_input, string_port__peek_cchr)
{
  test_peek_cchr(string_port);
}

TEST(port_input, file_port__peek_cchr__return_same_char_with_preceding_peek_cchr)
{
  test_peek_cchr__return_same_char_with_preceding_peek_cchr(file_port);
}

TEST(port_input, string_port__peek_cchr__return_same_char_with_preceding_peek_cchr)
{
  test_peek_cchr__return_same_char_with_preceding_peek_cchr(string_port);
}

TEST(port_input, file_port__peek_cchr__return_same_char_with_next_call_to_read_cchr)
{
  test_peek_cchr__return_same_char_with_next_call_to_read_cchr(file_port);
}

TEST(port_input, string_port__peek_cchr__return_same_char_with_next_call_to_read_cchr)
{
  test_peek_cchr__return_same_char_with_next_call_to_read_cchr(string_port);
}

TEST(port_input, file_port__peek_cchr__return_EOF)
{
  test_peek_cchr__return_EOF(file_port);
}

TEST(port_input, string_port__peek_cchr__return_EOF)
{
  test_peek_cchr__return_EOF(string_port);
}

TEST(port_input, file_port__peek_cchr__specify_closed_port__return_ERROR)
{
  test_peek_cchr__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__peek_cchr__specify_closed_port__return_ERROR)
{
  test_peek_cchr__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__peek_char)
{
  test_peek_char(file_port);
}

TEST(port_input, string_port__peek_char)
{
  test_peek_char(string_port);
}

TEST(port_input, file_port__peek_char__return_same_char_with_preceding_peek_char)
{
  test_peek_char__return_same_char_with_preceding_peek_char(file_port);
}

TEST(port_input, string_port__peek_char__return_same_char_with_preceding_peek_char)
{
  test_peek_char__return_same_char_with_preceding_peek_char(string_port);
}

TEST(port_input, file_port__peek_char__return_same_char_with_next_call_to_read_char)
{
  test_peek_char__return_same_char_with_next_call_to_read_char(file_port);
}

TEST(port_input, string_port__peek_char__return_same_char_with_next_call_to_read_char)
{
  test_peek_char__return_same_char_with_next_call_to_read_char(string_port);
}

TEST(port_input, file_port__peek_char__return_EOF)
{
  test_peek_char__return_EOF(file_port);
}

TEST(port_input, string_port__peek_char__return_EOF)
{
  test_peek_char__return_EOF(string_port);
}

TEST(port_input, file_port__peek_char__specify_closed_port__return_ERROR)
{
  test_peek_char__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__peek_char__specify_closed_port__return_ERROR)
{
  test_peek_char__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__read_line__upt_to_EOL)
{
  test_read_line__upt_to_EOL(file_port);
}

TEST(port_input, string_port__read_line__upt_to_EOL)
{
  test_read_line__upt_to_EOL(string_port);
}

TEST(port_input, file_port__read_line__upt_to_EOF)
{
  test_read_line__upt_to_EOF(file_port);
}

TEST(port_input, string_port__read_line__upt_to_EOF)
{
  test_read_line__upt_to_EOF(string_port);
}

TEST(port_input, file_port__read_line__return_EOF)
{
  test_read_line__return_EOF(file_port);
}

TEST(port_input, string_port__read_line__return_EOF)
{
  test_read_line__return_EOF(string_port);
}

TEST(port_input, file_port__read_line__specify_closed_port__return_ERROR)
{
  test_read_line__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__read_line__specify_closed_port__return_ERROR)
{
  test_read_line__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__char_ready__return_TRUE)
{
  test_char_ready__return_TRUE(file_port);
}

TEST(port_input, string_port__char_ready__return_TRUE)
{
  test_char_ready__return_TRUE(string_port);
}

IGNORE_TEST(port_input, file_port__char_ready__return_FALSE)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_char_ready(file_port, &actual));
  TEST_ASSERT_FALSE(actual);
}

IGNORE_TEST(port_input, string_port__char_ready__return_FALSE)
{
  bool actual;

  TEST_ASSERT_EQUAL_INT(0, scm_char_ready(string_port, &actual));
  TEST_ASSERT_FALSE(actual);
}

TEST(port_input, file_port__char_ready__specify_closed_port__return_ERROR)
{
  test_char_ready__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__char_ready__specify_closed_port__return_ERROR)
{
  test_char_ready__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__char_ready_P__return_TRUE)
{
  test_char_ready_P__return_TRUE(file_port);
}

TEST(port_input, string_port__char_ready_P__return_TRUE)
{
  test_char_ready_P__return_TRUE(string_port);
}

IGNORE_TEST(port_input, file_port__char_ready_P__return_FALSE)
{
  TEST_ASSERT_SCM_FALSE(scm_char_ready_P(file_port));
}

IGNORE_TEST(port_input, string_port__char_ready_P__return_FALSE)
{
  TEST_ASSERT_SCM_FALSE(scm_char_ready_P(string_port));
}

TEST(port_input, file_port__char_ready_P__specify_closed_port__return_ERROR)
{
  test_char_ready_P__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__char_ready_P__specify_closed_port__return_ERROR)
{
  test_char_ready_P__specify_closed_port__return_ERROR(string_port);
}

TEST(port_input, file_port__read_string)
{
  test_read_string(file_port);
}

TEST(port_input, string_port__read_string)
{
  test_read_string(string_port);
}

TEST(port_input, file_port__read_string__read_up_to_EOF)
{
  test_read_string__read_up_to_EOF(file_port);
}

TEST(port_input, string_port__read_string__read_up_to_EOF)
{
  test_read_string__read_up_to_EOF(string_port);
}

TEST(port_input, file_port__read_string__return_EOF)
{
  test_read_string__return_EOF(file_port);
}

TEST(port_input, string_port__read_string__return_EOF)
{
  test_read_string__return_EOF(string_port);
}

TEST(port_input, file_port__read_string__specify_closed_port__return_ERROR)
{
  test_read_string__specify_closed_port__return_ERROR(file_port);
}

TEST(port_input, string_port__read_string__specify_closed_port__return_ERROR)
{
  test_read_string__specify_closed_port__return_ERROR(string_port);
}
