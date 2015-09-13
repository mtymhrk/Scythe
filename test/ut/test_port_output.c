#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/char.h"
#include "scythe/pair.h"
#include "scythe/port.h"

#include "test.h"

TEST_GROUP(port_output);

#define TEST_FILE_PATH "test_api_output_test_file"

static ScmScythe *scy;
static ScmRefStackInfo rsi;
static ScmObj file_port = SCM_OBJ_INIT;
static ScmObj string_port = SCM_OBJ_INIT;

static void
delete_test_file(void)
{
  remove(TEST_FILE_PATH);
}

TEST_SETUP(port_output)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);

  file_port = string_port = SCM_OBJ_NULL;
  scm_register_extra_rfrn(SCM_REF_MAKE(file_port));
  scm_register_extra_rfrn(SCM_REF_MAKE(string_port));
  file_port = scm_open_output_file(TEST_FILE_PATH, NULL);
  string_port = scm_open_output_string();
}

TEST_TEAR_DOWN(port_output)
{
  delete_test_file();

  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

enum { FILEPORT, STRINGPORT };

static void
chk_file_contents(const char *expected)
{
  FILE *fp;
  char actual[256];
  size_t i;
  int c;

  fp = fopen(TEST_FILE_PATH, "r");
  for (i = 0; i < sizeof(actual) - 1 && (c = fgetc(fp)) != EOF; i++)
    actual[i] = (char)c;
  fclose(fp);
  actual[i] = '\0';

  TEST_ASSERT(c == EOF);
  TEST_ASSERT_EQUAL_STRING(expected, actual);
}

static void
chk_string_port_contents(ScmObj port, const char *expected)
{
  ScmObj actual = SCM_OBJ_INIT, expe;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expe);

  expe = scm_make_string_from_cstr(expected, SCM_ENC_SRC);
  actual = scm_get_output_string(port);

  TEST_ASSERT_SCM_EQUAL(expe, actual);
}

static void
test_write_shared(ScmObj port, int type)
{
  const char *expected = "\"hello\"";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_EQUAL_INT(0, scm_write_shared(o, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_shared__write_shared_structure(ScmObj port, int type)
{
  const char *expected = "#0=(#t . #0#)";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  scm_set_cdr(o, o);

  TEST_ASSERT_EQUAL_INT(0, scm_write_shared(o, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_shared__write_NULL(ScmObj port, int type)
{
  const char *expected = "#<INTERNAL-NULL-VALUE 0>";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_write_shared(SCM_OBJ_NULL, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_shared__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_write_shared(o, port));
}

static void
test_write_simple(ScmObj port, int type)
{
  const char *expected = "\"hello\"";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_EQUAL_INT(0, scm_write_simple(o, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_simple__write_NULL(ScmObj port, int type)
{
  const char *expected = "#<INTERNAL-NULL-VALUE 0>";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_write_simple(SCM_OBJ_NULL, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_simple__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_write_simple(o, port));
}

static void
test_display(ScmObj port, int type)
{
  const char *expected = "hello";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_EQUAL_INT(0, scm_display(o, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_display__write_shared_structure(ScmObj port, int type)
{
  const char *expected = "#0=(#t . #0#)";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  scm_set_cdr(o, o);

  TEST_ASSERT_EQUAL_INT(0, scm_display(o, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_display__write_NULL(ScmObj port, int type)
{
  const char *expected = "#<INTERNAL-NULL-VALUE 0>";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_display(SCM_OBJ_NULL, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_display__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_display(o, port));
}

static void
test_newline(ScmObj port, int type)
{
  const char *expected = "\n";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_newline(port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_newline__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_newline(port));
}

static void
test_write_cchr(ScmObj port, int type)
{
  const char *expected = "?";
  scm_char_t c = { .ascii = '?' };

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_write_cchr(c, SCM_ENC_SRC, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_cchr__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  scm_char_t c = { .ascii = '?' };

  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_write_cchr(c, SCM_ENC_SRC, port));
}

static void
test_write_char(ScmObj port, int type)
{
  const char *expected = "?";
  ScmObj c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  c = scm_make_char(&(scm_char_t){ .ascii = '?' }, SCM_ENC_SRC);
  TEST_ASSERT_EQUAL_INT(0, scm_write_char(c, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_char__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  c = scm_make_char(&(scm_char_t){ .ascii = '?' }, SCM_ENC_SRC);
  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_write_char(c, port));
}

static void
test_write_cstr(ScmObj port, int type)
{
  const char *expected = "hello";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_EQUAL_INT(0, scm_write_cstr(expected, SCM_ENC_SRC, port));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_cstr__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);

  TEST_ASSERT_EQUAL_INT(-1, scm_write_cstr("hello", SCM_ENC_SRC, port));
}

static void
test_write_string(ScmObj port, int type)
{
  const char *expected = "hello";
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s);

  s = scm_make_string_from_cstr(expected, SCM_ENC_SRC);
  TEST_ASSERT_EQUAL_INT(0, scm_write_string(s, port, -1, -1));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_string__specify_start(ScmObj port, int type)
{
  const char *expected = "ello";
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s);

  s = scm_make_string_from_cstr("hello", SCM_ENC_SRC);
  TEST_ASSERT_EQUAL_INT(0, scm_write_string(s, port, 1, -1));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_write_string__specify_start_end(ScmObj port, int type)
{
  const char *expected = "ell";
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s);

  s = scm_make_string_from_cstr("hello", SCM_ENC_SRC);
  TEST_ASSERT_EQUAL_INT(0, scm_write_string(s, port, 1, 4));

  scm_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_flush_output_port(ScmObj port, int type)
{
  const char *expected = "hello";

  SCM_REFSTK_INIT_REG(&port);

  scm_write_cstr(expected, SCM_ENC_SRC, port);
  TEST_ASSERT_EQUAL_INT(0, scm_flush_output_port(port));

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_flush_output_port__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_close_port(port);
  TEST_ASSERT_EQUAL_INT(-1, scm_flush_output_port(port));
}


IGNORE_TEST(port_output, write)
{
}

TEST(port_output, file_port__write_shared)
{
  test_write_shared(file_port, FILEPORT);
}

TEST(port_output, string_port__write_shared)
{
  test_write_shared(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_shared__write_shared_structure)
{
  test_write_shared__write_shared_structure(file_port, FILEPORT);
}

TEST(port_output, string_port__write_shared__write_shared_structure)
{
  test_write_shared__write_shared_structure(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_shared__write_NULL)
{
  test_write_shared__write_NULL(file_port, FILEPORT);
}

TEST(port_output, string_port__write_shared__write_NULL)
{
  test_write_shared__write_NULL(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_shared__specify_closed_port__return_ERROR)
{
  test_write_shared__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__write_shared__specify_closed_port__return_ERROR)
{
  test_write_shared__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_simple)
{
  test_write_simple(file_port, FILEPORT);
}

TEST(port_output, string_port__write_simple)
{
  test_write_simple(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_simple__write_NULL)
{
  test_write_simple__write_NULL(file_port, FILEPORT);
}

TEST(port_output, string_port__write_simple__write_NULL)
{
  test_write_simple__write_NULL(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_simple__specify_closed_port__return_ERROR)
{
  test_write_simple__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__write_simple__specify_closed_port__return_ERROR)
{
  test_write_simple__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__display)
{
  test_display(file_port, FILEPORT);
}

TEST(port_output, string_port__display)
{
  test_display(string_port, STRINGPORT);
}

TEST(port_output, file_port__display__write_shared_structure)
{
  test_display__write_shared_structure(file_port, FILEPORT);
}

TEST(port_output, string_port__display__write_shared_structure)
{
  test_display__write_shared_structure(string_port, STRINGPORT);
}

TEST(port_output, file_port__display__write_NULL)
{
  test_display__write_NULL(file_port, FILEPORT);
}

TEST(port_output, string_port__display__write_NULL)
{
  test_display__write_NULL(string_port, STRINGPORT);
}

TEST(port_output, file_port__display__specify_closed_port__return_ERROR)
{
  test_display__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__display__specify_closed_port__return_ERROR)
{
  test_display__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__newline)
{
  test_newline(file_port, FILEPORT);
}

TEST(port_output, string_port__newline)
{
  test_newline(string_port, STRINGPORT);
}

TEST(port_output, file_port__newline__specify_closed_port__return_ERROR)
{
  test_newline__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__newline__specify_closed_port__return_ERROR)
{
  test_newline__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_cchr)
{
  test_write_cchr(file_port, FILEPORT);
}

TEST(port_output, string_port__write_cchr)
{
  test_write_cchr(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_cchr__specify_closed_port__return_ERROR)
{
  test_write_cchr__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__write_cchr__specify_closed_port__return_ERROR)
{
  test_write_cchr__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_char)
{
  test_write_char(file_port, FILEPORT);
}

TEST(port_output, string_port__write_char)
{
  test_write_char(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_char__specify_closed_port__return_ERROR)
{
  test_write_char__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__write_char__specify_closed_port__return_ERROR)
{
  test_write_char__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_cstr)
{
  test_write_cstr(file_port, FILEPORT);
}

TEST(port_output, string_port__write_cstr)
{
  test_write_cstr(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_cstr__specify_closed_port__return_ERROR)
{
  test_write_cstr__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__write_cstr__specify_closed_port__return_ERROR)
{
  test_write_cstr__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_string)
{
  test_write_string(file_port, FILEPORT);
}

TEST(port_output, string_port__write_string)
{
  test_write_string(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_string__specify_start)
{
  test_write_string__specify_start(file_port, FILEPORT);
}

TEST(port_output, string_port__write_string__specify_start)
{
  test_write_string__specify_start(string_port, STRINGPORT);
}

TEST(port_output, file_port__write_string__specify_start_end)
{
  test_write_string__specify_start_end(file_port, FILEPORT);
}

TEST(port_output, string_port__write_string__specify_start_end)
{
  test_write_string__specify_start_end(string_port, STRINGPORT);
}

TEST(port_output, file_port__flush_output_port)
{
  test_flush_output_port(file_port, FILEPORT);
}

TEST(port_output, string_port__flush_output_port)
{
  test_flush_output_port(string_port, STRINGPORT);
}

TEST(port_output, file_port__flush_output_port__specify_closed_port__return_ERROR)
{
  test_flush_output_port__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(port_output, string_port__flush_output_port__specify_closed_port__return_ERROR)
{
  test_flush_output_port__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}
