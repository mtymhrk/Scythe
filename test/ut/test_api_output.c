#include "scythe/api.h"

#include "test.h"

TEST_GROUP(api_output);

#define TEST_FILE_PATH "test_api_output_test_file"

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;
static ScmObj file_port = SCM_OBJ_INIT;
static ScmObj string_port = SCM_OBJ_INIT;

static void
delete_test_file(void)
{
  remove(TEST_FILE_PATH);
}

TEST_SETUP(api_output)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_fcd_ref_stack_save(&rsi);

  file_port = string_port = SCM_OBJ_NULL;
  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(file_port));
  scm_fcd_mem_register_extra_rfrn(SCM_REF_MAKE(string_port));
  file_port = scm_fcd_open_output_file(TEST_FILE_PATH, NULL);
  string_port = scm_api_open_output_string();
}

TEST_TEAR_DOWN(api_output)
{
  delete_test_file();

  scm_fcd_ref_stack_restore(&rsi);
  scm_capi_evaluator_end(ev);
}

enum { FILEPORT, STRINGPORT };

void
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

void
chk_string_port_contents(ScmObj port, const char *expected)
{
  ScmObj actual = SCM_OBJ_INIT, expe;

  SCM_REFSTK_INIT_REG(&port,
                      &actual, &expe);

  expe = scm_fcd_make_string_from_cstr(expected, SCM_ENC_SRC);
  actual = scm_api_get_output_string(port);

  TEST_ASSERT_SCM_EQUAL(expe, actual);
}

static void
test_api_write_shared(ScmObj port, int type)
{
  const char *expected = "\"hello\"";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_write_shared(o, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_shared__write_shared_structure(ScmObj port, int type)
{
  const char *expected = "#0=(#t . #0#)";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  scm_fcd_set_cdr_i(o, o);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_write_shared(o, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_shared__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_write_shared(o, port));
}

static void
test_api_write_simple(ScmObj port, int type)
{
  const char *expected = "\"hello\"";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_write_simple(o, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_simple__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_write_simple(o, port));
}

static void
test_api_display(ScmObj port, int type)
{
  const char *expected = "hello";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_display(o, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_display__write_shared_structure(ScmObj port, int type)
{
  const char *expected = "#0=(#t . #0#)";
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_cons(SCM_TRUE_OBJ, SCM_FALSE_OBJ);
  scm_fcd_set_cdr_i(o, o);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_display(o, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_display__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &o);

  o = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);

  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_display(o, port));
}

static void
test_api_newline(ScmObj port, int type)
{
  const char *expected = "\n";

  SCM_REFSTK_INIT_REG(&port);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_newline(port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_newline__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_newline(port));
}

static void
test_api_write_char(ScmObj port, int type)
{
  const char *expected = "?";
  ScmObj c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  c = scm_fcd_make_char(&(scm_char_t){ .ascii = '?' }, SCM_ENC_SRC);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_write_char(c, port));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_char__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  ScmObj c = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  c = scm_fcd_make_char(&(scm_char_t){ .ascii = '?' }, SCM_ENC_SRC);
  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_write_char(c, port));
}

static void
test_api_write_string(ScmObj port, int type)
{
  const char *expected = "hello";
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s);

  s = scm_fcd_make_string_from_cstr(expected, SCM_ENC_SRC);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ,
                     scm_api_write_string(s, port, SCM_OBJ_NULL, SCM_OBJ_NULL));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_string__specify_start(ScmObj port, int type)
{
  const char *expected = "llo";
  ScmObj s = SCM_OBJ_INIT, n1 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s, &n1);

  s = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);
  n1 = scm_fcd_make_number_from_sword(2);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ,
                     scm_api_write_string(s, port, n1, SCM_OBJ_NULL));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_string__specify_start_end(ScmObj port, int type)
{
  const char *expected = "l";
  ScmObj s = SCM_OBJ_INIT, n1 = SCM_OBJ_INIT, n2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s, &n1, &n2);

  s = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);
  n1 = scm_fcd_make_number_from_sword(2);
  n2 = scm_fcd_make_number_from_sword(3);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_write_string(s, port, n1, n2));

  scm_api_close_port(port);

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_write_string__specify_invalid_start_end__return_ERROR(ScmObj port, int type)
{
  ScmObj s = SCM_OBJ_INIT, n1 = SCM_OBJ_INIT, n2 = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &s, &n1, &n2);

  s = scm_fcd_make_string_from_cstr("hello", SCM_ENC_SRC);
  n1 = scm_fcd_make_number_from_sword(3);
  n2 = scm_fcd_make_number_from_sword(1);
  TEST_ASSERT_SCM_NULL(scm_api_write_string(s, port, n1, n2));
}

static void
test_api_flush_output_port(ScmObj port, int type)
{
  const char *expected = "hello";

  SCM_REFSTK_INIT_REG(&port);

  scm_fcd_write_cstr(expected, SCM_ENC_SRC, port);
  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_flush_output_port(port));

  if (type == FILEPORT)
    chk_file_contents(expected);
  else if (type == STRINGPORT)
    chk_string_port_contents(port, expected);
}

static void
test_api_flush_output_port__specify_closed_port__return_ERROR(ScmObj port, int type)
{
  SCM_REFSTK_INIT_REG(&port);

  scm_api_close_port(port);
  TEST_ASSERT_SCM_NULL(scm_api_flush_output_port(port));
}


IGNORE_TEST(api_output, api_write)
{
}

TEST(api_output, file_port__api_write_shared)
{
  test_api_write_shared(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_shared)
{
  test_api_write_shared(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_shared__write_shared_structure)
{
  test_api_write_shared__write_shared_structure(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_shared__write_shared_structure)
{
  test_api_write_shared__write_shared_structure(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_shared__specify_closed_port__return_ERROR)
{
  test_api_write_shared__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_shared__specify_closed_port__return_ERROR)
{
  test_api_write_shared__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_simple)
{
  test_api_write_simple(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_simple)
{
  test_api_write_simple(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_simple__specify_closed_port__return_ERROR)
{
  test_api_write_simple__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_simple__specify_closed_port__return_ERROR)
{
  test_api_write_simple__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_display)
{
  test_api_display(file_port, FILEPORT);
}

TEST(api_output, string_port__api_display)
{
  test_api_display(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_display__write_shared_structure)
{
  test_api_display__write_shared_structure(file_port, FILEPORT);
}

TEST(api_output, string_port__api_display__write_shared_structure)
{
  test_api_display__write_shared_structure(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_display__specify_closed_port__return_ERROR)
{
  test_api_display__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_display__specify_closed_port__return_ERROR)
{
  test_api_display__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_newline)
{
  test_api_newline(file_port, FILEPORT);
}

TEST(api_output, string_port__api_newline)
{
  test_api_newline(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_newline__specify_closed_port__return_ERROR)
{
  test_api_newline__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_newline__specify_closed_port__return_ERROR)
{
  test_api_newline__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_char)
{
  test_api_write_char(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_char)
{
  test_api_write_char(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_char__specify_closed_port__return_ERROR)
{
  test_api_write_char__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_char__specify_closed_port__return_ERROR)
{
  test_api_write_char__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_string)
{
  test_api_write_string(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_string)
{
  test_api_write_string(string_port, STRINGPORT);
}

TEST(api_output, string_port__api_write_string__specify_start)
{
  test_api_write_string__specify_start(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_string__specify_start_end)
{
  test_api_write_string__specify_start_end(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_string__specify_start_end)
{
  test_api_write_string__specify_start_end(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_write_string__specify_invalid_start_end__return_ERROR)
{
  test_api_write_string__specify_invalid_start_end__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_write_string__specify_invalid_start_end__return_ERROR)
{
  test_api_write_string__specify_invalid_start_end__return_ERROR(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_flush_output_port)
{
  test_api_flush_output_port(file_port, FILEPORT);
}

TEST(api_output, string_port__api_flush_output_port)
{
  test_api_flush_output_port(string_port, STRINGPORT);
}

TEST(api_output, file_port__api_flush_output_port__specify_closed_port__return_ERROR)
{
  test_api_flush_output_port__specify_closed_port__return_ERROR(file_port, FILEPORT);
}

TEST(api_output, string_port__api_flush_output_port__specify_closed_port__return_ERROR)
{
  test_api_flush_output_port__specify_closed_port__return_ERROR(string_port, STRINGPORT);
}
