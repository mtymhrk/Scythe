#include <stdio.h>

#include "api.h"

#include "test.h"

TEST_GROUP(api_ports);

#define TEST_FILE_PATH "test_api_ports_test_file"
#define TEST_FILE_CONTENTS "hello, world\n"

#define TEST_INEXISTENT_FILE_PATH "inexistent_file"

static ScmEvaluator *ev;

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
  remove(TEST_INEXISTENT_FILE_PATH);
}

TEST_SETUP(api_ports)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);

  make_test_file();
}

TEST_TEAR_DOWN(api_ports)
{
  delete_test_file();

  scm_capi_evaluator_end(ev);
}

TEST(api_ports, capi_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_port_p(port));
}

TEST(api_ports, capi_port_p__return_false)
{
  TEST_ASSERT_FALSE(scm_capi_port_p(SCM_TRUE_OBJ));
}

TEST(api_ports, api_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_port_P(port));
}

TEST(api_ports, api_port_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_api_port_P(SCM_TRUE_OBJ));
}

TEST(api_ports, capi_input_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_input_port_p(port));
}

TEST(api_ports, capi_input_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_capi_input_port_p(port));
}

TEST(api_ports, api_input_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_P(port));
}

TEST(api_ports, api_input_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_api_input_port_P(port));
}

TEST(api_ports, capi_output_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_output_port_p(port));
}

TEST(api_ports, capi_output_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_capi_output_port_p(port));
}

TEST(api_ports, api_output_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_P(port));
}

TEST(api_ports, api_output_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_api_output_port_P(port));
}

TEST(api_ports, capi_textual_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_textual_port_p(port));
}

IGNORE_TEST(api_ports, capi_textual_port_p__return_false)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_capi_textual_port_p(port));
}

TEST(api_ports, api_textual_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_textual_port_P(port));
}

IGNORE_TEST(api_ports, api_textual_port_P__return_false)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_api_textual_port_P(port));
}

IGNORE_TEST(api_ports, capi_binary_port_p__return_true)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_binary_port_p(port));
}

TEST(api_ports, capi_binary_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_capi_binary_port_p(port));
}

IGNORE_TEST(api_ports, api_binary_port_P__return_true)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_binary_port_P(port));
}

TEST(api_ports, api_binary_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_api_binary_port_P(port));
}

TEST(api_ports, capi_input_port_open_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_input_port_open_p(port));
}

TEST(api_ports, capi_input_port_open_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);
  scm_api_close_port(port);

  TEST_ASSERT_FALSE(scm_capi_input_port_open_p(port));
}

TEST(api_ports, api_input_port_open_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_open_P(port));
}

TEST(api_ports, api_input_port_open_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);
  scm_api_close_port(port);

  TEST_ASSERT_SCM_FALSE(scm_api_input_port_open_P(port));
}

TEST(api_ports, capi_output_port_open_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_capi_output_port_open_p(port));
}

TEST(api_ports, capi_output_port_open_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);
  scm_api_close_port(port);

  TEST_ASSERT_FALSE(scm_capi_output_port_open_p(port));
}

TEST(api_ports, api_output_port_open_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_open_P(port));
}

TEST(api_ports, api_output_port_open_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);
  scm_api_close_port(port);

  TEST_ASSERT_SCM_FALSE(scm_api_output_port_open_P(port));
}

IGNORE_TEST(api_ports, capi_open_input_fd)
{
}

IGNORE_TEST(api_ports, capi_oepn_output_fd)
{
}

TEST(api_ports, capi_open_input_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_open_P(port));
}

TEST(api_ports, capi_open_input_file__specify_inexistent_file__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_capi_open_input_file(TEST_INEXISTENT_FILE_PATH,
                                                NULL));
}

TEST(api_ports, api_open_input_file)
{
  ScmObj port = SCM_OBJ_INIT, path = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &path);

  path = scm_capi_make_string_from_cstr(TEST_FILE_PATH, SCM_ENC_SRC);
  port = scm_api_open_input_file(path);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_open_P(port));
}

TEST(api_ports, api_open_input_file__specify_inexistent_file__return_ERROR)
{
  ScmObj path = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&path);

  path = scm_capi_make_string_from_cstr(TEST_INEXISTENT_FILE_PATH, SCM_ENC_SRC);

  TEST_ASSERT_SCM_NULL(scm_api_open_input_file(path));
}

TEST(api_ports, capi_open_output_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_open_P(port));
}

TEST(api_ports, capi_open_output_file__specify_inexistent_file__create_new_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_INEXISTENT_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_open_P(port));
}

TEST(api_ports, api_open_output_file)
{
  ScmObj port = SCM_OBJ_INIT, path = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &path);

  path = scm_capi_make_string_from_cstr(TEST_FILE_PATH, SCM_ENC_SRC);
  port = scm_api_open_output_file(path);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_open_P(port));
}

TEST(api_ports, api_open_output_file__specify_inexistent_file__create_new_file)
{
  ScmObj port = SCM_OBJ_INIT, path = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &path);

  path = scm_capi_make_string_from_cstr(TEST_INEXISTENT_FILE_PATH, SCM_ENC_SRC);
  port = scm_api_open_output_file(path);

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_open_P(port));
}

TEST(api_ports, api_close_port__close_input_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_close_port(port));
  TEST_ASSERT_SCM_FALSE(scm_api_input_port_open_P(port));
}

TEST(api_ports, api_close_port__close_output_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_close_port(port));
  TEST_ASSERT_SCM_FALSE(scm_api_output_port_open_P(port));
}

TEST(api_ports, api_close_input_port__close_input_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_close_input_port(port));
  TEST_ASSERT_SCM_FALSE(scm_api_input_port_open_P(port));
}

TEST(api_ports, api_close_input_port__close_output_port__return_ERROR)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_NULL(scm_api_close_input_port(port));
}

TEST(api_ports, api_close_output_port__close_input_port__return_ERROR)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_NULL(scm_api_close_output_port(port));
}

TEST(api_ports, api_close_output_port__close_output_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_EQ(SCM_UNDEF_OBJ, scm_api_close_output_port(port));
  TEST_ASSERT_SCM_FALSE(scm_api_output_port_open_P(port));
}

TEST(api_ports, capi_open_input_string_cstr)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_string_cstr("foo", SCM_ENC_NAME_SRC);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_api_textual_port_P(port));
}

TEST(api_ports, api_open_input_string)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &str);

  str = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  port = scm_api_open_input_string(str);

  TEST_ASSERT_SCM_TRUE(scm_api_input_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_api_textual_port_P(port));
}

TEST(api_ports, api_open_output_string)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_api_open_output_string();

  TEST_ASSERT_SCM_TRUE(scm_api_output_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_api_textual_port_P(port));
}

TEST(api_ports, api_get_output_string)
{
  ScmObj port = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &expected, &actual);

  expected = scm_capi_make_string_from_cstr("hello, world", SCM_ENC_SRC);
  actual = scm_capi_make_string_from_cstr("hello, world", SCM_ENC_SRC);

  port = scm_api_open_output_string();

  scm_api_write_string(expected, port, SCM_OBJ_NULL, SCM_OBJ_NULL);

  actual = scm_api_get_output_string(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(api_ports, api_get_output_string__specify_port_dose_not_created_with_open_output_string__return_ERROR)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &str);

  str = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  port = scm_api_open_input_string(str);

  TEST_ASSERT_SCM_NULL(scm_api_get_output_string(port));
}
