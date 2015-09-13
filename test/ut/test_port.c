#include "port.c"

#include <stdio.h>

#include "scythe/object.h"
#include "scythe/refstk.h"
#include "scythe/string.h"
#include "scythe/port.h"

#include "test.h"

TEST_GROUP(port);

#define TEST_FILE_PATH "test_port_test_file"
#define TEST_FILE_CONTENTS "hello, world\n"

#define TEST_INEXISTENT_FILE_PATH "inexistent_file"

static ScmScythe *scy;
static ScmRefStackInfo rsi;

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

TEST_SETUP(port)
{
  scy = ut_scythe_setup(false);
  scm_ref_stack_save(&rsi);

  make_test_file();
}

TEST_TEAR_DOWN(port)
{
  delete_test_file();

  scm_ref_stack_restore(&rsi);
  ut_scythe_tear_down(scy);
}

TEST(port, port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_port_p(port));
}

TEST(port, port_p__return_false)
{
  TEST_ASSERT_FALSE(scm_port_p(SCM_TRUE_OBJ));
}

TEST(port, port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_port_P(port));
}

TEST(port, port_P__return_false)
{
  TEST_ASSERT_SCM_FALSE(scm_port_P(SCM_TRUE_OBJ));
}

TEST(port, input_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_input_port_p(port));
}

TEST(port, input_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_input_port_p(port));
}

TEST(port, input_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_input_port_P(port));
}

TEST(port, input_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_input_port_P(port));
}

TEST(port, output_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_output_port_p(port));
}

TEST(port, output_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_output_port_p(port));
}

TEST(port, output_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_output_port_P(port));
}

TEST(port, output_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_output_port_P(port));
}

TEST(port, textual_port_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_textual_port_p(port));
}

IGNORE_TEST(port, textual_port_p__return_false)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_textual_port_p(port));
}

TEST(port, textual_port_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_textual_port_P(port));
}

IGNORE_TEST(port, textual_port_P__return_false)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_textual_port_P(port));
}

IGNORE_TEST(port, binary_port_p__return_true)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_binary_port_p(port));
}

TEST(port, binary_port_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_FALSE(scm_binary_port_p(port));
}

IGNORE_TEST(port, binary_port_P__return_true)
/* binary port が未実装 */
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_binary_port_P(port));
}

TEST(port, binary_port_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_FALSE(scm_binary_port_P(port));
}

TEST(port, input_port_open_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_input_port_open_p(port));
}

TEST(port, input_port_open_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);
  scm_close_port(port);

  TEST_ASSERT_FALSE(scm_input_port_open_p(port));
}

TEST(port, input_port_open_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_input_port_open_P(port));
}

TEST(port, input_port_open_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);
  scm_close_port(port);

  TEST_ASSERT_SCM_FALSE(scm_input_port_open_P(port));
}

TEST(port, output_port_open_p__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_TRUE(scm_output_port_open_p(port));
}

TEST(port, output_port_open_p__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);
  scm_close_port(port);

  TEST_ASSERT_FALSE(scm_output_port_open_p(port));
}

TEST(port, output_port_open_P__return_true)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_output_port_open_P(port));
}

TEST(port, output_port_open_P__return_false)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);
  scm_close_port(port);

  TEST_ASSERT_SCM_FALSE(scm_output_port_open_P(port));
}

IGNORE_TEST(port, open_input_fd)
{
}

IGNORE_TEST(port, oepn_output_fd)
{
}

TEST(port, open_input_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_input_port_open_P(port));
}

TEST(port, open_input_file__specify_inexistent_file__return_ERROR)
{
  TEST_ASSERT_SCM_NULL(scm_open_input_file(TEST_INEXISTENT_FILE_PATH,
                                                NULL));
}

TEST(port, open_output_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_output_port_open_P(port));
}

TEST(port, open_output_file__specify_inexistent_file__create_new_file)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_INEXISTENT_FILE_PATH, NULL);

  TEST_ASSERT_SCM_TRUE(scm_output_port_open_P(port));
}

TEST(port, close_port__close_input_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_EQUAL_INT(0, scm_close_port(port));
  TEST_ASSERT_SCM_FALSE(scm_input_port_open_P(port));
}

TEST(port, close_port__close_output_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_EQUAL_INT(0, scm_close_port(port));
  TEST_ASSERT_SCM_FALSE(scm_output_port_open_P(port));
}

TEST(port, close_input_port__close_input_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_EQUAL_INT(0, scm_close_input_port(port));
  TEST_ASSERT_SCM_FALSE(scm_input_port_open_P(port));
}

TEST(port, close_output_port__close_output_port)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_file(TEST_FILE_PATH, NULL);

  TEST_ASSERT_EQUAL_INT(0, scm_close_output_port(port));
  TEST_ASSERT_SCM_FALSE(scm_output_port_open_P(port));
}

TEST(port, open_input_string_cstr)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_input_string_cstr("foo", SCM_ENC_NAME_SRC);

  TEST_ASSERT_SCM_TRUE(scm_input_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_textual_port_P(port));
}

TEST(port, open_input_string)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &str);

  str = scm_make_string_from_cstr("foo", SCM_ENC_SRC);
  port = scm_open_input_string(str);

  TEST_ASSERT_SCM_TRUE(scm_input_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_textual_port_P(port));
}

TEST(port, open_output_string)
{
  ScmObj port = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_open_output_string();

  TEST_ASSERT_SCM_TRUE(scm_output_port_P(port));
  TEST_ASSERT_SCM_TRUE(scm_textual_port_P(port));
}

TEST(port, get_output_string)
{
  ScmObj port = SCM_OBJ_INIT, expected = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &expected, &actual);

  expected = scm_make_string_from_cstr("hello, world", SCM_ENC_SRC);
  actual = scm_make_string_from_cstr("hello, world", SCM_ENC_SRC);

  port = scm_open_output_string();

  scm_write_string(expected, port, -1, -1);

  actual = scm_get_output_string(port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}
