#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "api.h"
#include "miscobjects.h"
#include "char.h"

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
test_scm_char_new(void)
{
  ScmObj chr = SCM_OBJ_INIT;
  scm_char_t c;

  SCM_CHR_SET_ASCII(c, 'a');
  chr = scm_char_new(SCM_MEM_HEAP, c, SCM_ENC_ASCII);

  cut_assert_true(scm_obj_not_null_p(chr));
  cut_assert(scm_obj_type_p(SCM_OBJ(chr), &SCM_CHAR_TYPE_INFO));
}

void
test_scm_char_value_a(void)
{
  ScmObj chr = SCM_OBJ_INIT;
  scm_char_t c;

  SCM_CHR_SET_ASCII(c, 'a');
  chr = scm_char_new(SCM_MEM_HEAP, c, SCM_ENC_ASCII);

  cut_assert_true(scm_obj_not_null_p(chr));
  cut_assert_equal_int('a',
                       scm_char_value(chr).ascii);

}

void
test_scm_char_cmp_1(void)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  scm_char_t c;
  int actual, err;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  SCM_CHR_SET_ASCII(c, 'a');
  chr1 = scm_capi_make_char(c, SCM_ENC_ASCII);

  SCM_CHR_SET_ASCII(c, 'a');
  chr2 = scm_capi_make_char(c, SCM_ENC_ASCII);

  err = scm_char_cmp(chr1, chr2, &actual);

  cut_assert_true(err >= 0);
  cut_assert_equal_int(0, actual);
}

void
test_scm_char_cmp_2(void)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  scm_char_t c;
  int actual, err;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  SCM_CHR_SET_ASCII(c, 'a');
  chr1 = scm_capi_make_char(c, SCM_ENC_ASCII);

  SCM_CHR_SET_ASCII(c, 'b');
  chr2 = scm_capi_make_char(c, SCM_ENC_ASCII);

  err = scm_char_cmp(chr1, chr2, &actual);

  cut_assert_true(err >= 0);
  cut_assert_equal_int(-1, actual);
}

void
test_scm_char_cmp_3(void)
{
  ScmObj chr1 = SCM_OBJ_INIT, chr2 = SCM_OBJ_INIT;
  scm_char_t c;
  int actual, err;

  SCM_STACK_FRAME_PUSH(&chr1, &chr2);

  SCM_CHR_SET_ASCII(c, 'b');
  chr1 = scm_capi_make_char(c, SCM_ENC_ASCII);

  SCM_CHR_SET_ASCII(c, 'a');
  chr2 = scm_capi_make_char(c, SCM_ENC_ASCII);

  err = scm_char_cmp(chr1, chr2, &actual);

  cut_assert_true(err >= 0);
  cut_assert_equal_int(1, actual);
}
