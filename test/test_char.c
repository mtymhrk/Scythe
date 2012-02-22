#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "api.h"
#include "miscobjects.h"
#include "char.h"

static ScmObj vm = SCM_OBJ_INIT;

void
cut_startup(void)
{
  vm = scm_vm_new();
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}

void
test_scm_char_new(void)
{
  ScmObj chr = SCM_OBJ_INIT;
  scm_char_t c;

  SCM_CHR_SET_ASCII(c, 'a');
  chr = scm_char_new(SCM_CAPI_MEM_HEAP, c, SCM_ENCODING_ASCII);

  cut_assert_true(scm_obj_not_null_p(chr));
  cut_assert(scm_obj_type_p(SCM_OBJ(chr), &SCM_CHAR_TYPE_INFO));
}

void
test_scm_char_value_a(void)
{
  ScmObj chr = SCM_OBJ_INIT;
  scm_char_t c;

  SCM_CHR_SET_ASCII(c, 'a');
  chr = scm_char_new(SCM_CAPI_MEM_HEAP, c, SCM_ENCODING_ASCII);

  cut_assert_true(scm_obj_not_null_p(chr));
  cut_assert_equal_int('a',
                       scm_char_value(chr).ascii);

}
