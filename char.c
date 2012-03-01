#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "encoding.h"
#include "char.h"


ScmTypeInfo SCM_CHAR_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmChar),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};


int
scm_char_initialize(ScmObj chr, scm_char_t value, SCM_ENC_T enc) /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);
  scm_assert(/* 0 <= enc && */ enc < SCM_ENC_NR_ENC);

  if (!SCM_ENCODING_VFUNC_VALID_P(enc)(value))
    return -1;                  /* [ERR] char: invalid byte sequence */

  SCM_CHAR_VALUE(chr) = value;
  SCM_CHAR_ENC(chr) = enc;

  return 0;
}

void
scm_char_finalize(ScmObj chr)   /* GC OK */
{
  return;                       /* nothing to do */
}

ScmObj
scm_char_new(SCM_MEM_TYPE_T mtype,
             scm_char_t value, SCM_ENC_T enc) /* GC OK */
{
  ScmObj chr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&chr);

  scm_assert(/* 0 <= enc && */ enc < SCM_ENC_NR_ENC);

  chr = scm_capi_mem_alloc(&SCM_CHAR_TYPE_INFO, mtype);
  if (scm_obj_null_p(chr)) return SCM_OBJ_NULL;

  if (scm_char_initialize(chr, value, enc) < 0)
    return SCM_OBJ_NULL;

  return chr;
}

ScmObj
scm_char_new_newline(SCM_MEM_TYPE_T  mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_LF_CHAR(enc), enc);
}

ScmObj
scm_char_new_space(SCM_MEM_TYPE_T mtype, SCM_ENC_T enc) /* GC OK */
{
  return scm_char_new(mtype, SCM_ENCODING_CONST_SPACE_CHAR(enc), enc);
}

scm_char_t
scm_char_value(ScmObj chr)      /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_VALUE(chr);
}

SCM_ENC_T
scm_char_encoding(ScmObj chr)   /* GC OK */
{
  scm_assert_obj_type(chr, &SCM_CHAR_TYPE_INFO);

  return SCM_CHAR_ENC(chr);
}
