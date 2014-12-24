#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/miscobjects.h"
#include "scythe/vm.h"

/* Memo:
 *  scm_api_nil() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_NIL_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_fcd_nil(void)
{
  return scm_bedrock_nil(scm_fcd_current_br());
}

extern inline bool
scm_fcd_nil_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, SCM_NIL_OBJ);
}

extern inline ScmObj
scm_fcd_nil_P(ScmObj obj)
{
  return scm_fcd_nil_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}



extern inline bool
scm_fcd_boolean_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BOOL_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_boolean_P(ScmObj obj)
{
  return scm_fcd_boolean_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

/* Memo:
 *  scm_api_true() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_TRUE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_fcd_true(void)
{
  return scm_bedrock_true(scm_fcd_current_br());
}

/* Memo:
 *  scm_api_false() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_FALSE_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_fcd_false(void)
{
  return scm_bedrock_false(scm_fcd_current_br());
}

extern inline bool
scm_fcd_true_object_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, SCM_TRUE_OBJ);
}

extern inline bool
scm_fcd_false_object_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, SCM_FALSE_OBJ);
}

extern inline bool
scm_fcd_true_p(ScmObj obj)
{
  return !scm_fcd_false_object_p(obj);
}

extern inline bool
scm_fcd_false_p(ScmObj obj)
{
  return scm_fcd_false_object_p(obj);
}

extern inline ScmObj
scm_fcd_not(ScmObj obj)
{
  return scm_fcd_false_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}



/* Memo:
 *  scm_api_eof() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_EOF_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_fcd_eof(void)
{
  return scm_bedrock_eof(scm_fcd_current_br());
}

extern inline bool
scm_fcd_eof_object_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, SCM_EOF_OBJ);
}

extern inline ScmObj
scm_fcd_eof_object_P(ScmObj obj)
{
  return scm_fcd_eof_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}


/* Memo:
 *  scm_api_undef() の関数の実行では GC が発生するのは NG。
 *  (マクロ SCM_UNDEF_OBJ を定義して定数的に使うため)
 */
extern inline ScmObj
scm_fcd_undef(void)
{
  return scm_bedrock_undef(scm_fcd_current_br());
}

extern inline bool
scm_fcd_undef_object_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, SCM_UNDEF_OBJ);
}



extern inline bool
scm_fcd_landmine_object_p(ScmObj obj)
{
  return scm_fcd_eq_p(obj, scm_bedrock_landmine(scm_fcd_current_br()));
}
