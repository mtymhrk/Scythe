#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include "object.h"
#include "vm.h"
#include "symbol.h"

static inline ScmObj
scm_api_eq_p(ScmObj obj1, ScmObj obj2)
{
  return (SCM_OBJ_IS_SAME_INSTANCE(obj1, obj2) ?
          scm_vm_bool_true_instance() : scm_vm_bool_false_instance());
}

static inline ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if (!SCM_OBJ_IS_TYPE(sym, &SCM_SYMBOL_TYPE_INFO))
    /* TODO: ランタイムエラーをどう処理するか。*/
    return SCM_OBJ_NULL;        /* 仮実装 */

  return SCM_SYMBOL_STR(sym);
}

ScmObj scm_api_string_to_symbol(ScmObj str);

#endif /* INCLUDE_API_H__ */
