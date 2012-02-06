#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "symbol.h"
#include "pair.h"


/** predicate ******************************************************/

static inline ScmObj
scm_api_eq_p(ScmObj obj1, ScmObj obj2)
{
  return (SCM_OBJ_IS_SAME_INSTANCE(obj1, obj2) ?
          scm_vm_bool_true_instance() : scm_vm_bool_false_instance());
}

ScmObj scm_api_eqv_p(ScmObj obj1, ScmObj obj2); /* TODO: write me */
ScmObj scm_api_equal_p(ScmObj obj1, ScmObj obj2); /* TODO: write me */


/** Symbol *********************************************************/

static inline ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if (!SCM_OBJ_IS_TYPE(sym, &SCM_SYMBOL_TYPE_INFO))
    /* TODO: ランタイムエラーをどう処理するか。*/
    return SCM_OBJ_NULL;        /* 仮実装 */

  return SCM_SYMBOL_STR(sym);
}

static inline ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if (SCM_OBJ_IS_TYPE(str, &SCM_STRING_TYPE_INFO))
    return SCM_OBJ_NULL;         /* 仮実装 */

  return scm_symtbl_symbol(scm_vm_current_symtbl(), str);
}


/** List and Pair  *************************************************/

static inline ScmObj
scm_api_cons(ScmObj car, ScmObj cdr)
{
  if (SCM_OBJ_IS_NULL(car) || SCM_OBJ_IS_NULL(cdr))
    return SCM_OBJ_NULL;         /* 仮実装 */

  return scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr);
}

static inline ScmObj
scm_api_car(ScmObj pair)
{
  if (!SCM_OBJ_IS_TYPE(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* 仮実装 */

  return scm_pair_car(pair);
}

static inline ScmObj
scm_api_cdr(ScmObj pair)
{
  if (!SCM_OBJ_IS_TYPE(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* 仮実装 */

  return scm_pair_cdr(pair);
}



#endif /* INCLUDE_API_H__ */
