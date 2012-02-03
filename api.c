#include "vm.h"
#include "string.h"
#include "symbol.h"
#include "chashtbl.h"
#include "api.h"


ScmObj
scm_api_string_to_symbol(ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_NULL;
  bool found;
  int rslt;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  if (SCM_OBJ_IS_TYPE(str, &SCM_STRING_TYPE_INFO))
    goto err;

  rslt = scm_chash_tbl_get(scm_vm_current_symtbl(), str, &sym, &found);
  if (rslt != 0) goto err;

  if (found) return sym;

  SCM_SETQ(sym, scm_symbol_new(SCM_MEM_ALLOC_HEAP, str));
  if (SCM_OBJ_IS_NULL(sym)) goto err;

  rslt = scm_chash_tbl_insert(scm_vm_current_symtbl(), str, sym);
  if (rslt != 0) goto err;

  return sym;

 err:
    /* TODO: ランタイムエラーをどう処理するか。
     * 案: VM にエラー状態を保持し、戻り値では undefined オブジェクトのような
     * ものを返す。
     */
  return SCM_OBJ_NULL; /* 仮実装 */
}
