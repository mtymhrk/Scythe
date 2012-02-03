#include "vm.h"
#include "string.h"
#include "symbol.h"
#include "chashtbl.h"
#include "api.h"


ScmObj
scm_api_string_to_symbol(ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_NULL;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  if (SCM_OBJ_IS_TYPE(str, &SCM_STRING_TYPE_INFO))
    goto err;

  return scm_symtbl_symbol(scm_vm_current_symtbl(), str);

 err:
    /* TODO: ランタイムエラーをどう処理するか。
     * 案: VM にエラー状態を保持し、戻り値では undefined オブジェクトのような
     * ものを返す。
     */
  return SCM_OBJ_NULL; /* 仮実装 */
}
