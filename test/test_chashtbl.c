#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "symbol.h"
#include "chashtbl.h"

static ScmObj vm = SCM_OBJ_INIT;

size_t
hash_func(ScmCHashTblKey key)
{
  return scm_symbol_hash_value(SCM_OBJ(key));
}

void
cut_startup(void)
{
  SCM_SETQ(vm, scm_vm_new());
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}


void
test_scm_chash_tbl_new(void)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  /* action */
  SCM_SETQ(tbl, scm_chash_tbl_new(SCM_MEM_ALLOC_ROOT, 256,
                                  SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                                  hash_func, scm_chash_tbl_cmp_func_eq));

  /* postcondition check */
  cut_assert_equal_size(256, SCM_CHASH_TBL(tbl)->tbl_size);
  cut_assert_equal_int(SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL(tbl)->key_kind);
  cut_assert_equal_int(SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL(tbl)->val_kind);
  cut_assert_equal_pointer(hash_func, SCM_CHASH_TBL(tbl)->hash_func);
  cut_assert_equal_pointer(scm_chash_tbl_cmp_func_eq, SCM_CHASH_TBL(tbl)->cmp_func);
}
