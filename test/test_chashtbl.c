#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "string.h"
#include "symbol.h"
#include "chashtbl.h"

static ScmObj vm = SCM_OBJ_INIT;

size_t
hash_func(ScmCHashTblKey key)
{
  return scm_string_hash_value(SCM_OBJ(key));
}

bool
cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_string_is_equal(SCM_OBJ(key1), SCM_OBJ(key2));
}

void
cut_startup(void)
{
  SCM_SETQ(vm, scm_vm_new());
  scm_mem_disable_current_mem_gc();
}

void
cut_shutdown(void)
{
  scm_vm_end(vm);
}


void
test_scm_chash_tbl_new(void)
{
  ScmCHashTbl *tbl;

  /* action */
  tbl = scm_chash_tbl_new(256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, cmp_func);

  /* postcondition check */
  cut_assert_equal_size(256, tbl->tbl_size);
  cut_assert_equal_int(SCM_CHASH_TBL_SCMOBJ, tbl->key_kind);
  cut_assert_equal_int(SCM_CHASH_TBL_SCMOBJ, tbl->val_kind);
  cut_assert_equal_pointer(hash_func, tbl->hash_func);
  cut_assert_equal_pointer(cmp_func, tbl->cmp_func);
}

void
test_scm_chash_tbl_insert__insert_new_entry(void)
/* scm_chash_tbl_insert で未登録の key/value を登録は成功する */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2);

  /* preprocess */
  tbl =  scm_chash_tbl_new(256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));

  /* action */
  /* 未登録の key/value の insert は成功し 0 を返す */
  cut_assert_equal_int(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* precondition check  */
  /* 登録済みの key の get は成功し true を返し、登録されている val を第 3 引
     数に設定 */
  cut_assert_equal_int(0, scm_chash_tbl_get(tbl, key1, &val2, &found));
  cut_assert_equal_boolean(true, found);
  cut_assert(scm_obj_same_instance_p(val1, val2));
}

void
test_scm_chash_tbl_insert__insert_an_entry_already_registered(void)
/* scm_chash_tbl_insert での登録済みの key の value 更新は失敗する */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&tbl, &key1, &val1, &val2, &val3);

  /* preprocess */
  tbl = scm_chash_tbl_new(256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));
  SCM_SETQ(val2, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "bar", sizeof("bar"), SCM_ENCODING_ASCII));

  cut_assert_equal_int(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 登録済みの key の insert は失敗し -1 を返す */
  cut_assert_equal_int(-1, scm_chash_tbl_insert(tbl, key1, val2));

  /* precondition check  */
  /* 失敗した insert では登録されている値を変更しない */
  cut_assert_equal_int(0, scm_chash_tbl_get(tbl, key1, &val3, &found));
  cut_assert_equal_boolean(true, found);
  cut_assert(scm_obj_same_instance_p(val1, val3));
}

void
test_scm_chash_tbl_update__update_new_entry(void)
/* scm_chash_tbl_update での未登録の key/value の登録は成功する */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&tbl, &key1, &val1, &val2);

  /* preprocess */
  tbl =  scm_chash_tbl_new(256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));

  /* action */
  /* 未登録の key/value の update は成功し 0 を返す */
  cut_assert_equal_int(0, scm_chash_tbl_update(tbl, key1, val1));

  /* precondition check  */
  /* 登録済みの key の get は成功し true を返し、登録されている val を第 3 引
     数に設定 */
  cut_assert_equal_int(0, scm_chash_tbl_get(tbl, key1, &val2, &found));
  cut_assert_equal_boolean(true, found);
  cut_assert(scm_obj_same_instance_p(val1, val2));
}

void
test_scm_chash_tbl_update__update_an_entry_already_registered(void)
/* scm_chash_tbl_update での登録済みの key の value 更新は成功する */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&tbl, &key1, &val1, &val2, &val3);

  /* preprocess */
  tbl = scm_chash_tbl_new(256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));
  SCM_SETQ(val2, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "bar", sizeof("bar"), SCM_ENCODING_ASCII));

  cut_assert_equal_int(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 登録済みの key の update は成功し 0 を返す */
  cut_assert_equal_int(0, scm_chash_tbl_update(tbl, key1, val2));

  /* precondition check  */
  /* 成功した update では登録されている値を変更する */
  cut_assert_equal_int(0, scm_chash_tbl_get(tbl, key1, &val3, &found));
  cut_assert_equal_boolean(true, found);
  cut_assert(scm_obj_same_instance_p(val2, val3));
}

void
test_scm_chash_tbl_delete__delete_an_entry_not_registered(void)
/* scm_chash_tbl_delete での未登録 key の削除は第 4 引数で false を返す */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool deleted;

  SCM_STACK_FRAME_PUSH(&tbl, &key1, &val1, &val2);

  /* preprocess */
  tbl = scm_chash_tbl_new(256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));

  /* action */
  /* 未登録の key/value の delete は 0 を返す */
  cut_assert_equal_int(0, scm_chash_tbl_delete(tbl, key1, &val1, &deleted));

  /* precondition check  */
  /* 未登録の key/value の delete は deleted に false を設定する */
  cut_assert_equal_boolean(false, deleted);
}

void
test_scm_chash_tbl_delete__delete_an_entry_already_registered(void)
/* scm_chash_tbl_delete での登録済み key の削除は第 4 引数で true を返し、
 * 第 3 引数で削除された値を返す */
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found, deleted;

  SCM_STACK_FRAME_PUSH(&tbl, &key1, &val1, &val2, &val3);

  /* preprocess */
  tbl =  scm_chash_tbl_new(256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  SCM_SETQ(key1, scm_string_new(SCM_MEM_ALLOC_HEAP,
                                "foo", sizeof("foo"), SCM_ENCODING_ASCII));
  SCM_SETQ(val1, scm_symbol_new(SCM_MEM_ALLOC_HEAP, key1));

  cut_assert_equal_int(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 未登録の key/value の insert は成功し 0 を返す */
  cut_assert_equal_int(0, scm_chash_tbl_delete(tbl, key1, &val2, &deleted));

  /* precondition check  */
  /* 登録済み key の delete は deleted に true を設定し、val に削除された値を
     設定する */
  cut_assert_equal_boolean(true, deleted);
  cut_assert(scm_obj_same_instance_p(val1, val2));

  /* 削除後は get による取得はできないため、found に false を返す */
  cut_assert_equal_int(0, scm_chash_tbl_get(tbl, key1, &val3, &found));
  cut_assert_equal_boolean(false, found);
}

