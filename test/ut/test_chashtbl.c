#include "../../src/chashtbl.c"

#include "string.h"

#include "test.h"

TEST_GROUP(chashtbl);

static ScmEvaluator *ev;

static size_t
hash_func(ScmCHashTblKey key)
{
  return scm_string_hash_value(SCM_OBJ(key));
}

static bool
cmp_func(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_string_is_equal(SCM_OBJ(key1), SCM_OBJ(key2));
}

TEST_SETUP(chashtbl)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
}

TEST_TEAR_DOWN(chashtbl)
{
  scm_capi_evaluator_end(ev);
}

TEST(chashtbl, chash_tbl_new)
{
  ScmCHashTbl *tbl;

  /* action */
  tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, cmp_func);

  /* postcondition check */
  TEST_ASSERT_EQUAL_INT(256, tbl->tbl_size);
  TEST_ASSERT_EQUAL_INT(SCM_CHASH_TBL_SCMOBJ, tbl->key_kind);
  TEST_ASSERT_EQUAL_INT(SCM_CHASH_TBL_SCMOBJ, tbl->val_kind);
  TEST_ASSERT_EQUAL_PTR(hash_func, tbl->hash_func);
  TEST_ASSERT_EQUAL_PTR(cmp_func, tbl->cmp_func);
}

TEST(chashtbl, scm_chash_tbl_insert__insert_new_entry)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2);

  /* preprocess */
  tbl =  scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);

  /* action */
  /* 未登録の key/value の insert は成功し 0 を返す */
  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* precondition check  */
  /* 登録済みの key の get は成功し true を返し、登録されている val を第 3 引
     数に設定 */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_get(tbl, key1,
                                         (ScmCHashTblVal *)SCM_CSETTER_L(val2),
                                         &found));
  TEST_ASSERT_EQUAL_INT(true, found);
  TEST_ASSERT(scm_obj_same_instance_p(val1, val2));
}

TEST(chashtbl, scm_chash_tbl_insert__insert_an_entry_already_registered)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2, &val3);

  /* preprocess */
  tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);
  val2 = SCM_NIL_OBJ;

  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 登録済みの key の insert は失敗し -1 を返す */
  TEST_ASSERT_EQUAL_INT(-1, scm_chash_tbl_insert(tbl, key1, val2));

  /* precondition check  */
  /* 失敗した insert では登録されている値を変更しない */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_get(tbl, key1,
                                         (ScmCHashTblVal *)SCM_CSETTER_L(val3),
                                         &found));
  TEST_ASSERT_EQUAL_INT(true, found);
  TEST_ASSERT(scm_obj_same_instance_p(val1, val3));
}

TEST(chashtbl, scm_chash_tbl_update__update_new_entry)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2);

  /* preprocess */
  tbl =  scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);

  /* action */
  /* 未登録の key/value の update は成功し 0 を返す */
  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_update(tbl, key1, val1));

  /* precondition check  */
  /* 登録済みの key の get は成功し true を返し、登録されている val を第 3 引
     数に設定 */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_get(tbl, key1,
                                         (ScmCHashTblVal *)SCM_CSETTER_L(val2),
                                         &found));
  TEST_ASSERT_EQUAL_INT(true, found);
  TEST_ASSERT(scm_obj_same_instance_p(val1, val2));
}

TEST(chashtbl, scm_chash_tbl_update__update_an_entry_already_registered)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2, &val3);

  /* preprocess */
  tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);
  val2 = scm_capi_make_symbol_from_cstr("bar", SCM_ENC_SRC);

  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 登録済みの key の update は成功し 0 を返す */
  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_update(tbl, key1, val2));

  /* precondition check  */
  /* 成功した update では登録されている値を変更する */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_get(tbl, key1,
                                         (ScmCHashTblVal *)SCM_CSETTER_L(val3),
                                         &found));
  TEST_ASSERT_EQUAL_INT(true, found);
  TEST_ASSERT(scm_obj_same_instance_p(val2, val3));
}

TEST(chashtbl, scm_chash_tbl_delete__delete_an_entry_not_registered)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  bool deleted;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2);

  /* preprocess */
  tbl = scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                          SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                          hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);

  /* action */
  /* 未登録の key/value の delete は 0 を返す */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_delete(tbl, key1,
                                            (ScmCHashTblVal *)SCM_CSETTER_L(val1),
                                            &deleted));

  /* precondition check  */
  /* 未登録の key/value の delete は deleted に false を設定する */
  TEST_ASSERT_EQUAL_INT(false, deleted);
}

TEST(chashtbl, scm_chash_tbl_delete__delete_an_entry_already_registered)
{
  ScmCHashTbl *tbl;
  ScmObj key1 = SCM_OBJ_INIT;
  ScmObj val1 = SCM_OBJ_INIT;
  ScmObj val2 = SCM_OBJ_INIT;
  ScmObj val3 = SCM_OBJ_INIT;
  bool found, deleted;

  SCM_STACK_FRAME_PUSH(&key1, &val1, &val2, &val3);

  /* preprocess */
  tbl =  scm_chash_tbl_new(SCM_OBJ_NULL, 256,
                           SCM_CHASH_TBL_SCMOBJ, SCM_CHASH_TBL_SCMOBJ,
                           hash_func, scm_chash_tbl_cmp_func_eq);

  key1 = scm_capi_make_string_from_cstr("foo", SCM_ENC_SRC);
  val1 = scm_api_string_to_symbol(key1);

  TEST_ASSERT_EQUAL_INT(0, scm_chash_tbl_insert(tbl, key1, val1));

  /* action */
  /* 未登録の key/value の insert は成功し 0 を返す */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_delete(tbl, key1,
                                            (ScmCHashTblVal *)SCM_CSETTER_L(val2),
                                            &deleted));

  /* precondition check  */
  /* 登録済み key の delete は deleted に true を設定し、val に削除された値を
     設定する */
  TEST_ASSERT_EQUAL_INT(true, deleted);
  TEST_ASSERT(scm_obj_same_instance_p(val1, val2));

  /* 削除後は get による取得はできないため、found に false を返す */
  TEST_ASSERT_EQUAL_INT(0,
                       scm_chash_tbl_get(tbl, key1,
                                         (ScmCHashTblVal *)SCM_CSETTER_L(val3),
                                         &found));
  TEST_ASSERT_EQUAL_INT(false, found);
}

