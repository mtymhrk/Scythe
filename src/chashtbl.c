#include "object.h"
#include "reference.h"
#include "api.h"
#include "chashtbl.h"

enum { ADD, UPDATE, DELETE, FIND };

void
scm_chash_tbl_initialize(ScmCHashTbl *tbl, ScmObj owner,
                         size_t size,
                         SCM_CHASH_TBL_VAL_KIND_T key_kind,
                         SCM_CHASH_TBL_VAL_KIND_T val_kind,
                         ScmCHashFunc hash_func,
                         ScmCHashTblKeyCmpFunc cmp_func)
{
  scm_assert(tbl != NULL);
  scm_assert(hash_func != NULL);
  scm_assert(cmp_func != NULL);
  scm_assert(size > 0);
  scm_assert(SIZE_MAX / size > sizeof(ScmCHashTblEntry *));

  SCM_WB_SETQ(owner, tbl->owner, owner);

  tbl->buckets = scm_capi_malloc(sizeof(ScmCHashTblEntry *) * size);
  if (tbl->buckets == NULL) return;

  tbl->tbl_size = size;
  tbl->hash_func = hash_func;
  tbl->cmp_func = cmp_func;
  tbl->key_kind = key_kind;
  tbl->val_kind = val_kind;

  for (size_t i = 0; i < tbl->tbl_size; i++)
    tbl->buckets[i] = NULL;
}

void
scm_chash_tbl_finalize(ScmCHashTbl *tbl)
{
  scm_assert(tbl != NULL);

  if (tbl->buckets != NULL) {
    for (size_t i = 0; i < tbl->tbl_size; i++) {
      ScmCHashTblEntry *e = tbl->buckets[i];
      while (e != NULL) {
        ScmCHashTblEntry *n = e->next;
        scm_capi_free(e);
        e = n;
      }
    }
  }

  scm_capi_free(tbl->buckets);
  tbl->buckets = NULL;
  tbl->tbl_size = 0;
  tbl->owner = SCM_OBJ_NULL;
}

ScmCHashTbl *
scm_chash_tbl_new(ScmObj owner, size_t size,
                  SCM_CHASH_TBL_VAL_KIND_T key_kind,
                  SCM_CHASH_TBL_VAL_KIND_T val_kind,
                  ScmCHashFunc hash_func,
                  ScmCHashTblKeyCmpFunc cmp_func)
{
  ScmCHashTbl *tbl;

  tbl = scm_capi_malloc(sizeof(*tbl));
  if (tbl == NULL) return NULL;

  scm_chash_tbl_initialize(tbl, owner, size, key_kind, val_kind,
                           hash_func, cmp_func);

  return tbl;
}

void
scm_chash_tbl_end(ScmCHashTbl *tbl)
{
  scm_assert(tbl != NULL);

  scm_chash_tbl_finalize(tbl);
  scm_capi_free(tbl);
}

bool
scm_chash_tbl_cmp_func_eq(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  return scm_obj_same_instance_p(key1, key2) ? true : false;
}

bool
scm_chash_tbl_cmp_func_eqv(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  /* TODO: write me */
  return true;
}

static ScmCHashTblEntry *
scm_chash_tbl_access(ScmCHashTbl *tbl,
                     ScmCHashTblKey key, ScmCHashTblVal val, int mode)
{
  ScmCHashTblEntry *entry;
  ScmCHashTblEntry **prev;
  size_t hash;

  SCM_STACK_FRAME;

  if (tbl->key_kind != SCM_CHASH_TBL_CVAL) SCM_STACK_PUSH(&key);
  if (tbl->val_kind != SCM_CHASH_TBL_CVAL) SCM_STACK_PUSH(&val);

  hash = tbl->hash_func(key) % tbl->tbl_size;

  for (entry = tbl->buckets[hash], prev = &tbl->buckets[hash];
       entry != NULL;
       entry = entry->next, prev = &entry->next) {
    if (tbl->cmp_func(key, entry->key)) {
      switch (mode) {
      case ADD:
        return NULL;
        break;
      case UPDATE:
        entry->key = key;
        entry->val = val;
        return entry;
        break;
      case DELETE:
        *prev = entry->next;
        return entry;
        break;
      case FIND:
        return entry;
        break;
      default:
        scm_assert(false);
        break;
      }
    }
  }

  if (mode == ADD || mode == UPDATE) {
    ScmCHashTblEntry *new = scm_capi_malloc(sizeof(ScmCHashTblEntry));
    if (new == NULL) return NULL;
    /* XXX: ADD のすでにエントリが存在するケースと区別がつかない */

    if (tbl->key_kind == SCM_CHASH_TBL_CVAL)
      new->key = key;
    else
      SCM_WB_SETQ(tbl->owner, new->key, SCM_OBJ(key));

    if (tbl->val_kind == SCM_CHASH_TBL_CVAL)
      new->val = val;
    else
      SCM_WB_SETQ(tbl->owner, new->val, SCM_OBJ(val));

    new->next = tbl->buckets[hash];
    return (tbl->buckets[hash] = new);
  }
  else
    return NULL;
}

/* key にマッチするエントリの値を val に設定して found に true を設定する。
 * マッチするものがなければ val を不定て found に false を返す。マッチする
 * エントリの有無に関わらず、処理が成功すれば 0 を返す。エラーの場合は -1
 * を返す(今のところその契機はない)。
 */
int
scm_chash_tbl_get(ScmCHashTbl *tbl, ScmCHashTblKey key,
                  ScmCHashTblVal *val, bool *found)
{
  ScmCHashTblEntry *e;

  /* scm_chash_tbl_access の呼出中に GC が走る可能性があるが、呼出後に
     tbl と key を参照しないので、これらを cstack に push する必要はない。
     scm_chash_tbl_access 内部ではハッシュテーブルへの参照を cstack に push
     しているのでハッシュテーブルが GC される心配もない。
  */

  scm_assert(tbl != NULL);
  scm_assert(tbl->key_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(key));

  e = scm_chash_tbl_access(tbl, key, SCM_CHASH_TBL_VAL(0), FIND);
  if (e != NULL) {
    if (val != NULL) {
      if (tbl->val_kind == SCM_CHASH_TBL_CVAL)
        *val = e->val;
      else
        scm_csetter_setq((scm_csetter_t *)val, e->val);
    }

    if (found != NULL) *found = true;
    return 0;
  }

  if (found != NULL) *found = false;
  return 0;
}

/* key にマッチするエントリを削除する。マッチするエントリがあれば戻り値として
 * val に削除した値を設定し、deleted に true を設定する。マッチするエントリが
 * 無い場合は val の値は不定で、deleted に false を設定する。マッチするエント
 * リの有無に関わらず、処理が成功するれば 0 を返す。エラーの場合は -1 を返す
 * (今のところその契機はない)。
 */
int
scm_chash_tbl_delete(ScmCHashTbl *tbl, ScmCHashTblKey key,
                     ScmCHashTblKey *val, bool *deleted)
{
  ScmCHashTblEntry *e;

  /* scm_chash_tbl_access の呼出中に GC が走る可能性があるが、呼出後に
     tbl と key を参照しないので、これらを cstack に push する必要はない。
     scm_chash_tbl_access 内部ではハッシュテーブルへの参照を cstack に push
     しているのでハッシュテーブルが GC される心配もない。
  */

  scm_assert(tbl != NULL);
  scm_assert(tbl->key_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(key));

  e = scm_chash_tbl_access(tbl, key, SCM_CHASH_TBL_VAL(0), DELETE);
  if (e != NULL) {
    if (val != NULL) {
      if (tbl->val_kind == SCM_CHASH_TBL_CVAL)
        *val = e->val;
      else
        scm_csetter_setq((scm_csetter_t *)val, e->val);
    }

    scm_capi_free(e);

    if (deleted != NULL) *deleted = true;
    return 0;
  }

  if (deleted != NULL) *deleted = false;
  return 0;
}

/* key と val を追加する。key にマッチするエントリがある場合はエラーとなり -1
 * を返す。追加が成功した場合は 0 を返す。
 */
int
scm_chash_tbl_insert(ScmCHashTbl *tbl, ScmCHashTblKey key, ScmCHashTblVal val)
{
  ScmCHashTblEntry *e;

  scm_assert(tbl != NULL);
  scm_assert(tbl->key_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(key));
  scm_assert(tbl->val_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(val));

  e = scm_chash_tbl_access(tbl, key, val, ADD);
  return (e != NULL) ? 0 : -1;
}

/* key にマッチするエントリの値を val で更新する。key にマッチするエントリない
 * 場合は追加する。エラーの場合は -1 を返す(今のところ契機は無い)。成功の場合は
 * 0 を返す。
 */
int
scm_chash_tbl_update(ScmCHashTbl *tbl, ScmCHashTblKey key, ScmCHashTblVal val)
{
  ScmCHashTblEntry *e;

  scm_assert(tbl != NULL);
  scm_assert(tbl->key_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(key));
  scm_assert(tbl->val_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(val));

  e = scm_chash_tbl_access(tbl, key, val, UPDATE);
  return (e != NULL) ? 0 : -1;
}

void
scm_chash_tbl_clean(ScmCHashTbl *tbl)
{
  scm_assert(tbl != NULL);

  if (tbl->buckets != NULL) {
    for (size_t i = 0; i < tbl->tbl_size; i++) {
      ScmCHashTblEntry *e = tbl->buckets[i];
      while (e != NULL) {
        ScmCHashTblEntry *n = e->next;
        scm_capi_free(e);
        e = n;
      }
      tbl->buckets[i] = NULL;
    }
  }
}

int
scm_chash_tbl_gc_accept(ScmCHashTbl *tbl, ScmObj owner,
                        ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(tbl != NULL);
  scm_assert(scm_obj_not_null_p(owner));
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (tbl->key_kind != SCM_CHASH_TBL_SCMOBJ
      && tbl->val_kind != SCM_CHASH_TBL_SCMOBJ)
    return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, tbl->owner, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < tbl->tbl_size; i++) {
    for (ScmCHashTblEntry *entry = tbl->buckets[i];
         entry != NULL;
         entry = entry->next) {
      if (tbl->key_kind == SCM_CHASH_TBL_SCMOBJ) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, entry->key, mem);
        if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      }
      if (tbl->val_kind == SCM_CHASH_TBL_SCMOBJ) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, entry->val, mem);
        if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
      }
    }
  }

  return rslt;
}

int
scm_chash_tbl_gc_accept_weak(ScmCHashTbl *tbl, ScmObj owner,
                             ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(tbl != NULL);
  scm_assert(scm_obj_not_null_p(owner));
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (tbl->key_kind != SCM_CHASH_TBL_SCMOBJ_W
      && tbl->val_kind != SCM_CHASH_TBL_SCMOBJ_W)
    return rslt;

  for (size_t i = 0; i < tbl->tbl_size; i++) {
    ScmCHashTblEntry **prev = &tbl->buckets[i];
    for (ScmCHashTblEntry *entry = tbl->buckets[i], *next = NULL;
         entry != NULL;
         entry = next) {
      bool delete_p = false;

      if (tbl->key_kind == SCM_CHASH_TBL_SCMOBJ_W) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, entry->key, mem);
        if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
        if (scm_obj_null_p(entry->key)) delete_p = true;
      }
      if (tbl->val_kind == SCM_CHASH_TBL_SCMOBJ_W) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, owner, entry->val, mem);
        if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
        if (scm_obj_null_p(entry->val)) delete_p = true;
      }

      next = entry->next;
      if (delete_p) {
        *prev = entry->next;
        scm_capi_free(entry);
      }
      else
        prev = &entry->next;
    }
  }

  return rslt;
}
