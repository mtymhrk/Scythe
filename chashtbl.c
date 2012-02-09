#include "object.h"
#include "memory.h"
#include "reference.h"
#include "chashtbl.h"

enum { ADD, UPDATE, DELETE, FIND };

void
scm_chash_tbl_initialize(ScmCHashTbl *tbl, size_t size,
                         SCM_CHASH_TBL_VAL_KIND_T key_kind,
                         SCM_CHASH_TBL_VAL_KIND_T val_kind,
                         ScmCHashFunc hash_func,
                         ScmCHashTblKeyCmpFunc cmp_func) /* GC OK */
{
  scm_assert(tbl != NULL);
  scm_assert(hash_func != NULL);
  scm_assert(cmp_func != NULL);
  scm_assert(SIZE_MAX / size > sizeof(ScmCHashTblEntry *));

  tbl->buckets = scm_memory_allocate(sizeof(ScmCHashTblEntry *) * size);
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
scm_chash_tbl_finalize(ScmCHashTbl *tbl) /* GC OK */
{
  scm_assert(tbl != NULL);

  if (tbl->buckets != NULL) {
    for (size_t i = 0; i < tbl->tbl_size; i++) {
      ScmCHashTblEntry *e = tbl->buckets[i];
      while (e != NULL) {
        ScmCHashTblEntry *n = e->next;
        scm_memory_release(e);
        e = n;
      }
    }
  }

  scm_memory_release(tbl->buckets);
  tbl->buckets = NULL;
}

ScmCHashTbl *
scm_chash_tbl_new(size_t size,
                  SCM_CHASH_TBL_VAL_KIND_T key_kind,
                  SCM_CHASH_TBL_VAL_KIND_T val_kind,
                  ScmCHashFunc hash_func,
                  ScmCHashTblKeyCmpFunc cmp_func)
{
  ScmCHashTbl *tbl;

  tbl = scm_memory_allocate(sizeof(*tbl));
  if (tbl == NULL) return NULL;

  scm_chash_tbl_initialize(tbl, size, key_kind, val_kind, hash_func, cmp_func);

  return tbl;
}

void
scm_chash_tbl_end(ScmCHashTbl *tbl)
{
  scm_assert(tbl != NULL);

  scm_chash_tbl_finalize(tbl);
  scm_memory_release(tbl);
}

bool
scm_chash_tbl_cmp_func_eq(ScmCHashTblKey key1, ScmCHashTblKey key2) /* GC OK */
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
                     ScmCHashTblKey key, ScmCHashTblVal val, int mode) /* GC OK */
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
    ScmCHashTblEntry *new = scm_memory_allocate(sizeof(ScmCHashTblEntry));

    if (tbl->key_kind == SCM_CHASH_TBL_CVAL)
      new->key = key;
    else
      SCM_SETQ(new->key, SCM_OBJ(key));

    if (tbl->val_kind == SCM_CHASH_TBL_CVAL)
      new->val = val;
    else
      SCM_SETQ(new->val, SCM_OBJ(val));

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
                  ScmCHashTblVal *val, bool *found) /* GC OK */
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
        SCM_REF_SETQ(val, e->val);
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
                     ScmCHashTblKey *val, bool *deleted) /* GC OK */
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
        SCM_REF_SETQ(val, e->val);
    }

    scm_memory_release(e);

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
scm_chash_tbl_insert(ScmCHashTbl *tbl, ScmCHashTblKey key, ScmCHashTblVal val) /* GC OK */
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
scm_chash_tbl_update(ScmCHashTbl *tbl, ScmCHashTblKey key, ScmCHashTblVal val) /* GC OK */
{
  ScmCHashTblEntry *e;

  scm_assert(tbl != NULL);
  scm_assert(tbl->key_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(key));
  scm_assert(tbl->val_kind == SCM_CHASH_TBL_CVAL || scm_obj_not_null_p(val));

  e = scm_chash_tbl_access(tbl, key, val, UPDATE);
  return (e != NULL) ? 0 : -1;
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
    for (ScmCHashTblEntry *entry = tbl->buckets[i];
         entry != NULL;
         entry = entry->next) {
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

      if (delete_p) {
        *prev = entry->next;
        scm_memory_release(entry);
      }
      else
        prev = &entry->next;
    }
  }

  return rslt;
}
