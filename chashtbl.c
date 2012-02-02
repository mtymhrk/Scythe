#include "object.h"
#include "memory.h"
#include "reference.h"
#include "chashtbl.h"

ScmTypeInfo SCM_CHASH_TBL_TYPE_INFO = {
  .pp_func = NULL,
  .obj_size = sizeof(ScmCHashTbl),
  .gc_ini_func = scm_chash_tbl_gc_initialize,
  .gc_fin_func = scm_chash_tbl_gc_finalize,
  .gc_accept_func = scm_chash_tbl_gc_accept,
  .gc_accept_func_weak = scm_chash_tbl_gc_accept_weak
};

enum { ADD, UPDATE, DELETE, FIND };

void
scm_chash_tbl_initialize(ScmObj tbl, size_t size,
                         SCM_CHASH_TBL_VAL_KIND_T key_kind,
                         SCM_CHASH_TBL_VAL_KIND_T val_kind,
                         ScmCHashFunc hash_func,
                         ScmCHashTblKeyCmpFunc cmp_func) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&tbl);

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);
  assert(hash_func != NULL);
  assert(cmp_func != NULL);
  assert(SIZE_MAX / size > sizeof(ScmCHashTblEntry *));

  SCM_CHASH_TBL(tbl)->buckets =
    scm_memory_allocate(sizeof(ScmCHashTblEntry *) * size);
  if (SCM_CHASH_TBL(tbl)->buckets == NULL) return;

  SCM_CHASH_TBL(tbl)->tbl_size = size;
  SCM_CHASH_TBL(tbl)->hash_func = hash_func;
  SCM_CHASH_TBL(tbl)->cmp_func = cmp_func;
  SCM_CHASH_TBL(tbl)->key_kind = key_kind;
  SCM_CHASH_TBL(tbl)->val_kind = val_kind;

  for (size_t i = 0; i < SCM_CHASH_TBL(tbl)->tbl_size; i++)
    SCM_CHASH_TBL(tbl)->buckets[i] = NULL;
}

void
scm_chash_tbl_finalize(ScmObj tbl) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);

  if (SCM_CHASH_TBL(tbl)->buckets != NULL) {
    for (size_t i = 0; i < SCM_CHASH_TBL(tbl)->tbl_size; i++) {
      ScmCHashTblEntry *e = SCM_CHASH_TBL(tbl)->buckets[i];
      while (e != NULL) {
        ScmCHashTblEntry *n = e->next;
        scm_memory_release(e);
        e = n;
      }
    }
  }

  scm_memory_release(SCM_CHASH_TBL(tbl)->buckets);
}

ScmObj
scm_chash_tbl_new(SCM_MEM_ALLOC_TYPE_T mtype, size_t size,
                  SCM_CHASH_TBL_VAL_KIND_T key_kind,
                  SCM_CHASH_TBL_VAL_KIND_T val_kind,
                  ScmCHashFunc hash_func,
                  ScmCHashTblKeyCmpFunc cmp_func)
{
  ScmObj tbl = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&tbl);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_CHASH_TBL_TYPE_INFO, mtype, SCM_REF_MAKE(tbl));

  scm_chash_tbl_initialize(tbl, size, key_kind, val_kind, hash_func, cmp_func);

  return tbl;
}


bool
scm_chash_tbl_cmp_func_eq(ScmCHashTblKey key1, ScmCHashTblKey key2) /* GC OK */
{
  return SCM_OBJ_IS_SAME_INSTANCE(key1, key2) ? true : false;
}

bool
scm_chash_tbl_cmp_func_eqv(ScmCHashTblKey key1, ScmCHashTblKey key2)
{
  /* TODO: write me */
  return true;
}

static ScmCHashTblEntry *
scm_chash_tbl_access(ScmObj tbl,
                     ScmCHashTblKey key, ScmCHashTblVal val, int mode) /* GC OK */
{
  ScmCHashTblEntry *entry;
  ScmCHashTblEntry **prev;
  size_t hash;

  SCM_STACK_FRAME;

  SCM_STACK_PUSH(&tbl);
  if (SCM_CHASH_TBL(tbl)->key_kind != SCM_CHASH_TBL_CVAL) SCM_STACK_PUSH(&key);
  if (SCM_CHASH_TBL(tbl)->val_kind != SCM_CHASH_TBL_CVAL) SCM_STACK_PUSH(&val);

  hash = SCM_CHASH_TBL(tbl)->hash_func(key) % SCM_CHASH_TBL(tbl)->tbl_size;

  for (entry = SCM_CHASH_TBL(tbl)->buckets[hash],
         prev = &SCM_CHASH_TBL(tbl)->buckets[hash];
       entry != NULL;
       entry = entry->next, prev = &entry->next) {
    if (SCM_CHASH_TBL(tbl)->cmp_func(key, entry->key)) {
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
        assert(false);
        break;
      }
    }
  }

  if (mode == ADD || mode == UPDATE) {
    ScmCHashTblEntry *new = scm_memory_allocate(sizeof(ScmCHashTblEntry));
    new->key = key;
    new->val = val;
    new->next = SCM_CHASH_TBL(tbl)->buckets[hash];
    return (SCM_CHASH_TBL(tbl)->buckets[hash] = new);
  }
  else
    return NULL;
}

/* key にマッチするエントリの値を val に設定する。マッチするものがなければ戻り
 * 値として false を返す。マッチするものばある場合は true を返す。
 */
bool
scm_chash_tbl_get(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal *val) /* GC OK */
{
  ScmCHashTblEntry *e;
  SCM_CHASH_TBL_VAL_KIND_T val_kind;

  /* scm_chash_tbl_access の呼出中に GC が走る可能性があるが、呼出後に
     tbl と key を参照しないので、これらを cstack に push する必要はない。
     scm_chash_tbl_access 内部ではハッシュテーブルへの参照を cstack に push
     しているのでハッシュテーブルが GC される心配もない。
  */

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_CHASH_TBL(tbl)->key_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(key));

  val_kind = SCM_CHASH_TBL(tbl)->val_kind;

  e = scm_chash_tbl_access(tbl, key, SCM_CHASH_TBL_VAL(0), FIND);
  if (e != NULL) {
    if (val != NULL) {
      if (val_kind == SCM_CHASH_TBL_CVAL)
        *val = e->val;
      else
        SCM_REF_SETQ(val, e->val);
    }

    return true;
  }

  return false;
}

/* key にマッチするエントリを削除する。マッチするエントリがあれば戻り値として
 * true を返す。それ以外は false を返す。戻り値が true の場合は val に削除さ
 * れたエントリの値が設定される。
 */
bool
scm_chash_tbl_delete(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblKey *val) /* GC )K */
{
  ScmCHashTblEntry *e;
  SCM_CHASH_TBL_VAL_KIND_T val_kind;

  /* scm_chash_tbl_access の呼出中に GC が走る可能性があるが、呼出後に
     tbl と key を参照しないので、これらを cstack に push する必要はない。
     scm_chash_tbl_access 内部ではハッシュテーブルへの参照を cstack に push
     しているのでハッシュテーブルが GC される心配もない。
  */

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_CHASH_TBL(tbl)->key_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(key));

  val_kind = SCM_CHASH_TBL(tbl)->val_kind;

  e = scm_chash_tbl_access(tbl, key, SCM_CHASH_TBL_VAL(0), DELETE);
  if (e != NULL) {
    if (val != NULL) {
      if (val_kind == SCM_CHASH_TBL_CVAL)
        *val = e->val;
      else
        SCM_REF_SETQ(val, e->val);
    }

    scm_memory_release(e);
    return true;
  }

  return false;
}

/* key と val を追加する。key にマッチするエントリがある場合はエラーとなり -1
 * を返す。追加が成功した場合は 0 を返す。
 */
int
scm_chash_tbl_insert(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal val) /* GC OK */
{
  ScmCHashTblEntry *e;

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_CHASH_TBL(tbl)->key_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(key));
  assert(SCM_CHASH_TBL(tbl)->val_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(val));

  e = scm_chash_tbl_access(tbl, key, val, ADD);
  return (e != NULL) ? 0 : -1;
}

/* key にマッチするエントリの値を val で更新する。key にマッチするエントリない
 * 場合は追加する。エラーの場合は -1 を返す(今のところ契機は無い)。成功の場合は
 * 0 を返す。
 */
int
scm_chash_tbl_update(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal val) /* GC OK */
{
  ScmCHashTblEntry *e;

  SCM_OBJ_ASSERT_TYPE(tbl, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_CHASH_TBL(tbl)->key_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(key));
  assert(SCM_CHASH_TBL(tbl)->val_kind != SCM_CHASH_TBL_CVAL
         && SCM_OBJ_IS_NOT_NULL(val));

  e = scm_chash_tbl_access(tbl, key, val, UPDATE);
  return (e != NULL) ? 0 : -1;
}

void
scm_chash_tbl_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));

  SCM_CHASH_TBL(obj)->buckets = NULL;
}

void
scm_chash_tbl_gc_finalize(ScmObj obj)
{
  scm_chash_tbl_finalize(obj);
}

int
scm_chash_tbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  if ((SCM_CHASH_TBL(obj)->key_kind != SCM_CHASH_TBL_SCMOBJ)
      && (SCM_CHASH_TBL(obj)->val_kind != SCM_CHASH_TBL_SCMOBJ))
    return rslt;

  for (size_t i = 0; i < SCM_CHASH_TBL(obj)->tbl_size; i++) {
    for (ScmCHashTblEntry *entry = SCM_CHASH_TBL(obj)->buckets[i];
         entry != NULL;
         entry = entry->next) {
      if (SCM_CHASH_TBL(obj)->key_kind == SCM_CHASH_TBL_SCMOBJ) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, entry->key, mem);
        if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
      }
      if (SCM_CHASH_TBL(obj)->val_kind == SCM_CHASH_TBL_SCMOBJ) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, entry->val, mem);
        if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
      }
    }
  }

  return rslt;
}

int
scm_chash_tbl_gc_accept_weak(ScmObj obj, ScmObj mem,
                             ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_CHASH_TBL_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(mem));
  assert(handler != NULL);

  if ((SCM_CHASH_TBL(obj)->key_kind != SCM_CHASH_TBL_SCMOBJ_W)
      && (SCM_CHASH_TBL(obj)->val_kind != SCM_CHASH_TBL_SCMOBJ_W))
    return rslt;

  for (size_t i = 0; i < SCM_CHASH_TBL(obj)->tbl_size; i++) {
    ScmCHashTblEntry **prev = &SCM_CHASH_TBL(obj)->buckets[i];
    for (ScmCHashTblEntry *entry = SCM_CHASH_TBL(obj)->buckets[i];
         entry != NULL;
         entry = entry->next) {
      bool delete_p = false;

      if (SCM_CHASH_TBL(obj)->key_kind == SCM_CHASH_TBL_SCMOBJ_W) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, entry->key, mem);
        if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
        if (SCM_OBJ_IS_NULL(entry->key)) delete_p = true;
      }
      if (SCM_CHASH_TBL(obj)->val_kind == SCM_CHASH_TBL_SCMOBJ_W) {
        rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, entry->val, mem);
        if (SCM_GC_IS_REF_HANDLER_FAILURE(rslt)) return rslt;
        if (SCM_OBJ_IS_NULL(entry->val)) delete_p = true;
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
