#ifndef INCLUDE_CHASHTBL_H__
#define INCLUDE_CHASHTBL_H__

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct ScmCHashTblRec ScmCHashTbl;
typedef struct ScmCHashTblEntryRec ScmCHashTblEntry;
typedef uintptr_t ScmCHashTblKey;
typedef uintptr_t ScmCHashTblVal;

typedef size_t (*ScmCHashFunc)(ScmCHashTblKey key);
typedef bool (*ScmCHashTblKeyCmpFunc)(ScmCHashTblKey key1,
                                      ScmCHashTblKey key2);


typedef enum scm_chash_tbl_val_kind scm_chash_tbl_val_kind_t;

#include "scythe/object.h"

enum  scm_chash_tbl_val_kind {
  SCM_CHASH_TBL_CVAL,
  SCM_CHASH_TBL_SCMOBJ,
  SCM_CHASH_TBL_SCMOBJ_W
};

struct ScmCHashTblRec {
  ScmObj owner;
  ScmCHashTblEntry **buckets;
  size_t tbl_size;
  ScmCHashFunc hash_func;
  ScmCHashTblKeyCmpFunc cmp_func;
  scm_chash_tbl_val_kind_t key_kind;
  scm_chash_tbl_val_kind_t val_kind;
};

struct ScmCHashTblEntryRec {
  ScmCHashTblKey key;
  ScmCHashTblVal val;
  ScmCHashTblEntry *next;
};

#define SCM_CHASH_TBL_KEY(k) ((ScmCHashTblKey)(k))
#define SCM_CHASH_TBL_VAL(v) ((ScmCHashTblVal)(v))

#define SCM_CHASH_TBL_ENTRY_KEY(entry)
#define SCM_CHASH_TBL_ENTRY_VAL(entry)



void scm_chash_tbl_initialize(ScmCHashTbl *tbl, ScmObj owner,
                              size_t size,
                              scm_chash_tbl_val_kind_t key_kind,
                              scm_chash_tbl_val_kind_t val_kind,
                              ScmCHashFunc hash_func,
                              ScmCHashTblKeyCmpFunc cmp_func);
void scm_chash_tbl_finalize(ScmCHashTbl *tbl);
ScmCHashTbl *scm_chash_tbl_new(ScmObj owner, size_t size,
                               scm_chash_tbl_val_kind_t key_kind,
                               scm_chash_tbl_val_kind_t val_kind,
                               ScmCHashFunc hash_func,
                               ScmCHashTblKeyCmpFunc cmp_func);
void scm_chash_tbl_end(ScmCHashTbl *tbl);
bool scm_chash_tbl_cmp_func_eq(ScmCHashTblKey key1, ScmCHashTblKey key2);
bool scm_chash_tbl_cmp_func_eqv(ScmCHashTblKey key1, ScmCHashTblKey key2);
int scm_chash_tbl_get(ScmCHashTbl *tbl, ScmCHashTblKey key,
                      ScmCHashTblVal *val, bool *found);
int scm_chash_tbl_delete(ScmCHashTbl *tbl, ScmCHashTblKey key,
                         ScmCHashTblKey *val, bool *deleted);
int scm_chash_tbl_insert(ScmCHashTbl *tbl,
                         ScmCHashTblKey key, ScmCHashTblVal val);
int scm_chash_tbl_update(ScmCHashTbl *tbl,
                         ScmCHashTblKey key, ScmCHashTblVal val);
void scm_chash_tbl_clean(ScmCHashTbl *tbl);

int scm_chash_tbl_gc_accept(ScmCHashTbl *tbl, ScmObj owner,
                            ScmObj mem, ScmGCRefHandlerFunc handler,
                            bool rehash);
int scm_chash_tbl_gc_accept_weak(ScmCHashTbl *tbl, ScmObj owner,
                                 ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_CHASHTBL_H__ */
