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

typedef enum {
  SCM_CHASH_TBL_CVAL,
  SCM_CHASH_TBL_SCMOBJ,
  SCM_CHASH_TBL_SCMOBJ_W
} SCM_CHASH_TBL_VAL_KIND_T;

#include "object.h"

extern ScmTypeInfo SCM_CHASH_TBL_TYPE_INFO;

struct ScmCHashTblRec {
  ScmObjHeader header;
  ScmCHashTblEntry **buckets;
  size_t tbl_size;
  ScmCHashFunc hash_func;
  ScmCHashTblKeyCmpFunc cmp_func;
  SCM_CHASH_TBL_VAL_KIND_T key_kind;
  SCM_CHASH_TBL_VAL_KIND_T val_kind;

  /* singleton method */
  /* ScmGCAcceptFunc gc_accept_func; */
  /* ScmGCAcceptFunc gc_accept_func_weak; */
};

struct ScmCHashTblEntryRec {
  ScmCHashTblKey key;
  ScmCHashTblVal val;
  ScmCHashTblEntry *next;
};

#define SCM_CHASH_TBL(obj) ((ScmCHashTbl *)(obj))

#define SCM_CHASH_TBL_KEY(k) ((ScmCHashTblKey)(k))
#define SCM_CHASH_TBL_VAL(v) ((ScmCHashTblVal)(v))

#define SCM_CHASH_TBL_ENTRY_KEY(entry)
#define SCM_CHASH_TBL_ENTRY_VAL(entry)



void scm_chash_tbl_initialize(ScmObj tbl, size_t size,
                              SCM_CHASH_TBL_VAL_KIND_T key_kind,
                              SCM_CHASH_TBL_VAL_KIND_T val_kind,
                              ScmCHashFunc hash_func,
                              ScmCHashTblKeyCmpFunc cmp_func);
void scm_chash_tbl_finalize(ScmObj tbl);
ScmObj scm_chash_tbl_new(SCM_MEM_ALLOC_TYPE_T mtype, size_t size,
                         SCM_CHASH_TBL_VAL_KIND_T key_kind,
                         SCM_CHASH_TBL_VAL_KIND_T val_kind,
                         ScmCHashFunc hash_func,
                         ScmCHashTblKeyCmpFunc cmp_func);
bool scm_chash_tbl_cmp_func_eq(ScmCHashTblKey key1, ScmCHashTblKey key2);
bool scm_chash_tbl_cmp_func_eqv(ScmCHashTblKey key1, ScmCHashTblKey key2);
bool scm_chash_tbl_get(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal *val);
bool scm_chash_tbl_delete(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblKey *val);
int scm_chash_tbl_insert(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal val);
int scm_chash_tbl_update(ScmObj tbl, ScmCHashTblKey key, ScmCHashTblVal val);
void scm_chash_tbl_gc_initialize(ScmObj obj, ScmObj mem);
void scm_chash_tbl_gc_finalize(ScmObj obj);
int scm_chash_tbl_gc_accept(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler);
int scm_chash_tbl_gc_accept_weak(ScmObj obj, ScmObj mem,
                                 ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_CHASHTBL_H__ */
