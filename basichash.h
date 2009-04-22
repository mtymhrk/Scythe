#ifndef INCLUDE_HASH_H__
#define INCLUDE_HASH_H__

#include <stdint.h>
#include <stdbool.h>

typedef struct ScmBasicHashTableRec ScmBasicHashTable;
typedef struct ScmBasicHashEntryRec ScmBasicHashEntry;
typedef struct ScmBasicHashItrRec ScmBasicHashItr;
typedef uintptr_t ScmBasicHashKey;
typedef uintptr_t ScmBasicHashValue;

struct ScmBasicHashEntryRec {
  ScmBasicHashKey key;
  ScmBasicHashValue value;
  unsigned int hash;
  struct ScmBasicHashEntryRec *prev;
  struct ScmBasicHashEntryRec *next;
}; 

#define SCM_BASIC_HASH_ENTRY_KEY(entry) ((entry)->key)
#define SCM_BASIC_HASH_ENTRY_VALUE(entry) ((entry)->value)

struct ScmBasicHashItrRec {
  ScmBasicHashTable *tbl;
  int idx;
  ScmBasicHashEntry *entry;
};

#define SCM_BASIC_HASH_ITR_BEGIN(tbl, itr) \
  (scm_basic_hash_itr_begin(tbl, &(itr)))
#define SCM_BASIC_HASH_ITR_ENTRY(itr) ((itr).entry)
#define SCM_BASIC_HASH_ITR_KEY(itr) ((itr).entry->key)
#define SCM_BASIC_HASH_ITR_VALUE(itr) ((itr).entry->value)
#define SCM_BASIC_HASH_ITR_NEXT(itr) (scm_basic_hash_itr_next(&(itr)))
#define SCM_BASIC_HASH_ITR_IS_END(itr) ((itr).entry == NULL)
#define SCM_BASIC_HASH_ITR_COPY(src, dst) ((src) = (dst))

typedef unsigned int (*ScmBasicHashFunc)(ScmBasicHashKey key);
typedef bool (*ScmBasicHashCompFunc)(ScmBasicHashKey key1,
                                     ScmBasicHashKey key2);
typedef void *(*ScmBasicHashIterBlock)(ScmBasicHashEntry *entry);
typedef void *(*ScmBasicHashInjectBlock)(ScmBasicHashEntry *entry,
                                         void *result);

#define SCM_BASIC_HASH_KEY(k) ((ScmBasicHashKey)(k))
#define SCM_BASIC_HASH_VALUE(v) ((ScmBasicHashValue)(v))

ScmBasicHashEntry *scm_basic_hash_insert(ScmBasicHashTable *table,
                                         ScmBasicHashKey key,
                                         ScmBasicHashValue value);
ScmBasicHashEntry *scm_basic_hash_put(ScmBasicHashTable *table,
                                      ScmBasicHashKey key,
                                      ScmBasicHashValue value);
ScmBasicHashEntry *scm_basic_hash_update_entry(ScmBasicHashTable *table,
                                               ScmBasicHashEntry *entry,
                                               ScmBasicHashValue value);
void scm_basic_hash_delete(ScmBasicHashTable *table, ScmBasicHashKey key);
void scm_basic_hash_delete_entry(ScmBasicHashTable *table,
                                 ScmBasicHashEntry *entry);
ScmBasicHashEntry *scm_basic_hash_get(ScmBasicHashTable *table,
                                      ScmBasicHashKey key);
void scm_basic_hash_clear(ScmBasicHashTable *table);
void *scm_basic_hash_iterate(ScmBasicHashTable *table,
                             ScmBasicHashIterBlock block);
void *scm_basic_hash_inject(ScmBasicHashTable *table,
                            ScmBasicHashInjectBlock block, void *init);
int scm_basic_hash_itr_begin(ScmBasicHashTable *table, ScmBasicHashItr *itr);
int scm_basic_hash_itr_next(ScmBasicHashItr *itr);
ScmBasicHashTable *scm_basic_hash_construct(size_t size,
                                            ScmBasicHashFunc hash_func,
                                            ScmBasicHashCompFunc comp_func);
void scm_basic_hash_destruct(ScmBasicHashTable *table);

#endif /* INCLUDE_HASH_H__ */
