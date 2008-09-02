#ifndef INCLUDE_HASH_H__
#define INCLUDE_HASH_H__

#include <stdbool.h>

typedef struct ScmBasicHashTableRec ScmBasicHashTable;
typedef struct ScmBasicHashEntryRec ScmBasicHashEntry;
typedef void *ScmBasicHashKey;
typedef void *ScmBasicHashValue;

struct ScmBasicHashEntryRec {
  ScmBasicHashKey key;
  ScmBasicHashValue value;
  struct ScmBasicHashEntryRec *next;
}; 

#define SCM_BASIC_HASH_ENTRY_KEY(entry) ((entry)->key)
#define SCM_BASIC_HASH_ENTRY_VALUE(entry) ((entry)->value)

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
ScmBasicHashEntry *scm_basic_hash_delete(ScmBasicHashTable *table,
                                         ScmBasicHashKey key);
ScmBasicHashEntry *scm_basic_hash_get(ScmBasicHashTable *table,
                                      ScmBasicHashKey key);
void *scm_basic_hash_iterate(ScmBasicHashTable *table,
                             ScmBasicHashIterBlock block);
void *scm_basic_hash_inject(ScmBasicHashTable *table,
                            ScmBasicHashInjectBlock block, void *init);
ScmBasicHashTable *scm_basic_hash_construct(size_t size,
                                            ScmBasicHashFunc hash_func,
                                            ScmBasicHashCompFunc comp_func);

#endif /* INCLUDE_HASH_H__ */
