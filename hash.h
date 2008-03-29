#ifndef INCLUDE_HASH_H__
#define INCLUDE_HASH_H__

#include <stdbool.h>

typedef struct ScmHashTableRec ScmHashTable;
typedef struct ScmHashEntryRec ScmHashEntry;
typedef void *ScmHashKey;
typedef void *ScmHashValue;

struct ScmHashEntryRec {
  ScmHashKey key;
  ScmHashValue value;
  struct ScmHashEntryRec *next;
}; 

#define SCM_HASH_ENTRY_KEY(entry) ((entry)->key)
#define SCM_HASH_ENTRY_VALUE(entry) ((entry)->value)

typedef unsigned int (*ScmHashFunc)(ScmHashKey key);
typedef bool (*ScmHashCompFunc)(ScmHashKey key1, ScmHashKey key2);
typedef void *(*ScmHashIterBlock)(ScmHashEntry *entry);
typedef void *(*ScmHashInjectBlock)(ScmHashEntry *entry, void *result);

#define SCM_HASH_KEY(k) ((ScmHashKey)(k))
#define SCM_HASH_VALUE(v) ((ScmHashValue)(v))

ScmHashEntry *scm_hash_insert(ScmHashTable *table,
			      ScmHashKey key, ScmHashValue value);
ScmHashEntry *scm_hash_put(ScmHashTable *table,
			   ScmHashKey key, ScmHashValue value);
ScmHashEntry *scm_hash_delete(ScmHashTable *table, ScmHashKey key);
ScmHashEntry *scm_hash_get(ScmHashTable *table, ScmHashKey key);
void *scm_hash_iterate(ScmHashTable *table, ScmHashIterBlock block);
void *scm_hash_inject(ScmHashTable *table,
		      ScmHashInjectBlock block, void *init);
ScmHashTable *scm_hash_construct(size_t size,
				 ScmHashFunc hash_func,
				 ScmHashCompFunc comp_func);

#endif /* INCLUDE_HASH_H__ */
