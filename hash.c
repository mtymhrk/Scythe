#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "hash.h"


struct ScmHashTableRec {
  ScmHashEntry **buckets;
  size_t tbl_size;
  ScmHashFunc hash_func;
  ScmHashCompFunc comp_func;
};

enum { ADD, UPDATE, DELETE, FIND };

static ScmHashEntry *
scm_hash_entry_construct(ScmHashKey key, ScmHashValue value, ScmHashEntry *next)
{
  ScmHashEntry *entry = scm_memory_allocate(sizeof(ScmHashEntry));
  entry->key = key;
  entry->value = value;
  entry->next = next;
  return entry;
}

static ScmHashEntry *
scm_hash_access(ScmHashTable *table, ScmHashKey key, ScmHashValue value, int mode)
{
  ScmHashEntry *entry;
  ScmHashEntry **prev_next;
  unsigned int hash;

  assert(table != NULL);
  
  hash = table->hash_func(key) % table->tbl_size;

  prev_next = table->buckets + hash;
  for (entry = table->buckets[hash]; entry != NULL; entry = entry->next) {
    if (table->comp_func(key, entry->key)) {
      switch (mode) {
      case ADD:
	return NULL;
	break;
      case UPDATE:
	entry->key = key;
	entry->value = value;
	return entry;
	break;
      case DELETE:
	*prev_next = entry->next;
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
    prev_next = &(entry->next);
  }

  if (mode == ADD || mode == UPDATE)
    return (table->buckets[hash]
	    = scm_hash_entry_construct(key, value, table->buckets[hash]));
  else 
    return NULL;
}

ScmHashEntry *
scm_hash_insert(ScmHashTable *table, ScmHashKey key, ScmHashValue value)
{
  assert(table != NULL);
  return scm_hash_access(table, key, value, ADD);
}

ScmHashEntry *
scm_hash_put(ScmHashTable *table, ScmHashKey key, ScmHashValue value)
{
  assert(table != NULL);
  return scm_hash_access(table, key, value, UPDATE);
}

ScmHashEntry *
scm_hash_delete(ScmHashTable *table, ScmHashKey key)
{
  assert(table != NULL);

  return scm_hash_access(table, key, NULL, DELETE);
}

ScmHashEntry *
scm_hash_get(ScmHashTable *table, ScmHashKey key)
{
  assert(table != NULL);

  return scm_hash_access(table, key, NULL, FIND);
}

void *
scm_hash_iterate(ScmHashTable *table, ScmHashIterBlock block)
{
  ScmHashEntry *entry;
  void *result;
  int i;
  
  assert(table != NULL);
  assert(block != NULL);

  result = NULL;
  for (i = 0; i < table->tbl_size; i++)
    for (entry = table->buckets[i]; entry != NULL; entry = entry->next)
      result = block(entry);

  return result;
}

void *
scm_hash_inject(ScmHashTable *table, ScmHashInjectBlock block, void *init)
{
  ScmHashEntry *entry;
  void *result;
  int i;
  
  assert(table != NULL);
  assert(block != NULL);

  result = init;
  for (i = 0; i < table->tbl_size; i++)
    for (entry = table->buckets[i]; entry != NULL; entry = entry->next)
      result = block(entry, result);

  return result;
}

ScmHashTable *
scm_hash_construct(size_t size,
		   ScmHashFunc hash_func, ScmHashCompFunc comp_func)
{
  ScmHashTable *table = NULL;
  int i;

  assert(hash_func != NULL); assert(comp_func != NULL);

  table = scm_memory_allocate(sizeof(ScmHashTable));
  table->buckets = scm_memory_allocate(sizeof(ScmHashEntry *) * size);
  table->tbl_size = size;
  table->hash_func = hash_func;
  table->comp_func = comp_func;

  for (i = 0; i < size; i++)
    table->buckets[i] = NULL;

  return table;
}

