#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "basichash.h"


struct ScmBasicHashTableRec {
  ScmBasicHashEntry **buckets;
  size_t tbl_size;
  ScmBasicHashFunc hash_func;
  ScmBasicHashCompFunc comp_func;
};

enum { ADD, UPDATE, DELETE, FIND };

static ScmBasicHashEntry *
scm_basic_hash_entry_construct(ScmBasicHashKey key,
                               ScmBasicHashValue value, ScmBasicHashEntry *next)
{
  ScmBasicHashEntry *entry = scm_memory_allocate(sizeof(ScmBasicHashEntry));
  entry->key = key;
  entry->value = value;
  entry->next = next;
  return entry;
}

static ScmBasicHashEntry *
scm_basic_hash_access(ScmBasicHashTable *table,
                      ScmBasicHashKey key, ScmBasicHashValue value, int mode)
{
  ScmBasicHashEntry *entry;
  ScmBasicHashEntry **prev_next;
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
	    = scm_basic_hash_entry_construct(key, value, table->buckets[hash]));
  else 
    return NULL;
}

ScmBasicHashEntry *
scm_basic_hash_insert(ScmBasicHashTable *table,
                      ScmBasicHashKey key, ScmBasicHashValue value)
{
  assert(table != NULL);
  return scm_basic_hash_access(table, key, value, ADD);
}

ScmBasicHashEntry *
scm_basic_hash_put(ScmBasicHashTable *table,
                   ScmBasicHashKey key, ScmBasicHashValue value)
{
  assert(table != NULL);
  return scm_basic_hash_access(table, key, value, UPDATE);
}

void
scm_basic_hash_delete(ScmBasicHashTable *table, ScmBasicHashKey key)
{
  ScmBasicHashEntry *entry;

  assert(table != NULL);

  entry = scm_basic_hash_access(table, key, NULL, DELETE);
  if (entry != NULL) scm_memory_release(entry);
}

ScmBasicHashEntry *
scm_basic_hash_get(ScmBasicHashTable *table, ScmBasicHashKey key)
{
  assert(table != NULL);

  return scm_basic_hash_access(table, key, NULL, FIND);
}

void
scm_basic_hash_clear(ScmBasicHashTable *table)
{
  ScmBasicHashEntry *entry, *next;
  int i;

  for (i = 0; i < table->tbl_size; i++) {
    for (entry = table->buckets[i]; entry != NULL; entry = next) {
      next = entry->next;
      scm_memory_release(entry);
    }
    table->buckets[i] = NULL;
  }
}

void *
scm_basic_hash_iterate(ScmBasicHashTable *table, ScmBasicHashIterBlock block)
{
  ScmBasicHashEntry *entry;
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
scm_basic_hash_inject(ScmBasicHashTable *table,
                      ScmBasicHashInjectBlock block, void *init)
{
  ScmBasicHashEntry *entry;
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

ScmBasicHashItr
scm_basic_hash_itr_begin(ScmBasicHashTable *table)
{
  ScmBasicHashItr itr;

  assert(table != NULL);

  itr.tbl = table;
  itr.idx = 0;
  itr.entry = table->buckets[0];

  return itr;
}

ScmBasicHashItr
scm_basic_hash_itr_next(const ScmBasicHashItr *itr)
{
  ScmBasicHashItr nxt_itr;

  assert(itr != NULL);

  nxt_itr.tbl = itr->tbl;
  if (itr->entry == NULL) {
    nxt_itr.idx = itr->idx;
    nxt_itr.entry = itr->entry;
  }
  else if (itr->entry->next != NULL) {
    nxt_itr.idx = itr->idx;
    nxt_itr.entry = itr->entry->next;
  }
  else {
    nxt_itr.idx = itr->idx + 1;
    if (nxt_itr.idx < itr->tbl->tbl_size)
      nxt_itr.entry = itr->tbl->buckets[nxt_itr.idx];
    else
      nxt_itr.entry = NULL;
  }

  return nxt_itr;
}

ScmBasicHashTable *
scm_basic_hash_construct(size_t size,
                         ScmBasicHashFunc hash_func,
                         ScmBasicHashCompFunc comp_func)
{
  ScmBasicHashTable *table = NULL;
  int i;

  assert(hash_func != NULL); assert(comp_func != NULL);

  table = scm_memory_allocate(sizeof(ScmBasicHashTable));
  table->buckets = scm_memory_allocate(sizeof(ScmBasicHashEntry *) * size);
  table->tbl_size = size;
  table->hash_func = hash_func;
  table->comp_func = comp_func;

  for (i = 0; i < size; i++)
    table->buckets[i] = NULL;

  return table;
}

void
scm_basci_hash_destruct(ScmBasicHashTable *table)
{

  int i;

  assert(table != NULL);

  for (i = 0; i < table->tbl_size; i++) {
    ScmBasicHashEntry *entry = table->buckets[i];   
    while (entry != NULL) {
      ScmBasicHashEntry *next = entry->next;
      scm_memory_release(entry);
      entry = next;
    }
  }

  scm_memory_release(table->buckets);
  scm_memory_release(table);
}
