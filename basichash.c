#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#include "memory.h"
#include "basichash.h"


#define CALC_HASH_VALUE(tbl, key) ((tbl)->hash_func(key) % (tbl)->tbl_size)


struct ScmBasicHashTableRec {
  ScmBasicHashEntry **buckets;
  size_t tbl_size;
  ScmBasicHashFunc hash_func;
  ScmBasicHashCompFunc comp_func;
};

enum { ADD, UPDATE, DELETE, FIND };

static ScmBasicHashEntry *
scm_basic_hash_entry_new(ScmBasicHashKey key, ScmBasicHashValue value,
                         size_t hash,
                         ScmBasicHashEntry *prev, ScmBasicHashEntry *next)
{
  ScmBasicHashEntry *entry = scm_memory_allocate(sizeof(ScmBasicHashEntry));
  entry->key = key;
  entry->value = value;
  entry->hash = hash;
  entry->prev = prev;
  entry->next = next;
  return entry;
}

static void
scm_basic_hash_purge_entry(ScmBasicHashTable *table, ScmBasicHashEntry *entry)
{
  assert(table != NULL);
  assert(entry != NULL);

  if (entry->prev != NULL)
    entry->prev->next = entry->next;
  else
    table->buckets[entry->hash] = entry->next;

  if (entry->next != NULL)
    entry->next->prev = entry->prev;
}

static ScmBasicHashEntry *
scm_basic_hash_access(ScmBasicHashTable *table,
                      ScmBasicHashKey key, ScmBasicHashValue value, int mode)
{
  ScmBasicHashEntry *entry;
  size_t hash;

  assert(table != NULL);

  hash = CALC_HASH_VALUE(table, key);

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
        scm_basic_hash_purge_entry(table, entry);
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
    ScmBasicHashEntry *new = scm_basic_hash_entry_new(key, value, hash, NULL,
                                                      table->buckets[hash]);
    if (new->next != NULL) new->next->prev = new;
    return (table->buckets[hash] = new);
  }
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

ScmBasicHashEntry *
scm_basic_hash_update_entry(ScmBasicHashTable *table,
                            ScmBasicHashEntry *entry, ScmBasicHashValue value)
{
  assert(table != NULL);
  assert(entry != NULL);

  entry->value = value;

  return entry;
}

void
scm_basic_hash_delete(ScmBasicHashTable *table, ScmBasicHashKey key)
{
  ScmBasicHashEntry *entry;

  assert(table != NULL);

  entry = scm_basic_hash_access(table, key, 0, DELETE);
  if (entry != NULL) scm_memory_release(entry);
}

void
scm_basic_hash_delete_entry(ScmBasicHashTable *table, ScmBasicHashEntry *entry)
{
  assert(table != NULL);
  assert(entry != NULL);

  scm_basic_hash_purge_entry(table, entry);
  scm_memory_release(entry);
}

ScmBasicHashEntry *
scm_basic_hash_get(ScmBasicHashTable *table, ScmBasicHashKey key)
{
  assert(table != NULL);

  return scm_basic_hash_access(table, key, 0, FIND);
}

void
scm_basic_hash_clear(ScmBasicHashTable *table)
{
  ScmBasicHashEntry *entry, *next;
  size_t i;

  for (i = 0; i < table->tbl_size; i++) {
    for (entry = table->buckets[i]; entry != NULL; entry = next) {
      next = entry->next;
      scm_memory_release(entry);
    }
    table->buckets[i] = NULL;
  }
}

void
scm_basic_hash_iterate(ScmBasicHashTable *table, ScmBasicHashIterBlock block)
{
  ScmBasicHashEntry *entry;
  SCM_BASIC_HASH_ITR_BLK_RET_T rslt;
  size_t i;

  assert(table != NULL);
  assert(block != NULL);

  rslt = SCM_BASIC_HASH_ITR_BLK_SUCC;
  for (i = 0; i < table->tbl_size; i++) {
    ScmBasicHashEntry *next = NULL;
    for (entry = table->buckets[i]; entry != NULL; entry = next) {
      next = entry->next;
      SCM_BASIC_HASH_ITR_BLK_RET_T rslt = block(entry);
      switch (rslt) {
      case SCM_BASIC_HASH_ITR_BLK_SUCC:
        break;
      case SCM_BASIC_HASH_ITR_BLK_DELETE:
        scm_basic_hash_purge_entry(table, entry);
        scm_memory_release(entry);
        break;
      case SCM_BASIC_HASH_ITR_BLK_BREAK:
        return;
        break;
      default:
        assert(0);              /* must not happen */
        break;
      }
    }
  }
}

void *
scm_basic_hash_inject(ScmBasicHashTable *table,
                      ScmBasicHashInjectBlock block, void *init)
{
  ScmBasicHashEntry *entry;
  void *result;
  size_t i;

  assert(table != NULL);
  assert(block != NULL);

  result = init;
  for (i = 0; i < table->tbl_size; i++)
    for (entry = table->buckets[i]; entry != NULL; entry = entry->next)
      result = block(entry, result);

  return result;
}

int
scm_basic_hash_itr_begin(ScmBasicHashTable *table, ScmBasicHashItr *itr)
{
  size_t i;

  assert(table != NULL);
  assert(itr != NULL);

  itr->tbl = table;
  itr->idx = table->tbl_size;
  itr->entry = NULL;

  for (i = 0; i < table->tbl_size; i++)
    if (table->buckets[i] != NULL) {
      itr->idx = i;
      itr->entry = table->buckets[i];
      break;
    }

  return 0;
}

int
scm_basic_hash_itr_next(ScmBasicHashItr *itr)
{
  assert(itr != NULL);

  if (itr->entry != NULL) {
    if (itr->entry->next != NULL) {
      itr->entry = itr->entry->next;
    }
    else {
      size_t i = itr->idx;
      itr->idx = itr->tbl->tbl_size;
      itr->entry = NULL;
      for (i = i + 1; i < itr->tbl->tbl_size; i++)
        if (itr->tbl->buckets[i] != NULL) {
          itr->idx = i;
          itr->entry = itr->tbl->buckets[i];
          break;
        }
    }
  }

  return 0;
}

int
scm_basic_hash_itr_update_key(ScmBasicHashItr *itr, ScmBasicHashKey key)
{
  size_t hash;

  assert(itr != NULL);

  if (SCM_BASIC_HASH_ITR_IS_END(*itr)) return -1;

  hash = CALC_HASH_VALUE(itr->tbl, key);
  if (itr->idx != hash) return -1;

  itr->entry->key = key;

  return 0;
}

ScmBasicHashTable *
scm_basic_hash_new(size_t size,
                         ScmBasicHashFunc hash_func,
                         ScmBasicHashCompFunc comp_func)
{
  ScmBasicHashTable *table = NULL;
  size_t i;

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
scm_basic_hash_end(ScmBasicHashTable *table)
{
  size_t i;

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
