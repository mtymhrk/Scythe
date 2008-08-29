#include <cutter.h>

#include <stdbool.h>

#include "hash.h"

unsigned int
hash_func(void *key)
{
  return (unsigned int)key;
}

bool
comp_func(void *key1, void *key2)
{
  return (key1 == key2);
}

void
test_scm_hash_construct(void)
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);

  cut_assert_not_null(table);
}

void
test_scm_hash_insert_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func); 
  ScmHashEntry *entry;

  entry = scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_insert_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  cut_assert_null(entry);
}

void
test_scm_hash_put_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func); 
  ScmHashEntry *entry;

  entry = scm_hash_put(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_put_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_put(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(999));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(999, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_delete_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry = scm_hash_delete(table, SCM_HASH_KEY(1));

  cut_assert_null(entry);
}

void
test_scm_hash_delete_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_delete(table, SCM_HASH_KEY(1));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}
