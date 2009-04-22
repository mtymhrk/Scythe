#include <cutter.h>

#include <stdbool.h>

#include "basichash.h"

unsigned int
hash_func(ScmBasicHashKey key)
{
  return (unsigned int)key;
}

bool
comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2);
}

void
test_scm_basic_hash_construct(void)
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);

  cut_assert_not_null(table);
}

void
test_scm_basic_hash_insert_nonexistent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func); 
  ScmBasicHashEntry *entry;

  entry = scm_basic_hash_insert(table,
                                SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_BASIC_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(100, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));
}

void
test_scm_basic_hash_insert_existent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  entry = scm_basic_hash_insert(table,
                                SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  cut_assert_null(entry);
}

void
test_scm_basic_hash_put_nonexistent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func); 
  ScmBasicHashEntry *entry;

  entry = scm_basic_hash_put(table,
                             SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_BASIC_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(100, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));
}

void
test_scm_basic_hash_put_existent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256, hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  entry = scm_basic_hash_put(table,
                             SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(999));
  cut_assert_not_null(entry);

  cut_assert_equal_int(1, (int)SCM_BASIC_HASH_ENTRY_KEY(entry));
  cut_assert_equal_int(999, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));
}

void
test_scm_basic_hash_update_entry()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;


  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  scm_basic_hash_update_entry(table, entry, SCM_BASIC_HASH_VALUE(200));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  cut_assert_equal_int(200, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));
}

void
test_scm_basic_hash_delete_nonexistent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_delete(table, SCM_BASIC_HASH_KEY(1));
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));

  cut_assert_null(entry);
}

void
test_scm_basic_hash_delete_existent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_delete(table, SCM_BASIC_HASH_KEY(1));
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));

  cut_assert_null(entry);
}

void
test_scm_basic_hash_delete_entry()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  scm_basic_hash_delete_entry(table, entry);

  cut_assert_null(scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1)));

  scm_basic_hash_destruct(table);
}

void
test_scm_basic_hash_delete_head_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));
  
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(513));
  scm_basic_hash_delete_entry(table, entry);
  cut_assert_null(scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(513)));

  scm_basic_hash_destruct(table);
}

void
test_scm_basic_hash_delete_tail_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));
  
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  scm_basic_hash_delete_entry(table, entry);
  cut_assert_null(scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1)));

  scm_basic_hash_destruct(table);
}

void
test_scm_basic_hash_delete_middle_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_construct(256,
                                                      hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));
  
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(257));
  scm_basic_hash_delete_entry(table, entry);
  cut_assert_null(scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(257)));

  scm_basic_hash_destruct(table);
}
