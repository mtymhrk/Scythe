#include <cutter.h>

#include <stdbool.h>

#include "basichash.h"

size_t
hash_func(ScmBasicHashKey key)
{
  return (size_t)key;
}

bool
comp_func(ScmBasicHashKey key1, ScmBasicHashKey key2)
{
  return (key1 == key2);
}

void
test_scm_basic_hash_new(void)
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);

  cut_assert_not_null(table);
}

void
test_scm_basic_hash_insert_nonexistent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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
  ScmBasicHashTable *table = scm_basic_hash_new(256, hash_func, comp_func);
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
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_delete(table, SCM_BASIC_HASH_KEY(1));
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));

  cut_assert_null(entry);
}

void
test_scm_basic_hash_delete_existent_key()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  
  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  scm_basic_hash_delete_entry(table, entry);

  cut_assert_null(scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1)));

  scm_basic_hash_end(table);
}

void
test_scm_basic_hash_delete_head_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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

  scm_basic_hash_end(table);
}

void
test_scm_basic_hash_delete_tail_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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

  scm_basic_hash_end(table);
}

void
test_scm_basic_hash_delete_middle_entry_synonym()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
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

  scm_basic_hash_end(table);
}

int itr_block_nr_call;
SCM_BASIC_HASH_ITR_BLK_RET_T itr_block_ret_values[256];

SCM_BASIC_HASH_ITR_BLK_RET_T itr_block(ScmBasicHashEntry *entry)
{
  return itr_block_ret_values[itr_block_nr_call++];
}

void
test_scm_basic_hash_iterate__SUCC()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));


  itr_block_nr_call = 0;
  itr_block_ret_values[0] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  itr_block_ret_values[1] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  itr_block_ret_values[2] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  scm_basic_hash_iterate(table, itr_block);

  cut_assert_equal_int(3, itr_block_nr_call);

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  cut_assert_not_null(entry);
  cut_assert_equal_int(100, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(257));
  cut_assert_not_null(entry);
  cut_assert_equal_int(101, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(513));
  cut_assert_not_null(entry);
  cut_assert_equal_int(102, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  scm_basic_hash_end(table);
}

void
test_scm_basic_hash_iterate__BREAK()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));

  itr_block_nr_call = 0;
  itr_block_ret_values[0] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  itr_block_ret_values[1] = SCM_BASIC_HASH_ITR_BLK_BREAK;
  itr_block_ret_values[2] = SCM_BASIC_HASH_ITR_BLK_DELETE;
  scm_basic_hash_iterate(table, itr_block);

  cut_assert_equal_int(2, itr_block_nr_call);

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  cut_assert_not_null(entry);
  cut_assert_equal_int(100, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(257));
  cut_assert_not_null(entry);
  cut_assert_equal_int(101, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(513));
  cut_assert_not_null(entry);
  cut_assert_equal_int(102, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  scm_basic_hash_end(table);
}

void
test_scm_basic_hash_iterate__DELETE()
{
  ScmBasicHashTable *table = scm_basic_hash_new(256,
                                                hash_func, comp_func);
  ScmBasicHashEntry *entry;

  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(1), SCM_BASIC_HASH_VALUE(100));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(257), SCM_BASIC_HASH_VALUE(101));
  scm_basic_hash_insert(table,
                        SCM_BASIC_HASH_KEY(513), SCM_BASIC_HASH_VALUE(102));

  itr_block_nr_call = 0;
  itr_block_ret_values[0] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  itr_block_ret_values[1] = SCM_BASIC_HASH_ITR_BLK_DELETE;
  itr_block_ret_values[2] = SCM_BASIC_HASH_ITR_BLK_SUCC;
  scm_basic_hash_iterate(table, itr_block);

  cut_assert_equal_int(3, itr_block_nr_call);

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(1));
  cut_assert_not_null(entry);
  cut_assert_equal_int(100, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(257));
  cut_assert_null(entry);

  entry = scm_basic_hash_get(table, SCM_BASIC_HASH_KEY(513));
  cut_assert_not_null(entry);
  cut_assert_equal_int(102, (int)SCM_BASIC_HASH_ENTRY_VALUE(entry));



  scm_basic_hash_end(table);
}

void
test_scm_basic_hash__external_iterator()
{
  unsigned int i;
  int keys[] = { 1, 10, 100 };
  int values[] = { 101, 110, 200,};
  bool checked[] = { false, false, false };
  ScmBasicHashItr itr;
  ScmBasicHashTable *table;

  /* preprocess */
  table = scm_basic_hash_new(256, hash_func, comp_func);
  for (i = 0; i < sizeof(keys)/sizeof(keys[0]); i++)
    scm_basic_hash_insert(table,
                          SCM_BASIC_HASH_KEY(keys[i]),
                          SCM_BASIC_HASH_VALUE(values[i]));

  /* action */
  SCM_BASIC_HASH_ITR_BEGIN(table, itr);

  /* postcondition check */
  cut_assert_false(SCM_BASIC_HASH_ITR_IS_END(itr));
  for (i = 0; i < sizeof(keys)/sizeof(keys[0]); i++)
    if (keys[i] == (int)SCM_BASIC_HASH_ITR_KEY(itr)) {
      cut_assert_false(checked[i]);
      cut_assert_equal_int(values[i], (int)SCM_BASIC_HASH_ITR_VALUE(itr));
      checked[i] = true;
    }

  /* action */
  SCM_BASIC_HASH_ITR_NEXT(itr);

  /* postcondition check */
  cut_assert_false(SCM_BASIC_HASH_ITR_IS_END(itr));
  for (i = 0; i < sizeof(keys)/sizeof(keys[0]); i++)
    if (keys[i] == (int)SCM_BASIC_HASH_ITR_KEY(itr)) {
      cut_assert_false(checked[i]); 
      cut_assert_equal_int(values[i], (int)SCM_BASIC_HASH_ITR_VALUE(itr));
      checked[i] = true;
    }

  /* action */
  SCM_BASIC_HASH_ITR_NEXT(itr);

  /* postcondition check */
  cut_assert_false(SCM_BASIC_HASH_ITR_IS_END(itr));
  for (i = 0; i < sizeof(keys)/sizeof(keys[0]); i++)
    if (keys[i] == (int)SCM_BASIC_HASH_ITR_KEY(itr)) {
      cut_assert_false(checked[i]); 
      cut_assert_equal_int(values[i], (int)SCM_BASIC_HASH_ITR_VALUE(itr));
      checked[i] = true;
    }

  /* action */
  SCM_BASIC_HASH_ITR_NEXT(itr);

  /* postcondition check */
  cut_assert_true(SCM_BASIC_HASH_ITR_IS_END(itr));

  /* postprocess */
  scm_basic_hash_end(table);
}
