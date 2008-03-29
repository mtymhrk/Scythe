#include <CUnit/CUnit.h>

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

  CU_ASSERT_PTR_NOT_NULL(table);
}

void
test_scm_hash_insert_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func); 
  ScmHashEntry *entry;

  entry = scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  CU_ASSERT_PTR_NOT_NULL(entry);

  CU_ASSERT_EQUAL(1, (int)SCM_HASH_ENTRY_KEY(entry));
  CU_ASSERT_EQUAL(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_insert_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  CU_ASSERT_PTR_NULL(entry);
}

void
test_scm_hash_put_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func); 
  ScmHashEntry *entry;

  entry = scm_hash_put(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  CU_ASSERT_PTR_NOT_NULL(entry);

  CU_ASSERT_EQUAL(1, (int)SCM_HASH_ENTRY_KEY(entry));
  CU_ASSERT_EQUAL(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_put_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_put(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(999));
  CU_ASSERT_PTR_NOT_NULL(entry);

  CU_ASSERT_EQUAL(1, (int)SCM_HASH_ENTRY_KEY(entry));
  CU_ASSERT_EQUAL(999, (int)SCM_HASH_ENTRY_VALUE(entry));
}

void
test_scm_hash_delete_nonexistent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry = scm_hash_delete(table, SCM_HASH_KEY(1));

  CU_ASSERT_PTR_NULL(entry);
}

void
test_scm_hash_delete_existent_key()
{
  ScmHashTable *table = scm_hash_construct(256, hash_func, comp_func);
  ScmHashEntry *entry;

  scm_hash_insert(table, SCM_HASH_KEY(1), SCM_HASH_VALUE(100));
  entry = scm_hash_delete(table, SCM_HASH_KEY(1));
  CU_ASSERT_PTR_NOT_NULL(entry);

  CU_ASSERT_EQUAL(1, (int)SCM_HASH_ENTRY_KEY(entry));
  CU_ASSERT_EQUAL(100, (int)SCM_HASH_ENTRY_VALUE(entry));
}

#define TEST_CASE(name) \
  { #name, name }

CU_ErrorCode
register_test_case(void)
{
  CU_TestInfo testcases[] = {
    { "Constructor", test_scm_hash_construct },
    { "Insert nonexistent key", test_scm_hash_insert_nonexistent_key },
    { "Insert exisitent key", test_scm_hash_insert_existent_key },
    { "Put nonexistent key", test_scm_hash_put_nonexistent_key },
    { "Put existent key", test_scm_hash_put_existent_key },
    { "Delete nonexistent key", test_scm_hash_delete_nonexistent_key },
    { "Delete existent key", test_scm_hash_delete_existent_key },
    CU_TEST_INFO_NULL
  };
  CU_SuiteInfo suites[] = {
    {"ScmHashTable", NULL, NULL, testcases},
    CU_SUITE_INFO_NULL
  };
 
  return  CU_register_suites(suites);
}
