#include "assert.h"

#include "memory.h"
#include "basiclist.h"

struct ScmBasicListRec {
  ScmBasicListEntry *head;
  ScmBasicListEntry *tail;
  size_t length;
};

struct ScmBasicListEntryRec {
  ScmBasicListEntry *next;
  ScmBasicListEntry *before;
  ScmBasicListValue value;
};

ScmBasicListValue
scm_basic_list_entry_value(ScmBasicListEntry *entry)
{
  assert(entry != NULL);
  return entry->value;
}

void
scm_basic_list_push(ScmBasicList *list, ScmBasicListValue value)
{
  ScmBasicListEntry *entry;

  assert(list != NULL);

  entry = scm_memory_allocate(sizeof(ScmBasicListEntry));
  entry->value = value;
  entry->next = NULL;

  if (list->tail == NULL) {
    entry->before = NULL;
    list->head = list->tail = entry;
  }
  else {
    entry->before = list->tail;
    list->tail = list->tail->next = entry;
  }

  list->length++;
}

void
scm_basic_list_pop(ScmBasicList *list)
{
  assert(list != NULL);

  if (list->tail != NULL) {
    if (list->tail == list->head) {
      list->head = list->tail = NULL;
    }
    else {
      list->tail = list->tail->before;
      list->tail->next = NULL;
    }
    list->length--;
  }
}

void
scm_basic_list_unshift(ScmBasicList *list, ScmBasicListValue value)
{
  ScmBasicListEntry *entry;

  assert(list != NULL);

  entry = scm_memory_allocate(sizeof(ScmBasicListEntry));
  entry->value = value;
  entry->before = NULL;

  if (list->head == NULL) {
    entry->before = NULL;
    list->head = list->tail = entry;
  }
  else {
    entry->next = list->head;
    list->head = list->head->before = entry;
  }

  list->length++;
}

void
scm_basic_list_shift(ScmBasicList *list)
{
  assert(list != NULL);
  
  if (list->head != NULL) {
    if (list->head == list->tail) {
      list->head = list->tail = NULL;
    }
    else {
      list->head = list->head->next;
      list->head->before = NULL;
    }
    list->length--;
  }
}

ScmBasicListEntry *
scm_basic_list_head(ScmBasicList *list)
{
  assert(list != NULL);
  return list->head;
}

ScmBasicListEntry *
scm_basic_list_tail(ScmBasicList *list)
{
  assert(list != NULL);
  return list->tail;
}

size_t
scm_basic_list_length(ScmBasicList *list)
{
  assert(list != NULL);
  return list->length;
}

ScmBasicList *
scm_basic_list_construct()
{
  ScmBasicList *list;

  list = scm_memory_allocate(sizeof(ScmBasicList));
  list->head = NULL;
  list->tail = NULL;
  list->length = 0;

  return list;
}

