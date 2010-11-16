#include "assert.h"

#include "memory.h"
#include "basiclist.h"

struct ScmBasicListRec {
  ScmBasicListEntry *head;
  ScmBasicListEntry *tail;
  size_t length;
};

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
    ScmBasicListEntry *p = list->tail;

    if (list->tail == list->head) {
      list->head = list->tail = NULL;
    }
    else {
      list->tail = list->tail->before;
      list->tail->next = NULL;
    }
    scm_memory_release(p);
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
    ScmBasicListEntry *p = list->head;

    if (list->head == list->tail) {
      list->head = list->tail = NULL;
    }
    else {
      list->head = list->head->next;
      list->head->before = NULL;
    }
    scm_memory_release(p);
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

void
scm_basic_list_cleanup(ScmBasicList *list)
{
  ScmBasicListEntry *p;

  assert(list != NULL);

  p = list->head;
  while (p != NULL) {
    ScmBasicListEntry *next = p->next;
    scm_memory_release(p);
    p = next;
  }

  list->head = NULL;
  list->tail = NULL;
  list->length = 0;
}

ScmBasicList *
scm_basic_list_new()
{
  ScmBasicList *list;

  list = scm_memory_allocate(sizeof(ScmBasicList));
  list->head = NULL;
  list->tail = NULL;
  list->length = 0;

  return list;
}

void
scm_basic_list_end(ScmBasicList *list)
{
  assert(list != NULL);

  scm_basic_list_cleanup(list);
  scm_memory_release(list);
}
