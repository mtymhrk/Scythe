#ifndef INCLUDE_BASICLIST_H__
#define INCLUDE_BASICLIST_H__

typedef struct ScmBasicListRec ScmBasicList;
typedef struct ScmBasicListEntryRec ScmBasicListEntry;
typedef void *ScmBasicListValue;

struct ScmBasicListEntryRec {
  ScmBasicListEntry *next;
  ScmBasicListEntry *before;
  ScmBasicListValue value;
};

#define SCM_BASIC_LIST_VALUE(v) ((ScmBasicListValue)(v))
#define SCM_BASIC_LIST_ENTRY_VALUE(entry) ((entry)->value)


void scm_basic_list_push(ScmBasicList *list, ScmBasicListValue value);
void scm_basic_list_pop(ScmBasicList *list);
void scm_basic_list_unshift(ScmBasicList *list, ScmBasicListValue value);
void scm_basic_list_shift(ScmBasicList *list);
ScmBasicListEntry *scm_basic_list_head(ScmBasicList *list);
ScmBasicListEntry *scm_basic_list_tail(ScmBasicList *list);
size_t scm_basic_list_length(ScmBasicList *list);
ScmBasicList *scm_basic_list_new();
void scm_basic_list_cleanup(ScmBasicList *list);
void scm_basic_list_end(ScmBasicList *list);

#endif /* INCLUDE_BASICLIST_H__ */
