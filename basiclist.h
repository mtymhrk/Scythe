#ifndef INCLUDE_BASICLIST_H__
#define INCLUDE_BASICLIST_H__

typedef struct ScmBasicListRec ScmBasicList;
typedef struct ScmBasicListEntryRec ScmBasicListEntry;
typedef void *ScmBasicListValue;

#define SCM_BASIC_LIST_VALUE(v) ((ScmBasicListValue)(v))

ScmBasicListValue scm_basic_list_entry_value(ScmBasicListEntry *entry);
void scm_basic_list_push(ScmBasicList *list, ScmBasicListValue value);
void scm_basic_list_pop(ScmBasicList *list);
void scm_basic_list_unshift(ScmBasicList *list, ScmBasicListValue value);
void scm_basic_list_shift(ScmBasicList *list);
ScmBasicListEntry *scm_basic_list_head(ScmBasicList *list);
ScmBasicListEntry *scm_basic_list_tail(ScmBasicList *list);
size_t scm_basic_list_length(ScmBasicList *list);
ScmBasicList *scm_basic_list_construct();
void scm_basic_list_cleanup(ScmBasicList *list);
void scm_basic_list_destruct(ScmBasicList *list);

#endif /* INCLUDE_BASICLIST_H__ */
