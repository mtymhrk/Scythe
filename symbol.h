#ifndef INCLUDE_SYMBOL_H__
#define INCLUDE_SYMBOL_H__

#include <stdbool.h>

typedef struct ScmSymbolRec ScmSymbol;
typedef struct ScmSymTableRec ScmSymTable;

#define SCM_SYMBOL(obj) ((ScmSymbol *)(obj))
#define SCM_SYMTABLE(obj) ((ScmSymTable *)obj)

#include "object.h"
#include "basichash.h"

extern ScmTypeInfo SCM_SYMBOL_TYPE_INFO;
extern ScmTypeInfo SCM_SYMTABLE_TYPE_INFO;

struct ScmSymbolRec {
  ScmObjHeader header;
  ScmObj table;
  ScmObj str;
};

#define SCM_SYMBOL_TABLE(obj) (SCM_SYMBOL(obj)->table)
#define SCM_SYMBOL_STR(obj) (SCM_SYMBOL(obj)->str)

struct ScmSymTableRec {
  ScmObjHeader header;
  ScmBasicHashTable *tbl;
};

#define SCM_SYMTABLE_TBL(obj) (SCM_SYMTABLE(obj)->tbl)

void scm_symbol_initialize(ScmObj sym, ScmObj table, ScmObj str);
ScmObj scm_symbol_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj str);
ScmObj scm_symbol_instance(ScmObj str);
bool scm_symbol_is_symbol(ScmObj obj);
size_t scm_symbol_length(ScmObj sym);
ScmObj scm_symbol_string(ScmObj sym);
size_t scm_symbol_hash_value(ScmObj sym);
void scm_symbol_gc_initialize(ScmObj obj, ScmObj mem);
int scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

void scm_symtable_initialize(ScmObj obj);
void scm_symtable_finalize(ScmObj obj);
ScmObj scm_symtable_new(SCM_MEM_ALLOC_TYPE_T mtype);
bool scm_symtable_is_symtable(ScmObj obj);
ScmObj scm_symtable_symbol(ScmObj symtbl, ScmObj str);
void scm_symtable_add(ScmObj symtbl, ScmObj sym);
void scm_symtable_delete(ScmObj symtbl, ScmObj str);
void scm_symtable_gc_initialize(ScmObj obj, ScmObj mem);
void scm_symtable_gc_finalize(ScmObj obj);
int scm_symtable_gc_accept(ScmObj obj, ScmObj mem,
                           ScmGCRefHandlerFunc handler);
int scm_symtable_gc_accept_week(ScmObj obj, ScmObj mem,
                                ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_SYMBOL_H__ */
