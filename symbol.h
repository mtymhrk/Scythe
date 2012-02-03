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

struct ScmSymbolRec {
  ScmObjHeader header;
  ScmObj str;
};

#define SCM_SYMBOL_STR(obj) (SCM_SYMBOL(obj)->str)

void scm_symbol_initialize(ScmObj sym, ScmObj str);
ScmObj scm_symbol_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj str);
ScmObj scm_symbol_instance(ScmObj str);
bool scm_symbol_is_symbol(ScmObj obj);
size_t scm_symbol_length(ScmObj sym);
ScmObj scm_symbol_string(ScmObj sym);
size_t scm_symbol_hash_value(ScmObj sym);
void scm_symbol_gc_initialize(ScmObj obj, ScmObj mem);
int scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_SYMBOL_H__ */
