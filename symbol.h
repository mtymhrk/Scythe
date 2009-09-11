#ifndef INCLUDE_SYMBOL_H__
#define INCLUDE_SYMBOL_H__

#include <stdbool.h>

typedef struct ScmSymbolRec ScmSymbol;

#define SCM_SYMBOL(obj) ((ScmSymbol *)(obj))

#include "object.h"

extern ScmTypeInfo SCM_SYMBOL_TYPE_INFO;

void scm_symbol_initialize(ScmSymbol *symbol, const char *str);
ScmSymbol *scm_symbol_construct(const char *str);
void scm_symbol_destruct(ScmSymbol *symbol);
ScmSymbol *scm_symbol_instance(const char *name);
char *scm_symbol_name(const ScmSymbol *symbol);
size_t scm_symbol_length(const ScmSymbol *symbol);
bool scm_symbol_is_symbol(ScmObj obj);
void scm_symbol_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
void scm_symbol_gc_finalize(ScmObj obj);

#endif /* INCLUDE_SYMBOL_H__ */
