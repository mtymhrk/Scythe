#ifndef INCLUDE_SYMBOL_H__
#define INCLUDE_SYMBOL_H__

#include <stdbool.h>

typedef struct ScmSymbolRec ScmSymbol;

#define SCM_SYMBOL(obj) ((ScmSymbol *)(obj))

#include "object.h"

ScmSymbol *scm_symbol_construct(const char *str);
ScmSymbol *scm_symbol_instance(const char *name);
char *scm_symbol_name(const ScmSymbol *symbol);
size_t scm_symbol_length(const ScmSymbol *symbol);
bool scm_symbol_is_symbol(ScmObj obj);

#endif /* INCLUDE_SYMBOL_H__ */
