#include <unistd.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "printer.h"
#include "hash.h"
#include "symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256

struct ScmSymbolRec {
  ScmObjHeader header;
  char* name;
  size_t length;
};

static ScmHashTable *symbol_table = NULL;

static void
scm_symbol_pretty_print(ScmObj obj, ScmPrinter *printer)
{
  assert(obj != NULL); assert(scm_symbol_is_symbol(obj));
  assert(printer != NULL);

  scm_printer_concatenate_string(printer, SCM_SYMBOL(obj)->name);
}

static unsigned int
scm_symbol_table_hash_func(ScmHashKey key)
{
  char *name = (char *)key;
  unsigned int hash;
  char *p;

  assert(name != NULL);

  hash = 0;
  for (p = name; *p != '\0'; p++)
    hash = (hash << 5) - hash + (unsigned char)*p;

  return hash;
}

static bool
scm_symbol_table_comp_func(ScmHashKey key1, ScmHashKey key2)
{
  char *name1 = (char *)key1;
  char *name2 = (char *)key2;

  return (strcmp(name1, name2) == 0) ? true : false;
}

ScmSymbol *
scm_symbol_construct(const char *str)
{
  ScmSymbol *symbol = NULL;

  assert(str != NULL);

  symbol = (ScmSymbol *)scm_memory_allocate(sizeof(ScmSymbol));
  scm_obj_init(SCM_OBJ(symbol), SCM_OBJ_TYPE_SYMBOL, scm_symbol_pretty_print);
  symbol->length = strlen(str);
  symbol->name = scm_memory_allocate(symbol->length + 1);
  strncpy(symbol->name, str, symbol->length + 1);

  return symbol;
}

char *
scm_symbol_name(const ScmSymbol *symbol)
{
  assert(symbol != NULL);

  return symbol->name;
}

size_t
scm_symbol_length(const ScmSymbol *symbol)
{
  assert(symbol != NULL);

  return symbol->length;
}

bool
scm_symbol_is_symbol(ScmObj obj)
{
  assert(obj != NULL);

  return (scm_obj_type(obj) == SCM_OBJ_TYPE_SYMBOL);
}

ScmSymbol *
scm_symbol_instance(const char *name)
{
  ScmHashEntry *entry;
  ScmSymbol *symbol;

  if (symbol_table == NULL)
    symbol_table = scm_hash_construct(SCM_SYMBOL_TABLE_SIZE,
				      scm_symbol_table_hash_func,
				      scm_symbol_table_comp_func);

  entry = scm_hash_get(symbol_table, SCM_HASH_KEY(name));
  if (entry == NULL) {
    symbol = scm_symbol_construct(name);
    scm_hash_put(symbol_table, SCM_HASH_KEY(name), SCM_HASH_VALUE(symbol));
  } else 
    symbol = (ScmSymbol *)SCM_HASH_ENTRY_VALUE(entry);

  return symbol;
}
