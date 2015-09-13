#ifndef INCLUDE_SYMBOL_H__
#define INCLUDE_SYMBOL_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/chashtbl.h"
#include "scythe/memory.h"
#include "scythe/encoding.h"


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

typedef struct ScmSymbolRec ScmSymbol;

struct ScmSymbolRec {
  ScmObjHeader header;
  ScmObj str;
};

extern ScmTypeInfo SCM_SYMBOL_TYPE_INFO;

#define SCM_SYMBOL(obj) ((ScmSymbol *)(obj))
#define SCM_SYMBOL_STR(obj) (SCM_SYMBOL(obj)->str)

ScmObj scm_symbol_P(ScmObj obj);
int scm_symbol_initialize(ScmObj sym, ScmObj str);
ScmObj scm_symbol_new(scm_mem_type_t mtype, ScmObj str);
ScmObj scm_make_symbol_from_cstr(const char *str, ScmEncoding *enc);
ScmObj scm_make_symbol_from_bin(const void *data, size_t size,
                                ScmEncoding *enc);
size_t scm_symbol_length(ScmObj sym);
size_t scm_symbol_bytesize(ScmObj sym);
ScmObj scm_symbol_string(ScmObj sym);
size_t scm_symbol_hash_value(ScmObj sym);
int scm_symbol_cmp(ScmObj s1, ScmObj s2, int *rslt);
ScmObj scm_symbol_eq_P(ScmObj sym1, ScmObj sym2);
ScmObj scm_symbol_eq_P_lst(ScmObj lst);
ScmObj scm_string_to_symbol(ScmObj str);
char *scm_symbol_to_cstr(ScmObj sym, char *cstr, size_t size);
int scm_symbol_eq_cstr(ScmObj sym, const char *str, ScmEncoding *enc, bool *cmp);
int scm_symbol_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_symbol_gc_initialize(ScmObj obj);
int scm_symbol_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_symbol_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_SYMBOL_TYPE_INFO) ? true : false;
}

static inline bool
scm_symbol_eq_p(ScmObj sym1, ScmObj sym2)
{
  scm_assert(scm_symbol_p(sym1));
  scm_assert(scm_symbol_p(sym2));
  return scm_obj_same_instance_p(sym1, sym2);
}

static inline ScmObj
scm_symbol_to_string(ScmObj sym)
{
  scm_assert(scm_symbol_p(sym));
  return scm_symbol_string(sym);
}


/*******************************************************************/
/*  Symbol Table                                                   */
/*******************************************************************/

typedef struct ScmSymTblRec ScmSymTbl;

struct ScmSymTblRec {
  ScmObjHeader header;
  ScmCHashTbl *tbl;
};

#define SCM_SYMTBL(obj) ((ScmSymTbl *)(obj))

extern ScmTypeInfo SCM_SYMTBL_TYPE_INFO;

int scm_symtbl_initialize(ScmObj tbl);
void scm_symtbl_finalize(ScmObj tbl);
ScmObj scm_symtbl_new(scm_mem_type_t mtype);
ScmObj scm_symtbl_symbol(ScmObj tbl, ScmObj str);
void scm_symtbl_clean(ScmObj tbl);
void scm_symtbl_gc_initialize(ScmObj obj);
void scm_symtbl_gc_finalize(ScmObj obj);
int scm_symtbl_gc_accept(ScmObj obj, ScmGCRefHandler handler);
int scm_symtbl_gc_accept_weak(ScmObj obj, ScmGCRefHandler handler);

#endif /* INCLUDE_SYMBOL_H__ */
