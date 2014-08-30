#ifndef INCLUDE_SYMBOL_H__
#define INCLUDE_SYMBOL_H__

#include <stdbool.h>

typedef struct ScmSymbolRec ScmSymbol;

#define SCM_SYMBOL(obj) ((ScmSymbol *)(obj))

typedef struct ScmSymTblRec ScmSymTbl;

#define SCM_SYMTBL(obj) ((ScmSymTbl *)(obj))

#include "object.h"
#include "api_type.h"
#include "chashtbl.h"

extern ScmTypeInfo SCM_SYMBOL_TYPE_INFO;

struct ScmSymbolRec {
  ScmObjHeader header;
  ScmObj str;
};

#define SCM_SYMBOL_STR(obj) (SCM_SYMBOL(obj)->str)

int scm_symbol_initialize(ScmObj sym, ScmObj str);
ScmObj scm_symbol_new(SCM_MEM_TYPE_T mtype, ScmObj str);
ScmObj scm_symbol_instance(ScmObj str);
size_t scm_symbol_length(ScmObj sym);
ScmObj scm_symbol_string(ScmObj sym);
size_t scm_symbol_hash_value(ScmObj sym);
int scm_symbol_cmp(ScmObj s1, ScmObj s2, int *rslt);
int scm_symbol_obj_print(ScmObj obj, ScmObj port, bool ext_rep);
void scm_symbol_gc_initialize(ScmObj obj, ScmObj mem);
int scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


extern ScmTypeInfo SCM_SYMTBL_TYPE_INFO;

struct ScmSymTblRec {
  ScmObjHeader header;
  ScmCHashTbl *tbl;
};

int scm_symtbl_initialize(ScmObj tbl);
void scm_symtbl_finalize(ScmObj tbl);
ScmObj scm_symtbl_new(SCM_MEM_TYPE_T mtype);
ScmObj scm_symtbl_symbol(ScmObj tbl, ScmObj str);
void scm_symtbl_clean(ScmObj tbl);
void scm_symtbl_gc_initialize(ScmObj obj, ScmObj mem);
void scm_symtbl_gc_finalize(ScmObj obj);
int scm_symtbl_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);
int scm_symtbl_gc_accept_weak(ScmObj obj, ScmObj mem,
                              ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_SYMBOL_H__ */
