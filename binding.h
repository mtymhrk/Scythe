#ifndef INCLUDE_BINDING_H__
#define INCLUDE_BINDING_H__

typedef struct ScmBindRefRec ScmBindRef;
typedef struct ScmBindTableRec ScmBindTable;

#include "basichash.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

#define SCM_BIND_REF(obj) ((ScmBindRef *)(obj))

extern ScmTypeInfo SCM_BIND_REF_TYPE_INFO;

struct ScmBindRefRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
};

#define SCM_BIND_REF_SYM(obj) (SCM_BIND_REF(obj)->sym)
#define SCM_BIND_REF_VAL(obj) (SCM_BIND_REF(obj)->val)

struct ScmBindTableRec {
  ScmBasicHashTable *table;
  ScmObj work; /* root of GC */
};

void scm_bind_ref_initialize(ScmObj bref, ScmObj sym, ScmObj val);
void scm_bind_ref_finalize(ScmBindRef *ref);
ScmObj scm_bind_ref_new(SCM_MEM_ALLOC_TYPE_T mtype,
                              ScmObj sym, ScmObj val);
ScmObj scm_bind_ref_symbol(ScmObj bref);
ScmObj scm_bind_ref_value(ScmObj bref);
void scm_bind_ref_bind(ScmObj bref, ScmObj obj);
void scm_bind_tbl_initialize(ScmBindTable *tbl, size_t size);
void scm_bind_tbl_finalize(ScmBindTable *tbl);
ScmBindTable *scm_bind_tbl_new(size_t size);
void scm_bind_tbl_end(ScmBindTable *tbl);
void scm_bind_tbl_clear(ScmBindTable *tbl);
ScmObj scm_bind_tbl_bind(ScmBindTable *tbl, ScmObj sym, ScmObj val);
void scm_bind_tbl_unbind(ScmBindTable *tbl, ScmObj sym);
ScmObj scm_bind_tbl_lookup(ScmBindTable *tbl, ScmObj sym);

#endif /* INCLUDE_BINDING_H__ */
