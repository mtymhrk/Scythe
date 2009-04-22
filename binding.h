#ifndef INCLUDE_BINDING_H__
#define INCLUDE_BINDING_H__

typedef struct ScmBindRefRec ScmBindRef;
typedef struct ScmBindTableRec ScmBindTable;

#include "basichash.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

#define SCM_BIND_REF(obj) (ScmBindRef *)(obj)

extern const ScmTypeInfo SCM_BIND_REF_TYPE_INFO;

void scm_bind_ref_initialize(ScmBindRef *ref, ScmObj sym, ScmObj val);
void scm_bind_ref_finalize(ScmBindRef *ref);
ScmBindRef *scm_bind_ref_construct(ScmObj sym, ScmObj val);
void scm_bind_ref_destruct(ScmBindRef *ref);
ScmObj scm_bind_ref_symbol(ScmBindRef *ref);
ScmObj scm_bind_ref_value(ScmBindRef *ref);
ScmObj scm_bind_ref_bind(ScmBindRef *ref, ScmObj obj);
void scm_bind_tbl_initialize(ScmBindTable *tbl, size_t size);
void scm_bind_tbl_finalize(ScmBindTable *tbl);
ScmBindTable *scm_bind_tbl_construct(size_t size);
void scm_bind_tbl_destruct(ScmBindTable *tbl);
void scm_bind_tbl_clear(ScmBindTable *tbl);
ScmBindRef *scm_bind_tbl_bind(ScmBindTable *tbl, ScmObj sym, ScmObj val);
void scm_bind_tbl_unbind(ScmBindTable *tbl, ScmObj sym);
ScmBindRef *scm_bind_tbl_lookup(ScmBindTable *tbl, ScmObj sym);

void scm_bind_ref_pretty_print(ScmObj obj, ScmOBuffer *obuffer);

#endif /* INCLUDE_BINDING_H__ */
