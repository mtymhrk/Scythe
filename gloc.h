#ifndef INCLUDE_GLOC_H__
#define INCLUDE_GLOC_H__

typedef struct ScmGLocRec ScmGLoc;

#define SCM_GLOC(obj) ((ScmGLoc *)(obj))

typedef struct ScmGLocTblRec ScmGLocTbl;

#define SCM_GLOCTBL(obj) ((ScmGLocTbl *)(obj))

#include "object.h"
#include "memory.h"
#include "chashtbl.h"

extern ScmTypeInfo SCM_GLOC_TYPE_INFO;

struct ScmGLocRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
};

static inline void
scm_gloc_bind(ScmObj gloc, ScmObj val)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);
  assert(scm_obj_not_null_p(val));

  SCM_SETQ(SCM_GLOC(gloc)->val, val);
}

static inline void
scm_gloc_unbind(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  SCM_SETQ(SCM_GLOC(gloc)->val, SCM_OBJ_NULL);
}

static inline ScmObj
scm_gloc_symbol(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->sym;
}

static inline ScmObj
scm_gloc_value(ScmObj gloc)
{
  scm_assert_obj_type(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->val;
}

void scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val);
ScmObj scm_gloc_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj sym);

void scm_gloc_gc_initialize(ScmObj obj, ScmObj mem);
int scm_gloc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


extern ScmTypeInfo SCM_GLOCTBL_TYPE_INFO;

struct ScmGLocTblRec {
  ScmObjHeader header;
  ScmCHashTbl *tbl;
};

void scm_gloctbl_initialize(ScmObj tbl);
void scm_gloctbl_finalize(ScmObj tbl);
ScmObj scm_gloctbl_new(SCM_MEM_ALLOC_TYPE_T mtype);

int scm_gloctbl_find(ScmObj tbl, ScmObj sym, ScmRef gloc);
ScmObj scm_gloctbl_gloc(ScmObj tbl, ScmObj sym);
ScmObj scm_gloctbl_bind(ScmObj tbl, ScmObj sym, ScmObj val);

void scm_gloctbl_gc_initialize(ScmObj obj, ScmObj mem);
void scm_gloctbl_gc_finalize(ScmObj obj);
int scm_gloctbl_gc_accept(ScmObj obj, ScmObj mem,
                           ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_GLOC_H__ */
