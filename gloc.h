#ifndef INCLUDE_GLOC_H__
#define INCLUDE_GLOC_H__

typedef struct ScmGLocRec ScmGLoc;

#define SCM_GLOC(obj) ((ScmGLoc *)(obj))

typedef struct ScmGLocTblRec ScmGLocTbl;

#define SCM_GLOCTBL(obj) ((ScmGLocTbl *)(obj))

#include "object.h"
#include "memory.h"

extern ScmTypeInfo SCM_GLOC_TYPE_INFO;

struct ScmGLocRec {
  ScmObjHeader header;
  ScmObj sym;
  ScmObj val;
};

inline static void
scm_gloc_bind(ScmObj gloc, ScmObj val)
{
  SCM_OBJ_ASSERT_TYPE(gloc, &SCM_GLOC_TYPE_INFO);
  assert(SCM_OBJ_IS_NOT_NULL(val));

  SCM_SETQ(SCM_GLOC(gloc)->val, val);
}

inline static void
scm_gloc_unbind(ScmObj gloc)
{
  SCM_OBJ_ASSERT_TYPE(gloc, &SCM_GLOC_TYPE_INFO);

  SCM_SETQ(SCM_GLOC(gloc)->val, SCM_OBJ_NULL);
}

inline static ScmObj
scm_gloc_symbol(ScmObj gloc)
{
  SCM_OBJ_ASSERT_TYPE(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->sym;
}

inline static ScmObj
scm_gloc_value(ScmObj gloc)
{
  SCM_OBJ_ASSERT_TYPE(gloc, &SCM_GLOC_TYPE_INFO);

  return SCM_GLOC(gloc)->val;
}

void scm_gloc_initialize(ScmObj gloc, ScmObj sym, ScmObj val);
ScmObj scm_gloc_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj sym);

void scm_gloc_gc_initialize(ScmObj obj, ScmObj mem);
int scm_gloc_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


extern ScmTypeInfo SCM_GLOCTBL_TYPE_INFO;

struct ScmGLocTblRec {
  ScmObjHeader header;
  ScmObj tbl;
};

void scm_gloctbl_initialize(ScmObj tbl);
ScmObj scm_gloctbl_new(SCM_MEM_ALLOC_TYPE_T mtype);

void scm_gloctbl_gc_initialize(ScmObj obj, ScmObj mem);
int scm_gloctbl_gc_accept(ScmObj obj, ScmObj mem,
                           ScmGCRefHandlerFunc handler);

#endif /* INCLUDE_GLOC_H__ */
