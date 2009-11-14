#ifndef INCLUDE_OBJECT_H__
#define INCLUDE_OBJECT_H__

#include <stdbool.h>
#include <stdint.h>
#include <assert.h>

typedef struct ScmObjHeaderRec ScmObjHeader;
typedef struct ScmMMObjRec ScmMMObj;
typedef uintptr_t ScmObj;
typedef struct ScmTypeInfoRec ScmTypeInfo;
typedef struct ScmGCRefItrRec ScmGCRefItr;
typedef ScmObj *ScmRef;
typedef const ScmObj *ScmCRef;

#define SCM_MMOBJ(obj) ((ScmMMObj *)(obj))
#define SCM_OBJ(obj) ((ScmObj)(obj))

#define SCM_OBJ_NULL 0u
#define SCM_OBJ_INIT SCM_OBJ_NULL
#define SCM_OBJ_IS_NULL(obj) ((obj) == SCM_OBJ_NULL)
#define SCM_OBJ_IS_NOT_NULL(obj) ((obj) != SCM_OBJ_NULL)
#define SCM_OBJ_IS_SAME_INSTANCE(obj1, obj2) \
  (SCM_OBJ_IS_NOT_NULL(obj1) && ((obj1) == (obj2)))


/* 将来的に write barrier を挿入しなければならないときに使用する */
/*   SCM_SETQ_PRIM: write barrier を挿入しない代入        */
/*   SCM_SETQ     : write barrier を挿入する代入          */
#define SCM_SETQ_PRIM(obj, val)                 \
  do {                                          \
    ScmObj tmp__ = (val); (obj) = tmp__;        \
  } while(0)
#define SCM_SETQ(obj, val) SCM_SETQ_PRIM(obj, val)


#define SCM_REF_MAKE(obj) ((ScmRef)&(obj))
#define SCM_REF_MAKE_FROM_PTR(ptr) ((ScmRef)(ptr))
#define SCM_REF_TO_PTR(ref) ((ScmObj *)(ref))
#define SCM_REF_NULL ((ScmRef)NULL)
#define SCM_REF_OBJ(ref) (*((ScmObj *)(ref)))
#define SCM_CREF_OBJ(ref) (*((const ScmObj *)(ref)))
#define SCM_REF_UPDATE(ref, obj) (*((ScmObj *)(ref)) = SCM_OBJ(obj))

#include "obuffer.h"

typedef void (*ScmPrettyPrintFunction)(ScmObj obj,
                                       ScmOBuffer *obuffer);
typedef void (*ScmGCInitializeFunc)(ScmObj obj, ScmObj mem);
typedef void (*ScmGCFinalizeFunc)(ScmObj obj);
typedef int (*ScmGCRefHandlerFunc)(ScmObj mem, ScmObj obj, ScmRef child);
typedef int (*ScmGCAcceptFunc)(ScmObj obj,
                               ScmObj mem, ScmGCRefHandlerFunc handler);

#define SCM_GC_CALL_REF_HANDLER(handler, obj, child, mem) \
  (handler(mem, obj, SCM_REF_MAKE(child)))
#define SCM_GC_IS_REF_HANDLER_FAILURE(rslt) ((rslt) < 0)
#define SCM_GC_REF_HANDLER_VAL_INIT 0


struct ScmTypeInfoRec {
  ScmPrettyPrintFunction pp_func;
  size_t obj_size;
  ScmGCInitializeFunc gc_ini_func;
  ScmGCFinalizeFunc gc_fin_func;
  ScmGCAcceptFunc gc_accept_func;
  ScmGCAcceptFunc gc_accept_func_weak;
};


#define SCM_TYPE_INFO_IS_SAME(t1, t2) ((t1) == (t2))
#define SCM_TYPE_INFO_PP_FUNC(type) ((type)->pp_func)
#define SCM_TYPE_INFO_OBJ_SIZE(type) ((type)->obj_size)
#define SCM_TYPE_INFO_GC_INI(type) ((type)->gc_ini_func)
#define SCM_TYPE_INFO_HAS_GC_INI(type) (SCM_TYPE_INFO_GC_INI(type) != NULL)
#define SCM_TYPE_INFO_GC_FIN(type) ((type)->gc_fin_func)
#define SCM_TYPE_INFO_HAS_GC_FIN(type) (SCM_TYPE_INFO_GC_FIN(type) != NULL)
#define SCM_TYPE_INFO_GC_ACCEPT_FUNC(type) ((type)->gc_accept_func)
#define SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC(type) \
  (SCM_TYPE_INFO_GC_ACCEPT_FUNC(type) != NULL)
#define SCM_TYPE_INFO_GC_ACCEPT_FUNC_WEAK(type) ((type)->gc_accept_func_weak)
#define SCM_TYPE_INFO_HAS_WEAK_REF(type) \
  (SCM_TYPE_INFO_GC_ACCEPT_FUNC_WEAK(type) != NULL)


struct ScmObjHeaderRec {
  ScmTypeInfo *type;
};

struct ScmMMObjRec {
  ScmObjHeader header;
};

extern ScmTypeInfo *SCM_OBJ_TAG2TYPE_TBL[];

#define SCM_OBJ_TAG(obj) ((uintptr_t)(obj) & 0x07u)
#define SCM_OBJ_IS_MEM_MANAGED(obj) (SCM_OBJ_TAG(obj) == 0x00u)
#define SCM_OBJ_HAS_PTR_TO_TYPE_INFO(obj) (SCM_OBJ_TAG(obj) == 0x00u)

#define SCM_OBJ_TYPE(obj)                                               \
  (SCM_OBJ_HAS_PTR_TO_TYPE_INFO(obj) ?                                  \
   (SCM_MMOBJ(obj)->header.type) : SCM_OBJ_TAG2TYPE_TBL[SCM_OBJ_TAG(obj)])
#define SCM_OBJ_IS_TYPE(obj, type) \
  (SCM_TYPE_INFO_IS_SAME(SCM_OBJ_TYPE(obj), type))
#define SCM_OBJ_PP_FUNC(obj) (SCM_TYPE_INFO_PP_FUNC(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_SIZE(obj) (SCM_TYPE_INFO_OBJ_SIZE(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_GC_INI(obj) (SCM_TYPE_INFO_GC_INI(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_HAS_GC_INI(obj) (SCM_TYPE_INFO_HAS_GC_INI(SCM_OBJ_TYPE(obj))
#define SCM_OBJ_GC_FIN(obj) (SCM_TYPE_INFO_GC_FIN(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_HAS_GC_FIN(obj) (SCM_TYPE_INFO_HAS_GC_FIN(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_GC_ACCEPT_FUNC(obj) \
  (SCM_TYPE_INFO_GC_ACCEPT_FUNC(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_HAS_GC_ACCEPT_FUNC(obj) \
  (SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_GC_ACCEPT_FUNC_WEAK(obj) \
  (SCM_TYPE_INFO_GC_ACCEPT_FUNC_WEAK(SCM_OBJ_TYPE(obj)))
#define SCM_OBJ_HAS_WEAK_REF(obj) \
  (SCM_TYPE_INFO_HAS_WEAK_REF(SCM_OBJ_TYPE(obj)))


void scm_obj_init(ScmObj obj, ScmTypeInfo *type);
void scm_obj_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
int scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2);


#define SCM_OBJ_ASSERT_TYPE(obj, type) \
  assert(SCM_OBJ_IS_NOT_NULL(obj));    \
  assert(SCM_OBJ_IS_TYPE(obj, type));

#define SCM_OBJ_ASSERT_TYPE_ACCEPT_NULL(obj, type)      \
  assert(SCM_OBJ_IS_NULL(obj) || SCM_OBJ_IS_TYPE(obj, type))


#endif /* INCLUDE_OBJECT_H__ */
