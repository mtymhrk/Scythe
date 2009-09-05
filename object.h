#ifndef INCLUDE_OBJECT_H__
#define INCLUDE_OBJECT_H__

#include <stdbool.h>

typedef struct ScmObjHeaderRec ScmObjHeader;
typedef struct ScmAtomRec ScmAtom;
typedef ScmAtom *ScmObj;
typedef struct ScmTypeInfoRec ScmTypeInfo;
typedef struct ScmGCRefItrRec ScmGCRefItr;

#define SCM_ATOM(obj) ((ScmAtom *)(obj))
#define SCM_OBJ(obj) ((ScmObj)(obj))

typedef enum {
  SCM_OBJ_TYPE_FORWARD,
  SCM_OBJ_TYPE_PAIR,
  SCM_OBJ_TYPE_STRING,
  SCM_OBJ_TYPE_SYMBOL,
  SCM_OBJ_TYPE_NIL,
  SCM_OBJ_TYPE_INTEGER,
  SCM_OBJ_TYPE_VECTOR,
  SCM_OBJ_TYPE_BOOL,
  SCM_OBJ_TYPE_CHAR,
  SCM_OBJ_TYPE_EOF,
  SCM_OBJ_TYPE_PORT,
  SCM_OBJ_TYPE_PRIM_PROC,
  SCM_OBJ_TYPE_BIND_REF,
  SCM_OBJ_TYPE_VM,
  SCM_OBJ_NR_TYPE,
} SCM_OBJ_TYPE_T;

extern const ScmTypeInfo const *SCM_TYPE_INFO_TBL[SCM_OBJ_NR_TYPE];

#include "obuffer.h"
#include "memory.h"

typedef void (*ScmPrettyPrintFunction)(ScmObj obj,
				       ScmOBuffer *obuffer);
typedef void (*ScmGCInitializeFunc)(ScmObj obj, ScmMem *mem);
typedef void (*ScmGCFinalizeFunc)(ScmObj obj);
typedef int (*ScmGCRefHandlerFunc)(ScmMem *vm, ScmObj obj, ScmRef child);
typedef int (*ScmGCAcceptFunc)(ScmObj obj,
                               ScmMem *vm, ScmGCRefHandlerFunc handler);

#define SCM_GC_CALL_REF_HANDLER(handler, obj, child, mem) \
  (handler(mem, obj, SCM_REF_MAKE(child)))
#define SCM_GC_IS_REF_HANDLER_FAILURE(rslt) ((rslt) < 0)
#define SCM_GC_REF_HANDLER_VAL_INIT 0

struct ScmObjHeaderRec {
  SCM_OBJ_TYPE_T type;
};

struct ScmAtomRec {
  ScmObjHeader header;
};

struct ScmTypeInfoRec {
  SCM_OBJ_TYPE_T type;
  ScmPrettyPrintFunction pp_func;
  size_t obj_size;
  ScmGCInitializeFunc gc_ini_func;
  ScmGCFinalizeFunc gc_fin_func;
  ScmGCAcceptFunc gc_accept_func;
  bool has_weak_ref;
};

/* TODO: replace scm_obj_type() to following macro function */
#define SCM_OBJ_TYPE(obj) ((obj)->header.type)

#define SCM_TYPE_INFO_PP(type) (SCM_TYPE_INFO_TBL[(type)]->pp_func)
#define SCM_TYPE_INFO_OBJ_SIZE(type) (SCM_TYPE_INFO_TBL[(type)]->obj_size)
#define SCM_TYPE_INFO_GC_INI(type) (SCM_TYPE_INFO_TBL[(type)]->gc_ini_func)
#define SCM_TYPE_INFO_HAS_GC_INI(type) (SCM_TYPE_INFO_GC_INI(type) != NULL)
#define SCM_TYPE_INFO_GC_FIN(type) (SCM_TYPE_INFO_TBL[(type)]->gc_fin_func)
#define SCM_TYPE_INFO_HAS_GC_FIN(type) (SCM_TYPE_INFO_GC_FIN(type) != NULL)
#define SCM_TYPE_INFO_GC_ACCEPT_FUNC(type)      \
  (SCM_TYPE_INFO_TBL[(type)]->gc_accept_func)
#define SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC(type)  \
  (SCM_TYPE_INFO_GC_ACCEPT_FUNC(type) != NULL)
#define SCM_TYPE_INFO_HAS_WEAK_REF(type) \
  (SCM_TYPE_INFO_TBL[(type)]->has_weak_ref)

#define SCM_TYPE_INFO_OBJ_SIZE_FROM_OBJ(obj) \
  SCM_TYPE_INFO_OBJ_SIZE(scm_obj_type(obj))
#define SCM_TYPE_INFO_GC_INI_FROM_OBJ(obj) \
  SCM_TYPE_INFO_GC_INI(scm_obj_type(obj))
#define SCM_TYPE_INFO_HAS_GC_INI_FROM_OBJ(obj) \
  SCM_TYPE_INFO_HAS_GC_INI(scm_obj_type(obj))
#define SCM_TYPE_INFO_GC_FIN_FROM_OBJ(obj) \
  SCM_TYPE_INFO_GC_FIN(scm_obj_type(obj))
#define SCM_TYPE_INFO_HAS_GC_FIN_FROM_OBJ(obj) \
  SCM_TYPE_INFO_HAS_GC_FIN(scm_obj_type(obj))
#define SCM_TYPE_INFO_GC_ACCEPT_FUNC_FROM_OBJ(obj) \
  SCM_TYPE_INFO_GC_ACCEPT_FUNC(scm_obj_type(obj))
#define SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC_FROM_OBJ(obj) \
  SCM_TYPE_INFO_HAS_GC_ACCEPT_FUNC(scm_obj_type(obj))
#define SCM_TYPE_INFO_HAS_WEAK_REF_FROM_OBJ(obj) \
  SCM_TYPE_INFO_HAS_WEAK_REF(scm_obj_type(obj))

void scm_obj_init(ScmObj obj, SCM_OBJ_TYPE_T type);
SCM_OBJ_TYPE_T scm_obj_type(ScmObj obj);
void scm_obj_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
int scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2);
bool smc_obj_is_valid_type_info_tbl(void);

#endif /* INCLUDE_OBJECT_H__ */
