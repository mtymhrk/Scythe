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
  SCM_OBJ_NR_TYPE
} SCM_OBJ_TYPE_T;

extern const ScmTypeInfo const *SCM_TYPE_INFO_TBL[SCM_OBJ_NR_TYPE];

#include "obuffer.h"

typedef void (*ScmPrettyPrintFunction)(ScmObj obj,
				       ScmOBuffer *obuffer);
typedef void (*ScmGCFinalizeFunc)(ScmObj obj);
typedef ScmGCRefItr (*ScmGCRefItrFunc)(ScmObj obj);

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
  ScmGCFinalizeFunc gc_fin_func;
  ScmGCRefItrFunc gc_ref_itr_func;
};

#define SCM_TYPE_INFO_PP(type) (SCM_TYPE_INFO_TBL[(type)]->pp_func)
#define SCM_TYPE_INFO_OBJ_SIZE(type) (SCM_TYPE_INFO_TBL[(type)]->obj_size)
#define SCM_TYPE_INFO_GC_FIN(type) (SCM_TYPE_INFO_TBL[(type)]->gc_fin_func)
#define SCM_TYPE_INFO_HAS_GC_FIN(type) (SCM_TYPE_INFO_GC_FIN(type) != NULL)
#define SCM_TYPE_INFO_GC_REF_ITR(type) \
  (SCM_TYPE_INFO_TBL[(type)]->gc_ref_itr_func)
#define SCM_TYPE_INFO_HAS_REF_ITR(type) (SCM_TYPE_INFO_GC_REF_ITR(type) != NULL)

struct ScmGCRefItrRec {
  ScmObj *ptr;
  ScmObj src;
  ScmGCRefItr (*next)(const ScmGCRefItr *itr);
};

#define SCM_GC_REF_ITR_NEXT(itr) ((itr).next(&(itr)))
#define SCM_GC_REF_ITR_IS_END(itr) ((itr).ptr == NULL)
#define SCM_GC_REF_ITR_REF(itr) (*(itr).ptr)
#define SCM_GC_REF_ITR_SET(itr, p) (*(itr).ptr = (p))

void scm_obj_init(ScmObj obj, SCM_OBJ_TYPE_T type);
SCM_OBJ_TYPE_T scm_obj_type(ScmObj obj);
void scm_obj_pretty_print(ScmObj obj, ScmOBuffer *obuffer);
int scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2);
bool smc_obj_is_valid_type_info_tbl(void);

#endif /* INCLUDE_OBJECT_H__ */
