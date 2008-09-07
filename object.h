#ifndef INCLUDE_OBJECT_H__
#define INCLUDE_OBJECT_H__

typedef struct ScmObjHeaderRec ScmObjHeader;
typedef struct ScmAtomRec ScmAtom;
typedef ScmAtom *ScmObj;

#include "printer.h"

typedef enum {
  SCM_OBJ_TYPE_PAIR,
  SCM_OBJ_TYPE_STRING,
  SCM_OBJ_TYPE_SYMBOL,
  SCM_OBJ_TYPE_NIL,
  SCM_OBJ_TYPE_INTEGER,
  SCM_OBJ_TYPE_VECTOR,
  SCM_OBJ_TYPE_EOF
} SCM_OBJ_TYPE_T;

typedef void (*ScmPrettyPrintFunction)(ScmObj obj,
				       ScmPrinter *printer);

struct ScmObjHeaderRec {
  SCM_OBJ_TYPE_T type;
  ScmPrettyPrintFunction pretty_print;
};

struct ScmAtomRec {
  ScmObjHeader header;
};


#define SCM_ATOM(obj) ((ScmAtom *)(obj))
#define SCM_OBJ(obj) ((ScmObj)(obj))

void scm_obj_init(ScmObj obj, SCM_OBJ_TYPE_T type,
		  ScmPrettyPrintFunction ppfunc);
SCM_OBJ_TYPE_T scm_obj_type(ScmObj obj);
void scm_obj_pretty_print(ScmObj obj, ScmPrinter *printer);
int scm_obj_is_same_instance(ScmObj obj1, ScmObj obj2);

#endif /* INCLUDE_OBJECT_H__ */
