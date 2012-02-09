#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;
typedef struct ScmBoolRec ScmBool;
typedef struct ScmNilRec ScmNil;

#define SCM_EOF(obj) ((ScmEof *)(obj))
#define SCM_BOOL(obj) ((ScmBool *)(obj))
#define SCM_NIL(obj) ((ScmNil *)(obj))

#include "object.h"
#include "memory.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

struct ScmEOFRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_EOF_TYPE_INFO;

void scm_eof_initialize(ScmObj eof);
void scm_eof_finalize(ScmObj eof);
ScmObj scm_eof_new(SCM_MEM_ALLOC_TYPE_T mtype);
ScmObj scm_eof_instance(void);
bool scm_eof_is_eof(ScmObj obj);



/*******************************************************/
/*  ScmBool                                            */
/*******************************************************/

struct ScmBoolRec {
  ScmObjHeader header;
  bool value;
};

extern ScmTypeInfo SCM_BOOL_TYPE_INFO;

#define SCM_BOOL_VALUE(obj) (SCM_BOOL(obj)->value)

void scm_bool_initialize(ScmObj obj, bool value);
void scm_bool_finalize(ScmObj obj);
ScmObj scm_bool_new(SCM_MEM_ALLOC_TYPE_T mtype, bool value);
ScmObj scm_bool_instance(bool value);
bool scm_bool_value(ScmObj bl);
bool scm_bool_is_bool(ScmObj obj);


/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

struct ScmNilRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_NIL_TYPE_INFO;

void scm_nil_initialize(ScmObj nil);
void scm_nil_finalize(ScmObj nil);
ScmObj scm_nil_new(SCM_MEM_ALLOC_TYPE_T mtype);
ScmObj scm_nil_instance(void);
bool scm_nil_is_nil(ScmObj obj);


#endif /*  INCLUDE_MISCOBJECTS_H__ */
