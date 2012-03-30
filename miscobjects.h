#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;
typedef struct ScmBoolRec ScmBool;
typedef struct ScmNilRec ScmNil;
typedef struct ScmUndefRec ScmUndef;

#define SCM_EOF(obj) ((ScmEof *)(obj))
#define SCM_BOOL(obj) ((ScmBool *)(obj))
#define SCM_NIL(obj) ((ScmNil *)(obj))
#define SCM_UNDEF(obj) ((ScmUndef *)(obj))

#include "object.h"
#include "api.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

struct ScmEOFRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_EOF_TYPE_INFO;

void scm_eof_initialize(ScmObj eof);
void scm_eof_finalize(ScmObj eof);
ScmObj scm_eof_new(SCM_MEM_TYPE_T mtype);
int scm_eof_pretty_print(ScmObj obj, ScmObj port, bool write_p);


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
ScmObj scm_bool_new(SCM_MEM_TYPE_T mtype, bool value);
bool scm_bool_value(ScmObj bl);
int scm_bool_pretty_print(ScmObj obj, ScmObj port, bool write_p);


/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

struct ScmNilRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_NIL_TYPE_INFO;

void scm_nil_initialize(ScmObj nil);
void scm_nil_finalize(ScmObj nil);
ScmObj scm_nil_new(SCM_MEM_TYPE_T mtype);
int scm_nil_pretty_print(ScmObj obj, ScmObj port, bool write_p);


/*******************************************************/
/*  ScmUndef                                           */
/*******************************************************/

struct ScmUndefRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_UNDEF_TYPE_INFO;

void scm_undef_initialize(ScmObj undef);
void scm_udef_finalize(ScmObj undef);
ScmObj scm_undef_new(SCM_MEM_TYPE_T mtype);
int scm_undef_pretty_print(ScmObj obj, ScmObj port, bool write_p);

#endif /*  INCLUDE_MISCOBJECTS_H__ */
