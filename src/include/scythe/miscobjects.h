#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

typedef struct ScmEOFRec ScmEOF;
typedef struct ScmBoolRec ScmBool;
typedef struct ScmNilRec ScmNil;
typedef struct ScmUndefRec ScmUndef;
typedef struct ScmLandmineRec ScmLandmine;


#define SCM_EOF(obj) ((ScmEof *)(obj))
#define SCM_BOOL(obj) ((ScmBool *)(obj))
#define SCM_NIL(obj) ((ScmNil *)(obj))
#define SCM_UNDEF(obj) ((ScmUndef *)(obj))
#define SCM_LANDMINE(obj) ((ScmLandmine *)(obj))

#include "scythe/object.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

struct ScmEOFRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_EOF_TYPE_INFO;

void scm_eof_initialize(ScmObj eof);
void scm_eof_finalize(ScmObj eof);
int scm_eof_obj_print(ScmObj obj, ScmObj port, int kind,
                      ScmObjPrintHandler handler);


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
bool scm_bool_value(ScmObj bl);
int scm_bool_obj_print(ScmObj obj, ScmObj port, int kind,
                       ScmObjPrintHandler handler);


/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

struct ScmNilRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_NIL_TYPE_INFO;

void scm_nil_initialize(ScmObj nil);
void scm_nil_finalize(ScmObj nil);
int scm_nil_obj_print(ScmObj obj, ScmObj port, int kind,
                      ScmObjPrintHandler handler);


/*******************************************************/
/*  ScmUndef                                           */
/*******************************************************/

struct ScmUndefRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_UNDEF_TYPE_INFO;

void scm_undef_initialize(ScmObj undef);
void scm_udef_finalize(ScmObj undef);


/*******************************************************/
/*  ScmLandmine                                        */
/*******************************************************/

struct ScmLandmineRec {
  ScmObjHeader header;
};

extern ScmTypeInfo SCM_LANDMINE_TYPE_INFO;

void scm_landmine_initialize(ScmObj mine);
void scm_landmine_finalize(ScmObj mine);


/*******************************************************/
/*  ScmBox                                             */
/*******************************************************/

extern ScmTypeInfo SCM_BOX_TYPE_INFO;

int scm_box_initialize(ScmObj box, ScmObj obj);
void scm_box_gc_initialize(ScmObj obj, ScmObj mem);
int scm_box_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler);


#endif /*  INCLUDE_MISCOBJECTS_H__ */
