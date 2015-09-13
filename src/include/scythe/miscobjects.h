#ifndef INCLUDE_MISCOBJECTS_H__
#define INCLUDE_MISCOBJECTS_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

typedef struct ScmEOFRec ScmEOF;

struct ScmEOFRec {
  ScmObjHeader header;
};

#define SCM_EOF(obj) ((ScmEof *)(obj))

extern ScmTypeInfo SCM_EOF_TYPE_INFO;

ScmObj scm_eof_object_P(ScmObj obj);
ScmObj scm_eof_new(scm_mem_type_t mtype);
int scm_eof_obj_print(ScmObj obj, ScmObj port, int kind,
                      ScmObjPrintHandler handler);

static inline bool
scm_eof_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_EOF_TYPE_INFO);
}

static inline bool
scm_eof_object_p(ScmObj obj)
{
  return scm_eof_p(obj);
}


/*******************************************************/
/*  ScmBool                                            */
/*******************************************************/

typedef struct ScmBoolRec ScmBool;

struct ScmBoolRec {
  ScmObjHeader header;
  bool value;
};

#define SCM_BOOL(obj) ((ScmBool *)(obj))
#define SCM_BOOL_VALUE(obj) (SCM_BOOL(obj)->value)

extern ScmTypeInfo SCM_BOOL_TYPE_INFO;

ScmObj scm_boolean_P(ScmObj obj);
void scm_bool_initialize(ScmObj obj, bool value);
void scm_bool_finalize(ScmObj obj);
ScmObj scm_bool_new(scm_mem_type_t mtype, bool value);
int scm_bool_obj_print(ScmObj obj, ScmObj port, int kind,
                       ScmObjPrintHandler handler);
ScmObj scm_not(ScmObj obj);

static inline bool
scm_boolean_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BOOL_TYPE_INFO);
}

static inline bool
scm_bool_value(ScmObj bl)
{
  scm_assert(scm_boolean_p(bl));
  return SCM_BOOL_VALUE(bl);
}

static inline bool
scm_true_object_p(ScmObj obj)
{
  return (scm_boolean_p(obj) && scm_bool_value(obj));
}

static inline bool
scm_false_object_p(ScmObj obj)
{
  return (scm_boolean_p(obj) && !scm_bool_value(obj));
}

static inline bool
scm_true_p(ScmObj obj)
{
  return !scm_false_object_p(obj);
}

static inline bool
scm_false_p(ScmObj obj)
{
  return scm_false_object_p(obj);
}


/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

typedef struct ScmNilRec ScmNil;

struct ScmNilRec {
  ScmObjHeader header;
};

#define SCM_NIL(obj) ((ScmNil *)(obj))

extern ScmTypeInfo SCM_NIL_TYPE_INFO;

ScmObj scm_nil_P(ScmObj obj);
ScmObj scm_nil_new(scm_mem_type_t mtype);
int scm_nil_obj_print(ScmObj obj, ScmObj port, int kind,
                      ScmObjPrintHandler handler);

static inline bool
scm_nil_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_NIL_TYPE_INFO);
}

static inline bool
scm_nil_object_p(ScmObj obj)
{
  return scm_nil_p(obj);
}


/*******************************************************/
/*  ScmUndef                                           */
/*******************************************************/

typedef struct ScmUndefRec ScmUndef;

struct ScmUndefRec {
  ScmObjHeader header;
};

#define SCM_UNDEF(obj) ((ScmUndef *)(obj))

extern ScmTypeInfo SCM_UNDEF_TYPE_INFO;

ScmObj scm_undef_new(scm_mem_type_t mtype);

static inline bool
scm_undef_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_UNDEF_TYPE_INFO);
}

static inline bool
scm_undef_object_p(ScmObj obj)
{
  return scm_undef_p(obj);
}


/*******************************************************/
/*  ScmLandmine                                        */
/*******************************************************/

typedef struct ScmLandmineRec ScmLandmine;

struct ScmLandmineRec {
  ScmObjHeader header;
};

#define SCM_LANDMINE(obj) ((ScmLandmine *)(obj))

extern ScmTypeInfo SCM_LANDMINE_TYPE_INFO;

ScmObj scm_landmine_new(scm_mem_type_t mtype);

static inline bool
scm_landmine_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_LANDMINE_TYPE_INFO);
}

static inline bool
scm_landmine_object_p(ScmObj obj)
{
  return scm_landmine_p(obj);
}

static inline bool
scm_uninit_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_LANDMINE_TYPE_INFO);
}

static inline bool
scm_uninit_object_p(ScmObj obj)
{
  return scm_landmine_p(obj);
}


/*******************************************************/
/*  ScmBox                                             */
/*******************************************************/

typedef struct ScmBoxRec ScmBox;

struct ScmBoxRec {
  ScmObjHeader header;
  ScmObj obj;
};

#define SCM_BOX(obj) ((ScmBox *)(obj))

extern ScmTypeInfo SCM_BOX_TYPE_INFO;

int scm_box_initialize(ScmObj box, ScmObj obj);
ScmObj scm_box_new(scm_mem_type_t mtype, ScmObj obj);
void scm_box_gc_initialize(ScmObj obj);
int scm_box_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_box_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BOX_TYPE_INFO);
}

static inline bool
scm_box_object_p(ScmObj obj)
{
  return scm_box_p(obj);
}

static inline ScmObj
scm_box_unbox(ScmObj box)
{
  scm_assert(scm_box_object_p(box));
  return SCM_BOX(box)->obj;
}

static inline void
scm_box_update(ScmObj box, ScmObj obj)
{
  scm_assert(scm_box_p(box));
  scm_assert(scm_obj_not_null_p(obj));
  SCM_SLOT_SETQ(ScmBox, box, obj, obj);
}

#endif /*  INCLUDE_MISCOBJECTS_H__ */
