#ifndef INCLUDE_FCD_MISCOBJECTS_H__
#define FCD_MISCOBJECTS_H

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"


/*******************************************************/
/*  Nil (empty list)                                   */
/*******************************************************/

ScmObj scm_fcd_nil_new(SCM_MEM_TYPE_T mtype);
bool scm_fcd_nil_p(ScmObj obj);
ScmObj scm_fcd_nil_P(ScmObj obj);
bool scm_fcd_nil_object_p(ScmObj obj);


/*******************************************************/
/*  Booleans                                           */
/*******************************************************/

bool scm_fcd_boolean_p(ScmObj obj);
ScmObj scm_fcd_boolean_P(ScmObj obj);
ScmObj scm_fcd_bool_new(SCM_MEM_TYPE_T mtype, bool value);
bool scm_fcd_true_object_p(ScmObj obj);
bool scm_fcd_false_object_p(ScmObj obj);
bool scm_fcd_true_p(ScmObj obj);
bool scm_fcd_false_p(ScmObj obj);
ScmObj scm_fcd_not(ScmObj obj);


/*******************************************************/
/*  EOF                                                */
/*******************************************************/

ScmObj scm_fcd_eof_new(SCM_MEM_TYPE_T mtype);
bool scm_fcd_eof_p(ScmObj obj);
bool scm_fcd_eof_object_p(ScmObj obj);
ScmObj scm_fcd_eof_object_P(ScmObj obj);


/*******************************************************/
/*  Undef                                              */
/*******************************************************/

ScmObj scm_fcd_undef_new(SCM_MEM_TYPE_T mtype);
bool scm_fcd_undef_p(ScmObj obj);
bool scm_fcd_undef_object_p(ScmObj obj);


/*******************************************************/
/*  Landmine (uninitialized)                           */
/*******************************************************/

ScmObj scm_fcd_landmine_new(SCM_MEM_TYPE_T mtype);
bool scm_fcd_landmine_p(ScmObj obj);
bool scm_fcd_landmine_object_p(ScmObj obj);


/*******************************************************/
/*  Boxing                                             */
/*******************************************************/

typedef struct ScmBoxRec ScmBox;
#define SCM_BOX(obj) ((ScmBox *)(obj))

struct ScmBoxRec {
  ScmObjHeader header;
  ScmObj obj;
};

bool scm_fcd_box_p(ScmObj obj);
ScmObj scm_fcd_box_new(SCM_MEM_TYPE_T mtype, ScmObj obj);

#define scm_fcd_box_object_p scm_fcd_box_p

static inline ScmObj
scm_fcd_box_unbox(ScmObj box)
{
  scm_assert(scm_fcd_box_object_p(box));
  return SCM_BOX(box)->obj;
}

static inline void
scm_fcd_box_update(ScmObj box, ScmObj obj)
{
  scm_assert(scm_fcd_box_object_p(box));
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmBox, box, obj, obj);
}


#endif /* INCLUDE_FCD_MISCOBJECTS_H__ */
