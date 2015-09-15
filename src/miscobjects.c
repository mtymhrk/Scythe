#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/bedrock.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/port.h"
#include "scythe/miscobjects.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

ScmTypeInfo SCM_EOF_TYPE_INFO = {
  .name                = "eof",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_obj_print_func_nameonly,
  .obj_size            = sizeof(ScmEOF),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_eof_object_P(ScmObj obj)
{
  return scm_eof_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_eof_new(scm_mem_type_t mtype)
{
  return scm_alloc_mem(&SCM_EOF_TYPE_INFO, 0, mtype);
}


/*******************************************************/
/*  ScmBool                                            */
/*******************************************************/

ScmTypeInfo SCM_BOOL_TYPE_INFO = {
  .name                = "boolean",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_bool_obj_print,
  .obj_size            = sizeof(ScmBool),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_boolean_P(ScmObj obj)
{
  return scm_boolean_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

void
scm_bool_initialize(ScmObj obj, bool value)
{
  scm_assert_obj_type(obj, &SCM_BOOL_TYPE_INFO);

  SCM_BOOL_VALUE(obj) = value;
}

void
scm_bool_finalize(ScmObj obj)
{
  return;                       /* nothing to do */
}

ScmObj
scm_bool_new(scm_mem_type_t mtype, bool value)
{
  ScmObj bl = SCM_OBJ_INIT;;

  SCM_REFSTK_INIT_REG(&bl);

  bl = scm_alloc_mem(&SCM_BOOL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bl)) return SCM_OBJ_NULL;

  scm_bool_initialize(bl, value);

  return bl;
}

int
scm_bool_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_BOOL_TYPE_INFO);

  if (SCM_BOOL(obj)->value == true)
    rslt = scm_write_cstr("#t", SCM_ENC_SRC, port);
  else
    rslt = scm_write_cstr("#f", SCM_ENC_SRC, port);

  if (rslt < 0) return -1;

  return 0;
}

ScmObj
scm_not(ScmObj obj)
{
  return scm_false_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}


/*******************************************************/
/*  ScmNil                                             */
/*******************************************************/

ScmTypeInfo SCM_NIL_TYPE_INFO = {
  .name                = "nil",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_nil_obj_print,
  .obj_size            = sizeof(ScmNil),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_nil_P(ScmObj obj)
{
  return scm_nil_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_nil_new(scm_mem_type_t mtype)
{
  return scm_alloc_mem(&SCM_NIL_TYPE_INFO, 0, mtype);
}

int
scm_nil_obj_print(ScmObj obj, ScmObj port, int kind, ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_NIL_TYPE_INFO);

  rslt = scm_write_cstr("()", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************/
/*  ScmUndef                                           */
/*******************************************************/

ScmTypeInfo SCM_UNDEF_TYPE_INFO = {
  .name                = "undef",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_obj_print_func_nameonly,
  .obj_size            = sizeof(ScmUndef),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_undef_new(scm_mem_type_t mtype)
{
  return scm_alloc_mem(&SCM_UNDEF_TYPE_INFO, 0, mtype);
}


/*******************************************************/
/*  ScmLandmine                                        */
/*******************************************************/

ScmTypeInfo SCM_LANDMINE_TYPE_INFO = {
  .name                = "landmine",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_obj_print_func_nameonly,
  .obj_size            = sizeof(ScmLandmine),
  .gc_ini_func         = NULL,
  .gc_fin_func         = NULL,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmObj
scm_landmine_new(scm_mem_type_t mtype)
{
  return scm_alloc_mem(&SCM_LANDMINE_TYPE_INFO, 0, mtype);
}


/*******************************************************/
/*  ScmBox                                             */
/*******************************************************/

ScmTypeInfo SCM_BOX_TYPE_INFO = {
  .name                = "box",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmBox),
  .gc_ini_func         = scm_box_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_box_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_box_initialize(ScmObj box, ScmObj obj)
{
  scm_assert_obj_type(box, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmBox, box, obj, obj);

  return 0;
}

ScmObj
scm_box_new(scm_mem_type_t mtype, ScmObj obj)
{
  ScmObj box = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj, &box);

  scm_assert(scm_obj_not_null_p(obj));

  box = scm_alloc_mem(&SCM_BOX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  if (scm_box_initialize(box, obj) < 0)
    return SCM_OBJ_NULL;

  return box;
}

void
scm_box_gc_initialize(ScmObj obj)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);

  SCM_BOX(obj)->obj = SCM_OBJ_NULL;
}

int
scm_box_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_BOX(obj)->obj);
}
