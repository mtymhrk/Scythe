#include <stdbool.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
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

void
scm_eof_initialize(ScmObj eof)
{
  return;                       /* nothing to do */
}

void
scm_eof_finalize(ScmObj eof)
{
  return;                       /* nothing to do */
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

bool
scm_bool_value(ScmObj bl)
{
  scm_assert_obj_type(bl, &SCM_BOOL_TYPE_INFO);

  return SCM_BOOL_VALUE(bl);
}

int
scm_bool_obj_print(ScmObj obj, ScmObj port, int kind,
                   ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_BOOL_TYPE_INFO);

  if (SCM_BOOL(obj)->value == true)
    rslt = scm_fcd_write_cstr("#t", SCM_ENC_SRC, port);
  else
    rslt = scm_fcd_write_cstr("#f", SCM_ENC_SRC, port);

  if (rslt < 0) return -1;

  return 0;
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

void
scm_nil_initialize(ScmObj nil)
{
  return;                       /* nothing to do */
}

void
scm_nil_finalize(ScmObj nil)
{
  return;                       /* nothing to do */
}

int
scm_nil_obj_print(ScmObj obj, ScmObj port, int kind, ScmObjPrintHandler handler)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_NIL_TYPE_INFO);

  rslt = scm_fcd_write_cstr("()", SCM_ENC_SRC, port);
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

void
scm_undef_initialize(ScmObj undef)
{
  return;                       /* nothing to do */
}

void
scm_udef_finalize(ScmObj undef)
{
  return;                       /* nothing to do */
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

void
scm_landmine_initialize(ScmObj mine)
{
  return;                       /* nothing to do */
}

void
scm_landmine_finalize(ScmObj mine)
{
  return;                       /* nothing to do */
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

void
scm_box_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_BOX(obj)->obj = SCM_OBJ_NULL;
}

int
scm_box_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_BOX_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_BOX(obj)->obj, mem);
}
