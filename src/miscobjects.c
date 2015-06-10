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
/*  ScmEOF (interface)                                 */
/*******************************************************/

bool
scm_fcd_eof_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_EOF_TYPE_INFO);
}

bool
scm_fcd_eof_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_EOF_OBJ);
}

ScmObj
scm_fcd_eof_object_P(ScmObj obj)
{
  return scm_fcd_eof_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_eof_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj eof = SCM_OBJ_INIT;

  eof = scm_fcd_mem_alloc(&SCM_EOF_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(eof)) return SCM_OBJ_NULL;

  scm_eof_initialize(eof);

  return eof;
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
/*  ScmBool (interface)                                */
/*******************************************************/

bool
scm_fcd_boolean_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BOOL_TYPE_INFO);
}

ScmObj
scm_fcd_boolean_P(ScmObj obj)
{
  return scm_fcd_boolean_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_bool_new(SCM_MEM_TYPE_T mtype, bool value)
{
  ScmObj bl = SCM_OBJ_INIT;;

  SCM_REFSTK_INIT_REG(&bl);

  bl = scm_fcd_mem_alloc(&SCM_BOOL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bl)) return SCM_OBJ_NULL;

  scm_bool_initialize(bl, value);

  return bl;
}

bool
scm_fcd_true_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_TRUE_OBJ);
}

bool
scm_fcd_false_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_FALSE_OBJ);
}

bool
scm_fcd_true_p(ScmObj obj)
{
  return !scm_fcd_false_object_p(obj);
}

bool
scm_fcd_false_p(ScmObj obj)
{
  return scm_fcd_false_object_p(obj);
}

ScmObj
scm_fcd_not(ScmObj obj)
{
  return scm_fcd_false_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
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
/*  ScmNil (interface)                                 */
/*******************************************************/

bool
scm_fcd_nil_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_NIL_TYPE_INFO);
}

ScmObj
scm_fcd_nil_P(ScmObj obj)
{
  return scm_fcd_nil_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

bool
scm_fcd_nil_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_NIL_OBJ);
}

ScmObj
scm_fcd_nil_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&nil);

  nil = scm_fcd_mem_alloc(&SCM_NIL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  scm_nil_initialize(nil);

  return nil;
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
/*  ScmUndef (interface)                               */
/*******************************************************/

bool
scm_fcd_undef_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_UNDEF_TYPE_INFO);
}

bool
scm_fcd_undef_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_UNDEF_OBJ);
}

ScmObj
scm_fcd_undef_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj undef = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&undef);

  undef = scm_fcd_mem_alloc(&SCM_UNDEF_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(undef)) return SCM_OBJ_NULL;

  scm_undef_initialize(undef);

  return undef;
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
/*  ScmLandmine (interface)                            */
/*******************************************************/

bool
scm_fcd_landmine_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_LANDMINE_TYPE_INFO);
}

bool
scm_fcd_landmine_object_p(ScmObj obj)
{
  return scm_obj_same_instance_p(obj, SCM_LANDMINE_OBJ);
}

ScmObj
scm_fcd_landmine_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj mine = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mine);

  mine = scm_fcd_mem_alloc(&SCM_LANDMINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(mine)) return SCM_OBJ_NULL;

  scm_landmine_initialize(mine);

  return mine;
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


/*******************************************************/
/*  ScmBox (interface)                                 */
/*******************************************************/

bool
scm_fcd_box_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BOX_TYPE_INFO);
}

ScmObj
scm_fcd_box_new(SCM_MEM_TYPE_T mtype, ScmObj obj)
{
  ScmObj box = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj, &box);

  scm_assert(scm_obj_not_null_p(obj));

  box = scm_fcd_mem_alloc(&SCM_BOX_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  if (scm_box_initialize(box, obj) < 0)
    return SCM_OBJ_NULL;

  return box;
}
