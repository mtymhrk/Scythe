#include <stdbool.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "miscobjects.h"


/*******************************************************/
/*  ScmEOF                                             */
/*******************************************************/

ScmTypeInfo SCM_EOF_TYPE_INFO = {
  .name                = "eof",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_eof_obj_print,
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

ScmObj
scm_eof_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj eof = SCM_OBJ_INIT;

  eof = scm_capi_mem_alloc(&SCM_EOF_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(eof)) return SCM_OBJ_NULL;

  scm_eof_initialize(eof);

  return eof;
}

int
scm_eof_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_EOF_TYPE_INFO);
  return scm_capi_write_cstr("#<eof>", SCM_ENC_SRC, port);
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

ScmObj
scm_bool_new(SCM_MEM_TYPE_T mtype, bool value)
{
  ScmObj bl = SCM_OBJ_INIT;;

  SCM_REFSTK_INIT_REG(&bl);

  bl = scm_capi_mem_alloc(&SCM_BOOL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(bl)) return SCM_OBJ_NULL;

  scm_bool_initialize(bl, value);

  return bl;
}

bool
scm_bool_value(ScmObj bl)
{
  scm_assert_obj_type(bl, &SCM_BOOL_TYPE_INFO);

  return SCM_BOOL_VALUE(bl);
}

int
scm_bool_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_BOOL_TYPE_INFO);

  if (SCM_BOOL(obj)->value == true)
    rslt = scm_capi_write_cstr("#t", SCM_ENC_SRC, port);
  else
    rslt = scm_capi_write_cstr("#f", SCM_ENC_SRC, port);

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

ScmObj
scm_nil_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj nil = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&nil);

  nil = scm_capi_mem_alloc(&SCM_NIL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(nil)) return SCM_OBJ_NULL;

  scm_nil_initialize(nil);

  return nil;
}

int
scm_nil_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  int rslt;

  scm_assert_obj_type(obj, &SCM_NIL_TYPE_INFO);

  rslt = scm_capi_write_cstr("()", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}


/*******************************************************/
/*  ScmUndef                                           */
/*******************************************************/

ScmTypeInfo SCM_UNDEF_TYPE_INFO = {
  .name                = "undef",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_undef_obj_print,
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

ScmObj
scm_undef_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj undef = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&undef);

  undef = scm_capi_mem_alloc(&SCM_UNDEF_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(undef)) return SCM_OBJ_NULL;

  scm_undef_initialize(undef);

  return undef;
}

int
scm_undef_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_UNDEF_TYPE_INFO);
  return scm_capi_write_cstr("#<undef>", SCM_ENC_SRC, port);
}


/*******************************************************/
/*  ScmLandmine                                        */
/*******************************************************/

ScmTypeInfo SCM_LANDMINE_TYPE_INFO = {
  .name                = "landmine",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = scm_landmine_obj_print,
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

ScmObj
scm_landmine_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj mine = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&mine);

  mine = scm_capi_mem_alloc(&SCM_LANDMINE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(mine)) return SCM_OBJ_NULL;

  scm_landmine_initialize(mine);

  return mine;
}

int
scm_landmine_obj_print(ScmObj obj, ScmObj port, bool ext_rep)
{
  scm_assert_obj_type(obj, &SCM_LANDMINE_TYPE_INFO);
  return scm_capi_write_cstr("#<landmine>", SCM_ENC_SRC, port);
}
