#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/bedrock.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/exception.h"
#include "scythe/format.h"
#include "scythe/symbol.h"
#include "scythe/pair.h"
#include "scythe/record.h"

ScmTypeInfo SCM_RECORDTYPE_TYPE_INFO = {
  .name = "record-type",
  .flags = SCM_TYPE_FLG_MMO,
  .obj_print_func = scm_recordtype_obj_print,
  .obj_size = sizeof(ScmRecordType),
  .gc_ini_func = scm_recordtype_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_recordtype_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

ScmTypeInfo SCM_RECORD_TYPE_INFO = {
  .name = "record-instance",
  .flags = SCM_TYPE_FLG_MMO,
  .obj_print_func = scm_record_obj_print,
  .obj_size = sizeof(ScmRecord),
  .gc_ini_func = scm_record_gc_initialize,
  .gc_fin_func = NULL,
  .gc_accept_func = scm_record_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra = NULL,
};

ScmObj
scm_record_P(ScmObj obj)
{
  return (scm_record_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

int
scm_recordtype_initialize(ScmObj type, ScmObj name)
{
  scm_assert(scm_recordtype_p(type));
  scm_assert(scm_symbol_p(name));

  SCM_RECORDTYPE_SET_NAME(type, name);
  return 0;
}

int
scm_record_initialize(ScmObj rec, ScmObj type, size_t n, ScmObj slots)
{
  ScmObj l = SCM_OBJ_INIT;

  scm_assert(scm_record_p(rec));
  scm_assert(scm_recordtype_p(type));
  scm_assert(n == 0 || scm_pair_p(slots));

  SCM_RECORD_SET_TYPE(rec, type);
  SCM_RECORD_SET_NR_SLOTS(rec, n);

  l = slots;
  for (size_t i = 0; i < n; i++) {
    if (!scm_pair_p(l)) {
      scm_error("failed to make a record: too few arguments", 1, type);
      return -1;
    }
    SCM_RECORD_SET_SLOT(rec, i, scm_car(l));
    l = scm_cdr(l);
  }

  return 0;
}

ScmObj
scm_recordtype_new(scm_mem_type_t mtype, ScmObj name)
{
  ScmObj type = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&name,
                      &type);

  scm_assert(scm_symbol_p(name));

  type = scm_alloc_mem(&SCM_RECORDTYPE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(type)) return SCM_OBJ_NULL;

  if (scm_recordtype_initialize(type, name) < 0)
    return SCM_OBJ_NULL;

  return type;
}

ScmObj
scm_record_new(scm_mem_type_t mtype, ScmObj type, size_t n, ScmObj slots)
{
  ScmObj rec = SCM_OBJ_INIT;

  scm_assert(scm_recordtype_p(type));
  scm_assert(n == 0 || scm_pair_p(slots));

  if (sizeof(ScmObj) > SIZE_MAX / n) {
    scm_error("failed to make a record: too many fields", 1, type);
    return SCM_OBJ_NULL;
  }

  rec = scm_alloc_mem(&SCM_RECORD_TYPE_INFO, sizeof(ScmObj) * n, mtype);
  if (scm_obj_null_p(rec)) return SCM_OBJ_NULL;

  if (scm_record_initialize(rec, type, n, slots) < 0)
    return SCM_OBJ_NULL;

  return rec;
}

int
scm_recordtype_obj_print(ScmObj obj,
                         ScmObj port, int kind, ScmObjPrintHandler handler)
{
  scm_assert(scm_recordtype_p(obj));
  return scm_pformat_cstr(port, "#<record-type ~a>", SCM_RECORDTYPE_NAME(obj));
}

void
scm_recordtype_gc_initialize(ScmObj obj)
{
  scm_assert(scm_recordtype_p(obj));
  SCM_RECORDTYPE_SET_NAME(obj, SCM_OBJ_NULL);
}

int
scm_recordtype_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert(scm_recordtype_p(obj));
  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_RECORDTYPE_NAME(obj));
}

int
scm_record_obj_print(ScmObj obj,
                        ScmObj port, int kind, ScmObjPrintHandler handler)
{
  scm_assert(scm_recordtype_p(obj));
  return scm_pformat_cstr(port, "#<record ~a>",
                          scm_recordtype_name(SCM_RECORD_TYPE(obj)));
}

void
scm_record_gc_initialize(ScmObj obj)
{
  scm_assert(scm_record_p(obj));
  SCM_RECORD_SET_TYPE(obj, SCM_OBJ_NULL);
  SCM_RECORD_SET_NR_SLOTS(obj, 0);
}

int
scm_record_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert(scm_record_p(obj));

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_RECORD_TYPE(obj));
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < SCM_RECORD_NR_SLOTS(obj); i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_RECORD_SLOT(obj, i));
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
