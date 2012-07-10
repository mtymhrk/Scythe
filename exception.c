#include <stdio.h>
#include <stdarg.h>

#include "object.h"
#include "api.h"
#include "exception.h"

ScmTypeInfo SCM_EXCEPTION_TYPE_INFO = {
  .name                = "exception",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_exception_pretty_print,
  .obj_size            = sizeof(ScmException),
  .gc_ini_func         = scm_exception_gc_initialize,
  .gc_fin_func         = scm_exception_gc_fianlize,
  .gc_accept_func      = scm_exception_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_exception_initialize_va(ScmObj exc, ScmObj msg, size_t n, va_list irris)
{
  ScmObj ir = SCM_OBJ_INIT;

  scm_assert_obj_type(exc, &SCM_EXCEPTION_TYPE_INFO);
  scm_assert(scm_capi_string_p(msg));
  scm_assert(n <= SCM_EXCEPTION_IRRITANTS_MAX);

  SCM_EXCEPTION(exc)->irritants = scm_capi_malloc(sizeof(ScmObj) * n);
  if (SCM_EXCEPTION(exc)->irritants == NULL)
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmException, exc, msg, msg);

  for (size_t i = 0; i < n; i++) {
    ir = va_arg(irris, ScmObj);
    SCM_SLOT_SETQ(ScmException, exc, irritants[i], ir);
  }

  SCM_EXCEPTION(exc)->nr_irris = n;

  return 0;
}

int
scm_exception_initialize_ary(ScmObj exc, ScmObj msg, size_t n, ScmObj *irris)
{
  scm_assert_obj_type(exc, &SCM_EXCEPTION_TYPE_INFO);
  scm_assert(scm_capi_string_p(msg));
  scm_assert(n <= SCM_EXCEPTION_IRRITANTS_MAX);

  SCM_EXCEPTION(exc)->irritants = scm_capi_malloc(sizeof(ScmObj) * n);
  if (SCM_EXCEPTION(exc)->irritants == NULL)
    return -1;                  /* [ERR]: [through] */

  SCM_SLOT_SETQ(ScmException, exc, msg, msg);

  for (size_t i = 0; i < n; i++)
    SCM_SLOT_SETQ(ScmException, exc, irritants[i], irris[i]);

  SCM_EXCEPTION(exc)->nr_irris = n;

  return 0;
}

void
scm_exception_finalize(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_EXCEPTION_TYPE_INFO);

  scm_capi_free(SCM_EXCEPTION(exc)->irritants);
  SCM_EXCEPTION(exc)->irritants = NULL;
  SCM_EXCEPTION(exc)->nr_irris = 0;
  SCM_EXCEPTION(exc)->msg = SCM_OBJ_NULL;
}

ScmObj
scm_exception_new(SCM_MEM_TYPE_T mtype, ScmObj msg, size_t n, ...)
{
  ScmObj exc = SCM_OBJ_INIT;
  va_list irris;
  int rslt;

  SCM_STACK_FRAME_PUSH(&msg, &exc);

  scm_assert(scm_capi_string_p(msg));
  scm_assert(n <= SCM_EXCEPTION_IRRITANTS_MAX);

  exc = scm_capi_mem_alloc(&SCM_EXCEPTION_TYPE_INFO, mtype);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  va_start(irris, n);
  rslt = scm_exception_initialize_va(exc, msg, n, irris);
  va_end(irris);

  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return exc;
}

ScmObj
scm_exception_new_va(SCM_MEM_TYPE_T mtype, ScmObj msg, size_t n, va_list irris)
{
  ScmObj exc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&msg, &exc);

  scm_assert(scm_capi_string_p(msg));
  scm_assert(n <= SCM_EXCEPTION_IRRITANTS_MAX);

  exc = scm_capi_mem_alloc(&SCM_EXCEPTION_TYPE_INFO, mtype);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_exception_initialize_va(exc, msg, n, irris);

  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return exc;
}

ScmObj
scm_exception_new_ary(SCM_MEM_TYPE_T mtype, ScmObj msg, size_t n, ScmObj *irris)
{
  ScmObj exc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&msg, &exc);

  scm_assert(scm_capi_string_p(msg));
  scm_assert(n <= SCM_EXCEPTION_IRRITANTS_MAX);

  exc = scm_capi_mem_alloc(&SCM_EXCEPTION_TYPE_INFO, mtype);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  rslt = scm_exception_initialize_ary(exc, msg, n, irris);

  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return exc;
}

int
scm_exception_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  ScmObj ro;
  int rslt;

  SCM_STACK_FRAME_PUSH(&obj, &port,
                       &ro);

  scm_assert_obj_type(obj, &SCM_EXCEPTION_TYPE_INFO);

  if (write_p) {
    char cstr[32];

    snprintf(cstr, sizeof(cstr), " 0x%llx>", (unsigned long long)obj);

    rslt = scm_capi_write_cstr("#<exception ", SCM_ENC_ASCII, port);
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    ro = scm_api_write_string(SCM_EXCEPTION(obj)->msg, port);
    if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */

    rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
    if (rslt < 0) return -1;    /* [ERR]: [through] */
  }
  else {
    rslt = scm_capi_write_cstr("Exception: ", SCM_ENC_ASCII, port);
    if (rslt < 0) return -1;    /* [ERR]: [through] */

    if (scm_obj_not_null_p(SCM_EXCEPTION(obj)->msg)) {
      const char *dlm;

      ro = scm_api_write_string(SCM_EXCEPTION(obj)->msg, port);
      if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */

      dlm = ": ";
      for (size_t i = 0; i < SCM_EXCEPTION(obj)->nr_irris; i++) {
        rslt = scm_capi_write_cstr(dlm, SCM_ENC_ASCII, port);
        if (rslt < 0) return -1;    /* [ERR]: [through] */

        ro = scm_api_write(SCM_EXCEPTION(obj)->irritants[i], port);
        if (scm_obj_null_p(ro)) return -1; /* [ERR]: [through] */

        dlm = ", ";
      }
    }
  }

  return 0;
}

void
scm_exception_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_EXCEPTION_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_EXCEPTION(obj)->msg = SCM_OBJ_NULL;
  SCM_EXCEPTION(obj)->irritants = NULL;
  SCM_EXCEPTION(obj)->nr_irris = 0;
}

void
scm_exception_gc_fianlize(ScmObj obj)
{
  scm_exception_finalize(obj);
}

int
scm_exception_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_EXCEPTION_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_EXCEPTION(obj)->msg, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < SCM_EXCEPTION(obj)->nr_irris; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_EXCEPTION(obj)->irritants[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
