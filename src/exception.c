#include <stdio.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/exception.h"


/*******************************************************************/
/*  Exception                                                      */
/*******************************************************************/

int
scm_exception_initialize(ScmObj exc, ScmObj msg)
{
  scm_assert(scm_obj_type_flag_set_p(exc, SCM_TYPE_FLG_EXC));
  scm_assert(scm_obj_null_p(msg) || scm_fcd_string_p(msg));

  SCM_SLOT_SETQ(ScmException, exc, msg, msg);

  return 0;
}

void
scm_exception_finalize(ScmObj exc)
{
  /* do not nothing */
}

void
scm_exception_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_EXC));

  SCM_EXCEPTION(obj)->msg = SCM_OBJ_NULL;
}

int
scm_exception_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert(scm_obj_type_flag_set_p(obj, SCM_TYPE_FLG_EXC));

  return SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_EXCEPTION(obj)->msg, mem);
}


/*******************************************************************/
/*  Error                                                          */
/*******************************************************************/

ScmTypeInfo SCM_ERROR_TYPE_INFO = {
  .name                = "error",
  .flags               = SCM_TYPE_FLG_MMO | SCM_TYPE_FLG_EXC,
  .obj_print_func      = scm_error_obj_print,
  .obj_size            = sizeof(ScmError),
  .gc_ini_func         = scm_error_gc_initialize,
  .gc_fin_func         = scm_error_gc_fianlize,
  .gc_accept_func      = scm_error_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

int
scm_error_initialize_cv(ScmObj exc, ScmObj msg,
                        ScmObj type, ScmObj *irris, size_t n)
{
  int r;

  SCM_REFSTK_INIT_REG(&exc, &msg, &type);

  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);
  scm_assert(scm_obj_null_p(type) || scm_fcd_symbol_p(type));
  scm_assert(n <= SCM_ERROR_IRRITANTS_MAX);

  r = scm_exception_initialize(exc, msg);
  if (r < 0) return -1;

  SCM_SLOT_SETQ(ScmError, exc, type, type);

  SCM_ERROR(exc)->irritants = scm_fcd_malloc(sizeof(ScmObj) * n);
  if (SCM_ERROR(exc)->irritants == NULL)
    return -1;

  for (size_t i = 0; i < n; i++) {
    if (scm_obj_null_p(irris[i])) {
      scm_fcd_error("failed to make exception object: invalid argument", 0);
      return -1;
    }
    SCM_SLOT_SETQ(ScmError, exc, irritants[i], irris[i]);
  }

  SCM_ERROR(exc)->nr_irris = n;

  return 0;
}

int
scm_error_initialize_lst(ScmObj exc, ScmObj msg, ScmObj type, ScmObj irris)
{
  ScmObj rest = SCM_OBJ_INIT, ir = SCM_OBJ_INIT;
  ssize_t len;
  size_t i;
  int r;

  SCM_REFSTK_INIT_REG(&exc, &msg, &type, &irris,
                      &rest, &ir);

  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);
  scm_assert(scm_obj_null_p(type) || scm_fcd_symbol_p(type));
  scm_assert(scm_obj_not_null_p(irris));

  r = scm_exception_initialize(exc, msg);
  if (r < 0) return -1;

  SCM_SLOT_SETQ(ScmError, exc, type, type);

  len = scm_fcd_length(irris);
  if (len < 0) return -1;

  SCM_ERROR(exc)->irritants = scm_fcd_malloc(sizeof(ScmObj) * (size_t)len);
  if (SCM_ERROR(exc)->irritants == NULL)
    return -1;

  i = 0;
  for (rest = irris; scm_fcd_pair_p(rest); rest = scm_fcd_cdr(rest)) {
    scm_assert(i < (size_t)len);
    ir = scm_fcd_car(rest);
    SCM_SLOT_SETQ(ScmError, exc, irritants[i++], ir);
  }

  SCM_ERROR(exc)->nr_irris = i;

  return 0;
}

void
scm_error_finalize(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  SCM_ERROR(exc)->type = SCM_OBJ_NULL;
  scm_fcd_free(SCM_ERROR(exc)->irritants);
  SCM_ERROR(exc)->irritants = NULL;
  SCM_ERROR(exc)->nr_irris = 0;

  scm_exception_finalize(exc);
}

ScmObj
scm_error_irris_to_list(ScmObj exc)
{
  scm_assert_obj_type(exc, &SCM_ERROR_TYPE_INFO);

  return scm_fcd_list_cv(SCM_ERROR(exc)->irritants, SCM_ERROR(exc)->nr_irris);
}

int
scm_error_obj_print(ScmObj obj, ScmObj port, int kind,
                    ScmObjPrintHandler handler)
{
  ScmObj msg = SCM_OBJ_INIT;
  const char *dlm;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &msg);

  scm_assert_obj_type(obj, &SCM_ERROR_TYPE_INFO);

  if (kind == SCM_OBJ_PRINT_DISPLAY) {
    msg = scm_exception_msg(obj);

    if (scm_obj_not_null_p(SCM_ERROR(obj)->type)) {
      rslt = scm_fcd_pformat_cstr(port, "~a-",
                                   SCM_ERROR(obj)->type, SCM_OBJ_NULL);
      if (rslt < 0) return -1;
    }

    if (scm_obj_not_null_p(msg))
      rslt = scm_fcd_pformat_cstr(port, "error: ~a", msg, SCM_OBJ_NULL);
    else
      rslt = scm_fcd_pformat_cstr(port, "error", SCM_OBJ_NULL);

    if (rslt < 0) return -1;

    dlm = ": ";
    for (size_t i = 0; i < SCM_ERROR(obj)->nr_irris; i++) {
      rslt = scm_fcd_write_cstr(dlm, SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = scm_fcd_write(SCM_ERROR(obj)->irritants[i], port);
      if (rslt < 0) return -1;

      dlm = ", ";
    }
  }
  else {
    char cstr[32];

    snprintf(cstr, sizeof(cstr), " 0x%llx>", (unsigned long long)obj);

    rslt = scm_fcd_write_cstr("#<exception ", SCM_ENC_SRC, port);
    if (rslt < 0) return -1;

    rslt = scm_fcd_write_string(SCM_EXCEPTION(obj)->msg, port,
                              SCM_OBJ_NULL, SCM_OBJ_NULL);
    if (rslt < 0) return -1;

    rslt = scm_fcd_write_cstr(cstr, SCM_ENC_SRC, port);
    if (rslt < 0) return -1;
  }

  return 0;
}

void
scm_error_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_ERROR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  scm_exception_gc_initialize(obj, mem);

  SCM_ERROR(obj)->type = SCM_OBJ_NULL;
  SCM_ERROR(obj)->irritants = NULL;
  SCM_ERROR(obj)->nr_irris = 0;
}

void
scm_error_gc_fianlize(ScmObj obj)
{
  scm_error_finalize(obj);
}

int
scm_error_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ERROR_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  rslt = scm_exception_gc_accept(obj, mem, handler);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_ERROR(obj)->type, mem);
  if (scm_gc_ref_handler_failure_p(rslt)) return rslt;

  for (size_t i = 0; i < SCM_ERROR(obj)->nr_irris; i++) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                   SCM_ERROR(obj)->irritants[i], mem);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}


/*******************************************************************/
/*  Error (interface)                                              */
/*******************************************************************/

ScmObj
scm_fcd_error_new_cv(SCM_MEM_TYPE_T mtype, ScmObj msg,
                     ScmObj type, ScmObj *irris, size_t n)
{
  ScmObj exc = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&msg, &type,
                      &exc);

  scm_assert(scm_obj_null_p(type) || scm_fcd_symbol_p(type));
  scm_assert(n <= SCM_ERROR_IRRITANTS_MAX);

  exc = scm_fcd_mem_alloc(&SCM_ERROR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL;

  rslt = scm_error_initialize_cv(exc, msg, type, irris, n);

  if (rslt < 0) return SCM_OBJ_NULL;

  return exc;
}

ScmObj
scm_fcd_error_new_lst(SCM_MEM_TYPE_T mtype, ScmObj msg, ScmObj type, ScmObj irris)
{
  ScmObj exc = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&msg, &type, &irris,
                      &exc);

  scm_assert(scm_obj_null_p(type) || scm_fcd_symbol_p(type));
  scm_assert(scm_obj_not_null_p(irris));

  exc = scm_fcd_mem_alloc(&SCM_ERROR_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL;

  rslt = scm_error_initialize_lst(exc, msg, type, irris);

  if (rslt < 0) return SCM_OBJ_NULL;

  return exc;
}

static int
terror_aux(const char *type, const char *msg, size_t n, va_list arg)
{
  ScmObj sym = SCM_OBJ_INIT, str = SCM_OBJ_INIT, exc = SCM_OBJ_INIT;
  ScmObj irris[n];
  int rslt;

  SCM_REFSTK_INIT_REG(&sym, &str, &exc);

  if (scm_obj_null_p(scm_fcd_current_vm())) {
    scm_fcd_fatal("Error has occured while initializing or finalizing VM");
    return 0;
  }

  for (size_t i = 0; i < n; i++)
    irris[i] = va_arg(arg, ScmObj);

  SCM_REFSTK_REG_ARY(irris, n);

  sym = SCM_OBJ_NULL;
  if (type != NULL) {
    sym = scm_fcd_make_symbol_from_cstr(type, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;
  }

  str = scm_fcd_make_string_from_cstr((msg == NULL) ? "" : msg, SCM_ENC_SRC);
  if (scm_obj_null_p(str)) return -1;

  exc = scm_fcd_error_new_cv(SCM_MEM_HEAP, str, sym, irris, n);
  if (scm_obj_null_p(exc)) return -1;

  rslt = scm_fcd_raise(exc);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_error(const char *msg, size_t n, ...)
{
  va_list arg;
  int r;

  va_start(arg, n);
  r = terror_aux(NULL, msg, n, arg);
  va_end(arg);

  return r;
}

int
scm_fcd_read_error(const char *msg, size_t n, ...)
{
  va_list arg;
  int r;

  va_start(arg, n);
  r = terror_aux("read", msg, n, arg);
  va_end(arg);

  return r;
}

int
scm_fcd_file_error(const char *msg, size_t n, ...)
{
  va_list arg;
  int r;

  va_start(arg, n);
  r = terror_aux("file", msg, n, arg);
  va_end(arg);

  return r;
}

ScmObj
scm_fcd_error_lst(ScmObj msg, ScmObj irris)
{
  ScmObj exc = SCM_OBJ_INIT;
  int r;

  scm_assert(scm_fcd_string_p(msg));
  scm_assert(scm_obj_not_null_p(irris));

  if (scm_obj_null_p(scm_fcd_current_vm())) {
    scm_fcd_fatal("Error has occured while initializing or finalizing VM");
    return SCM_OBJ_NULL;
  }

  exc = scm_fcd_error_new_lst(SCM_MEM_HEAP, msg, SCM_OBJ_NULL, irris);
  if (scm_obj_null_p(exc)) return SCM_OBJ_NULL;

  r = scm_fcd_raise(exc);
  if (r < 0) return SCM_OBJ_NULL;

  return SCM_UNDEF_OBJ;
}

extern inline bool
scm_fcd_error_object_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_ERROR_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_error_object_P(ScmObj obj)
{
  return (scm_fcd_error_object_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

extern inline ScmObj
scm_fcd_error_object_message(ScmObj obj)
{
  scm_assert(scm_fcd_error_object_p(obj));
  return scm_exception_msg(obj);
}

extern inline ScmObj
scm_fcd_error_object_irritants(ScmObj obj)
{
  scm_assert(scm_fcd_error_object_p(obj));
  return scm_error_irris_to_list(obj);
}

static int
error_object_type_eq(ScmObj obj, const char *type, bool *rslt)
{
  ScmObj sym = SCM_OBJ_INIT, etype = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&obj,
                      &sym, &etype);

  if (!scm_fcd_error_object_p(obj)) {
    *rslt = false;
    return 0;
  }

  etype = scm_error_type(obj);

  sym = SCM_OBJ_NULL;
  if (type != NULL) {
    sym = scm_fcd_make_symbol_from_cstr(type, SCM_ENC_SRC);
    if (scm_obj_null_p(sym)) return -1;
  }

  *rslt = scm_fcd_eq_p(sym, etype);

  return 0;
}

ScmObj
scm_fcd_read_error_P(ScmObj obj)
{
  bool cmp;
  int r;

  r = error_object_type_eq(obj, "read", &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return (cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}

ScmObj
scm_fcd_file_error_P(ScmObj obj)
{
  bool cmp;
  int r;

  r = error_object_type_eq(obj, "file", &cmp);
  if (r < 0) return SCM_OBJ_NULL;

  return (cmp ? SCM_TRUE_OBJ : SCM_FALSE_OBJ);
}
