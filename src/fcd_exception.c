#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/vm.h"
#include "scythe/exception.h"

int
scm_fcd_raise(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_fcd_current_vm(), obj, false);
}

int
scm_fcd_raise_continuable(ScmObj obj)
{
  return scm_vm_setup_stat_raise(scm_fcd_current_vm(), obj, true);
}

bool
scm_fcd_raised_p(void)
{
  return scm_vm_raised_p(scm_fcd_current_vm());
}

ScmObj
scm_fcd_raised_obj(void)
{
  return scm_vm_raised_obj(scm_fcd_current_vm());
}

void
scm_fcd_discard_raised_obj(void)
{
  scm_vm_discard_raised_obj(scm_fcd_current_vm());
}

int
scm_fcd_push_exception_handler(ScmObj handler)
{
  return scm_vm_push_exc_handler(scm_fcd_current_vm(), handler);
}

int
scm_fcd_pop_exception_handler(void)
{
  return scm_vm_pop_exc_handler(scm_fcd_current_vm());
}

void
scm_fcd_disposal_unhandled_exec(void)
{
  scm_vm_disposal_unhandled_exc(scm_fcd_current_vm());
}

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
