#include <unistd.h>
#include <string.h>
#include <assert.h>


#include "object.h"
#include "memory.h"
#include "basichash.h"
#include "reference.h"
#include "string.h"
#include "vm.h"
#include "symbol.h"

#define SCM_SYMBOL_TABLE_SIZE 256


ScmTypeInfo SCM_SYMBOL_TYPE_INFO = {
  NULL,                         /* pp_func              */
  sizeof(ScmSymbol),            /* obj_size             */
  scm_symbol_gc_initialize,     /* gc_ini_func          */
  NULL,                         /* gc_fin_func          */
  scm_symbol_gc_accept,         /* gc_accept_func       */
  NULL,                         /* gc_accpet_func_weak  */
};

void
scm_symbol_initialize(ScmObj sym, ScmObj str) /* GC OK */
{
  SCM_STACK_FRAME_PUSH(&sym, &str);

  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);
  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_obj_init(sym, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ(SCM_SYMBOL_STR(sym), scm_string_dup(str));
}

ScmObj
scm_symbol_new(SCM_MEM_ALLOC_TYPE_T mtype, ScmObj str) /* GC OK */
{
  ScmObj sym = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &sym);

  SCM_OBJ_ASSERT_TYPE(str, &SCM_STRING_TYPE_INFO);

  scm_mem_alloc(scm_vm_current_mm(),
                &SCM_SYMBOL_TYPE_INFO, mtype, SCM_REF_MAKE(sym));
  scm_symbol_initialize(sym, str);

  return sym;
}

bool
scm_symbol_is_symbol(ScmObj obj) /* GC OK */
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);
}

size_t
scm_symbol_length(ScmObj sym)   /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_length(SCM_SYMBOL_STR(sym));
}

ScmObj
scm_symbol_string(ScmObj sym)
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_dup(SCM_SYMBOL_STR(sym));
}

size_t
scm_symbol_hash_value(ScmObj sym)
{
  SCM_OBJ_ASSERT_TYPE(sym, &SCM_SYMBOL_TYPE_INFO);

  return scm_string_hash_value(SCM_SYMBOL_STR(sym));
}

void
scm_symbol_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  SCM_SETQ_PRIM(SCM_SYMBOL_STR(obj), SCM_OBJ_NULL);
}

int
scm_symbol_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  SCM_OBJ_ASSERT_TYPE(obj, &SCM_SYMBOL_TYPE_INFO);

  rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, SCM_SYMBOL_STR(obj), mem);

  return rslt;
}
