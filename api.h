#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include "object.h"
#include "memory.h"
#include "vm.h"
#include "string.h"
#include "symbol.h"
#include "procedure.h"
#include "gloc.h"
#include "pair.h"


/*******************************************************************/
/*  Predicate                                                      */
/*******************************************************************/

static inline ScmObj
scm_api_eq_p(ScmObj obj1, ScmObj obj2)
{
  if (scm_obj_null_p(obj1) || scm_obj_null_p(obj2))
      return SCM_OBJ_NULL;         /* provisional implemntation */

  return (scm_obj_same_instance_p(obj1, obj2) ?
          scm_vm_bool_true_instance() : scm_vm_bool_false_instance());
}

ScmObj scm_api_eqv_p(ScmObj obj1, ScmObj obj2); /* TODO: write me */
ScmObj scm_api_equal_p(ScmObj obj1, ScmObj obj2); /* TODO: write me */


/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

static inline ScmObj
scm_api_make_string_ascii(const char *str)
{
  if (str == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_string_new(SCM_MEM_ALLOC_HEAP,
                        str, strlen(str), SCM_ENCODING_ASCII);
}


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

static inline ScmObj
scm_api_symbol_to_string(ScmObj sym)
{
  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    /* TODO: ランタイムエラーをどう処理するか。*/
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return SCM_SYMBOL_STR(sym);
}

static inline ScmObj
scm_api_string_to_symbol(ScmObj str)
{
  if (scm_obj_null_p(str) || !scm_obj_type_p(str, &SCM_STRING_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_symtbl_symbol(scm_vm_current_symtbl(), str);
}

static inline ScmObj
scm_api_make_symbol_ascii(const char *str)
{
  if (str == NULL)
    return SCM_OBJ_NULL;        /* provisional implemntation */

  return scm_api_string_to_symbol(scm_api_make_string_ascii(str));
}


/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

static inline ScmObj
scm_api_cons(ScmObj car, ScmObj cdr)
{
  if (scm_obj_null_p(car) || scm_obj_null_p(cdr))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_new(SCM_MEM_ALLOC_HEAP, car, cdr);
}

static inline ScmObj
scm_api_car(ScmObj pair)
{
  if (scm_obj_null_p(pair) || !scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_car(pair);
}

static inline ScmObj
scm_api_cdr(ScmObj pair)
{
  if (scm_obj_null_p(pair) || !scm_obj_type_p(pair, &SCM_PAIR_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_pair_cdr(pair);
}


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

static inline ScmObj
scm_api_make_subrutine(ScmSubrFunc func)
{
  if (func == NULL)
    return SCM_OBJ_NULL;         /* provisional implemntation */

  return scm_subrutine_new(SCM_MEM_ALLOC_HEAP, func);
}


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

static inline ScmObj
scm_api_global_var_ref(ScmObj sym)
{
  ScmObj gloc = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&sym, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  rslt = scm_gloctbl_find(scm_vm_current_gloctbl(), sym, SCM_REF_MAKE(gloc));
  if (rslt != 0) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  /* 未束縛変数の参照の場合は SCM_OBJ_NULL を返す */
  return (scm_obj_null_p(gloc) ?  SCM_OBJ_NULL : scm_gloc_value(gloc));
}

static inline ScmObj
scm_api_global_var_bound_p(ScmObj sym)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &o);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  SCM_SETQ(o, scm_api_global_var_ref(sym));

  return (scm_obj_null_p(o) ?
          scm_vm_bool_false_instance() : scm_vm_bool_true_instance());
}

static inline ScmObj
scm_api_global_var_define(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */
  if (scm_obj_null_p(val))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  SCM_SETQ(gloc, scm_gloctbl_bind(scm_vm_current_gloctbl(), sym, val));
  if (scm_obj_null_p(gloc)) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  return val;
}

static inline ScmObj
scm_api_global_var_set(ScmObj sym, ScmObj val)
{
  ScmObj gloc = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &val, &gloc);

  if (scm_obj_null_p(sym) || !scm_obj_type_p(sym, &SCM_SYMBOL_TYPE_INFO))
    return SCM_OBJ_NULL;         /* provisional implemntation */
  if (scm_obj_null_p(val))
    return SCM_OBJ_NULL;         /* provisional implemntation */

  /* 未束縛変数の参照の場合は SCM_OBJ_NULL を返す */
  if (scm_obj_same_instance_p(scm_api_global_var_bound_p(sym),
                               scm_vm_bool_false_instance()))
    return SCM_OBJ_NULL;

  SCM_SETQ(gloc, scm_gloctbl_bind(scm_vm_current_gloctbl(), sym, val));
  if (scm_obj_null_p(gloc)) {
    ;                           /* TODO: error handling */
    return SCM_OBJ_NULL;
  }

  return val;
}


/*******************************************************************/
/*  Access to Argument of Function                                 */
/*******************************************************************/

static inline int
scm_api_get_nr_func_arg(void)
{
  /* 2012.02.07: 今のところスタックフレームが全く無い状態でこの api を
     呼ばれると assertion に引っ掛って落ちる */
  return scm_vm_frame_argc(scm_vm_current_vm());
}

static inline ScmObj
scm_api_get_func_arg(int nth)
{
  if (nth >= scm_vm_frame_argc(scm_vm_current_vm()))
    return SCM_OBJ_NULL;                  /* provisional implemntation */

  return scm_vm_frame_argv(scm_vm_current_vm(), nth);
}


#endif /* INCLUDE_API_H__ */
