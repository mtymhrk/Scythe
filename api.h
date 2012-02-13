#ifndef INCLUDE_API_H__
#define INCLUDE_API_H__

#include "object.h"

/*******************************************************************/
/*  Predicate                                                      */
/*******************************************************************/

ScmObj scm_api_eq_p(ScmObj obj1, ScmObj obj2);


/*******************************************************************/
/*  String                                                         */
/*******************************************************************/

ScmObj scm_api_make_string_ascii(const char *str);


/*******************************************************************/
/*  Symbol                                                         */
/*******************************************************************/

ScmObj scm_api_make_symbol_ascii(const char *str);
ScmObj scm_api_symbol_to_string(ScmObj sym);
ScmObj scm_api_string_to_symbol(ScmObj str);


/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

ScmObj scm_api_cons(ScmObj car, ScmObj cdr);
ScmObj scm_api_car(ScmObj pair);
ScmObj scm_api_cdr(ScmObj pair);


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

ScmObj scm_api_make_subrutine(ScmSubrFunc func);


/*******************************************************************/
/*  Global Variable                                                */
/*******************************************************************/

ScmObj scm_api_global_var_ref(ScmObj sym);
ScmObj scm_api_global_var_bound_p(ScmObj sym);
ScmObj scm_api_global_var_define(ScmObj sym, ScmObj val);
ScmObj scm_api_global_var_set(ScmObj sym, ScmObj val);


/*******************************************************************/
/*  Access to Argument of Function                                 */
/*******************************************************************/

int scm_api_get_nr_func_arg(void);
ScmObj scm_api_get_func_arg(int nth);

#endif /* INCLUDE_API_H__ */
