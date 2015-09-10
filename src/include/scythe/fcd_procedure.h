#ifndef INCLUDE_FCD_PROCEDURE_H__
#define INCLUDE_FCD_PROCEDURE_H__

#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/fcd_memory.h"

/*******************************************************************/
/*  Procedure                                                      */
/*******************************************************************/

enum {
  SCM_PROC_ADJ_UNWISHED = 0x0001,
};

bool scm_fcd_procedure_p(ScmObj proc);
ScmObj scm_fcd_procedure_P(ScmObj proc);
int scm_fcd_arity(ScmObj proc);
bool scm_fcd_procedure_flg_set_p(ScmObj proc, unsigned int flg);


/*******************************************************************/
/*  Subrutine                                                      */
/*******************************************************************/

typedef int (*ScmSubrFunc)(ScmObj subr, int argc, const ScmObj *argv);

bool scm_fcd_subrutine_p(ScmObj obj);
ScmObj scm_fcd_subrutine_new(scm_mem_type_t mtype,
                             ScmSubrFunc func, ScmObj name, int arity,
                             unsigned int flags, ScmObj env);
ScmObj scm_fcd_make_subrutine(ScmSubrFunc func, int arity, unsigned int flags,
                              ScmObj env);
int scm_fcd_call_subrutine(ScmObj subr, int argc, const ScmObj *argv);
ScmObj scm_fcd_subrutine_env(ScmObj subr);


/*******************************************************************/
/*  Closure                                                        */
/*******************************************************************/

bool scm_fcd_closure_p(ScmObj obj);
ScmObj scm_fcd_closure_new(scm_mem_type_t mtype,
                           ScmObj iseq, ScmObj env, ScmObj name, int arity);
ScmObj scm_fcd_make_closure(ScmObj iseq, ScmObj env, int arity);
ScmObj scm_fcd_closure_to_iseq(ScmObj clsr);
scm_byte_t *scm_fcd_closure_to_ip(ScmObj clsr);
ScmObj scm_fcd_closure_env(ScmObj clsr);


/*******************************************************************/
/*  Parameter                                                      */
/*******************************************************************/

bool scm_fcd_parameter_p(ScmObj obj);
ScmObj scm_fcd_parameter_new(scm_mem_type_t mtype, ScmObj init, ScmObj conv);
ScmObj scm_fcd_make_parameter(ScmObj init, ScmObj conv);
ScmObj scm_fcd_parameter_init_val(ScmObj prm);
ScmObj scm_fcd_parameter_converter(ScmObj prm);
void scm_fcd_parameter_set_init_val(ScmObj prm, ScmObj val);


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

bool scm_fcd_continuation_p(ScmObj obj);
ScmObj scm_fcd_continuation_new(scm_mem_type_t mtype, ScmObj contcap);
ScmObj scm_fcd_make_continuation(void);


#endif /* INCLUDE_FCD_PROCEDURE_H__ */
