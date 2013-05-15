#ifndef INCLUDE_CORE_SUBR_H__
#define INCLUDE_CORE_SUBR_H__

#include <stdint.h>

#include "object.h"

/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

#define SCM_SUBR_ARITY_NULL_P 1

#define SCM_SUBR_FLAG_NULL_P 0

int scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Pair and Lists                                                 */
/*******************************************************************/

#define SCM_SUBR_ARITY_PAIR_P 1
#define SCM_SUBR_ARITY_CONS 2
#define SCM_SUBR_ARITY_CAR 1
#define SCM_SUBR_ARITY_CDR 1
#define SCM_SUBR_ARITY_SET_CAR 2
#define SCM_SUBR_ARITY_SET_CDR 2
#define SCM_SUBR_ARITY_LIST_P 1
#define SCM_SUBR_ARITY_MAKE_LIST -2
#define SCM_SUBR_ARITY_LIST -1
#define SCM_SUBR_ARITY_LENGTH 1
#define SCM_SUBR_ARITY_APPEND -1
#define SCM_SUBR_ARITY_REVERSE 1
#define SCM_SUBR_ARITY_LIST_TAIL 2
#define SCM_SUBR_ARITY_LIST_REF 2
#define SCM_SUBR_ARITY_LIST_SET 3
#define SCM_SUBR_ARITY_LIST_COPY 1

#define SCM_SUBR_FLAG_PAIR_P 0
#define SCM_SUBR_FLAG_CONS 0
#define SCM_SUBR_FLAG_CONS 0
#define SCM_SUBR_FLAG_CAR 0
#define SCM_SUBR_FLAG_CDR 0
#define SCM_SUBR_FLAG_SET_CAR 0
#define SCM_SUBR_FLAG_SET_CDR 0
#define SCM_SUBR_FLAG_LIST_P 0
#define SCM_SUBR_FLAG_MAKE_LIST SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_LIST 0
#define SCM_SUBR_FLAG_LENGTH 0
#define SCM_SUBR_FLAG_APPEND SCM_PROC_ADJ_UNWISHED
#define SCM_SUBR_FLAG_REVERSE 0
#define SCM_SUBR_FLAG_LIST_TAIL 0
#define SCM_SUBR_FLAG_LIST_REF 0
#define SCM_SUBR_FLAG_LIST_SET 0
#define SCM_SUBR_FLAG_LIST_COPY 0

int scm_subr_func_pair_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_cons(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_car(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_cdr(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_set_car(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_set_cdr(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_P(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_make_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_length(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_append(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_reverse(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_tail(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_ref(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_set(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_list_copy(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

#define SCM_SUBR_ARITY_READ -1
#define SCM_SUBR_ARITY_WRITE -2
#define SCM_SUBR_ARITY_DISPLAY -2
#define SCM_SUBR_ARITY_NEWLINE -1
#define SCM_SUBR_ARITY_FLUSH_OUTPUT_PORT -1

#define SCM_SUBR_FLAG_READ 0
#define SCM_SUBR_FLAG_WRITE 0
#define SCM_SUBR_FLAG_DISPLAY 0
#define SCM_SUBR_FLAG_NEWLINE 0
#define SCM_SUBR_FLAG_FLUSH_OUTPUT_PORT 0

int scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

#define SCM_SUBR_ARITY_CALLCC 1

#define SCM_SUBR_FLAG_CALLCC 0

int scm_subr_func_callcc(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Multiple Return Values                                         */
/*******************************************************************/

#define SCM_SUBR_ARITY_VALUES -1

#define SCM_SUBR_FLAG_VALUES SCM_PROC_ADJ_UNWISHED

int scm_subr_func_values(ScmObj subr, int argc, const ScmObj *argv);

extern const char *scm_clsr_code_call_with_values;


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

#define SCM_SUBR_ARITY_EVAL_ASM 1
#define SCM_SUBR_ARITY_EVAL 1

#define SCM_SUBR_FLAG_EVAL_ASM 0
#define SCM_SUBR_FLAG_EVAL 0

int scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv);
int scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

#define SCM_SUBR_ARITY_EXIT -1

#define SCM_SUBR_FLAG_EXIT 0

int scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv);


/*******************************************************************/
/*  Default Exception Handler                                      */
/*******************************************************************/

#define SCM_SUBR_ARITY_DEFAULT_EXCEPTION_HANDLER 1

#define SCM_SUBR_FLAG_DEFAULT_EXCEPTION_HANDLER 0

int scm_subr_func_default_exception_handler(ScmObj subr,
                                            int argc, const ScmObj *argv);


#endif /* INCLUDE_CORE_SUBR_H__ */
