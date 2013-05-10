
#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"

/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

int
scm_subr_func_null_P(int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("cons: 2 arugments required, but got ", 0);
    return -1;
  }

  val = scm_api_nil_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/

int
scm_subr_func_cons(int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val);

  if (argc != 2) {
    /* TODO: change error message */
    scm_capi_error("cons: 2 arugments required, but got ", 0);
    return -1;
  }

  val = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_car(int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("car: 1 arugment required, but got ", 0);
    return -1;
  }

  val = scm_api_car(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cdr(int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&val);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("cdr: 1 arugment required, but got ", 0);
    return -1;
  }

  val = scm_api_cdr(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

int
scm_subr_func_read(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  port = SCM_OBJ_NULL;
  if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("read: too many arugments", 0);
    return -1;
  }

  val = scm_api_read(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_write(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  port = SCM_OBJ_NULL;
  if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("write: too many arugments", 0);
    return -1;
  }

  val = scm_api_write(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_display(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_NULL;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  port = SCM_OBJ_NULL;
  if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("display: too many arugments", 0);
    return -1;
  }

  val = scm_api_display(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_newline(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  port = SCM_OBJ_NULL;
  if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("newline: too many arugments", 0);
    return -1;
  }

  val = scm_api_newline(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_flush_output_port(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  port = SCM_OBJ_NULL;
  if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("flush-output-port: too many arugments", 0);
    return -1;
  }

  val = scm_api_flush_output_port(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Continuation                                                   */
/*******************************************************************/

int
scm_subr_func_callcc(int argc, const ScmObj *argv)
{
  ScmObj cont = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&cont, &args, &val);

  if (argc != 1) {
    scm_capi_error("call/cc: 1 argumetn is require, but got ", 0);
    return -1;
  }

  cont = scm_capi_capture_cont();
  if (scm_obj_null_p(cont)) return -1;

  args = scm_capi_list(1, cont);
  if (scm_obj_null_p(args)) return -1;

  rslt = scm_capi_trampolining(argv[0], args, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1; /* [ERR]: [through] */

  val = scm_api_undef();

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Multiple Return Values                                         */
/*******************************************************************/

int
scm_subr_func_values(int argc, const ScmObj *argv)
{
  return scm_capi_return_val(argv, argc);
}

const char *scm_clsr_code_call_with_values =
  "((eframe)"
  " (cframe)"
  " (sref 0 0)"
  " (call 0)"
  " (arity -1)"
  " (mvpush)"
  " (sref 1 0)"
  " (tapply)"
  ")";


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

int
scm_subr_func_eval_asm(int argc, const ScmObj *argv)
{
  ScmObj code = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t i;
  int rslt;

  SCM_STACK_FRAME_PUSH(&code, &args, &val);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("eval-asm: 1 argument is require, but got ", 0);
    return -1;
  }

  if (scm_capi_pair_p(argv[0])) {
    code = scm_api_assemble(argv[0]);
    if (scm_obj_null_p(code)) return -1; /* [ERR]: [through] */
  }
  else if (scm_capi_iseq_p(argv[0])) {
    code = argv[0];
  }
  else {
    scm_capi_error("eval-asm: argument is not pair or iseq", 1, argv[0]);
    return -1;
  }

  i = scm_capi_iseq_push_opfmt_noarg(code, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  code = scm_capi_make_closure(code, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(code)) return -1; /* [ERR]: [through] */

  args = scm_api_nil();

  rslt = scm_capi_trampolining(code, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1; /* [ERR]: [through] */

  val = scm_api_undef();

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eval(int argc, const ScmObj *argv)
{
  ScmObj exp = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t i;
  int rslt;

  SCM_STACK_FRAME_PUSH(&exp, &args, &val);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("eval-asm: 1 argument is require, but got ", 0);
    return -1;
  }

  exp = scm_api_compile(argv[0], SCM_OBJ_NULL);
  if (scm_obj_null_p(exp)) return -1; /* [ERR]: [through] */

  exp = scm_api_assemble(exp);
  if (scm_obj_null_p(exp)) return -1; /* [ERR]: [through] */

  i = scm_capi_iseq_push_opfmt_noarg(exp, SCM_OPCODE_RETURN);
  if (i < 0) return -1;

  exp = scm_capi_make_closure(exp, SCM_OBJ_NULL, 0);
  if (scm_obj_null_p(exp)) return -1; /* [ERR]: [through] */

  args = scm_api_nil();

  rslt = scm_capi_trampolining(exp, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1; /* [ERR]: [through] */

  val = scm_api_undef();

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

int
scm_subr_func_exit(int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  val = SCM_OBJ_NULL;
  if (len == 1) {
    val = scm_api_car(argv[0]);
    if (scm_obj_null_p(val)) return -1;
  }
  else if (len > 1) {
    scm_capi_error("exit: too many arugments", 0);
    return -1;
  }

  val = scm_api_exit(val);
  if (scm_obj_null_p(val)) return -1;

  val = scm_api_undef();

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Default Exception Handler                                      */
/*******************************************************************/

int
scm_subr_func_default_exception_handler(int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, ro = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &ro, &val);

  if (argc < 1) {
    scm_capi_error("Exception Handler: too few arguments", 0);
    return SCM_OBJ_NULL;
  }
  else if (argc > 1) {
    scm_capi_error("Exception Handler: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  port = scm_api_standard_error_port();
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  ro = scm_api_display(argv[0], port);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  ro = scm_api_newline(port);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  ro = scm_api_flush_output_port(port);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  ro = scm_api_exit(scm_api_undef());
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  val = scm_api_undef();

  return scm_capi_return_val(&val, 1);
}
