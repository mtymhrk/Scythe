
#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"

/*******************************************************************/
/*  nil                                                            */
/*******************************************************************/

int
scm_subr_func_null_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr, &val);

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
/*  Pair and Lists                                                 */
/*******************************************************************/

int
scm_subr_func_pair_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_pair_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cons(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_cons(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_car(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_car(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_cdr(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_cdr(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_car(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_set_car(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_set_cdr(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_set_cdr(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_P(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_P(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_make_list(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT, fill = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val, &fill);

  if (argc > 2) {
    scm_capi_error("make-list: too many arguments", 0);
    return -1;
  }

  fill = (argc > 1) ? argv[1] : SCM_OBJ_NULL;
  val = scm_api_make_list(argv[0], fill);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list(ScmObj subr, int argc, const ScmObj *argv)
{
  return scm_capi_return_val(argv, 1);
}

int
scm_subr_func_length(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_length(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_append(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_capi_append_cv(argv, (size_t)argc);
  if (scm_obj_null_p(val)) return SCM_OBJ_NULL;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_reverse(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_reverse(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_tail(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_tail(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_ref(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_ref(argv[0], argv[1]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_list_set(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_set(argv[0], argv[1], argv[2]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_memq(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("memq: not implemented", 0);
  return -1;
}

int
scm_subr_func_memv(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("memv: not implemented", 0);
  return -1;
}

int
scm_subr_func_member(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("member: not implemented", 0);
  return -1;
}

int
scm_subr_func_assq(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assq: not implemented", 0);
  return -1;
}

int
scm_subr_func_assv(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assv: not implemented", 0);
  return -1;
}

int
scm_subr_func_assoc(ScmObj subr, int argc, const ScmObj *argv)
{
  scm_capi_error("assoc: not implemented", 0);
  return -1;
}

int
scm_subr_func_list_copy(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

  val = scm_api_list_copy(argv[0]);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

static ScmObj
scm_get_current_port(ScmObj subr, const char *var)
{
  ScmObj mod = SCM_OBJ_INIT;
  ScmObj sym = SCM_OBJ_INIT, prm = SCM_OBJ_INIT, port = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &mod, &prm, &port);

  rslt = scm_capi_subrutine_module(subr, SCM_CSETTER_L(mod));
  if (rslt < 0) return SCM_OBJ_NULL;

  sym = scm_capi_make_symbol_from_cstr(var, SCM_ENC_ASCII);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  rslt = scm_capi_global_var_ref(mod, sym, SCM_CSETTER_L(prm));
  if (rslt < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(prm)) {
    scm_capi_error("faild to get current port", 0);
    return SCM_OBJ_NULL;
  }

  port = scm_capi_parameter_value(prm);
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  return port;
}

int
scm_subr_func_read(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-input-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("read: too many arugments", 0);
    return -1;
  }

  val = scm_api_read(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_write(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("write: too many arugments", 0);
    return -1;
  }

  val = scm_api_write(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_display(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_NULL;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[1]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[1]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("display: too many arugments", 0);
    return -1;
  }

  val = scm_api_display(argv[0], port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_newline(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
    scm_capi_error("newline: too many arugments", 0);
    return -1;
  }

  val = scm_api_newline(port);
  if (scm_obj_null_p(val)) return -1;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_flush_output_port(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &val);

  len = scm_capi_length(argv[0]);
  if (len < 0) return -1;

  if (len == 0) {
    port = scm_get_current_port(subr, "current-output-port");
    if (scm_obj_null_p(port)) return -1;
  }
  else if (len == 1) {
    port = scm_api_car(argv[0]);
    if (scm_obj_null_p(port)) return -1;
  }
  else {
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
scm_subr_func_callcc(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj cont = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &cont, &args, &val);

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

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Multiple Return Values                                         */
/*******************************************************************/

int
scm_subr_func_values(ScmObj subr, int argc, const ScmObj *argv)
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
scm_subr_func_eval_asm(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj code = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t i;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr,
                       &code, &args, &val);

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

  args = SCM_NIL_OBJ;

  rslt = scm_capi_trampolining(code, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1; /* [ERR]: [through] */

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}

int
scm_subr_func_eval(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj exp = SCM_OBJ_INIT, args = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  ssize_t i;
  int rslt;

  SCM_STACK_FRAME_PUSH(&subr, &exp, &args, &val);

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

  args = SCM_NIL_OBJ;

  rslt = scm_capi_trampolining(exp, args, SCM_OBJ_NULL, SCM_OBJ_NULL);
  if (rslt < 0) return -1; /* [ERR]: [through] */

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

int
scm_subr_func_exit(ScmObj subr, int argc, const ScmObj *argv)
{
  ScmObj val = SCM_OBJ_INIT;
  ssize_t len;

  SCM_STACK_FRAME_PUSH(&subr,
                       &val);

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

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}


/*******************************************************************/
/*  Default Exception Handler                                      */
/*******************************************************************/

int
scm_subr_func_default_exception_handler(ScmObj subr,
                                        int argc, const ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT, ro = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&subr,
                       &port, &ro, &val);

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

  ro = scm_api_exit(SCM_UNDEF_OBJ);
  if (scm_obj_null_p(ro)) return SCM_OBJ_NULL;

  val = SCM_UNDEF_OBJ;

  return scm_capi_return_val(&val, 1);
}
