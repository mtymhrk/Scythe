#include "object.h"
#include "vm.h"
#include "api.h"
#include "core_subr.h"

/*******************************************************************/
/*  List and Pair                                                  */
/*******************************************************************/


ScmObj
scm_subr_func_cons(int argc, ScmObj *argv)
{
  if (argc != 2) {
    /* TODO: change error message */
    scm_capi_error("cons: 2 arugments required, but got ", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_cons(argv[0], argv[1]);
}

ScmObj
scm_subr_func_car(int argc, ScmObj *argv)
{
  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("car: 1 arugment required, but got ", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_car(argv[0]);
}

ScmObj
scm_subr_func_cdr(int argc, ScmObj *argv)
{
  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("cdr: 1 arugment required, but got ", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_cdr(argv[0]);
}


/*******************************************************************/
/*  Input Output                                                   */
/*******************************************************************/

ScmObj
scm_subr_func_read(int argc, ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT;

  if (argc == 0)
    port = scm_api_current_input_port();
  else if (argc == 1)
    port = argv[0];
  else {
    scm_capi_error("read: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_read(port);
}

ScmObj
scm_subr_func_write(int argc, ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT;

  if (argc == 1)
    port = scm_api_current_output_port();
  else if (argc == 2)
    port = argv[1];
  else {
    scm_capi_error("write: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_write(argv[0], port);
}

ScmObj
scm_subr_func_display(int argc, ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT;

  if (argc == 1)
    port = scm_api_current_output_port();
  else if (argc == 2)
    port = argv[1];
  else {
    scm_capi_error("display: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_display(argv[0], port);
}

ScmObj
scm_subr_func_newline(int argc, ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT;

  if (argc == 0)
    port = scm_api_current_output_port();
  else if (argc == 1)
    port = argv[1];
  else {
    scm_capi_error("newline: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_newline(port);
}

ScmObj
scm_subr_func_flush_output_port(int argc, ScmObj *argv)
{
  ScmObj port = SCM_OBJ_INIT;

  if (argc == 0)
    port = scm_api_current_output_port();
  else if (argc == 1)
    port = argv[0];
  else {
    scm_capi_error("flush-output-port: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_flush_output_port(port);
}


/*******************************************************************/
/*  Eval                                                           */
/*******************************************************************/

ScmObj
scm_subr_func_eval_asm(int argc, ScmObj *argv)
{
  ScmObj code = SCM_OBJ_INIT;
  ScmObj args = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&code, &args);

  if (argc != 1) {
    /* TODO: change error message */
    scm_capi_error("eval-asm: 1 argument is require, but got ", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_capi_pair_p(argv[0])) {
    code = scm_api_assemble(argv[0]);
    if (scm_obj_null_p(code)) return SCM_OBJ_NULL; /* [ERR]: [through] */
  }
  else if (scm_capi_iseq_p(argv[0])) {
    code = argv[0];
  }
  else {
    scm_capi_error("eval-asm: argument is not pair or iseq", 1, argv[0]);
    return SCM_OBJ_NULL;
  }

  args = scm_api_nil();

  rslt = scm_capi_trampolining(code, args, argc, NULL);
  if (rslt < 0) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_api_undef();
}


/*******************************************************************/
/*  Process-Context Library Procedure                              */
/*******************************************************************/

ScmObj
scm_subr_func_exit(int argc, ScmObj *argv)
{
  if (argc == 0)
    scm_api_exit(SCM_OBJ_NULL);
  else if (argc == 1)
    scm_api_exit(argv[0]);
  else {
    scm_capi_error("exit: too many arguments", 0);
    return SCM_OBJ_NULL;
  }

  return scm_api_undef();
}


/*******************************************************************/
/*******************************************************************/

void
scm_core_subr_system_setup(void)
{
  const char *syms[] = { "cons", "car", "cdr", "read", "write", "display",
                         "newline", "flush-output-port", "eval-asm", "exit" };
  ScmSubrFunc funcs[] = { scm_subr_func_cons, scm_subr_func_car,
                          scm_subr_func_cdr, scm_subr_func_read,
                          scm_subr_func_write, scm_subr_func_display,
                          scm_subr_func_newline,
                          scm_subr_func_flush_output_port,
                          scm_subr_func_eval_asm, scm_subr_func_exit };
  ScmObj sym  = SCM_OBJ_INIT;
  ScmObj subr = SCM_OBJ_INIT;
  ScmObj rslt = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &subr, &rslt);

  for (size_t i = 0; i < sizeof(syms)/sizeof(syms[0]); i++) {
    sym = scm_capi_make_symbol_from_cstr(syms[i], SCM_ENC_ASCII);
    subr = scm_capi_make_subrutine(funcs[i]);
    rslt = scm_api_global_var_define(sym, subr);

    if (scm_obj_null_p(rslt) || scm_obj_null_p(subr) ||scm_obj_null_p(sym))
      return;                   /* [ERR]: [through] */
  }

}

