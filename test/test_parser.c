#include <cutter.h>

#include "object.h"
#include "vm.h"
#include "reference.h"
#include "api.h"
#include "parser.h"

static ScmParser *parser;
static ScmObj vm = SCM_OBJ_INIT;
static ScmObj port = SCM_OBJ_INIT;

ScmParser *
new_parser()
{
  ScmLexer *lexer = scm_lexer_new();

  parser = scm_parser_new(lexer);

  return parser;
}

ScmObj
new_port(const char *str)
{
  port = scm_capi_open_input_string_port_from_cstr(str, SCM_ENC_ASCII);

  return port;
}

void
cut_setup(void)
{
  parser = NULL;
  vm = scm_vm_new();
  port = SCM_OBJ_NULL;
  scm_mem_register_extra_rfrn(scm_vm_current_mm(), SCM_REF_MAKE(port));
}

void
cut_teardown(void)
{
  port = SCM_OBJ_NULL;
  scm_vm_end(vm);
  vm = SCM_OBJ_NULL;
  parser = NULL;
}

void
test_parser_new(void)
{
  ScmLexer *lexer = scm_lexer_new();
  ScmParser *parser = scm_parser_new(lexer);

  cut_assert_not_null(parser);
}

void
test_parser_parse_string(void)
{
  char actual[256];
  ScmObj str = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&str, &eof);

  new_parser();
  new_port("\"this is string\"");

  str = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_string_p(str));
  cut_assert_equal_int(14, scm_capi_string_length(str));
  scm_capi_string_to_cstr(str, actual, sizeof(actual));
  cut_assert_equal_string("this is string", actual);

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_symbol(void)
{
  char actual[256];
  ScmObj sym = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sym, &eof);

  new_parser();
  new_port(" symbol ");

  sym = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_symbol_p(sym));
  scm_capi_symbol_to_cstr(sym, actual, sizeof(actual));
  cut_assert_equal_string("symbol", actual);

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_fixnum(void)
{
  ScmObj num = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num, &eof);

  new_parser();
  new_port(" 100 ");

  num = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_fixnum_p(num));
  cut_assert_equal_int(100, scm_capi_fixnum_to_clong(num));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_fixnum_signed(void)
{
  ScmObj num1 = SCM_OBJ_INIT, num2 = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&num1, &num2, &eof);

  new_parser();
  new_port(" +98 -23");

  num1 = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_fixnum_p(num1));
  cut_assert_equal_int(98, scm_capi_fixnum_to_clong(num1));

  num2 = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_fixnum_p(num2));
  cut_assert_equal_int(-23, scm_capi_fixnum_to_clong(num2));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}


void
test_parser_parse_bool_true(void)
{
  ScmObj bl = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl, &eof);

  new_parser();
  new_port(" #t ");

  bl = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_true_p(bl));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_bool_false(void)
{
  ScmObj bl = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&bl, &eof);

  new_parser();
  new_port(" #t ");

  bl = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_false_p(bl));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_char_newline(void)
{
  scm_char_t actual;
  ssize_t rslt;
  ScmObj nl = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&nl, &eof);

  new_parser();
  new_port(" #\\newline ");

  nl = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_char_p(nl));
  rslt = scm_capi_char_to_cchar(nl, &actual);
  cut_assert_equal_int(1, rslt);
  cut_assert_equal_int('\n', actual.ascii);

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_char_space(void)
{
  scm_char_t actual;
  ssize_t rslt;
  ScmObj sp = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&sp, &eof);

  new_parser();
  new_port(" #\\space ");

  sp = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_char_p(sp));
  rslt = scm_capi_char_to_cchar(sp, &actual);
  cut_assert_equal_int(1, rslt);
  cut_assert_equal_int(' ', actual.ascii);

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_quote(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port(" '^abc ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("quote",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("^abc",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_quasiquote(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port(" `12abc ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("quasiquote",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("12abc",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_unquote(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port(" ,&ab-12 ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("unquote",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("&ab-12",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_unquote_splicing(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT;
  ScmObj cdr = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port(" ,@?abc.12 ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("unquote-splicing", SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("?abc.12",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_empty_list(void)
{
  ScmObj obj = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&obj, &eof);

  new_parser();
  new_port(" () ");

  obj = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_nil_p(obj));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_proper_list(void)
{
  char actual[256];
  ScmObj lst = SCM_OBJ_INIT, car= SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port("(<abc> 123 \"str\" :def:)");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("<abc>",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_fixnum_p(car));
  cut_assert_equal_int(123, scm_capi_fixnum_to_clong(car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_string_p(car));
  scm_capi_string_to_cstr(car, actual, sizeof(actual));
  cut_assert_equal_string("str", actual);

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr(":def:",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_improper_list(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port(" (<abc> . 123) ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("<abc>",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_fixnum_p(cdr));
  cut_assert_equal_int(123, scm_capi_fixnum_to_clong(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_nested_list(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj car_n = SCM_OBJ_INIT, cdr_n = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &car_n, &cdr_n, &eof);

  new_parser();
  new_port(" (+ (? ! _) *) ");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("+",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_pair_p(car));

  car_n = scm_api_car(car);
  cdr_n = scm_api_cdr(car);

  cut_assert_true(scm_capi_symbol_p(car_n));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("?",
                                                               SCM_ENC_ASCII),
                                car_n));

  cut_assert_true(scm_capi_pair_p(cdr_n));

  car_n = scm_api_car(cdr_n);
  cdr_n = scm_api_cdr(cdr_n);

  cut_assert_true(scm_capi_symbol_p(car_n));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("!",
                                                               SCM_ENC_ASCII),
                                car_n));

  cut_assert_true(scm_capi_pair_p(cdr_n));

  car_n = scm_api_car(cdr_n);
  cdr_n = scm_api_cdr(cdr_n);

  cut_assert_true(scm_capi_symbol_p(car_n));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("_",
                                                               SCM_ENC_ASCII),
                                car_n));

  cut_assert_true(scm_capi_nil_p(cdr_n));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("*",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parse_parse_list_inserted_comment(void)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&lst, &car, &cdr, &eof);

  new_parser();
  new_port("(<abc> ; comment \n 123)");

  lst = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_pair_p(lst));

  car = scm_api_car(lst);
  cdr = scm_api_cdr(lst);

  cut_assert_true(scm_capi_symbol_p(car));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("<abc>",
                                                               SCM_ENC_ASCII),
                                car));

  cut_assert_true(scm_capi_pair_p(cdr));

  car = scm_api_car(cdr);
  cdr = scm_api_cdr(cdr);

  cut_assert_true(scm_capi_fixnum_p(car));
  cut_assert_equal_int(123, scm_capi_fixnum_to_clong(car));

  cut_assert_true(scm_capi_nil_p(cdr));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_empty_vector(void)
{
  ScmObj vec = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vec, &eof);

  new_parser();
  new_port(" #() ");

  vec = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_vector_p(vec));
  cut_assert_equal_int(0, scm_capi_vector_length(vec));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}

void
test_parser_parse_vector(void)
{
  char actual[256];
  ScmObj vec = SCM_OBJ_INIT, elm0 = SCM_OBJ_INIT, elm1 = SCM_OBJ_INIT;
  ScmObj elm2 = SCM_OBJ_INIT, elm3 = SCM_OBJ_INIT, eof = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&vec, &elm0, &elm1, &elm2, &elm3, &eof);

  new_parser();
  new_port(" #(<abc> 123 \"str\" #()) ");

  vec = scm_parser_parse_expression(parser, port);

  cut_assert_true(scm_capi_vector_p(vec));
  cut_assert_equal_int(4, scm_capi_vector_length(vec));


  elm0 = scm_capi_vector_ref(vec, 0);
  elm1 = scm_capi_vector_ref(vec, 1);
  elm2 = scm_capi_vector_ref(vec, 2);
  elm3 = scm_capi_vector_ref(vec, 3);


  cut_assert_true(scm_capi_symbol_p(elm0));
  cut_assert_true(scm_capi_eq_p(scm_capi_make_symbol_from_cstr("<abc>",
                                                               SCM_ENC_ASCII),
                                elm0));

  cut_assert_true(scm_capi_fixnum_p(elm1));
  cut_assert_equal_int(123, scm_capi_fixnum_to_clong(elm1));

  cut_assert_true(scm_capi_string_p(elm2));
  scm_capi_string_to_cstr(elm2, actual, sizeof(actual));
  cut_assert_equal_string("str", actual);

  cut_assert_true(scm_capi_vector_p(elm3));
  cut_assert_equal_int(0, scm_capi_vector_length(elm3));

  eof = scm_parser_parse_expression(parser, port);
  cut_assert_true(scm_capi_eof_p(eof));
}
