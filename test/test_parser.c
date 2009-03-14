#include <cutter.h>

#include "parser.h"
#include "port.h"
#include "object.h"
#include "string.h"
#include "symbol.h"
#include "integer.h"
#include "bool.h"
#include "char.h"
#include "nil.h"
#include "pair.h"
#include "vector.h"
#include "miscobjects.h"

ScmParser *
construct_parser_from_string(const char *str)
{
  ScmPort *port = scm_port_open_input_string(str, strlen(str));
  ScmIBuffer *ibuffer = scm_ibuffer_construct(port);
  ScmLexer *lexer = scm_lexer_construct(ibuffer);
  ScmParser *parser = scm_parser_construct(lexer);

  return parser;
}

void
string_content_to_null_terminate_string(ScmString *str, char *buf)
{
  memcpy(buf, scm_string_content(str),
         scm_string_bytesize(str));
  buf[scm_string_bytesize(str)] = '\0';
}

void
test_parser_construct(void)
{
  ScmPort *port = scm_port_open_input_string("", 0);
  ScmIBuffer *ibuffer = scm_ibuffer_construct(port);
  ScmLexer *lexer = scm_lexer_construct(ibuffer);
  ScmParser *parser = scm_parser_construct(lexer);

  cut_assert_not_null(parser);
}

void
test_parser_parse_string(void)
{
  char actual[256];
  ScmParser *parser = construct_parser_from_string(" \"this is string\" ");

  ScmObj obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_string_is_string(obj));
  string_content_to_null_terminate_string(SCM_STRING(obj), actual);
  cut_assert_equal_string("this is string", actual);
  cut_assert_equal_int(14, scm_string_length(SCM_STRING(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_symbol(void)
{
  ScmParser *parser = construct_parser_from_string(" symbol ");

  ScmObj obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_symbol_is_symbol(obj));
  cut_assert_equal_string("symbol", scm_symbol_name(SCM_SYMBOL(obj)));
  cut_assert_equal_int(6, scm_symbol_length(SCM_SYMBOL(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_integer(void)
{
  ScmParser *parser = construct_parser_from_string(" 100 ");

  ScmObj obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_integer_is_integer(obj));
  cut_assert_equal_int(100, scm_integer_value(SCM_INTEGER(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_signed_integer(void)
{
  ScmParser *parser = construct_parser_from_string(" +100 -100 ");
  ScmObj obj;
  
  obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_integer_is_integer(obj));
  cut_assert_equal_int(100, scm_integer_value(SCM_INTEGER(obj)));

  obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_integer_is_integer(obj));
  cut_assert_equal_int(-100, scm_integer_value(SCM_INTEGER(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}


void
test_parser_parse_bool_true(void)
{
  ScmParser *parser = construct_parser_from_string(" #t ");

  ScmObj obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_bool_is_bool(obj));
  cut_assert_true(scm_bool_value(SCM_BOOL(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));  
}

void
test_parser_parse_bool_false(void)
{
  ScmParser *parser = construct_parser_from_string(" #f ");

  ScmObj obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_bool_is_bool(obj));
  cut_assert_false(scm_bool_value(SCM_BOOL(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));  
}

void
test_parser_parse_char_newline(void)
{
  ScmParser *parser = construct_parser_from_string(" #\\newline #\\NEWLINE ");
  ScmObj obj;

  obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_char_is_char(obj));
  cut_assert_equal_int('\n', SCM_CHR_ASCII(scm_char_value(SCM_CHAR(obj))));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_char_is_char(obj));
  cut_assert_equal_int('\n', SCM_CHR_ASCII(scm_char_value(SCM_CHAR(obj))));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));  
}

void
test_parser_parse_char_space(void)
{
  ScmParser *parser = construct_parser_from_string(" #\\space #\\SPACE ");
  ScmObj obj;

  obj = scm_parser_parse_expression(parser);
  
  cut_assert_true(scm_char_is_char(obj));
  cut_assert_equal_int(' ', SCM_CHR_ASCII(scm_char_value(SCM_CHAR(obj))));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_char_is_char(obj));
  cut_assert_equal_int(' ', SCM_CHR_ASCII(scm_char_value(SCM_CHAR(obj))));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));  
}

void
test_parser_parse_quote(void)
{
  ScmParser *parser = construct_parser_from_string(" '^abc ");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("quote", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("^abc", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_quasiquote(void)
{
  ScmParser *parser = construct_parser_from_string(" `12abc ");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("quasiquote", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("12abc", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));  
}

void
test_parser_parse_unquote(void)
{
  ScmParser *parser = construct_parser_from_string(" ,&ab-12 ");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("unquote", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("&ab-12", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_unquote_splicing(void)
{
  ScmParser *parser = construct_parser_from_string(" ,@?abc.12 ");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("unquote-splicing", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("?abc.12", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_empty_list(void)
{
  ScmParser *parser = construct_parser_from_string(" () ");
  ScmObj obj;

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_nil_is_nil(obj));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_proper_list(void)
{
  char actual[256];
  ScmParser *parser = construct_parser_from_string("(<abc> 123 \"str\" :def:)");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("<abc>", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_integer_is_integer(car));
  cut_assert_equal_int(123, scm_integer_value(SCM_INTEGER(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_string_is_string(car));
  string_content_to_null_terminate_string(SCM_STRING(car), actual);
  cut_assert_equal_string("str", actual);

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string(":def:", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_improper_list(void)
{
  ScmParser *parser = construct_parser_from_string(" (<abc> . 123) ");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("<abc>", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_integer_is_integer(cdr));
  cut_assert_equal_int(123, scm_integer_value(SCM_INTEGER(cdr)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_nexted_list(void)
{
  ScmParser *parser =
    construct_parser_from_string(" (+ (? ! _) *) ");
  ScmObj obj, car, cdr, car_n, cdr_n;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("+", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_pair_is_pair(car));
  car_n = scm_pair_car(SCM_PAIR(car));
  cdr_n = scm_pair_cdr(SCM_PAIR(car));

  cut_assert_true(scm_symbol_is_symbol(car_n));
  cut_assert_equal_string("?", scm_symbol_name(SCM_SYMBOL(car_n)));
  
  cut_assert_true(scm_pair_is_pair(cdr_n));
  car_n = scm_pair_car(SCM_PAIR(cdr_n));
  cdr_n = scm_pair_cdr(SCM_PAIR(cdr_n));

  cut_assert_true(scm_symbol_is_symbol(car_n));
  cut_assert_equal_string("!", scm_symbol_name(SCM_SYMBOL(car_n)));

  cut_assert_true(scm_pair_is_pair(cdr_n));
  car_n = scm_pair_car(SCM_PAIR(cdr_n));
  cdr_n = scm_pair_cdr(SCM_PAIR(cdr_n));

  cut_assert_true(scm_symbol_is_symbol(car_n));
  cut_assert_equal_string("_", scm_symbol_name(SCM_SYMBOL(car_n)));

  cut_assert_true(scm_nil_is_nil(cdr_n));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("*", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_nil_is_nil(cdr));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parse_parse_list_inserted_comment(void)
{
  ScmParser *parser = construct_parser_from_string("(<abc> ; comment \n 123)");
  ScmObj obj, car, cdr;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_pair_is_pair(obj));
  car = scm_pair_car(SCM_PAIR(obj));
  cdr = scm_pair_cdr(SCM_PAIR(obj));

  cut_assert_true(scm_symbol_is_symbol(car));
  cut_assert_equal_string("<abc>", scm_symbol_name(SCM_SYMBOL(car)));

  cut_assert_true(scm_pair_is_pair(cdr));
  car = scm_pair_car(SCM_PAIR(cdr));
  cdr = scm_pair_cdr(SCM_PAIR(cdr));

  cut_assert_true(scm_integer_is_integer(car));
  cut_assert_equal_int(123, scm_integer_value(SCM_INTEGER(car)));

  cut_assert_true(scm_nil_is_nil(cdr));
}

void
test_parser_parse_empty_vector(void)
{
  ScmParser *parser = construct_parser_from_string(" #() ");
  ScmObj obj;

  obj = scm_parser_parse_expression(parser);

  cut_assert_true(scm_vector_is_vector(obj));
  cut_assert_equal_int(0, scm_vector_length(SCM_VECTOR(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}

void
test_parser_parse_vector(void)
{
  char actual[256];
  ScmParser *parser =
    construct_parser_from_string(" #(<abc> 123 \"str\" #()) ");
  ScmObj vector, obj;

  vector = scm_parser_parse_expression(parser);

  cut_assert_true(scm_vector_is_vector(vector));
  cut_assert_equal_int(4, scm_vector_length(SCM_VECTOR(vector)));

  obj = scm_vector_ref(SCM_VECTOR(vector), 0);

  cut_assert_true(scm_symbol_is_symbol(obj));
  cut_assert_equal_string("<abc>", scm_symbol_name(SCM_SYMBOL(obj)));

  obj = scm_vector_ref(SCM_VECTOR(vector), 1);

  cut_assert_true(scm_integer_is_integer(obj));
  cut_assert_equal_int(123, scm_integer_value(SCM_INTEGER(obj)));

  obj = scm_vector_ref(SCM_VECTOR(vector), 2);

  cut_assert_true(scm_string_is_string(obj));
  string_content_to_null_terminate_string(SCM_STRING(obj), actual);
  cut_assert_equal_string("str", actual);

  obj = scm_vector_ref(SCM_VECTOR(vector), 3);

  cut_assert_true(scm_vector_is_vector(obj));
  cut_assert_equal_int(0, scm_vector_length(SCM_VECTOR(obj)));

  obj = scm_parser_parse_expression(parser);
  cut_assert_true(scm_eof_is_eof(obj));
}
