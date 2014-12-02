#include "parser.c"

#include <stdarg.h>

#include "scythe/api.h"

#include "test.h"

TEST_GROUP(parser);

static ScmEvaluator *ev;
static ScmRefStackInfo rsi;

static void (*tear_down_func)(void);

TEST_SETUP(parser)
{
  ev = scm_capi_evaluator();
  scm_capi_evaluator_make_vm(ev);
  scm_capi_ref_stack_save(&rsi);
}

TEST_TEAR_DOWN(parser)
{
  scm_capi_ref_stack_restore(&rsi);

  if (tear_down_func != NULL)
    tear_down_func();

  scm_capi_evaluator_end(ev);
}

static ScmLexer *lexer;

static void
lexer_tear_down_func(void)
{
  if (lexer != NULL) {
    scm_lexer_end(lexer);
    lexer = NULL;
  }
  tear_down_func = NULL;
}

static void
check_token_string(const char *expected, ScmToken *token)
{
  TEST_ASSERT_EQUAL_INT(strlen(expected), token->len);

  for (size_t i = 0; i < token->len; i++)
    TEST_ASSERT_TRUE(chr_same_p(token->str[i], expected[i], true, SCM_ENC_SRC));
}

static void
test_tokenize(SCM_TOKEN_TYPE_T expe_type, const char *expe_str,
              const char *data)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmToken *token;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&port);

  port = scm_capi_open_input_string_cstr(data, SCM_ENC_NAME_SRC);
  enc = scm_capi_port_internal_encoding(port);
  lexer = scm_lexer_new();
  tear_down_func = lexer_tear_down_func;

  token = scm_lexer_head_token(lexer, port, enc);

  TEST_ASSERT_EQUAL_INT(expe_type, token->type);
  check_token_string(expe_str, token);

  TEST_ASSERT_FALSE(scm_lexer_error_p(lexer));
}

TEST(parser, token_new)
{
  scm_char_t c = { .ascii = '('};
  ScmToken *token = scm_token_new(SCM_TOKEN_TYPE_LPAREN, &c, 1, NULL);

  TEST_ASSERT_NOT_NULL(token);
  TEST_ASSERT_EQUAL_INT(SCM_TOKEN_TYPE_LPAREN, token->type);
  check_token_string("(", token);
}

TEST(parser, lexer_new)
{
  ScmLexer *lexer = scm_lexer_new();

  TEST_ASSERT_NOT_NULL(lexer);
  TEST_ASSERT_FALSE(scm_lexer_error_p(lexer));
}

TEST(parser, tokenize_left_parentesis)
{
  test_tokenize(SCM_TOKEN_TYPE_LPAREN, "(",
                " ( ");
}

TEST(parser, tokenize_right_parentesis)
{
  test_tokenize(SCM_TOKEN_TYPE_RPAREN, ")",
                " ) ");
}

TEST(parser, tokenize_dot)
{
  test_tokenize(SCM_TOKEN_TYPE_DOT, ".",
                " . ");
}

TEST(parser, tokenize_quote)
{
  test_tokenize(SCM_TOKEN_TYPE_QUOTE, "'",
                " ' ");
}

TEST(parser, tokenize_quasiquote)
{
  test_tokenize(SCM_TOKEN_TYPE_QUASIQUOTE, "`",
                " ` ");
}

TEST(parser, tokenize_unquote)
{
  test_tokenize(SCM_TOKEN_TYPE_UNQUOTE, ",",
                " , ");
}

TEST(parser, tokenize_unquote_splicing)
{
  test_tokenize(SCM_TOKEN_TYPE_UNQUOTE_SPLICING, ",@",
                " ,@ ");
}

TEST(parser, tokenize_string)
{
  test_tokenize(SCM_TOKEN_TYPE_STRING, "abc\\ndef",
                " \"abc\\ndef\" ");
}

TEST(parser, tokenize_identifier)
{
  test_tokenize(SCM_TOKEN_TYPE_IDENTIFIER, "abc-def_ghi",
                " abc-def_ghi ");
}

TEST(parser, tokenize_identifier_vline)
{
  test_tokenize(SCM_TOKEN_TYPE_IDENTIFIER_VLINE, "abc\\x41;def\\tghi\\|jkl",
                " |abc\\x41;def\\tghi\\|jkl| ");
}

TEST(parser, tokenize_identifier_plus)
{
  test_tokenize(SCM_TOKEN_TYPE_IDENTIFIER, "+",
                " + ");
}

TEST(parser, tokenize_identifier_minus)
{
  test_tokenize(SCM_TOKEN_TYPE_IDENTIFIER, "-",
                " - ");
}

TEST(parser, tokenize_identifier_3dots)
{
  test_tokenize(SCM_TOKEN_TYPE_IDENTIFIER, "...",
                " ... ");
}

IGNORE_TEST(parser, tokenize_reference_decl)
{
}

IGNORE_TEST(parser, tokenize_reference_use)
{
}

TEST(parser, tokenize_numeric_int)
{
  test_tokenize(SCM_TOKEN_TYPE_NUMERIC, "123",
                " 123 ");
}

TEST(parser, tokenize_numeric_int_plus)
{
  test_tokenize(SCM_TOKEN_TYPE_NUMERIC, "+456",
                " +456 ");
}

TEST(parser, tokenize_numeric_int_minus)
{
  test_tokenize(SCM_TOKEN_TYPE_NUMERIC, "-789",
                " -789 ");
}

TEST(parser, tokenize_bool_true)
{
  test_tokenize(SCM_TOKEN_TYPE_BOOL_TRUE, "#true",
                " #true ");
}

TEST(parser, tokenize_bool_true_short)
{
  test_tokenize(SCM_TOKEN_TYPE_BOOL_TRUE, "#t",
                " #t ");
}

TEST(parser, tokenize_bool_false)
{
  test_tokenize(SCM_TOKEN_TYPE_BOOL_FALSE, "#false",
                " #false ");
}

TEST(parser, tokenize_bool_false_short)
{
  test_tokenize(SCM_TOKEN_TYPE_BOOL_FALSE, "#f",
                " #f ");
}

TEST(parser, tokenize_vector_start)
{
  test_tokenize(SCM_TOKEN_TYPE_VECTOR_START, "#(",
                " #( ");
}

TEST(parser, tokenize_bytevector_start)
{
  test_tokenize(SCM_TOKEN_TYPE_BYTEVECTOR_START, "#u8(",
                " #u8( ");
}

TEST(parser, tokenize_char)
{
  test_tokenize(SCM_TOKEN_TYPE_CHAR, "#\\a",
                " #\\a ");
}

TEST(parser, tokenize_char_name)
{
  test_tokenize(SCM_TOKEN_TYPE_CHAR, "#\\newline",
                " #\\newline ");
}

TEST(parser, tokenize_char_hex_scalar)
{
  test_tokenize(SCM_TOKEN_TYPE_CHAR, "#\\x08ab;",
                " #\\x08ab; ");
}

TEST(parser, tokenize_eof)
{
  test_tokenize(SCM_TOKEN_TYPE_EOF, "",
                "");
}

static ScmParser *parser;

static void
parser_tear_down_func(void)
{
  if (parser != NULL) {
    scm_parser_end(parser);
    parser = NULL;
  }
  tear_down_func = NULL;
}

static ScmObj
make_list_of_symbol(bool proper, size_t n, ...)
{
  ScmObj s[n], l = SCM_OBJ_INIT;
  size_t x;
  va_list ap;

  for (size_t i = 0; i < n; i++) s[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT_REG(&l);
  SCM_REFSTK_REG_ARY(s, n);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    const char *p = va_arg(ap, const char *);
    s[i] = scm_capi_make_symbol_from_cstr(p, SCM_ENC_SRC);
  }
  va_end(ap);

  if (proper) {
    l = SCM_NIL_OBJ;
    x = n;
  }
  else {
    l = s[n - 1];
    x = n - 1;
  }

  for (size_t i = 0; i < x; i++)
    l = scm_api_cons(s[x - i - 1], l);

  return l;
}

static ScmObj
make_vector_of_symbol(size_t n, ...)
{
  ScmObj s[n];
  va_list ap;

  for (size_t i = 0; i < n; i++) s[i] = SCM_OBJ_NULL;

  SCM_REFSTK_INIT;
  SCM_REFSTK_REG_ARY(s, n);

  va_start(ap, n);
  for (size_t i = 0; i < n; i++) {
    const char *p = va_arg(ap, const char *);
    s[i] = scm_capi_make_symbol_from_cstr(p, SCM_ENC_SRC);
    SCM_REFSTK_REG(&s[i]);
  }
  va_end(ap);

  return scm_capi_vector_cv(s, n);
}

static void
test_parse(ScmObj expected, const char *data)
{
  ScmObj port = SCM_OBJ_INIT, actual = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &actual);

  port = scm_capi_open_input_string_cstr(data, SCM_ENC_NAME_SRC);
  parser = scm_parser_new();
  tear_down_func = parser_tear_down_func;

  actual = scm_parser_parse_expression(parser, port);

  TEST_ASSERT_SCM_EQUAL(expected, actual);
}

TEST(parser, parse_string)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_string_from_cstr("this is a string", SCM_ENC_SRC);

  test_parse(expected, "\"this is a string\"");
}

TEST(parser, parse_string_escape)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_string_from_cstr("abc\ndef", SCM_ENC_SRC);

  test_parse(expected, "\"abc\\nde\\x66;\"");
}

TEST(parser, parse_symbol)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_symbol_from_cstr("symbol", SCM_ENC_SRC);

  test_parse(expected, "symbol");
}

TEST(parser, parse_symbol_vline)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_symbol_from_cstr("symbol", SCM_ENC_SRC);

  test_parse(expected, "|symbol|");
}

TEST(parser, parse_symbol_vline_escape)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_symbol_from_cstr("foo\nbar\tbaz", SCM_ENC_SRC);

  test_parse(expected, "|foo\\nbar\\tb\\x61;z|");
}

TEST(parser, parse_number_int)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_number_from_sword(123);

  test_parse(expected, "123");
}

TEST(parser, parse_number_int_plus)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_number_from_sword(123);

  test_parse(expected, "+123");
}

TEST(parser, parse_number_int_minus)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_number_from_sword(-123);

  test_parse(expected, "-123");
}

TEST(parser, parse_number_bool_true)
{
  test_parse(SCM_TRUE_OBJ, "#true");
}

TEST(parser, parse_number_bool_true_short)
{
  test_parse(SCM_TRUE_OBJ, "#t");
}

TEST(parser, parse_number_bool_false)
{
  test_parse(SCM_FALSE_OBJ, "#false");
}

TEST(parser, parse_number_bool_false_short)
{
  test_parse(SCM_FALSE_OBJ, "#f");
}

TEST(parser, parse_char)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_char(&(scm_char_t){ .ascii = 'a' }, SCM_ENC_SRC);

  test_parse(expected, "#\\a");
}

TEST(parser, parse_char_name_newline)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_char(&(scm_char_t){ .ascii = '\n' }, SCM_ENC_SRC);

  test_parse(expected, "#\\newline");
}

TEST(parser, parse_char_name_hex_scalar_value)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_char(&(scm_char_t){ .ascii = 'b' }, SCM_ENC_SRC);

  test_parse(expected, "#\\x62;");
}

TEST(parser, parse_char_x)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = scm_capi_make_char(&(scm_char_t){ .ascii = 'x' }, SCM_ENC_SRC);

  test_parse(expected, "#\\x");
}

TEST(parser, parse_quote)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 2, "quote", "abc");

  test_parse(expected, "'abc");
}

TEST(parser, parse_quasiquote)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 2, "quasiquote", "abc");

  test_parse(expected, "`abc");
}

TEST(parser, parse_unquote)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 2, "unquote", "abc");

  test_parse(expected, ",abc");
}

TEST(parser, parse_unquote_splicing)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 2, "unquote-splicing", "abc");

  test_parse(expected, ",@abc");
}

TEST(parser, parse_empty_list)
{
  test_parse(SCM_NIL_OBJ, "()");
}

TEST(parser, parse_proper_list)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 2, "abc", "def");

  test_parse(expected, "(abc def)");
}

TEST(parser, parse_improper_list)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(false, 2, "abc", "def");

  test_parse(expected, "(abc . def)");
}

TEST(parser, parse_nexted_list)
{
  ScmObj expected = SCM_OBJ_INIT, o = SCM_OBJ_INIT, x = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected, &o, &x);

  expected = make_list_of_symbol(true, 3, "abc", "def", "ghi");
  o = make_list_of_symbol(true, 2, "+", "-");
  x = scm_api_cdr(expected);
  scm_api_set_car_i(x, o);

  test_parse(expected, "(abc (+ -) ghi)");
}

TEST(parser, parse_list__comment)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_list_of_symbol(true, 3, "abc", "def", "ghi");

  test_parse(expected, "(abc def ; comment \n ghi)");
}

TEST(parser, parse_empty_vector)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_vector_of_symbol(0);

  test_parse(expected, "#()");
}

TEST(parser, parse_vector)
{
  ScmObj expected = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&expected);

  expected = make_vector_of_symbol(2, "abc", "def");

  test_parse(expected, "#(abc def)");
}

TEST(parser, parse_eof)
{
  test_parse(SCM_EOF_OBJ, "");
}

