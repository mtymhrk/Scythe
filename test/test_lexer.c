#include <cutter.h>

#include "parser.h"

ScmIBuffer *
new_ibuffer_from_string(const char *str)
{
  return scm_ibuffer_new(scm_port_open_input_string(str, strlen(str)));
}

void
test_token_new(void)
{
  ScmToken *token = scm_token_new(SCM_TOKEN_TYPE_LPAREN, "(");

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("(", (char *)SCM_TOKEN_STRING(token));
}

void
test_lexer_new(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ( ");
  ScmLexer *lexer = scm_lexer_new(buffer);

  cut_assert_not_null(lexer);
}

void
test_lexer_tokenize_left_parenthesis(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ( ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("(", (char *)SCM_TOKEN_STRING(token));
  
  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}  

void
test_lexer_tokenize_right_parenthesis(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ) ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_RPAREN, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string(")", (char *)SCM_TOKEN_STRING(token));
  
  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}  

void
test_lexer_tokenize_dot(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" . ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_DOT, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string(".", (char *)SCM_TOKEN_STRING(token));
  

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}  

void
test_lexer_tokenize_quote(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ' ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("'", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_quasiquote(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ` ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_QUASIQUOTE, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("`", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_unquote_splicing(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ,@ ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_UNQUOTE_SPLICING, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string(",@", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_string(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" \"abc\\ndef\" ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_STRING, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("abc\ndef", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_identifier(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" abc-def_ghi ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("abc-def_ghi", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_idintifier_spec(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" + - ... ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token;

  token=  scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("+", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);
  token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("-", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);
  token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("...", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);
  token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_numeric(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string("123456");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_NUMERIC, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("123456", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_numeric_following_not_numeric_char(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string("123456)");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_NUMERIC, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("123456", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_RPAREN, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_bool_true(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #t ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_BOOL, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#t", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_bool_false(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #f ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_BOOL, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#f", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_vector_start(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #( ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_VECTOR_START, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#(", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_char(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #\\c ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_CHAR, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#\\c", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_char_newline(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #\\newline ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_CHAR, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#\\newline", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_char_spece(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" #\\space ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_CHAR, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("#\\space", (char *)SCM_TOKEN_STRING(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_eof(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string("  ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_commnet(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" ; comment \n ( ");
  ScmLexer *lexer = scm_lexer_new(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_twich(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string("  symbol \n ( ");
  ScmLexer *lexer = scm_lexer_new(buffer);  
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, SCM_TOKEN_TYPE(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE(token));

  scm_lexer_shift_token(lexer);

  token = scm_lexer_head_token(lexer);
  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_string_unexpected_eof(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" \n \"abcdef");
  ScmLexer *lexer = scm_lexer_new(buffer);  
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_TOKENIZE_ERR, SCM_TOKEN_TYPE(token));

  cut_assert_true(scm_lexer_has_error(lexer));
  cut_assert_equal_int((int)SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF,
                       (int)scm_lexer_error_type(lexer));
  cut_assert_equal_int(2, scm_lexer_error_line(lexer));
  cut_assert_equal_int(9, scm_lexer_error_column(lexer));
}

void
test_lexer_tokenize_numeric_sign_unexpected_eor(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" \n \n #");
  ScmLexer *lexer = scm_lexer_new(buffer);  
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_TOKENIZE_ERR, SCM_TOKEN_TYPE(token));

  cut_assert_true(scm_lexer_has_error(lexer));
  cut_assert_equal_int((int)SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF,
                       (int)scm_lexer_error_type(lexer));
  cut_assert_equal_int(3, scm_lexer_error_line(lexer));
  cut_assert_equal_int(3, scm_lexer_error_column(lexer));
}

void
test_lexer_tokenize_error_cannot_shift_until_error_state_cleared(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" \n \"abcdef");
  ScmLexer *lexer = scm_lexer_new(buffer);  
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_TOKENIZE_ERR, SCM_TOKEN_TYPE(token));
  cut_assert_true(scm_lexer_has_error(lexer));

  scm_lexer_shift_token(lexer);
  token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_TOKENIZE_ERR, SCM_TOKEN_TYPE(token));

  scm_lexer_error_state_clear(lexer);

  cut_assert_false(scm_lexer_has_error(lexer));

  scm_lexer_shift_token(lexer);
  token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, SCM_TOKEN_TYPE(token));
}

void
test_lexer_tokenize_nested_list(void)
{
  ScmIBuffer *buffer = new_ibuffer_from_string(" (+ (? ! _) *) ");
  ScmLexer *lexer = scm_lexer_new(buffer);  

  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
  scm_lexer_shift_token(lexer);
}
