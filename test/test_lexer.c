#include <cutter.h>

#include "parser.h"

void
test_token_construct(void)
{
  ScmToken *token = scm_token_construct(SCM_TOKEN_TYPE_LPAREN,
                                        (unsigned char *)"(");

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, scm_token_type(token));
  cut_assert_equal_string("(", (char *)scm_token_string(token));
}

void
test_lexer_construct(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);

  cut_assert_not_null(lexer);
}

void
test_lexer_tokenize_left_parenthesis(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, scm_token_type(token));
  cut_assert_equal_string("(", (char *)scm_token_string(token));
  
}  

void
test_lexer_tokenize_right_parenthesis(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ) ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_RPAREN, scm_token_type(token));
  cut_assert_equal_string(")", (char *)scm_token_string(token));
  
}  

void
test_lexer_tokenize_dot(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" . ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_DOT, scm_token_type(token));
  cut_assert_equal_string(".", (char *)scm_token_string(token));
  
}  

void
test_lexer_tokenize_quote(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ' ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_QUOTE, scm_token_type(token));
  cut_assert_equal_string("'", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_quasiquote(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ` ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_QUASIQUOTE, scm_token_type(token));
  cut_assert_equal_string("`", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_unquote_splicing(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ,@ ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_UNQUOTE_SPLICING, scm_token_type(token));
  cut_assert_equal_string(",@", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_string(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" \"abc\\ndef\" ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_STRING, scm_token_type(token));
  cut_assert_equal_string("abc\ndef", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_identifier(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" abc-def_ghi ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_IDENTIFIER, scm_token_type(token));
  cut_assert_equal_string("abc-def_ghi", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_numeric(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string("123456");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_NUMERIC, scm_token_type(token));
  cut_assert_equal_string("123456", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_bool_true(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #t ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_BOOL, scm_token_type(token));
  cut_assert_equal_string("#t", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_bool_false(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #f ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_BOOL, scm_token_type(token));
  cut_assert_equal_string("#f", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_vector_start(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_VECTOR_START, scm_token_type(token));
  cut_assert_equal_string("#(", (char *)scm_token_string(token));
}

void
test_lexer_tokenize_char(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #\\c ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_CHAR, scm_token_type(token));
  cut_assert_equal_string("#\\c", (char *)scm_token_string(token));
}


void
test_lexer_tokenize_eof(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string("  ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
  ScmToken *token = scm_lexer_head_token(lexer);

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_EOF, scm_token_type(token));
}
