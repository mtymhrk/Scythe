#include <cutter.h>

#include "parser.h"

void
test_token_construct(void)
{
  ScmToken *token = scm_token_construct(SCM_TOKEN_TYPE_LPAREN,
                                        (unsigned char *)"(");

  cut_assert_not_null(token);
  cut_assert_equal_int(SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE(token));
  cut_assert_equal_string("(", (char *)SCM_TOKEN_STRING(token));
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ) ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" . ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ' ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ` ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ,@ ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" \"abc\\ndef\" ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" abc-def_ghi ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
test_lexer_tokenize_numeric(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string("123456");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
test_lexer_tokenize_bool_true(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #t ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #f ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" #\\c ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
test_lexer_tokenize_eof(void)
{
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string("  ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string(" ; comment \n ( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);
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
  ScmIBuffer *buffer = scm_ibuffer_construct_from_string("  symbol \n ( ");
  ScmLexer *lexer = scm_lexer_construct(buffer);  
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
