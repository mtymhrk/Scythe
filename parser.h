#ifndef INCLUDE_PARSER_H__
#define INCLUDE_PARSER_H__

#include <stdbool.h>

#include "ibuffer.h"
#include "object.h"

typedef struct ScmParserRec ScmParser;
typedef struct ScmLexerRec ScmLexer;
typedef struct ScmTokenRec ScmToken;

typedef enum {
  SCM_TOKEN_TYPE_NONE,
  SCM_TOKEN_TYPE_LPAREN,
  SCM_TOKEN_TYPE_RPAREN,
  SCM_TOKEN_TYPE_DOT,
  SCM_TOKEN_TYPE_QUOTE,
  SCM_TOKEN_TYPE_QUASIQUOTE,
  SCM_TOKEN_TYPE_UNQUOTE,
  SCM_TOKEN_TYPE_UNQUOTE_SPLICING,
  SCM_TOKEN_TYPE_STRING,
  SCM_TOKEN_TYPE_IDENTIFIER,
  SCM_TOKEN_TYPE_NUMERIC,
  SCM_TOKEN_TYPE_BOOL,
  SCM_TOKEN_TYPE_VECTOR_START,
  SCM_TOKEN_TYPE_CHAR,
  SCM_TOKEN_TYPE_EOF,
  SCM_TOKEN_TYPE_TOKENIZE_ERR
} SCM_TOKEN_TYPE_T;

struct ScmTokenRec {
  SCM_TOKEN_TYPE_T type;
  char *string;
};

typedef enum {
  SCM_LEXER_ERR_TYPE_NONE,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF,
} SCM_LEXER_ERR_TYPE_T;

#define SCM_TOKEN(obj) ((ScmToken *)(obj))
#define SCM_TOKEN_TYPE(token) ((token)->type)
#define SCM_TOKEN_STRING(token) ((token)->string)

ScmToken *scm_token_new(SCM_TOKEN_TYPE_T type, char *string);
void scm_token_end(ScmToken *token);
ScmLexer *scm_lexer_new(ScmIBuffer *reader);
ScmLexer *scm_lexer_end(ScmLexer *lexer);
ScmToken *scm_lexer_head_token(ScmLexer *lexer);
void scm_lexer_shift_token(ScmLexer *lexer);
void scm_lexer_unshift_token(ScmLexer *lexer, ScmToken *token);
void scm_lexer_error_state_clear(ScmLexer *lexer);
bool scm_lexer_has_error(ScmLexer *lexer);
SCM_LEXER_ERR_TYPE_T scm_lexer_error_type(ScmLexer *lexer);
int scm_lexer_error_line(ScmLexer *lexer);
int scm_lexer_error_column(ScmLexer *lexer);
ScmParser *scm_parser_new(ScmLexer *lexer);
ScmObj scm_parser_parse_expression(ScmParser *parser);

#endif /* INCLUDE_PARSER_H__ */
