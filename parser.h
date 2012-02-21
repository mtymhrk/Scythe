#ifndef INCLUDE_PARSER_H__
#define INCLUDE_PARSER_H__

#include <stdbool.h>



typedef struct ScmParserRec ScmParser;
typedef struct ScmLexerRec ScmLexer;
typedef struct ScmTokenRec ScmToken;

#include "object.h"


/****************************************************************************/
/*  ScmToken                                                                */
/****************************************************************************/

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
  SCM_TOKEN_TYPE_REFERENCE_DECL,
  SCM_TOKEN_TYPE_REFERENCE_USE,
  SCM_TOKEN_TYPE_NUMERIC,
  SCM_TOKEN_TYPE_BOOL_TRUE,
  SCM_TOKEN_TYPE_BOOL_FALSE,
  SCM_TOKEN_TYPE_VECTOR_START,
  SCM_TOKEN_TYPE_CHAR,
  SCM_TOKEN_TYPE_EOF,
  SCM_TOKEN_TYPE_TOKENIZE_ERR
} SCM_TOKEN_TYPE_T;

struct ScmTokenRec {
  SCM_TOKEN_TYPE_T type;
  char *string;
  size_t size;
  ScmToken *next;
};

#define SCM_TOKEN(obj) ((ScmToken *)(obj))
#define SCM_TOKEN_TYPE(token) ((token)->type)
#define SCM_TOKEN_STRING(token) ((token)->string)

ScmToken *scm_token_new(SCM_TOKEN_TYPE_T type, char *string, size_t size);
void scm_token_end(ScmToken *token);


/****************************************************************************/
/*  ScmLexer                                                                */
/****************************************************************************/

typedef enum {
  SCM_LEXER_ERR_TYPE_NONE,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF,
  SCM_LEXER_ERR_TYPE_PORT_ERR,
} SCM_LEXER_ERR_TYPE_T;

struct ScmLexerRec {
  char *buffer;
  size_t buf_capacity;
  size_t buf_used;
  SCM_TOKEN_TYPE_T token_type;
  ScmToken *tokens_head;
  ScmToken *tokens_tail;
  SCM_LEXER_ERR_TYPE_T error_type;
};

ScmLexer *scm_lexer_new(void);
ScmLexer *scm_lexer_end(ScmLexer *lexer);
ScmToken *scm_lexer_head_token(ScmLexer *lexer, ScmObj port);
void scm_lexer_shift_token(ScmLexer *lexer);
void scm_lexer_error_state_clear(ScmLexer *lexer);
bool scm_lexer_error_p(ScmLexer *lexer);
SCM_LEXER_ERR_TYPE_T scm_lexer_error_type(ScmLexer *lexer);


/****************************************************************************/
/*  ScmParser                                                               */
/****************************************************************************/

struct ScmParserRec {
  ScmLexer* lexer;
};

ScmParser *scm_parser_new(ScmLexer *lexer);
ScmObj scm_parser_parse_expression(ScmParser *parser, ScmObj port);

#endif /* INCLUDE_PARSER_H__ */
