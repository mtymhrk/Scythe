#ifndef INCLUDE_PARSER_H__
#define INCLUDE_PARSER_H__

typedef struct ScmParserRec ScmParser;
typedef struct ScmLexerRec ScmLexer;
typedef struct ScmTokenRec ScmToken;

typedef enum {
  SCM_TOKEN_TYPE_NONE,
  SCM_TOKEN_TYPE_LPAREN,
  SCM_TOKEN_TYPE_RPAREN,
  SCM_TOKEN_TYPE_DOT,
  SCM_TOKEN_TYPE_COMMA,
  SCM_TOKEN_TYPE_QUOTE,
  SCM_TOKEN_TYPE_QUASIQUOTE,
  SCM_TOKEN_TYPE_UNQUOTE,
  SCM_TOKEN_TYPE_UNQUOTE_SPLICING,
  SCM_TOKEN_TYPE_STRING,
  SCM_TOKEN_TYPE_IDENTIFIER,
  SCM_TOKEN_TYPE_INTEGER,
  SCM_TOKEN_TYPE_BOOL,
  SCM_TOKEN_TYPE_VECTOR_START,
  SCM_TOKEN_TYPE_CHAR,
  SCM_TOKEN_TYPE_NUMERIC,
  SCM_TOKEN_TYPE_EOF
} SCM_TOKEN_TYPE_T;

ScmToken *scm_token_construct(SCM_TOKEN_TYPE_T type, unsigned char *string);
SCM_TOKEN_TYPE_T scm_token_type(ScmToken *token);
const unsigned char *scm_token_string(ScmToken *token);
ScmLexer *scm_lexer_construct(ScmIBuffer *reader);
ScmToken *scm_lexer_head_token(ScmLexer *lexer);
void scm_lexer_push_token(ScmLexer *lexer, ScmToken *token);
void scm_lexer_shift_token(ScmLexer *lexer);
void scm_lexer_unshift_token(ScmLexer *lexer, ScmToken *token);

#endif /* INCLUDE_PARSER_H__ */
