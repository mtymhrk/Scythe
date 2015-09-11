#ifndef INCLUDE_PARSER_H__
#define INCLUDE_PARSER_H__

#include <stdbool.h>

typedef struct ScmTokenRec ScmToken;
typedef struct ScmLexerRec ScmLexer;
typedef struct ScmParserRefRec ScmParserRef;
typedef struct ScmDatumLabelUseRec ScmDatumLabelUse;
typedef struct ScmParserRec ScmParser;

typedef enum scm_token_type scm_token_type_t;
typedef enum scm_lexer_err_type scm_lexer_err_type_t;

#define SCM_DATUM_LABEL_USE(obj) ((ScmDatumLabelUse *)(obj))
#define SCM_PARSER(obj) ((ScmParser *)(obj))

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/earray.h"
#include "scythe/number_parser.h"


/****************************************************************************/
/*  ScmToken                                                                */
/****************************************************************************/

enum scm_token_type {
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
  SCM_TOKEN_TYPE_IDENTIFIER_VLINE,
  SCM_TOKEN_TYPE_REFERENCE_DECL,
  SCM_TOKEN_TYPE_REFERENCE_USE,
  SCM_TOKEN_TYPE_NUMERIC,
  SCM_TOKEN_TYPE_BOOL_TRUE,
  SCM_TOKEN_TYPE_BOOL_FALSE,
  SCM_TOKEN_TYPE_VECTOR_START,
  SCM_TOKEN_TYPE_BYTEVECTOR_START,
  SCM_TOKEN_TYPE_CHAR,
  SCM_TOKEN_TYPE_EOF,
  SCM_TOKEN_TYPE_TOKENIZE_ERR
};

struct ScmTokenRec {
  scm_token_type_t type;
  scm_char_t *str;
  size_t len;
  ScmNumParseData npd;
  ScmToken *next;
};


/****************************************************************************/
/*  ScmLexer                                                                */
/****************************************************************************/

enum scm_lexer_err_type {
  SCM_LEXER_ERR_TYPE_NONE,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR,
  SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF,
};

struct ScmLexerRec {
  scm_token_type_t token_type;
  EArray str;
  ScmNumParseData npd;
  ScmToken *tokens_head;
  ScmToken *tokens_tail;
  scm_lexer_err_type_t error_type;
};

ScmLexer *scm_lexer_new(void);
void scm_lexer_end(ScmLexer *lexer);
ScmToken *scm_lexer_head_token(ScmLexer *lexer, ScmObj port, ScmEncoding *enc);
void scm_lexer_shift_token(ScmLexer *lexer);
void scm_lexer_clear_error_state(ScmLexer *lexer);
bool scm_lexer_error_p(ScmLexer *lexer);
scm_lexer_err_type_t scm_lexer_error_type(ScmLexer *lexer);


/****************************************************************************/
/*  ScmDatumLabel                                                           */
/****************************************************************************/

struct ScmParserRefRec {
  ScmObj referrer;
  size_t pos;
};

#define SCM_PARSER_REF_INIT { .referrer = SCM_OBJ_INIT, .pos = 0 }

extern ScmTypeInfo SCM_DATUM_LABEL_USE_TYPE_INFO;

struct ScmDatumLabelUseRec {
  ScmObjHeader header;
  ScmParserRef ref;
};

int scm_datum_label_use_initialize(ScmObj use, ScmParserRef *ref);
ScmObj scm_datum_label_use_new(scm_mem_type_t mtype, ScmParserRef *ref);
int scm_datum_label_use_resolve(ScmObj use);
void scm_datum_label_use_gc_initialize(ScmObj obj, ScmObj mem);
int scm_datum_label_use_gc_accept(ScmObj obj, ScmObj mem,
                                  ScmGCRefHandler handler);


/****************************************************************************/
/*  ScmParser                                                               */
/****************************************************************************/

extern ScmTypeInfo SCM_PARSER_TYPE_INFO;

struct ScmParserRec {
  ScmObjHeader header;
  ScmLexer* lexer;
  EArray label_decl;
  EArray label_use;
};

int scm_parser_initialize(ScmObj parser);
void scm_parser_finalize(ScmObj parser);
ScmObj scm_parser_new(scm_mem_type_t mtype);
ScmObj scm_parser_parse(ScmObj parser, ScmObj port);
void scm_parser_gc_initialize(ScmObj obj, ScmObj mem);
void scm_parser_gc_finalize(ScmObj obj);
int scm_parser_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandler handler);

#endif /* INCLUDE_PARSER_H__ */
