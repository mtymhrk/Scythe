#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "impl_utils.h"
#include "earray.h"
#include "number_parser.h"
#include "parser.h"

#define SCM_LEXER_INITIAL_BUFFER_SIZE 16

#define IDENTIFIER_START_CHARS "abcdefghijklmnopqrstuvwxyz!$%&*/:<=>?~_^"
#define IDENTIFIER_FOLLOW_ON_CHARS \
  IDENTIFIER_START_CHARS "0123456789.+-"
#define IDENTIFIER_DELIMITER "()'`,\"[] \t\n\r;"
#define NUMERIC_START_CHAR "+-0123456789"

#define IS_LINE_FEED(c) ((c) == '\n')

#define IS_IDENTIFIER_START_CHAR(c) \
  (strchr(IDENTIFIER_START_CHARS, tolower(c)) != NULL)

#define IS_IDENTIFIER_FOLLOW_ON_CHAR(c) \
  (strchr(IDENTIFIER_FOLLOW_ON_CHARS, tolower(c)) != NULL)

#define IDENTIFIER_DELIMITER_P(c) \
  (strchr(IDENTIFIER_DELIMITER, tolower(c)) != NULL)

#define IS_NUMERIC_START_CHAR(c) \
  (strchr(NUMERIC_START_CHAR, tolower(c)) != NULL)

#define NONASCII ((char)0xff)

#define WHITESPACE " \t\n\r"
#define DELIMITER (WHITESPACE "()[]\";|")


inline bool
chr_same_p(scm_char_t c1, char c2, bool c_sensitive, ScmEncoding *enc)
{
  if (c_sensitive)
    return scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), c2);
  else
    return (scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), toupper(c2))
            || scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), tolower(c2)));
}

inline const char *
chr_find(const char *str, scm_char_t c, ScmEncoding *enc)
{
  for (const char *p = str; *p != '\0'; p++)
    if (scm_enc_same_char_p(enc, c.bytes, sizeof(c), *p))
      return p;
  return NULL;
}

inline bool
chr_whitespace_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(WHITESPACE, c, enc) != NULL);
}

inline bool
chr_intraline_whitespace_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(" \t", c, enc) != NULL);
}

inline bool
chr_delimiter_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(DELIMITER, c, enc) != NULL);
}

inline bool
chr_sign_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find("-+", c, enc) != NULL);
}

inline bool
chr_dec_digit_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find("0123456789", c, enc) != NULL);
}

bool
str_same_p(const scm_char_t *ca, size_t l,
           const char *s, bool c_sensitive, ScmEncoding *enc)
{
  size_t i;

  for (i = 0; i < l && s[i] != '\0'; i++) {
    if (!chr_same_p(ca[i], s[i], c_sensitive, enc))
      return false;
  }

  return (i == l && s[i] == '\0');
}


typedef enum {
  LEXER_STATE_DONE,
  LEXER_STATE_INIT,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
  LEXER_STATE_IDENTIFIER_VBAR,
  LEXER_STATE_DOT,
  LEXER_STATE_UNQUOTE,
  LEXER_STATE_COMMENT,
  LEXER_STATE_NUMBER_SIGN,
  LEXER_STATE_BOOL_TRUE,
  LEXER_STATE_BOOL_FALSE,
  LEXER_STATE_STRING,
  LEXER_STATE_CHAR,
  LEXER_STATE_REFERENCE_DECL_OR_USE,
  LEXER_STATE_NUMERIC,
  LEXER_STATE_BYTEVECTOR_START,
  LEXER_STATE_ERROR
} LEXER_STATE_T;


static ScmToken *
scm_token_new(SCM_TOKEN_TYPE_T type, scm_char_t *str, size_t len,
              ScmNumParseData *npd)
{
  ScmToken *token;

  scm_assert(type != SCM_TOKEN_TYPE_NUMERIC || npd != NULL);


  token = scm_capi_malloc(sizeof(ScmToken));
  if (token == NULL) return NULL;

  token->type = type;
  token->str = str;
  token->len = len;
  if (type == SCM_TOKEN_TYPE_NUMERIC) {
    token->npd = *npd;
  }

  return token;
}

static void
scm_token_end(ScmToken *token)
{
  scm_assert(token != NULL);

  scm_capi_free(token->str);
  scm_capi_free(token);
}

static void
scm_lexer_set_token_type(ScmLexer *lexer, SCM_TOKEN_TYPE_T type)
{
  scm_assert(lexer != NULL);
  lexer->token_type = type;
}

static void
scm_lexer_setup_error_state(ScmLexer *lexer, SCM_LEXER_ERR_TYPE_T error)
{
  scm_assert(lexer != NULL);

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_TOKENIZE_ERR);
  lexer->error_type = error;
}

static void
scm_lexer_clear_state(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  eary_fin(&lexer->str);
  eary_init(&lexer->str, sizeof(scm_char_t), 0);
  lexer->token_type = SCM_TOKEN_TYPE_NONE;
}

static int
scm_lexer_push_char(ScmLexer *lexer, scm_char_t chr)
{
  int err;

  scm_assert(lexer != NULL);

  EARY_PUSH(&lexer->str, scm_char_t, chr, err);
  if (err < 0) return -1;

  return 0;
}

ScmToken *
scm_lexer_new_token(ScmLexer *lexer)
{
  ScmToken *token;
  scm_char_t *str;
  size_t len;

  len = EARY_SIZE(&lexer->str);
  str = eary_chuck_ary(&lexer->str);

  token = scm_token_new(lexer->token_type, str, len, &lexer->npd);
  if (token == NULL) return NULL;

  scm_lexer_clear_state(lexer);

  return token;
}

static int
scm_lexer_tokenize_init(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  const char *one_char_token_chars = "()[]'`";
  const SCM_TOKEN_TYPE_T one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  scm_char_t current;
  ssize_t width;
  const char *p;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_capi_peek_cchr(&current, port);

  if (width < 0) {
      return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    return LEXER_STATE_DONE;
  }

  if (!scm_enc_ascii_p(enc, current.bytes, (size_t)width)) {
    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
  else if (chr_whitespace_p(current, enc)) {
    scm_capi_read_cchr(&current, port);
    return LEXER_STATE_DISREGARD;
  }
  else if ((p = chr_find(one_char_token_chars, current, enc))) {
    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_set_token_type(lexer,
                             one_char_token_types[p - one_char_token_chars]);
    return LEXER_STATE_DONE;
  }
  else if (chr_dec_digit_p(current, enc)
           || chr_sign_p(current, enc)
           || chr_same_p(current, '.', true, enc)
           || chr_same_p(current, '#', true, enc)) {
    return LEXER_STATE_NUMERIC;
  }
  else if (chr_same_p(current, ',', true, enc)) {
    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_UNQUOTE;
  }
  else if (chr_same_p(current, ';', true, enc)) {
    scm_capi_read_cchr(&current, port);
    return LEXER_STATE_COMMENT;
  }
  else if (chr_same_p(current, '"', true, enc)) {
    scm_capi_read_cchr(&current, port);
    return LEXER_STATE_STRING;
  }
  else if (chr_same_p(current, '|', true, enc)) {
    scm_capi_read_cchr(&current, port);
    return LEXER_STATE_IDENTIFIER_VBAR;
  }
  else {
    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
}

static int
scm_lexer_tokenize_identifier(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    ssize_t width = scm_capi_peek_cchr(&current, port);
    if (width < 0) {
        return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      break;
    }
    else if (chr_delimiter_p(current, enc)) {
      break;
    }

    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
  return LEXER_STATE_DONE;
}

static int
scm_lexer_tokenize_ident_vbar(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;
  bool escaped_p;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  escaped_p = false;
  while (1) {
    ssize_t width = scm_capi_read_cchr(&current, port);
    if (width < 0) {
        return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_cchr(&current, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    if (escaped_p) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = false;
    }
    else if (chr_same_p(current, '|', true, enc)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER_VBAR);
      return LEXER_STATE_DONE;
    }
    else if (chr_same_p(current, '\\', true, enc)) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = true;
    }
    else {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }

  return 0;
}

static int
scm_lexer_tokenize_dot(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_capi_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return  LEXER_STATE_DONE;
  }

  if (chr_whitespace_p(current, enc) || chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_DOT);
    return LEXER_STATE_DONE;
  }
  else {
    return LEXER_STATE_IDENTIFIER;
  }
}

static int
scm_lexer_tokenize_comment(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    ssize_t width = scm_capi_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
      return LEXER_STATE_DONE;
    }
    else if (chr_find("\n\r", current, enc) != NULL) {
      return LEXER_STATE_DISREGARD;
    }
  }
}

static int
scm_lexer_tokenize_unquote(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_capi_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }

  if (chr_same_p(current, '@', true, enc)) {
    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE_SPLICING);
    return LEXER_STATE_DONE;
  }
  else {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }
}

static int
scm_lexer_tokenize_number_sign(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int state, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_capi_read_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_cchr(&current, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  rslt = scm_lexer_push_char(lexer, current);
  if (rslt < 0) return LEXER_STATE_ERROR;

  if (chr_same_p(current, 't', false, enc)) {
    state = LEXER_STATE_BOOL_TRUE;
  }
  else if (chr_same_p(current, 'f', false, enc)) {
    state = LEXER_STATE_BOOL_FALSE;
  }
  else if (chr_same_p(current, 'u', false, enc)) {
    state = LEXER_STATE_BYTEVECTOR_START;
  }
  else if (chr_same_p(current, '\\', true, enc)) {
    state = LEXER_STATE_CHAR;
  }
  else if (chr_same_p(current, '(', true, enc)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_VECTOR_START);
    state = LEXER_STATE_DONE;
  }
  else if (chr_dec_digit_p(current, enc)) {
    state = LEXER_STATE_REFERENCE_DECL_OR_USE;
  }
  else {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    state = LEXER_STATE_ERROR;
  }

  return state;
}

static int
scm_lexer_tokenize_bool_true_or_false(ScmLexer *lexer, ScmObj port,
                                      ScmEncoding *enc, const char *str,
                                      SCM_TOKEN_TYPE_T ttype)
{
  scm_char_t current;
  ssize_t width;
  int rslt;
  int idx;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);
  scm_assert(str != NULL);

  width = scm_capi_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  idx = 0;

  if (chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  scm_capi_read_cchr(&current, port);

  if (!chr_same_p(current, str[idx], false, enc)) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  rslt = scm_lexer_push_char(lexer, current);
  if (rslt < 0) return LEXER_STATE_ERROR;

  idx++;

  while (str[idx] != '\0') {
    width = scm_capi_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (!chr_same_p(current, str[idx], false, enc)) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;

    idx++;
  }

  width = scm_capi_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_capi_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else if (chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else {
    scm_capi_read_cchr(&current, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }
}

static int
scm_lexer_tokenize_bool_true(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, enc,
                                               "rue",
                                               SCM_TOKEN_TYPE_BOOL_TRUE);
}

static int
scm_lexer_tokenize_bool_false(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, enc,
                                               "alse",
                                               SCM_TOKEN_TYPE_BOOL_FALSE);
}

static int
scm_lexer_tokenize_string(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;
  bool escaped_p;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  escaped_p = false;
  while (1) {
    width = scm_capi_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    if (escaped_p) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = false;
    }
    else if (chr_same_p(current, '"', true, enc)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_STRING);
      return LEXER_STATE_DONE;
    }
    else if (chr_same_p(current, '\\', true, enc)) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = true;
    }
    else {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
    }
  }
}

static int
scm_lexer_tokenize_char(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_capi_read_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  rslt = scm_lexer_push_char(lexer, current);
  if (rslt < 0) return LEXER_STATE_ERROR;

  while (1) {
    width = scm_capi_peek_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }
    else if (chr_delimiter_p(current, enc)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
      return LEXER_STATE_DONE;
    }

    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }
}

static int
scm_lexer_tokenize_reference(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    width = scm_capi_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (chr_same_p(current, '#', true, enc)) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_USE);;
      return LEXER_STATE_DONE;
    }
    else if (chr_same_p(current, '=', true, enc)) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_REFERENCE_DECL);;
      return LEXER_STATE_DONE;
    }
    else if (!chr_dec_digit_p(current, enc)) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }
}

static int
scm_lexer_tokenize_numeric(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  ScmNumParseData *p;
  scm_char_t chr;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  p = scm_num_parse(port, &lexer->str, &lexer->npd);
  if (p == NULL) {
    return -1;
  }

  if (lexer->npd.rslt == SCM_NUM_PARSE_SUCCESS) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_NUMERIC);
    return LEXER_STATE_DONE;
  }
  else if (lexer->npd.rslt == SCM_NUM_PARSE_INTERNAL_ERR) {
    return LEXER_STATE_ERROR;
  }
  else if (EARY_SIZE(&lexer->str) != 1) {
    return LEXER_STATE_IDENTIFIER;
  }

  EARY_GET(&lexer->str, scm_char_t, 0, chr);

  if (chr_same_p(chr, '.', true, enc))
    return LEXER_STATE_DOT;
  else if (chr_same_p(chr, '#', true, enc))
    return LEXER_STATE_NUMBER_SIGN;
  else
    return LEXER_STATE_IDENTIFIER;
}

static int
scm_lexer_tokenize_bv_start(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  for (const char *p = "8("; *p != '\0'; p++) {
    width = scm_capi_peek_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_capi_read_cchr(&current, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (!chr_same_p(current, *p, true, enc)) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    scm_capi_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BYTEVECTOR_START);

  return LEXER_STATE_DONE;
}

static void
scm_lexer_push_token(ScmLexer *lexer, ScmToken *token)
{
  assert(lexer != NULL);
  assert(token != NULL);

  if (lexer->tokens_head == NULL)
    lexer->tokens_head = token;
  else
    lexer->tokens_tail->next = token;

  lexer->tokens_tail = token;
  token->next = NULL;
}

static int
scm_lexer_tokenize(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  LEXER_STATE_T state;
  ScmToken *token;

  scm_assert(lexer != NULL);
  scm_assert(EARY_SIZE(&lexer->str) == 0);
  scm_assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  if (scm_lexer_error_p(lexer)) return -1;

  state = LEXER_STATE_INIT;
  while (state != LEXER_STATE_DONE && state != LEXER_STATE_ERROR) {
    switch (state) {
    case LEXER_STATE_INIT:
    case LEXER_STATE_DISREGARD:
      state = scm_lexer_tokenize_init(lexer, port, enc);
      break;
    case LEXER_STATE_IDENTIFIER:
      state = scm_lexer_tokenize_identifier(lexer, port, enc);
      break;
    case LEXER_STATE_IDENTIFIER_VBAR:
      state = scm_lexer_tokenize_ident_vbar(lexer, port, enc);
      break;
    case LEXER_STATE_DOT:
      state = scm_lexer_tokenize_dot(lexer, port, enc);
      break;
    case LEXER_STATE_UNQUOTE:
      state = scm_lexer_tokenize_unquote(lexer, port, enc);
      break;
    case LEXER_STATE_COMMENT:
      state = scm_lexer_tokenize_comment(lexer, port, enc);
      break;
    case LEXER_STATE_NUMBER_SIGN:
      state = scm_lexer_tokenize_number_sign(lexer, port, enc);
      break;
    case LEXER_STATE_BOOL_TRUE:
      state = scm_lexer_tokenize_bool_true(lexer, port, enc);
      break;
    case LEXER_STATE_BOOL_FALSE:
      state = scm_lexer_tokenize_bool_false(lexer, port, enc);
      break;
    case LEXER_STATE_STRING:
      state = scm_lexer_tokenize_string(lexer, port, enc);
      break;
    case LEXER_STATE_CHAR:
      state = scm_lexer_tokenize_char(lexer, port, enc);
      break;
    case LEXER_STATE_REFERENCE_DECL_OR_USE:
      state = scm_lexer_tokenize_reference(lexer, port, enc);
      break;
    case LEXER_STATE_NUMERIC:
      state = scm_lexer_tokenize_numeric(lexer, port, enc);
      break;
    case LEXER_STATE_BYTEVECTOR_START:
      state = scm_lexer_tokenize_bv_start(lexer, port, enc);
      break;
    case LEXER_STATE_DONE:      /* fall through */
    case LEXER_STATE_ERROR:     /* fall through */
    default:
      assert(false); // must not happen
      break;
    }
  }

  if (state == LEXER_STATE_ERROR && !scm_lexer_error_p(lexer))
    return -1;

  token = scm_lexer_new_token(lexer);
  if (token == NULL) return -1;

  scm_lexer_push_token(lexer, token);

  return 0;
}


ScmLexer *
scm_lexer_new(void)
{
  ScmLexer *lexer;

  lexer = scm_capi_malloc(sizeof(ScmLexer));
  if (lexer == NULL) return NULL;

  lexer->tokens_head = NULL;
  lexer->tokens_tail = NULL;

  eary_init(&lexer->str, sizeof(scm_char_t), 0);

  scm_lexer_clear_state(lexer);
  scm_lexer_clear_error_state(lexer);

  return lexer;
}

void
scm_lexer_end(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  while (lexer->tokens_head != NULL)
    scm_lexer_shift_token(lexer);

  scm_lexer_clear_state(lexer);

  eary_fin(&lexer->str);
  scm_capi_free(lexer);
}

ScmToken *
scm_lexer_head_token(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_assert(lexer != NULL);

  if (lexer->tokens_head == NULL) {
    int r = scm_lexer_tokenize(lexer, port, enc);
    if (r < 0) return NULL;
  }

  return lexer->tokens_head;
}

void
scm_lexer_shift_token(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  if (lexer->tokens_head != NULL) {
    ScmToken *token = lexer->tokens_head;

    lexer->tokens_head = token->next;
    if (lexer->tokens_head == NULL)
      lexer->tokens_tail = NULL;

    if (token->type == SCM_TOKEN_TYPE_TOKENIZE_ERR)
      scm_lexer_clear_error_state(lexer);
    scm_token_end(token);
  }
}

void
scm_lexer_clear_error_state(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  lexer->error_type = SCM_LEXER_ERR_TYPE_NONE;
}

bool
scm_lexer_error_p(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  return (lexer->error_type != SCM_LEXER_ERR_TYPE_NONE);
}

SCM_LEXER_ERR_TYPE_T
scm_lexer_error_type(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  return lexer->error_type;
}

static ScmObj
scm_parser_parse_list(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &car, &cdr);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL)  return SCM_OBJ_NULL;

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return SCM_NIL_OBJ;
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car)) return SCM_OBJ_NULL;
  if (scm_capi_eof_object_p(car)) {
    scm_capi_read_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  if (token->type == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(parser->lexer);

    cdr = scm_parser_parse_expression(parser, port);
    if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL;
    if (scm_capi_eof_object_p(car)) {
      scm_capi_read_error("Parser: unexpected eof", 0);
      return SCM_OBJ_NULL;
    }

    token = scm_lexer_head_token(parser->lexer, port, enc);
    if (token == NULL) return SCM_OBJ_NULL;

    if (token->type == SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      if (scm_lexer_error_type(parser->lexer)
          == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
        scm_capi_error("unexpected character", 0);
      }
      else {
        scm_capi_read_error("unexpected eof", 0);
      }
      return SCM_OBJ_NULL;
    }
    else if (token->type != SCM_TOKEN_TYPE_RPAREN) {
      /* TODO: change error message */
      scm_capi_error("Parser: unexpected token", 0);
      return SCM_OBJ_NULL;
    }

    scm_lexer_shift_token(parser->lexer);
  }
  else {
    cdr = scm_parser_parse_list(parser, port, enc);
    if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL;
  }

  return scm_api_cons(car, cdr);
}

static ScmObj
scm_parser_parse_quote(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  const char *quote_str = "quote";
  const char *quasiquote_str = "quasiquote";
  const char *unquote_str = "unquote";
  const char *unquote_splicing_str = "unquote-splicing";

  ScmObj quote = SCM_OBJ_INIT;
  ScmObj quoted = SCM_OBJ_INIT;
  ScmToken *token;

  SCM_STACK_FRAME_PUSH(&port, &quote, &quoted);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  switch (token->type) {
  case SCM_TOKEN_TYPE_QUOTE:
    quote = scm_capi_make_symbol_from_cstr(quote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    quote = scm_capi_make_symbol_from_cstr(quasiquote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    quote = scm_capi_make_symbol_from_cstr(unquote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    quote = scm_capi_make_symbol_from_cstr(unquote_splicing_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_NONE:               /* fall through */
  case SCM_TOKEN_TYPE_LPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_RPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_DOT:                /* fall through */
  case SCM_TOKEN_TYPE_STRING:             /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER:         /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER_VBAR:    /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_DECL:     /* fall through */
  case SCM_TOKEN_TYPE_REFERENCE_USE:      /* fall through */
  case SCM_TOKEN_TYPE_NUMERIC:            /* fall through */
  case SCM_TOKEN_TYPE_BOOL_TRUE:          /* fall through */
  case SCM_TOKEN_TYPE_BOOL_FALSE:         /* fall through */
  case SCM_TOKEN_TYPE_VECTOR_START:       /* fall through */
  case SCM_TOKEN_TYPE_BYTEVECTOR_START:   /* fall through */
  case SCM_TOKEN_TYPE_CHAR:               /* fall through */
  case SCM_TOKEN_TYPE_EOF:                /* fall through */
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:       /* fall through */
  default:
    scm_assert(false);   /* must not happen */
    break;
  }

  if (scm_capi_null_value_p(quote)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(parser->lexer);

  quoted = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL;
  if (scm_capi_eof_object_p(quoted)) {
    scm_capi_read_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  quoted = scm_api_cons(quoted, SCM_NIL_OBJ);
  if (scm_capi_null_value_p(quoted)) return SCM_OBJ_NULL;

  return scm_api_cons(quote, quoted);
}

static int
scm_parser_parse_inline_hex_escape(const scm_char_t *str, size_t size,
                                   ScmEncoding *enc,
                                   long long *scalar)
{
  const char *hex = "0123456789abcdefABCDEF";
  size_t i;

  scm_assert(str != NULL);
  scm_assert(scalar != NULL);

  if (size < 4
      || !str_same_p(str, 2, "\\x", false, enc)) {
    scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
    return -1;
  }

  *scalar = 0;
  for (i = 2; i < size && !chr_same_p(str[i], ';', true, enc); i++) {
    const char *p = chr_find(hex, str[i], enc);
    if (p == NULL) {
      scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
      return -1;
    }

    if (*scalar > (LLONG_MAX >> 4)) {
      scm_capi_error("Paraser: too big scalar value", 0);
      return -1;
    }

    *scalar = *scalar << 4;
    if (p - hex < 0xf)
      *scalar |= (p - hex);
    else
      *scalar |= ((p - hex) - 6);
  }

  if (i == 2 || i >= size) {
    scm_capi_error("Paraser: invalid inline hex escape sequence", 0);
    return -1;
  }

  return (int)i + 1;
}

static ssize_t
scm_parser_parse_mnemonic_escape(const scm_char_t *str, size_t size,
                                 ScmEncoding *enc,
                                 scm_char_t *chr, size_t *skip)
{
  ssize_t width;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);
  scm_assert(chr != NULL);
  scm_assert(skip != NULL);

  if (chr_same_p(str[1], 'a', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '\a', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else if (chr_same_p(str[1], 'b', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '\b', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else if (chr_same_p(str[1], 't', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '\t', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else if (chr_same_p(str[1], 'n', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '\n', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else if (chr_same_p(str[1], 'r', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '\r', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else {
    *skip = 1;
    width = 0;
  }

  return width;
}

static ssize_t
scm_parser_parse_str_fold_eec_seq(const scm_char_t *str, size_t size,
                                  ScmEncoding *enc)
{
  size_t i;
  bool line_end_p;
  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (size < 2 || !chr_same_p(str[0], '\\', true, enc)) {
    scm_capi_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  for (i = 1; i < size && chr_intraline_whitespace_p(str[i], enc); i++)
    ;

  if (i >= size) {
    scm_capi_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  line_end_p = false;

  if (chr_same_p(str[i], '\r', true, enc)) {
    line_end_p = true;
    i++;
  }

  if (chr_same_p(str[i], '\n', true, enc)) {
    line_end_p = true;
    i++;
  }

  if (!line_end_p)
    return 1;

  for (; i < size && chr_intraline_whitespace_p(str[i], enc); i++)
    ;

  return (ssize_t)i;
}

static ssize_t
scm_parser_parse_string_esc_seq(const scm_char_t *str, size_t size,
                                ScmEncoding *enc,
                                scm_char_t *chr, size_t *skip)
{
  long long hv;
  int rslt;
  ssize_t width;
  ssize_t sk;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);
  scm_assert(chr != NULL);
  scm_assert(skip != NULL);

  if (size < 2 || !chr_same_p(str[0], '\\', true, enc)) {
    scm_capi_error("Parser: invalid escpase sequence", 0);
    return -1;
  }

  if (chr_find(" \t\r\n", str[1], enc) != NULL) {
    sk = scm_parser_parse_str_fold_eec_seq(str, size, enc);
    if (sk < 0) return -1;
    *skip = (size_t)sk;
    width = 0;
  }
  else if (chr_same_p(str[1], 'x', true, enc)) {
    rslt = scm_parser_parse_inline_hex_escape(str, size, enc, &hv);
    if (rslt < 0) return -1;
    *skip = (size_t)rslt;
    width = scm_enc_cnv_from_scalar(enc, hv, chr);
    if (width < 0) {
      scm_capi_error("Parser: invalid scalar value", 0);
      return -1;
    }
  }
  else
    width = scm_parser_parse_mnemonic_escape(str, size, enc, chr, skip);

  return width;
}

int
scm_parser_unescape_string(ScmToken *token, ScmObj str, ScmEncoding *enc)
{
  size_t idx, inc;
  ssize_t width;
  int rslt;
  scm_char_t chr;

  SCM_STACK_FRAME_PUSH(&str);

  scm_assert(token != NULL);
  scm_assert(scm_capi_string_p(str));
  scm_assert(enc != NULL);

  idx = 0;
  while (idx < token->len) {
    if (chr_same_p(token->str[idx], '\\', true, enc)) {
      width = scm_parser_parse_string_esc_seq(token->str + idx,
                                              token->len - idx,
                                              enc, &chr, &inc);
      if (width < 0) return -1;
    }
    else {
      chr = token->str[idx];
      width = 1;
      inc = 1;
    }

    if (width > 0) {
      rslt = scm_capi_string_push(str, chr, enc);
      if (rslt < 0) return -1;
    }

    idx += inc;
  }

  return 0;
}

static ScmObj
scm_parser_parse_string(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmToken *token;
  int rslt;

  SCM_STACK_FRAME_PUSH(&port, &str);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  str = scm_capi_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
  if (token->str != NULL) {
    rslt = scm_parser_unescape_string(token, str, enc);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  scm_lexer_shift_token(parser->lexer);

  return str;
}

static ssize_t
scm_parser_parse_ident_esc_seq(const scm_char_t *str, size_t size,
                               ScmEncoding *enc,
                               scm_char_t *chr, size_t *skip)
{
  long long hv;
  int rslt;
  ssize_t width;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);
  scm_assert(chr != NULL);
  scm_assert(skip != NULL);

  if (size < 2 || !chr_same_p(str[0], '\\', true, enc)) {
    scm_capi_error("Parser: invalid escpase sequence", 0);
    return -1;
  }

  if (chr_same_p(str[1], '|', true, enc)) {
    scm_enc_cnv_from_ascii(enc, '|', chr);
    width = scm_enc_char_width(enc, chr->bytes, sizeof(*chr));
    *skip = 2;
  }
  else if (chr_same_p(str[1], 'x', true, enc)) {
    rslt = scm_parser_parse_inline_hex_escape(str, size, enc, &hv);
    if (rslt < 0) return -1;
    *skip = (size_t)rslt;
    width = scm_enc_cnv_from_scalar(enc, hv, chr);
    if (width < 0) {
      scm_capi_error("Parser: invalid scalar value", 0);
      return -1;
    }
  }
  else
    width = scm_parser_parse_mnemonic_escape(str, size, enc, chr, skip);

  return width;
}

static int
scm_parser_unescape_ident(ScmToken *token, ScmObj str, ScmEncoding *enc)
{
  size_t idx, inc;
  int rslt;
  scm_char_t chr;
  ssize_t width;

  SCM_STACK_FRAME_PUSH(&str);

  scm_assert(token != NULL);
  scm_assert(scm_capi_string_p(str));
  scm_assert(enc != NULL);

  idx = 0;
  while (idx < token->len) {
    if (chr_same_p(token->str[idx], '\\', true, enc)) {
      width = scm_parser_parse_ident_esc_seq(token->str + idx,
                                             token->len - idx,
                                             enc,
                                             &chr, &inc);
      if (width < 0) return -1;
    }
    else {
      chr = token->str[idx];
      width = 1;
      inc = 1;
    }

    if (width > 0) {
      rslt = scm_capi_string_push(str, chr, enc);
      if (rslt < 0) return -1;
    }

    idx += inc;
  }

  return 0;
}

static ScmObj
scm_parser_parse_identifier(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  ScmObj str = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  int rslt;

  SCM_STACK_FRAME_PUSH(&port, &str, &sym);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  str = scm_capi_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL)  return SCM_OBJ_NULL;

  if (token->str != NULL) {
    rslt = scm_parser_unescape_ident(token, str, enc);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  sym = scm_api_string_to_symbol(str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(parser->lexer);

  return sym;
}

static ScmObj
scm_parser_parse_numeric(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj num = SCM_OBJ_INIT;
  ScmToken *token;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  num = scm_num_make_from_parsedata(token->str, enc, &token->npd);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(parser->lexer);

  return num;
}

static ScmObj
scm_parser_parse_bool(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  SCM_TOKEN_TYPE_T type;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  type = token->type;
  scm_lexer_shift_token(parser->lexer);

  if (type == SCM_TOKEN_TYPE_BOOL_TRUE)
    return SCM_TRUE_OBJ;
  else
    return SCM_FALSE_OBJ;
}

static ScmObj
scm_parser_parse_vector_aux(ScmParser *parser,
                            ScmObj port, ScmEncoding *enc, size_t *len)
{
  ScmToken *token;
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&port, &car, &cdr);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);
  scm_assert(len != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL)  return SCM_OBJ_NULL;

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(parser->lexer);
    return SCM_NIL_OBJ;
  }

  car = scm_parser_parse_expression(parser, port);
  if (scm_capi_null_value_p(car)) return SCM_OBJ_NULL;
  if (scm_capi_eof_object_p(car)) {
    scm_capi_read_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  *len += 1;

  cdr = scm_parser_parse_vector_aux(parser, port, enc, len);
  if (scm_capi_null_value_p(cdr)) return SCM_OBJ_NULL;

  return scm_api_cons(car, cdr);
}

static ScmObj
scm_parser_parse_vector(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj vec = SCM_OBJ_INIT, elms = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;
  size_t len, idx;
  int rslt;

  SCM_STACK_FRAME_PUSH(&port, &vec, &elms, &elm);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  scm_lexer_shift_token(parser->lexer);

  len = 0;
  elms = scm_parser_parse_vector_aux(parser, port, enc, &len);
  if (scm_capi_null_value_p(elms)) return SCM_OBJ_NULL;

  vec = scm_capi_make_vector(len, SCM_OBJ_NULL);
  if (scm_capi_null_value_p(vec)) return SCM_OBJ_NULL;

  for (idx = 0; idx < len; idx++) {
    elm = scm_api_car(elms);
    rslt = scm_capi_vector_set_i(vec, idx, elm);
    if (rslt < 0) return SCM_OBJ_NULL;
    elms = scm_api_cdr(elms);
  }

  return vec;
}


static ScmObj
scm_parser_parse_char(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  ScmObj chr = SCM_OBJ_INIT;

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  if (str_same_p(token->str, token->len, "#\\newline", false, enc)) {
    chr = scm_api_make_char_newline(enc);
  }
  else if (str_same_p(token->str, token->len, "#\\space", false, enc)) {
    chr = scm_api_make_char_space(enc);
  }
  else if (token->len == 3) {
    chr = scm_capi_make_char(token->str + 2, enc);
  }
  else {
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unknown character name", 0);
    return SCM_OBJ_NULL;
  }

  scm_lexer_shift_token(parser->lexer);

  return chr;
}

static ScmObj
scm_parser_parse_eof(ScmParser *parser, ScmObj port, ScmEncoding *enc)
{
  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));
  scm_assert(enc != NULL);

  scm_lexer_shift_token(parser->lexer);
  return SCM_EOF_OBJ;
}

ScmParser *
scm_parser_new(void)
{
  ScmParser *parser;

  parser = scm_capi_malloc(sizeof(ScmParser));
  if (parser == NULL) return NULL;

  parser->lexer = scm_lexer_new();
  if (parser->lexer == NULL) {
    scm_capi_free(parser);
    return NULL;
  }

  return parser;
}

void
scm_parser_end(ScmParser *parser)
{
  scm_assert(parser != NULL);

  if (parser->lexer != NULL)
    scm_lexer_end(parser->lexer);
  scm_capi_free(parser);
}

ScmObj
scm_parser_parse_expression(ScmParser *parser, ScmObj port)
{
  ScmObj rslt = SCM_OBJ_INIT;
  ScmToken *token;
  ScmEncoding *enc;

  SCM_STACK_FRAME_PUSH(&port, &rslt);

  scm_assert(parser != NULL);
  scm_assert(scm_capi_input_port_p(port));

  enc = scm_capi_port_internal_encoding(port);
  if (enc == NULL) return SCM_OBJ_NULL;

  token = scm_lexer_head_token(parser->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  switch (token->type) {
  case SCM_TOKEN_TYPE_LPAREN:
    scm_lexer_shift_token(parser->lexer);
    rslt = scm_parser_parse_list(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_RPAREN:
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unexpeted right paren", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_DOT:
    scm_lexer_shift_token(parser->lexer);
    scm_capi_error("Parser: unexpeted dot", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_QUOTE:
    rslt = scm_parser_parse_quote(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    rslt = scm_parser_parse_quote(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    rslt = scm_parser_parse_quote(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    rslt = scm_parser_parse_quote(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_STRING:
    rslt = scm_parser_parse_string(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_IDENTIFIER:
  case SCM_TOKEN_TYPE_IDENTIFIER_VBAR: /* fall through */
    rslt = scm_parser_parse_identifier(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_REFERENCE_DECL:
    /* do not supported */
    scm_capi_error("Parser: reference not supported", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_REFERENCE_USE:
    /* do not supported */
    scm_capi_error("Parser: reference not supported", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_NUMERIC:
    rslt = scm_parser_parse_numeric(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_BOOL_TRUE: /* fall through */
  case SCM_TOKEN_TYPE_BOOL_FALSE:
    rslt = scm_parser_parse_bool(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_VECTOR_START:
    rslt = scm_parser_parse_vector(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_BYTEVECTOR_START:
    scm_capi_error("Parser: byte vector  not supported", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_CHAR:
    rslt = scm_parser_parse_char(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_EOF:
    rslt = scm_parser_parse_eof(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:
    if (scm_lexer_error_type(parser->lexer)
        == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
      scm_capi_error("Parser: unexpected char", 0);
    }
    else {
      scm_capi_read_error("Parser: unexpected eof", 0);
    }
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_NONE:     /* fall through */
  default:
    scm_assert(false); /* must not happen */
    break;
  }

  return rslt;
}
