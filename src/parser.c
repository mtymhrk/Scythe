#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/impl_utils.h"
#include "scythe/earray.h"
#include "scythe/number_parser.h"
#include "scythe/parser.h"

#define SCM_LEXER_INITIAL_BUFFER_SIZE 16

#define IDENTIFIER_START_CHARS "abcdefghijklmnopqrstuvwxyz!$%&*/:<=>?~_^"
#define IDENTIFIER_FOLLOW_ON_CHARS              \
  IDENTIFIER_START_CHARS "0123456789.+-"
#define IDENTIFIER_DELIMITER "()'`,\"[] \t\n\r;"
#define NUMERIC_START_CHAR "+-0123456789"

#define IS_LINE_FEED(c) ((c) == '\n')

#define IS_IDENTIFIER_START_CHAR(c)                     \
  (strchr(IDENTIFIER_START_CHARS, tolower(c)) != NULL)

#define IS_IDENTIFIER_FOLLOW_ON_CHAR(c)                         \
  (strchr(IDENTIFIER_FOLLOW_ON_CHARS, tolower(c)) != NULL)

#define IDENTIFIER_DELIMITER_P(c)                       \
  (strchr(IDENTIFIER_DELIMITER, tolower(c)) != NULL)

#define IS_NUMERIC_START_CHAR(c)                        \
  (strchr(NUMERIC_START_CHAR, tolower(c)) != NULL)

#define NONASCII ((char)0xff)

#define WHITESPACE " \t\n\r"
#define DELIMITER (WHITESPACE "()[]\";|")


static inline bool
chr_same_p(scm_char_t c1, char c2, bool c_sensitive, ScmEncoding *enc)
{
  if (c_sensitive)
    return scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), c2);
  else
    return (scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), toupper(c2))
            || scm_enc_same_char_p(enc, c1.bytes, sizeof(c1), tolower(c2)));
}

static inline const char *
chr_find(const char *str, scm_char_t c, ScmEncoding *enc)
{
  for (const char *p = str; *p != '\0'; p++)
    if (scm_enc_same_char_p(enc, c.bytes, sizeof(c), *p))
      return p;
  return NULL;
}

static inline bool
chr_whitespace_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(WHITESPACE, c, enc) != NULL);
}

static inline bool
chr_intraline_whitespace_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(" \t", c, enc) != NULL);
}

static inline bool
chr_delimiter_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find(DELIMITER, c, enc) != NULL);
}

static inline bool
chr_sign_p(scm_char_t c, ScmEncoding *enc)
{
  return (chr_find("-+", c, enc) != NULL);
}

static inline bool
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


typedef enum lexer_state {
  LEXER_STATE_DONE,
  LEXER_STATE_INIT,
  LEXER_STATE_DISREGARD,
  LEXER_STATE_IDENTIFIER,
  LEXER_STATE_IDENTIFIER_VLINE,
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
} lexer_state_t;


static ScmToken *
scm_token_new(scm_token_type_t type, scm_char_t *str, size_t len,
              ScmNumParseData *npd)
{
  ScmToken *token;

  scm_assert(type != SCM_TOKEN_TYPE_NUMERIC || npd != NULL);


  token = scm_fcd_malloc(sizeof(ScmToken));
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

  scm_fcd_free(token->str);
  scm_fcd_free(token);
}

static void
scm_lexer_set_token_type(ScmLexer *lexer, scm_token_type_t type)
{
  scm_assert(lexer != NULL);
  lexer->token_type = type;
}

static void
scm_lexer_setup_error_state(ScmLexer *lexer, scm_lexer_err_type_t error)
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

static lexer_state_t
scm_lexer_tokenize_init(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  const char *one_char_token_chars = "()[]'`";
  const scm_token_type_t one_char_token_types[] =
    { SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_LPAREN, SCM_TOKEN_TYPE_RPAREN,
      SCM_TOKEN_TYPE_QUOTE, SCM_TOKEN_TYPE_QUASIQUOTE };
  scm_char_t current;
  ssize_t width;
  const char *p;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_peek_cchr(&current, port);

  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_fcd_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_EOF);
    return LEXER_STATE_DONE;
  }

  if (!scm_enc_ascii_p(enc, current.bytes, (size_t)width)) {
    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
  else if (chr_whitespace_p(current, enc)) {
    scm_fcd_read_cchr(&current, port);
    return LEXER_STATE_DISREGARD;
  }
  else if ((p = chr_find(one_char_token_chars, current, enc))) {
    scm_fcd_read_cchr(&current, port);
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
    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_UNQUOTE;
  }
  else if (chr_same_p(current, ';', true, enc)) {
    scm_fcd_read_cchr(&current, port);
    return LEXER_STATE_COMMENT;
  }
  else if (chr_same_p(current, '"', true, enc)) {
    scm_fcd_read_cchr(&current, port);
    return LEXER_STATE_STRING;
  }
  else if (chr_same_p(current, '|', true, enc)) {
    scm_fcd_read_cchr(&current, port);
    return LEXER_STATE_IDENTIFIER_VLINE;
  }
  else {
    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
    return LEXER_STATE_IDENTIFIER;
  }
}

static lexer_state_t
scm_lexer_tokenize_identifier(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    ssize_t width = scm_fcd_peek_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      break;
    }
    else if (chr_delimiter_p(current, enc)) {
      break;
    }

    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER);
  return LEXER_STATE_DONE;
}

static lexer_state_t
scm_lexer_tokenize_ident_vline(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;
  bool escaped_p;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  escaped_p = false;
  while (1) {
    ssize_t width = scm_fcd_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_fcd_read_cchr(&current, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    if (escaped_p) {
      rslt = scm_lexer_push_char(lexer, current);
      if (rslt < 0) return LEXER_STATE_ERROR;
      escaped_p = false;
    }
    else if (chr_same_p(current, '|', true, enc)) {
      scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_IDENTIFIER_VLINE);
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

static lexer_state_t
scm_lexer_tokenize_dot(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_peek_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_comment(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    ssize_t width = scm_fcd_read_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_unquote(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  int rslt;
  ssize_t width;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_UNQUOTE);
    return LEXER_STATE_DONE;
  }

  if (chr_same_p(current, '@', true, enc)) {
    scm_fcd_read_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_number_sign(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int state, rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_read_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_fcd_read_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_bool_true_or_false(ScmLexer *lexer, ScmObj port,
                                      ScmEncoding *enc, const char *str,
                                      scm_token_type_t ttype)
{
  scm_char_t current;
  ssize_t width;
  int rslt;
  int idx;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);
  scm_assert(str != NULL);

  width = scm_fcd_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_fcd_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  idx = 0;

  if (chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }

  scm_fcd_read_cchr(&current, port);

  if (!chr_same_p(current, str[idx], false, enc)) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }

  rslt = scm_lexer_push_char(lexer, current);
  if (rslt < 0) return LEXER_STATE_ERROR;

  idx++;

  while (str[idx] != '\0') {
    width = scm_fcd_read_cchr(&current, port);
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

  width = scm_fcd_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_fcd_read_cchr(&current, port);
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else if (chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, ttype);
    return LEXER_STATE_DONE;
  }
  else {
    scm_fcd_read_cchr(&current, port);
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
    return LEXER_STATE_ERROR;
  }
}

static lexer_state_t
scm_lexer_tokenize_bool_true(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, enc,
                                               "rue",
                                               SCM_TOKEN_TYPE_BOOL_TRUE);
}

static lexer_state_t
scm_lexer_tokenize_bool_false(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  return scm_lexer_tokenize_bool_true_or_false(lexer, port, enc,
                                               "alse",
                                               SCM_TOKEN_TYPE_BOOL_FALSE);
}

static lexer_state_t
scm_lexer_tokenize_string(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;
  bool escaped_p;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  escaped_p = false;
  while (1) {
    width = scm_fcd_read_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_char_hex_scalar(ScmLexer *lexer, ScmObj port,
                                   ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_peek_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }

  if (width == 0 || chr_delimiter_p(current, enc)) {
    scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
    return LEXER_STATE_DONE;
  }

  do {
    width = scm_fcd_read_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }

    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;

  } while (!chr_same_p(current, ';', true, enc));

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_CHAR);
  return LEXER_STATE_DONE;
}

static lexer_state_t
scm_lexer_tokenize_char(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  width = scm_fcd_read_cchr(&current, port);
  if (width < 0) {
    return LEXER_STATE_ERROR;
  }
  else if (width == 0) {
    scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
    return LEXER_STATE_ERROR;
  }

  rslt = scm_lexer_push_char(lexer, current);
  if (rslt < 0) return LEXER_STATE_ERROR;

  if (chr_same_p(current, 'x', false, enc))
    return scm_lexer_tokenize_char_hex_scalar(lexer, port, enc);

  while (1) {
    width = scm_fcd_peek_cchr(&current, port);
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

    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }
}

static lexer_state_t
scm_lexer_tokenize_reference(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  while (1) {
    width = scm_fcd_read_cchr(&current, port);
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

static lexer_state_t
scm_lexer_tokenize_numeric(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  ScmNumParseData *p;
  scm_char_t chr;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
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

static lexer_state_t
scm_lexer_tokenize_bv_start(ScmLexer *lexer, ScmObj port, ScmEncoding *enc)
{
  scm_char_t current;
  ssize_t width;
  int rslt;

  scm_assert(lexer != NULL);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  for (const char *p = "8("; *p != '\0'; p++) {
    width = scm_fcd_peek_cchr(&current, port);
    if (width < 0) {
      return LEXER_STATE_ERROR;
    }
    else if (width == 0) {
      scm_fcd_read_cchr(&current, port);
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_EOF);
      return LEXER_STATE_ERROR;
    }
    else if (!chr_same_p(current, *p, true, enc)) {
      scm_lexer_setup_error_state(lexer, SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR);
      return LEXER_STATE_ERROR;
    }

    scm_fcd_read_cchr(&current, port);
    rslt = scm_lexer_push_char(lexer, current);
    if (rslt < 0) return LEXER_STATE_ERROR;
  }

  scm_lexer_set_token_type(lexer, SCM_TOKEN_TYPE_BYTEVECTOR_START);

  return LEXER_STATE_DONE;
}

static void
scm_lexer_push_token(ScmLexer *lexer, ScmToken *token)
{
  scm_assert(lexer != NULL);
  scm_assert(token != NULL);

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
  lexer_state_t state;
  ScmToken *token;

  scm_assert(lexer != NULL);
  scm_assert(EARY_SIZE(&lexer->str) == 0);
  scm_assert(lexer->token_type == SCM_TOKEN_TYPE_NONE);
  scm_assert(scm_fcd_input_port_p(port));
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
    case LEXER_STATE_IDENTIFIER_VLINE:
      state = scm_lexer_tokenize_ident_vline(lexer, port, enc);
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
      scm_assert(false);        /* must not happen */
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

  lexer = scm_fcd_malloc(sizeof(ScmLexer));
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
  scm_fcd_free(lexer);
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

scm_lexer_err_type_t
scm_lexer_error_type(ScmLexer *lexer)
{
  scm_assert(lexer != NULL);

  return lexer->error_type;
}

ScmTypeInfo SCM_DATUM_LABEL_USE_TYPE_INFO = {
  .name                = "datum-label-use",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmDatumLabelUse),
  .gc_ini_func         = scm_datum_label_use_gc_initialize,
  .gc_fin_func         = NULL,
  .gc_accept_func      = scm_datum_label_use_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL
};

#define DATUM_LABEL_INIT_OBJ SCM_UNDEF_OBJ

int
scm_datum_label_use_initialize(ScmObj use, ScmParserRef *ref)
{
  scm_assert_obj_type(use, &SCM_DATUM_LABEL_USE_TYPE_INFO);
  scm_assert(ref != NULL);

  SCM_SLOT_SETQ(ScmDatumLabelUse, use, ref.referrer, ref->referrer);
  SCM_DATUM_LABEL_USE(use)->ref.pos = ref->pos;

  return 0;
}

ScmObj
scm_datum_label_use_new(scm_mem_type_t mtype, ScmParserRef *ref)
{
  ScmObj use = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&use);

  scm_assert(ref != NULL);

  use = scm_fcd_mem_alloc(&SCM_DATUM_LABEL_USE_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(use)) return SCM_OBJ_NULL;

  if (scm_datum_label_use_initialize(use, ref) < 0)
    return SCM_OBJ_NULL;

  return use;
}

static ScmObj
datum_label_get_box(ScmParserRef *ref)
{
  scm_assert(ref != NULL);

  if (scm_fcd_pair_p(ref->referrer)) {
    if (ref->pos == 0)
      return scm_fcd_car(ref->referrer);
    else
      return scm_fcd_cdr(ref->referrer);
  }
  else if (scm_fcd_vector_p(ref->referrer)) {
    scm_assert(ref->pos < scm_fcd_vector_length(ref->referrer));
    return scm_fcd_vector_ref(ref->referrer, ref->pos);
  }
  else {
    scm_fcd_error("Parser: invlaid datum label reference", 0);
    return SCM_OBJ_NULL;
  }
}

static int
datum_label_set(ScmParserRef *ref, ScmObj datum)
{
  scm_assert(ref != NULL);
  scm_assert(scm_obj_not_null_p(datum));

  if (scm_fcd_pair_p(ref->referrer)) {
    if (ref->pos == 0)
      scm_fcd_set_car_i(ref->referrer, datum);
    else
      scm_fcd_set_cdr_i(ref->referrer, datum);
    return 0;
  }
  else if (scm_fcd_vector_p(ref->referrer)) {
    scm_assert(ref->pos < scm_fcd_vector_length(ref->referrer));
    scm_fcd_vector_set_i(ref->referrer, ref->pos, datum);
    return 0;
  }
  else {
    scm_fcd_error("Parser: invlaid datum label reference", 0);
    return -1;
  }
}

int
scm_datum_label_use_resolve(ScmObj use)
{
  ScmObj box = SCM_OBJ_INIT, datum = SCM_OBJ_INIT;
  int r;

  scm_assert_obj_type(use, &SCM_DATUM_LABEL_USE_TYPE_INFO);

  box = datum_label_get_box(&SCM_DATUM_LABEL_USE(use)->ref);
  if (!scm_fcd_box_object_p(box))
    return 0;

  datum = scm_fcd_box_unbox(box);
  if (scm_fcd_eq_p(datum, DATUM_LABEL_INIT_OBJ)) {
    scm_fcd_error("Parser: datum label not declared", 0);
    return -1;
  }

  r = datum_label_set(&SCM_DATUM_LABEL_USE(use)->ref, datum);
  if (r < 0) return -1;

  return 0;
}

void
scm_datum_label_use_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_DATUM_LABEL_USE_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));

  SCM_DATUM_LABEL_USE(obj)->ref.referrer = SCM_OBJ_NULL;
}

int
scm_datum_label_use_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  scm_assert_obj_type(obj, &SCM_DATUM_LABEL_USE_TYPE_INFO);

  return SCM_GC_CALL_REF_HANDLER(handler,
                                 obj, SCM_DATUM_LABEL_USE(obj)->ref.referrer);
}

ScmTypeInfo SCM_PARSER_TYPE_INFO = {
  .name                = "parser",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmParser),
  .gc_ini_func         = scm_parser_gc_initialize,
  .gc_fin_func         = scm_parser_gc_finalize,
  .gc_accept_func      = scm_parser_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL
};

static ScmObj scm_parser_parse_expression(ScmObj parser,
                                          ScmObj port, ScmParserRef *ref);

static int
scm_parser_reg_dl_use(ScmObj parser, ScmParserRef *ref)
{
  ScmObj dlu = SCM_OBJ_INIT;
  int err;

  SCM_REFSTK_INIT_REG(&parser,
                      &dlu);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(ref != NULL);

  dlu = scm_datum_label_use_new(SCM_MEM_HEAP, ref);
  if (scm_obj_null_p(dlu)) return -1;

  EARY_PUSH_SCMOBJ(&SCM_PARSER(parser)->label_use, dlu, parser, err);
  if(err < 0) return -1;

  return 0;
}

static ScmObj
scm_parser_reg_dl_decl(ScmObj parser, size_t label)
{
  ScmObj box = SCM_OBJ_INIT;
  int err;

  SCM_REFSTK_INIT_REG(&parser,
                      &box);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  if (label < EARY_SIZE(&SCM_PARSER(parser)->label_decl)) {
    EARY_GET(&SCM_PARSER(parser)->label_decl, ScmObj, label, box);
    if (scm_obj_not_null_p(box)) {
      scm_fcd_error("Parser: datum label duplicated", 0);
      return SCM_OBJ_NULL;
    }
  }

  box = scm_fcd_box_new(SCM_MEM_HEAP, DATUM_LABEL_INIT_OBJ);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  EARY_SET_SCMOBJ(&SCM_PARSER(parser)->label_decl, label, box, parser, err);
  if (err < 0) return SCM_OBJ_NULL;

  return box;
}

static int
scm_parser_resolve_dl_use(ScmObj parser)
{
  ScmObj *use;
  size_t i;
  int r;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  EARY_FOR_EACH(&SCM_PARSER(parser)->label_use, i, use) {
    scm_assert_obj_type(*use, &SCM_DATUM_LABEL_USE_TYPE_INFO);
    r = scm_datum_label_use_resolve(*use);
    if (r < 0) return -1;
  }

  return 0;
}

static ScmObj
scm_parser_labeled_datum(ScmObj parser, size_t label)
{
  ScmObj box = SCM_OBJ_INIT;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  if (label >= EARY_SIZE(&SCM_PARSER(parser)->label_decl))
    goto err;

  EARY_GET(&SCM_PARSER(parser)->label_decl, ScmObj, label, box);
  if (scm_obj_null_p(box)) goto err;

  return box;

 err:
  scm_fcd_error("Parser: undeclared datum label referenced", 0);
  return SCM_OBJ_NULL;
}

static ScmObj
scm_parser_parse_list(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmParserRef ref = SCM_PARSER_REF_INIT;
  ScmToken *token;
  ScmObj pair = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &ref.referrer, &pair, &car, &cdr);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL)  return SCM_OBJ_NULL;

  if (token->type == SCM_TOKEN_TYPE_RPAREN) {
    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
    return SCM_NIL_OBJ;
  }

  pair = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_UNDEF_OBJ);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  ref.referrer = pair;
  ref.pos = 0;

  car = scm_parser_parse_expression(parser, port, &ref);
  if (scm_obj_null_p(car)) return SCM_OBJ_NULL;
  if (scm_fcd_eof_object_p(car)) {
    scm_fcd_read_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  if (token->type == SCM_TOKEN_TYPE_DOT) {
    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

    ref.pos = 1;
    cdr = scm_parser_parse_expression(parser, port, &ref);
    if (scm_obj_null_p(cdr)) return SCM_OBJ_NULL;
    if (scm_fcd_eof_object_p(car)) {
      scm_fcd_read_error("Parser: unexpected eof", 0);
      return SCM_OBJ_NULL;
    }

    token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
    if (token == NULL) return SCM_OBJ_NULL;

    if (token->type == SCM_TOKEN_TYPE_TOKENIZE_ERR) {
      if (scm_lexer_error_type(SCM_PARSER(parser)->lexer)
          == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
        scm_fcd_error("unexpected character", 0);
      }
      else {
        scm_fcd_read_error("unexpected eof", 0);
      }
      return SCM_OBJ_NULL;
    }
    else if (token->type != SCM_TOKEN_TYPE_RPAREN) {
      /* TODO: change error message */
      scm_fcd_error("Parser: unexpected token", 0);
      return SCM_OBJ_NULL;
    }

    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
  }
  else {
    cdr = scm_parser_parse_list(parser, port, enc);
    if (scm_obj_null_p(cdr)) return SCM_OBJ_NULL;
  }

  scm_fcd_set_car_i(pair, car);
  scm_fcd_set_cdr_i(pair, cdr);
  return pair;
}

static ScmObj
scm_parser_parse_quote(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  const char *quote_str = "quote";
  const char *quasiquote_str = "quasiquote";
  const char *unquote_str = "unquote";
  const char *unquote_splicing_str = "unquote-splicing";

  ScmParserRef ref = SCM_PARSER_REF_INIT;
  ScmObj sym = SCM_OBJ_INIT, quote = SCM_OBJ_INIT, quoted = SCM_OBJ_INIT;
  ScmToken *token;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &ref.referrer, &sym, &quote, &quoted);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  switch (token->type) {
  case SCM_TOKEN_TYPE_QUOTE:
    sym = scm_fcd_make_symbol_from_cstr(quote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_QUASIQUOTE:
    sym = scm_fcd_make_symbol_from_cstr(quasiquote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE:
    sym = scm_fcd_make_symbol_from_cstr(unquote_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_UNQUOTE_SPLICING:
    sym = scm_fcd_make_symbol_from_cstr(unquote_splicing_str, SCM_ENC_SRC);
    break;
  case SCM_TOKEN_TYPE_NONE:               /* fall through */
  case SCM_TOKEN_TYPE_LPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_RPAREN:             /* fall through */
  case SCM_TOKEN_TYPE_DOT:                /* fall through */
  case SCM_TOKEN_TYPE_STRING:             /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER:         /* fall through */
  case SCM_TOKEN_TYPE_IDENTIFIER_VLINE:   /* fall through */
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

  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  quote = scm_fcd_cons(SCM_UNDEF_OBJ, SCM_NIL_OBJ);
  if (scm_obj_null_p(quote)) return SCM_OBJ_NULL;

  ref.referrer = quote;
  ref.pos = 0;

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  quoted = scm_parser_parse_expression(parser, port, &ref);
  if (scm_obj_null_p(quoted)) return SCM_OBJ_NULL;
  if (scm_fcd_eof_object_p(quoted)) {
    scm_fcd_read_error("Parser: unexpected eof", 0);
    return SCM_OBJ_NULL;
  }

  scm_fcd_set_car_i(quote, quoted);

  return scm_fcd_cons(sym, quote);
}

static int
scm_parser_parse_hex_scalar_value(const scm_char_t *str, size_t size,
                                  ScmEncoding *enc,
                                  long long *scalar)
{
  const char *hex = "0123456789abcdefABCDEF";
  size_t i;

  scm_assert(str != NULL);
  scm_assert(scalar != NULL);

  *scalar = 0;
  for (i = 0; i < size && !chr_same_p(str[i], ';', true, enc); i++) {
    const char *p = chr_find(hex, str[i], enc);
    if (p == NULL) {
      scm_fcd_error("Paraser: invalid hex scalar value", 0);
      return -1;
    }

    if (*scalar > (LLONG_MAX >> 4)) {
      scm_fcd_error("Paraser: too big scalar value", 0);
      return -1;
    }

    *scalar = *scalar << 4;
    if (p - hex < 0xf)
      *scalar |= (p - hex);
    else
      *scalar |= ((p - hex) - 6);
  }

  if (i == 0 || i >= size) {
    scm_fcd_error("Paraser: invalid hex scalar value", 0);
    return -1;
  }

  return (int)i + 1;
}

static int
scm_parser_parse_inline_hex_escape(const scm_char_t *str, size_t size,
                                   ScmEncoding *enc,
                                   long long *scalar)
{
  int i;

  scm_assert(str != NULL);
  scm_assert(scalar != NULL);

  if (size < 4
      || !str_same_p(str, 2, "\\x", false, enc)) {
    scm_fcd_error("Paraser: invalid inline hex escape sequence", 0);
    return -1;
  }

  i = scm_parser_parse_hex_scalar_value(str + 2, size - 2, enc, scalar);
  if (i < 0) return -1;

  return i + 2;
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
    scm_fcd_error("Parser: invalid string fold escape sequence", 0);
    return -1;
  }

  for (i = 1; i < size && chr_intraline_whitespace_p(str[i], enc); i++)
    ;

  if (i >= size) {
    scm_fcd_error("Parser: invalid string fold escape sequence", 0);
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
    scm_fcd_error("Parser: invalid escpase sequence", 0);
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
      scm_fcd_error("Parser: invalid scalar value", 0);
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

  SCM_REFSTK_INIT_REG(&str);

  scm_assert(token != NULL);
  scm_assert(scm_fcd_string_p(str));
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
      if (scm_fcd_string_bytesize(str) + (size_t)width > SSIZE_MAX) {
        scm_fcd_error("Parser: string too big", 0);
        return -1;
      }

      rslt = scm_fcd_string_push(str, chr, enc);
      if (rslt < 0) return -1;
    }

    idx += inc;
  }

  return 0;
}

static ScmObj
scm_parser_parse_string(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj str = SCM_OBJ_INIT;
  ScmToken *token;
  int rslt;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &str);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  str = scm_fcd_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;
  if (token->str != NULL) {
    rslt = scm_parser_unescape_string(token, str, enc);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

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
    scm_fcd_error("Parser: invalid escpase sequence", 0);
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
      scm_fcd_error("Parser: invalid scalar value", 0);
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

  SCM_REFSTK_INIT_REG(&str);

  scm_assert(token != NULL);
  scm_assert(scm_fcd_string_p(str));
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
      rslt = scm_fcd_string_push(str, chr, enc);
      if (rslt < 0) return -1;
    }

    idx += inc;
  }

  return 0;
}

static ScmObj
scm_parser_parse_identifier(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  ScmObj str = SCM_OBJ_INIT, sym = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &str, &sym);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  str = scm_fcd_make_string_from_cstr(NULL, enc);
  if (scm_obj_null_p(str)) return SCM_OBJ_NULL;

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL)  return SCM_OBJ_NULL;

  if (token->str != NULL) {
    rslt = scm_parser_unescape_ident(token, str, enc);
    if (rslt < 0) return SCM_OBJ_NULL;
  }

  sym = scm_fcd_string_to_symbol(str);
  if (scm_obj_null_p(sym)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  return sym;
}

static int
scm_parser_extract_datum_label(ScmToken *token, ScmEncoding *enc, size_t *label)
{
  scm_assert(token != NULL);
  scm_assert(token->type == SCM_TOKEN_TYPE_REFERENCE_DECL
             || token->type == SCM_TOKEN_TYPE_REFERENCE_USE);
  scm_assert(enc != NULL);
  scm_assert(label != NULL);

  *label = 0;
  for (size_t i = 1; i < token->len - 1; i++) {
    int c = scm_enc_cnv_to_ascii(enc, &token->str[i]);
    size_t n;

    scm_assert('0' <= c && c <= '9');

    n = (size_t)(c - '0');
    if (*label > (SIZE_MAX - n) / 10) {
      scm_fcd_error("Parser: too big datum label number", 0);
      return -1;
    }

    *label = *label * 10 + n;
  }

  return 0;
}

static ScmObj
scm_parser_parse_reference_decl(ScmObj parser, ScmObj port, ScmEncoding *enc,
                                ScmParserRef *ref)
{
  ScmObj box = SCM_OBJ_INIT, datum = SCM_OBJ_INIT;
  ScmToken *token;
  size_t label;
  int r;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &box, &datum);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  r = scm_parser_extract_datum_label(token, enc, &label);
  if (r < 0) return SCM_OBJ_NULL;

  box = scm_parser_reg_dl_decl(parser, label);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  datum = scm_parser_parse_expression(parser, port, ref);
  if (scm_obj_null_p(datum)) return SCM_OBJ_NULL;

  scm_fcd_box_update(box, datum);
  return datum;
}

static ScmObj
scm_parser_parse_reference_use(ScmObj parser, ScmObj port, ScmEncoding *enc,
                               ScmParserRef *ref)
{
  ScmObj box = SCM_OBJ_INIT, datum = SCM_OBJ_INIT;
  ScmToken *token;
  size_t label;
  int r;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &box, &datum);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  if (ref == NULL) {
    scm_fcd_error("Parser: invalid datum label reference", 0);
    return SCM_OBJ_NULL;
  }

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  r = scm_parser_extract_datum_label(token, enc, &label);
  if (r < 0) return SCM_OBJ_NULL;

  box = scm_parser_labeled_datum(parser, label);
  if (scm_obj_null_p(box)) return SCM_OBJ_NULL;

  datum = scm_fcd_box_unbox(box);
  if (scm_fcd_eq_p(datum, DATUM_LABEL_INIT_OBJ)) {
    r = scm_parser_reg_dl_use(parser, ref);
    if (r < 0) return SCM_OBJ_NULL;
    datum = box;
  }

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  return datum;
}

static ScmObj
scm_parser_parse_numeric(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj num = SCM_OBJ_INIT;
  ScmToken *token;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  num = scm_num_make_from_parsedata(token->str, enc, &token->npd);
  if (scm_obj_null_p(num)) return SCM_OBJ_NULL;

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  return num;
}

static ScmObj
scm_parser_parse_bool(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmToken *token;
  scm_token_type_t type;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  type = token->type;
  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  if (type == SCM_TOKEN_TYPE_BOOL_TRUE)
    return SCM_TRUE_OBJ;
  else
    return SCM_FALSE_OBJ;
}

static int
scm_parser_parse_vector_aux(ScmObj parser, ScmObj port, ScmEncoding *enc,
                            ScmObj vec, int (*push_func)(ScmObj, ScmObj))
{
  ScmParserRef ref = SCM_PARSER_REF_INIT;
  ScmObj elm = SCM_OBJ_INIT;
  ScmToken *token;
  size_t count;
  int r;

  SCM_REFSTK_INIT_REG(&parser, &port, &vec,
                      &elm);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);
  scm_assert(scm_fcd_vector_p(vec) || scm_fcd_bytevector_p(vec));
  scm_assert(push_func != NULL);

  count = 0;
  while (1) {
    token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
    if (token == NULL) return -1;

    if (token->type == SCM_TOKEN_TYPE_RPAREN) {
      scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
      break;
    }

    if (count >= SSIZE_MAX) {
      if (scm_fcd_vector_p(vec))
        scm_fcd_error("Parser: vector too big", 0);
      else
        scm_fcd_error("Parser: bytevector too big", 0);
      return -1;
    }


    ref.referrer = vec;
    ref.pos = count;

    elm = scm_parser_parse_expression(parser, port,
                                      scm_fcd_vector_p(vec) ? &ref : NULL);
    if (scm_obj_null_p(elm)) return SCM_OBJ_NULL;
    if (scm_fcd_eof_object_p(elm)) {
      scm_fcd_read_error("Parser: unexpected eof", 0);
      return -1;
    }

    r = push_func(vec, elm);
    if (r < 0) return -1;

    count++;
  }

  return 0;
}

static ScmObj
scm_parser_parse_vector(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj vec = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &vec);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  vec = scm_fcd_make_vector(0, SCM_OBJ_NULL);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  r = scm_parser_parse_vector_aux(parser, port, enc, vec, scm_fcd_vector_push);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_vector_contract_redundant_space(vec);
  if (r < 0) return SCM_OBJ_NULL;

  return vec;
}

static int
scm_parser_check_bv_element(ScmObj elm)
{
  scm_sword_t v;
  int r;

  if (!scm_fcd_exact_integer_p(elm))
    goto invalid;

  r = scm_fcd_integer_to_sword(elm, &v);
  if (r < 0) return -1;

  if (v < 0 || 255 < v)
    goto invalid;

  return (int)v;

 invalid:
  scm_fcd_error("Parser: invalid bytevector element", elm);
  return -1;
}

static int
scm_parser_push_bv_element(ScmObj vec, ScmObj elm)
{
  int v;

  scm_assert(scm_fcd_bytevector_p(vec));
  scm_assert(scm_obj_not_null_p(elm));

  v = scm_parser_check_bv_element(elm);
  if (v < 0) return -1;

  return scm_fcd_bytevector_push(vec, v);
}

static ScmObj
scm_parser_parse_bytevector(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  ScmObj vec = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&port,
                      &vec);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  vec = scm_fcd_make_bytevector(0, 0);
  if (scm_obj_null_p(vec)) return SCM_OBJ_NULL;

  r = scm_parser_parse_vector_aux(parser, port, enc,
                                  vec, scm_parser_push_bv_element);
  if (r < 0) return SCM_OBJ_NULL;

  r = scm_fcd_bytevector_contract_redundant_space(vec);
  if (r < 0) return SCM_OBJ_NULL;

  return vec;
}

static ScmObj
scm_parser_parse_char_hex_scalar(const scm_char_t *str, size_t size,
                                 ScmEncoding *enc)
{
  long long hv;
  scm_char_t c;
  ssize_t w;
  int r;

  scm_assert(str != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(enc != NULL);

  r = scm_parser_parse_hex_scalar_value(str + 3, size - 3, enc, &hv);
  if (r < 0) return SCM_OBJ_NULL;

  w = scm_enc_cnv_from_scalar(enc, hv, &c);
  if (w < 0) {
    scm_fcd_error("Parser: invalid scalar value", 0);
    return SCM_OBJ_NULL;
  }

  return scm_fcd_make_char(&c, enc);
}

static ScmObj
scm_parser_parse_char(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  static const struct { const char *name; char chr; } chr_names[] = {
    { .name = "alarm",     .chr = '\a' },
    { .name = "backspace", .chr = '\b' },
    { .name = "delete",    .chr = 0x7f },
    { .name = "escape",    .chr = 0x1b },
    { .name = "newline",   .chr = '\n' },
    { .name = "null",      .chr = '\0' },
    { .name = "return",    .chr = '\r' },
    { .name = "space",     .chr = ' '  },
    { .name = "tab",       .chr = '\t' },
    { .name = NULL,        .chr = '\0' }
  };
  ScmToken *token;
  ScmObj chr = SCM_OBJ_INIT;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  if (token->len == 3) {
    chr = scm_fcd_make_char(token->str + 2, enc);
  }
  else if (token->len > 3 && chr_same_p(token->str[2], 'x', false, enc)) {
    chr = scm_parser_parse_char_hex_scalar(token->str, token->len, enc);
  }
  else {
    size_t i;
    for (i = 0; chr_names[i].name != NULL; i++) {
      if (str_same_p(token->str + 2, token->len - 2,
                     chr_names[i].name, false, enc)) {
        scm_char_t c;
        scm_enc_cnv_from_ascii(enc, chr_names[i].chr, &c);
        chr = scm_fcd_make_char(&c, enc);
        break;
      }
    }

    if (chr_names[i].name == NULL) {
      scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
      scm_fcd_error("Parser: unknown character name", 0);
      return SCM_OBJ_NULL;
    }
  }

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);

  return chr;
}

static ScmObj
scm_parser_parse_eof(ScmObj parser, ScmObj port, ScmEncoding *enc)
{
  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(enc != NULL);

  scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
  return SCM_EOF_OBJ;
}

static ScmObj
scm_parser_parse_expression(ScmObj parser, ScmObj port, ScmParserRef *ref)
{
  ScmObj rslt = SCM_OBJ_INIT;
  ScmToken *token;
  ScmEncoding *enc;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &rslt);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));

  enc = scm_fcd_port_internal_encoding(port);
  if (enc == NULL) return SCM_OBJ_NULL;

  token = scm_lexer_head_token(SCM_PARSER(parser)->lexer, port, enc);
  if (token == NULL) return SCM_OBJ_NULL;

  switch (token->type) {
  case SCM_TOKEN_TYPE_LPAREN:
    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
    rslt = scm_parser_parse_list(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_RPAREN:
    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
    scm_fcd_error("Parser: unexpeted right paren", 0);
    return SCM_OBJ_NULL;
    break;
  case SCM_TOKEN_TYPE_DOT:
    scm_lexer_shift_token(SCM_PARSER(parser)->lexer);
    scm_fcd_error("Parser: unexpeted dot", 0);
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
  case SCM_TOKEN_TYPE_IDENTIFIER_VLINE: /* fall through */
    rslt = scm_parser_parse_identifier(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_REFERENCE_DECL:
    rslt = scm_parser_parse_reference_decl(parser, port, enc, ref);
    break;
  case SCM_TOKEN_TYPE_REFERENCE_USE:
    rslt = scm_parser_parse_reference_use(parser, port, enc, ref);
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
    rslt = scm_parser_parse_bytevector(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_CHAR:
    rslt = scm_parser_parse_char(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_EOF:
    rslt = scm_parser_parse_eof(parser, port, enc);
    break;
  case SCM_TOKEN_TYPE_TOKENIZE_ERR:
    if (scm_lexer_error_type(SCM_PARSER(parser)->lexer)
        == SCM_LEXER_ERR_TYPE_UNEXPECTED_CHAR) {
      scm_fcd_error("Parser: unexpected char", 0);
    }
    else {
      scm_fcd_read_error("Parser: unexpected eof", 0);
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

static int
scm_parser_init_parse_expr(ScmObj parser)
{
  int r;

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  eary_fin(&SCM_PARSER(parser)->label_decl);
  eary_fin(&SCM_PARSER(parser)->label_use);

  r = eary_init(&SCM_PARSER(parser)->label_decl, sizeof(ScmObj), 0);
  if (r < 0) return -1;

  r = eary_init(&SCM_PARSER(parser)->label_use, sizeof(ScmObj), 0);
  if (r < 0) return -1;

  return 0;
}

int
scm_parser_initialize(ScmObj parser)
{
  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  SCM_PARSER(parser)->lexer = scm_lexer_new();
  if (SCM_PARSER(parser)->lexer == NULL) return -1;

  return 0;
}

void
scm_parser_finalize(ScmObj parser)
{
  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);

  if (SCM_PARSER(parser)->lexer != NULL) {
    scm_lexer_end(SCM_PARSER(parser)->lexer);
    SCM_PARSER(parser)->lexer = NULL;
  }

  eary_fin(&SCM_PARSER(parser)->label_decl);
  eary_fin(&SCM_PARSER(parser)->label_use);
}

ScmObj
scm_parser_new(scm_mem_type_t mtype)
{
  ScmObj parser;

  parser = scm_fcd_mem_alloc(&SCM_PARSER_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(parser)) return SCM_OBJ_NULL;

  if (scm_parser_initialize(parser) < 0)
    return SCM_OBJ_NULL;

  return parser;
}

ScmObj
scm_parser_parse(ScmObj parser, ScmObj port)
{
  ScmObj obj = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&parser, &port,
                      &obj);

  scm_assert_obj_type(parser, &SCM_PARSER_TYPE_INFO);
  scm_assert(scm_fcd_input_port_p(port));

  r = scm_parser_init_parse_expr(parser);
  if (r < 0) return SCM_OBJ_NULL;

  obj = scm_parser_parse_expression(parser, port, NULL);
  if (scm_obj_null_p(obj)) return SCM_OBJ_NULL;

  r = scm_parser_resolve_dl_use(parser);
  if (r < 0) return SCM_OBJ_NULL;

  return obj;
}

void
scm_parser_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_PARSER_TYPE_INFO);

  SCM_PARSER(obj)->lexer = NULL;
  eary_init(&SCM_PARSER(obj)->label_decl, 0, 0);
  eary_init(&SCM_PARSER(obj)->label_use, 0, 0);
}

void
scm_parser_gc_finalize(ScmObj obj)
{
  scm_parser_finalize(obj);
}

int
scm_parser_gc_accept(ScmObj obj, ScmGCRefHandler handler)
{
  ScmObj *itr;
  size_t idx;
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_PARSER_TYPE_INFO);
  scm_assert(handler != NULL);

  EARY_FOR_EACH(&SCM_PARSER(obj)->label_decl, idx, itr) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *itr);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  EARY_FOR_EACH(&SCM_PARSER(obj)->label_use, idx, itr) {
    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, *itr);
    if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
  }

  return rslt;
}
