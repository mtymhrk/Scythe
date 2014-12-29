#include <stdbool.h>
#include <stddef.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/fcd.h"
#include "scythe/port.h"
#include "scythe/string.h"
#include "scythe/vector.h"
#include "scythe/vm.h"
#include "scythe/parser.h"
#include "scythe/char.h"

/*******************************************************************/
/*  Port                                                           */
/*******************************************************************/

extern inline bool
scm_fcd_port_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_PORT_TYPE_INFO);
}

extern inline ScmObj
scm_fcd_port_P(ScmObj obj)
{
  return scm_fcd_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_file_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_file_port_p(obj));
}

extern inline bool
scm_fcd_string_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_string_port_p(obj));
}

extern inline bool
scm_fcd_input_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_input_port_p(obj));
}

extern inline ScmObj
scm_fcd_input_port_P(ScmObj obj)
{
  return scm_fcd_input_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_output_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_output_port_p(obj));
}

extern inline ScmObj
scm_fcd_output_port_P(ScmObj obj)
{
  return scm_fcd_output_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_textual_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_textual_port_p(obj));
}

extern inline ScmObj
scm_fcd_textual_port_P(ScmObj obj)
{
  return scm_fcd_textual_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_binary_port_p(ScmObj obj)
{
  return (scm_fcd_port_p(obj) && scm_port_binary_port_p(obj));
}

extern inline ScmObj
scm_fcd_binary_port_P(ScmObj obj)
{
  return scm_fcd_binary_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_input_port_open_p(ScmObj port)
{
  return (scm_fcd_input_port_p(port) && !scm_port_closed_p(port));
}

extern inline ScmObj
scm_fcd_input_port_open_P(ScmObj port)
{
  return scm_fcd_input_port_open_p(port) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

extern inline bool
scm_fcd_output_port_open_p(ScmObj port)
{
  return (scm_fcd_output_port_p(port) && !scm_port_closed_p(port));
}

ScmObj
scm_fcd_output_port_open_P(ScmObj port)
{
  return scm_fcd_output_port_open_p(port) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_open_input_fd(int fd, const char *enc)
{
  char ext_enc_name[64];

  scm_assert(fd >= 0);

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_fcd_error("failed to open input-port: "
                    "failed to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_fd(fd, "r", SCM_PORT_BUF_DEFAULT,
                          scm_fcd_system_encoding(), enc);
}

ScmObj
scm_fcd_open_binary_input_fd(int fd)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "rb", SCM_PORT_BUF_DEFAULT,
                          scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_open_output_fd(int fd, const char *enc)
{
  char ext_enc_name[64];

  scm_assert(fd >= 0);

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_fcd_error("failed to open output-port: "
                    "failed to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_fd(fd, "w", SCM_PORT_BUF_DEFAULT,
                          scm_fcd_system_encoding(), enc);
}

ScmObj
scm_fcd_open_binary_output_fd(int fd)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "wb", SCM_PORT_BUF_DEFAULT,
                          scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_open_input_file(const char *path, const char *enc)
{
  char ext_enc_name[64];

  scm_assert(path != NULL);

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_fcd_error("failed to open input-file-port: "
                     "failed to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_file(path, "r", SCM_PORT_BUF_DEFAULT,
                            0, scm_fcd_system_encoding(), enc);
}

ScmObj
scm_fcd_open_binary_input_file(const char *path)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "rb", SCM_PORT_BUF_DEFAULT,
                            0, scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_open_output_file(const char *path, const char *enc)
{
  char ext_enc_name[64];

  scm_assert(path != NULL);

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_fcd_error("failed to open output-file-port: "
                    "failed to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_file(path, "w", SCM_PORT_BUF_DEFAULT,
                            0644, scm_fcd_system_encoding(), enc);
}

ScmObj
scm_fcd_open_binary_output_file(const char *path)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "wb", SCM_PORT_BUF_DEFAULT,
                            0644, scm_fcd_system_encoding(), NULL);
}

int
scm_fcd_close_port(ScmObj port)
{
  scm_assert(scm_fcd_port_p(port));

  return scm_port_close(port);
}

int
scm_fcd_close_input_port(ScmObj port)
{
  scm_assert(scm_fcd_input_port_p(port));
  return scm_port_close(port);
}

int
scm_fcd_close_output_port(ScmObj port)
{
  scm_assert(scm_fcd_output_port_p(port));
  return scm_port_close(port);
}

ScmObj
scm_fcd_open_input_string_cstr(const char *str, const char *enc)
{
  char ext_enc_name[64];

  if (enc == NULL) {
    ssize_t r = scm_enc_locale_to_enc_name(ext_enc_name, sizeof(ext_enc_name));
    if (r < 0) {
      scm_fcd_error("failed to open input-string-port: "
                    "failed to get external encoding name", 0);
      return SCM_OBJ_NULL;
    }
    enc = ext_enc_name;;
  }

  return scm_port_open_string(str, (str == NULL)? 0 : strlen(str),
                              "r", scm_fcd_system_encoding(), enc);
}

ScmObj
scm_fcd_open_input_string(ScmObj str)
{
  scm_assert(scm_fcd_string_p(str));
  return scm_port_open_string(scm_string_content(str),
                              scm_string_bytesize(str),
                              "r",
                              scm_fcd_system_encoding(),
                              scm_enc_name(scm_string_encoding(str)));
}

ScmObj
scm_fcd_open_output_string(void)
{
  return scm_port_open_string(NULL, 0, "w", scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_get_output_string(ScmObj port)
{
  const void *p;
  ssize_t s;
  const char *enc_name;
  ScmEncoding *e;

  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_port_string_port_p(port));
  scm_assert(scm_fcd_textual_port_P(port));

  p = scm_port_string_buffer(port);
  if (p == NULL) return SCM_OBJ_NULL;

  s = scm_port_string_buffer_length(port);
  if (s < 0) return SCM_OBJ_NULL;

  enc_name = scm_port_external_enc(port);
  if (*enc_name == '\0') {
    e = scm_port_internal_enc(port);
  }
  else {
    e = scm_enc_find_enc(enc_name);
    if (e == NULL) {
      scm_fcd_error("failed to get output string: unsupported encoding", 0);
      return SCM_OBJ_NULL;
    }
  }

  return scm_fcd_make_string_from_bin(p, (size_t)s, e);
}

ScmObj
scm_fcd_open_input_bytevector_cbytes(const void *bytes, size_t size)
{
  return scm_port_open_string(bytes, (bytes == NULL) ? 0 : size,
                              "rb", scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_open_input_bytevector(ScmObj vec)
{
  scm_assert(scm_fcd_bytevector_p(vec));
  return scm_port_open_string(scm_bytevector_content(vec),
                              scm_bytevector_length(vec),
                              "rb",
                              scm_fcd_system_encoding(),
                              NULL);
}

ScmObj
scm_fcd_open_output_bytevector(void)
{
  return scm_port_open_string(NULL, 0, "wb", scm_fcd_system_encoding(), NULL);
}

ScmObj
scm_fcd_get_output_bytevector(ScmObj port)
{
  const void *p;
  ssize_t s;

  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_port_string_port_p(port));
  scm_assert(scm_fcd_binary_port_p(port));

  p = scm_port_string_buffer(port);
  if (p == NULL) return SCM_OBJ_NULL;

  s = scm_port_string_buffer_length(port);
  if (s < 0) return SCM_OBJ_NULL;

  return scm_bytevector_new_cbyte(SCM_MEM_HEAP, p, (size_t)s);
}

off_t
scm_fcd_port_seek(ScmObj port, off_t offset, int whence)
{
  scm_assert(scm_fcd_port_p(port));
  return scm_port_seek(port, offset, whence);
}

off_t
scm_fcd_port_pos(ScmObj port)
{
  scm_assert(scm_fcd_port_p(port));
  return scm_port_pos(port);
}

const char *
scm_fcd_port_encoding(ScmObj port)
{
  const char *enc_name;

  scm_assert(scm_fcd_port_p(port));

  enc_name = scm_port_external_enc(port);
  if (*enc_name == '\0') {
    ScmEncoding *e = scm_port_internal_enc(port);
    enc_name = scm_enc_name(e);
  }

  return enc_name;
}

ScmEncoding *
scm_fcd_port_internal_encoding(ScmObj port)
{
  scm_assert(scm_fcd_port_p(port));
  return scm_port_internal_enc(port);
}


/*******************************************************************/
/*  Input                                                          */
/*******************************************************************/

static ScmObj
default_input_port(bool textual, bool binary)
{
  ScmObj val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&val);

  r = scm_fcd_cached_global_var_ref(SCM_CACHED_GV_CURRENT_INPUT_PORT,
                                    SCM_CSETTER_L(val));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_fcd_error("unbound variable: current-input-port", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_fcd_parameter_p(val)) {
    val = scm_vm_parameter_value(scm_fcd_current_vm(), val);
    if (scm_obj_null_p(val)) return SCM_OBJ_NULL;
  }

  if (!scm_fcd_input_port_p(val)) {
    scm_fcd_error("failed to get default input-port"
                  "inuput-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  if (textual && !scm_fcd_textual_port_p(val)) {
    scm_fcd_error("failed to get default input-port: "
                  "textual-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }


  if (binary && !scm_fcd_binary_port_p(val)) {
    scm_fcd_error("failed to get default input-port: "
                  "binary-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  return val;
}

ScmObj
scm_fcd_read(ScmObj port)
{
  ScmObj obj = SCM_OBJ_INIT;
  ScmParser *parser;

  SCM_REFSTK_INIT_REG(&port,
                      &obj);

  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to read a S-exp: input-port closed", 1, port);
    return SCM_OBJ_NULL;
  }

  parser = scm_parser_new();
  if (parser == NULL) return SCM_OBJ_NULL;

  obj = scm_parser_parse_expression(parser, port);

  scm_parser_end(parser);

  return obj;
}

ssize_t
scm_fcd_read_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));
  scm_assert(chr != NULL);

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to read a character: input-port closed", 1, port);
    return -1;
  }

  return scm_port_read_char(port, chr);
}

ScmObj
scm_fcd_read_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  SCM_REFSTK_INIT_REG(&port);

  s = scm_fcd_read_cchr(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return scm_fcd_eof();
  else
    return scm_fcd_make_char(&chr, scm_port_internal_enc(port));
}

ssize_t
scm_fcd_peek_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));
  scm_assert(chr != NULL);

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to peek a character: input-port closed", 1, port);
    return -1;
  }

  return scm_port_peek_char(port, chr);
}

ScmObj
scm_fcd_peek_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  s = scm_fcd_peek_cchr(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return scm_fcd_eof();
  else
    return scm_fcd_make_char(&chr, scm_port_internal_enc(port));
}

ScmObj
scm_fcd_read_line(ScmObj port)
{
  ScmObj line = SCM_OBJ_INIT;
  ScmStringIO *sio;
  ssize_t ret;

  SCM_REFSTK_INIT_REG(&port, &line);

  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to read a line: input-port closed", 1, port);
    return SCM_OBJ_NULL;
  }

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  ret = scm_port_read_line(port, (ScmIO *)sio);

  if (ret < 0)
    line = SCM_OBJ_NULL;
  else if (ret == 0)
    line = scm_fcd_eof();
  else
    line = scm_fcd_make_string_from_bin(scm_stringio_buffer(sio),
                                        scm_stringio_length(sio),
                                        scm_port_internal_enc(port));

  scm_stringio_end(sio);

  return line;
}

int
scm_fcd_char_ready(ScmObj port, bool *rslt)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to verify a character is ready: input-port closed",
                  1, port);
    return -1;
  }

  return scm_port_char_ready(port, rslt);
}

ScmObj
scm_fcd_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  ret = scm_fcd_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_fcd_read_string(size_t n, ScmObj port)
{
  ScmObj  str = SCM_OBJ_INIT;
  ScmStringIO *sio;
  ssize_t nr;

  SCM_REFSTK_INIT_REG(&port,
                      &str);

  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to read string: input-port closed", 1, port);
    return SCM_OBJ_NULL;
  }

  if (n == 0)
    return scm_fcd_make_string_from_bin(NULL, 0, scm_port_internal_enc(port));

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  nr = scm_port_read_string(n, port, (ScmIO *)sio);
  if (nr < 0) goto end;

  if (scm_stringio_length(sio) == 0)
    str = SCM_EOF_OBJ;
  else
    str = scm_fcd_make_string_from_bin(scm_stringio_buffer(sio),
                                       scm_stringio_length(sio),
                                       scm_port_internal_enc(port));

 end:
  scm_stringio_end(sio);
  return str;
}

ssize_t
scm_fcd_read_cbytes(void *buf, size_t size, ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  if (scm_obj_null_p(port)) {
    port = default_input_port(false, true);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(scm_fcd_input_port_p(port));
  scm_assert(scm_fcd_binary_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to read byte sequences: input-port closed", 1, port);
    return -1;
  }

  return scm_port_read_bytes(port, buf, size);
}


/*******************************************************************/
/*  Output                                                         */
/*******************************************************************/

static ScmObj
default_output_port(bool textual, bool binary)
{
  ScmObj val = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&val);

  r = scm_fcd_cached_global_var_ref(SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
                                    SCM_CSETTER_L(val));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_fcd_error("unbound variable: current-output-port", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_fcd_parameter_p(val)) {
    val = scm_vm_parameter_value(scm_fcd_current_vm(), val);
    if (scm_obj_null_p(val)) return SCM_OBJ_NULL;
  }

  if (!scm_fcd_output_port_p(val)) {
    scm_fcd_error("failed to get default output-port"
                  "outuput-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  if (binary && !scm_fcd_binary_port_p(val)) {
      scm_fcd_error("failed to get default output-port: "
                    "binary-port required, but got", 1, val);
      return SCM_OBJ_NULL;
  }

  if (textual && !scm_fcd_textual_port_p(val)) {
      scm_fcd_error("failed to get default output-port: "
                    "textual-port required, but got", 1, val);
      return SCM_OBJ_NULL;
  }

  return val;
}

static bool
ws_interesting_p(ScmObj obj)
{
  return (scm_fcd_pair_p(obj)
          || scm_fcd_string_p(obj)
          || scm_fcd_vector_p(obj)
          || scm_fcd_bytevector_p(obj));
}

static ScmObj
ws_acons(ScmObj car, ScmObj cdr, ScmObj alist)
{
  ScmObj pair = SCM_OBJ_INIT;

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));
  scm_assert(scm_fcd_pair_p(alist) || scm_fcd_nil_p(alist));

  SCM_REFSTK_INIT_REG(&car, &cdr, &alist,
                      &pair);

  pair = scm_fcd_cons(car, cdr);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  return scm_fcd_cons(pair, alist);
}

static ScmObj
ws_scan_internal(ScmObj obj, ScmObj alist)
{
  ScmObj pair = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  scm_assert(scm_fcd_pair_p(alist) || scm_fcd_nil_p(alist));

  SCM_REFSTK_INIT_REG(&obj, &alist,
                      &pair, &elm);

  if (!ws_interesting_p(obj))
    return alist;

  pair = scm_fcd_assq(obj, alist);
  if (scm_fcd_true_p(pair)) {
    if (scm_fcd_true_p(scm_fcd_cdr(pair)))
      return alist;
    else
      return ws_acons(obj, SCM_TRUE_OBJ, alist);
  }

  alist = ws_acons(obj, SCM_FALSE_OBJ, alist);
  if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

  if (scm_fcd_pair_p(obj)) {
    elm = scm_fcd_cdr(obj);
    alist = ws_scan_internal(elm, alist);
    if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

    elm = scm_fcd_car(obj);
    return ws_scan_internal(elm, alist);
  }
  else if (scm_fcd_vector_p(obj)) {
    size_t len = scm_fcd_vector_length(obj);
    for (size_t i = 0; i < len; i++) {
      elm = scm_fcd_vector_ref(obj, i);
      alist = ws_scan_internal(elm, alist);
      if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;
    }
    return alist;
  }
  else {
    return alist;
  }
}

static ScmObj
ws_scan(ScmObj obj)
{
  ScmObj alist = SCM_OBJ_INIT;

  alist = ws_scan_internal(obj, SCM_NIL_OBJ);
  if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

  return ws_acons(SCM_EOF_OBJ, SCM_FIXNUM_NN_1, alist);
}

static int
ws_extract_counter(ScmObj alist, scm_sword_t *n)
{
  ScmObj num = SCM_OBJ_INIT;

  scm_assert(scm_fcd_pair_p(alist) || scm_fcd_nil_p(alist));
  scm_assert(n != NULL);

  num = scm_fcd_cxr(alist, "da");
  return scm_fcd_integer_to_sword(num, n);
}

static int
ws_print_shared_dec(ScmObj obj, ScmObj port, int kind,
                    ScmObjPrintHandler handler)
{
  ScmObj alist = SCM_OBJ_INIT, num = SCM_OBJ_INIT;
  char str[64];
  scm_sword_t n;
  int r;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &alist);

  alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
  scm_assert(scm_fcd_pair_p(alist) || scm_fcd_nil_p(alist));

  r = ws_extract_counter(alist, &n);
  if (r < 0) return -1;

  if (n >= SCM_SWORD_MAX) {
    scm_fcd_error("failed to print shared structure object: "
                  "too many shared object", 0);
    return -1;
  }

  snprintf(str, sizeof(str), "#%lu=", ++n);
  r = scm_fcd_write_cstr(str, SCM_ENC_SRC, port);
  if (r < 0) return -1;

  num = scm_fcd_make_number_from_sword(n);
  if (scm_obj_null_p(num)) return -1;

  alist = ws_acons(obj, num, alist);
  if (scm_obj_null_p(alist)) return -1;

  SCM_OBJ_PRINT_HANDLER_BODY(handler)->val = alist;

  return scm_obj_call_print_func(obj, port, kind, handler);
}

static int
ws_print_shared_use(ScmObj num, ScmObj port)
{
  char str[64];
  size_t n;
  int r;

  scm_assert(scm_fcd_integer_p(num));

  r = scm_fcd_integer_to_size_t(num, &n);
  if (r < 0) return -1;

  snprintf(str, sizeof(str), "#%lu#", n);
  r = scm_fcd_write_cstr(str, SCM_ENC_SRC, port);
  if (r < 0) return -1;

  return 0;
}

static int
obj_print_handler_print_obj_shared(ScmObjPrintHandler handler,
                                   ScmObj obj, ScmObj port, int kind)
{
  ScmObj alist = SCM_OBJ_INIT, val = SCM_OBJ_INIT;

  alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
  if (ws_interesting_p(obj)) {
    val = scm_fcd_cdr(scm_fcd_assq(obj, alist));
    if (scm_fcd_number_p(val))
      return ws_print_shared_use(val, port);
    else if (scm_fcd_true_p(val))
      return ws_print_shared_dec(obj, port, kind, handler);
  }

  return scm_obj_call_print_func(obj, port, kind, handler);
}

static int
obj_print_handler_print_obj_simple(ScmObjPrintHandler handler,
                                   ScmObj obj, ScmObj port, int kind)
{
  return scm_obj_call_print_func(obj, port, kind, handler);
}

static int
obj_print_handler_print_obj_display(ScmObjPrintHandler handler,
                                    ScmObj obj, ScmObj port, int kind)
{
  return scm_obj_call_print_func(obj, port, kind, handler);
}

static int
obj_print_handler_print_list(ScmObjPrintHandler handler,
                             ScmObj obj, ScmObj port, int kind)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj alist = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &lst, &car, &cdr,
                      &alist, &val);

  scm_assert(scm_fcd_pair_p(obj));

  rslt = scm_fcd_write_cstr("(", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  lst = obj;
  while (1) {
    car = scm_fcd_car(lst);
    cdr = scm_fcd_cdr(lst);

    rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler, car, port, kind);
    if (rslt < 0) return -1;

    if (scm_fcd_nil_p(cdr)) {
      break;
    }
    else if (!scm_fcd_pair_p(cdr)) {
      rslt = scm_fcd_write_cstr(" . ", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler, cdr, port, kind);
      if (rslt < 0) return -1;
      break;
    }
    else if (kind == SCM_OBJ_PRINT_SHARED && ws_interesting_p(cdr)) {
      alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
      val = scm_fcd_cdr(scm_fcd_assq(cdr, alist));
      if (scm_fcd_true_p(val)) {
        rslt = scm_fcd_write_cstr(" . ", SCM_ENC_SRC, port);
        if (rslt < 0) return -1;

        if (scm_fcd_number_p(val))
          rslt = ws_print_shared_use(val, port);
        else
          rslt = ws_print_shared_dec(cdr, port, kind, handler);

        if (rslt < 0) return -1;
        break;
      }
    }

    rslt = scm_fcd_write_cstr(" ", SCM_ENC_SRC, port);
    if (rslt < 0) return -1;

    lst = cdr;
  }

  rslt = scm_fcd_write_cstr(")", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write_shared(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .print_obj = obj_print_handler_print_obj_shared,
    .print_list = obj_print_handler_print_list,
    .val = SCM_OBJ_NULL
  };
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &handler.val);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a object: output-port closed", 1, port);
    return -1;
  }

  handler.val = ws_scan(obj);
  if (scm_obj_null_p(handler.val)) return -1;

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_SHARED);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write_simple(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .print_obj = obj_print_handler_print_obj_simple,
    .print_list = obj_print_handler_print_list,
    .val = SCM_OBJ_NULL
  };
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a object: output-port closed", 1, port);
    return -1;
  }

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_SIMPLE);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write(ScmObj obj, ScmObj port)
{
  /* TODO: write me */
  return scm_fcd_write_simple(obj, port);
}

int
scm_fcd_display(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .print_obj = obj_print_handler_print_obj_display,
    .print_list = obj_print_handler_print_list,
    .val = SCM_OBJ_NULL
  };
  int rslt;

  /* TODO: obj の構造が循環していても無限ループにならないようにする */

  SCM_REFSTK_INIT_REG(&obj, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a object: output-port closed", 1, port);
    return -1;
  }

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_DISPLAY);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_newline(ScmObj port)
{
  ScmEncoding *enc;
  scm_char_t nl;
  ssize_t rslt;

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a newline: output-port closed", 1, port);
    return -1;
  }

  enc = scm_port_internal_enc(port);
  scm_enc_cnv_from_ascii(enc, '\n', &nl);
  rslt = scm_port_write_char(port, nl);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write_cchr(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  ScmObj c = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  scm_assert(enc != NULL);

  /* TODO: scm_fcd_write_char を使わない (char オブジェクトを生成しない) */

  c = scm_fcd_make_char(&chr, enc);
  if (scm_obj_null_p(c)) return -1;

  r = scm_fcd_write_char(c, port);
  if (r < 0) return -1;

  return 0;
}

int
scm_fcd_write_char(ScmObj chr, ScmObj port)
{
  ScmEncoding *p_enc, *c_enc;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&chr, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_char_p(chr));
  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a character: output-port closed", 1, port);
    return -1;
  }

  p_enc = scm_port_internal_enc(port);
  c_enc = scm_char_encoding(chr);

  if (p_enc != c_enc) {
    chr = scm_char_encode(chr, p_enc);
    if (scm_obj_null_p(chr)) return -1;
  }

  rslt = scm_port_write_char(port, scm_char_value(chr));
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write_cstr(const char *str, ScmEncoding *enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &s);

  scm_assert(enc != NULL);

  /* TODO: scm_fcd_write_string を使わない (string オブジェクトを生成しない) */

  s = scm_fcd_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return -1;

  return scm_fcd_write_string(s, port, -1, -1);
}

int
scm_fcd_write_string(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  ScmEncoding *p_enc;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&str, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_string_p(str));
  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_textual_port_p(port));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write a string: output-port closed", 1, port);
    return -1;
  }

  if (start < 0)
    start = 0;

  if (end < 0)
    end = (ssize_t)scm_string_length(str);

  if (start != 0 || end != (ssize_t)scm_string_length(str)) {
    str = scm_string_substr(str, (size_t)start, (size_t)(end - start));
    if (scm_obj_null_p(str)) return -1;
  }

  p_enc = scm_port_internal_enc(port);
  if (p_enc != scm_string_encoding(str)) {
    str = scm_string_encode(str, p_enc);
    if (scm_obj_null_p(str)) return -1;
  }

  rslt = scm_port_write_bytes(port,
                              scm_string_content(str),
                              scm_string_bytesize(str));
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_write_cbytes(const void *bytes, size_t size, ScmObj port)
{
  ssize_t rslt;

  if (scm_obj_null_p(port)) {
    port = default_output_port(false, true);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_output_port_p(port));
  scm_assert(scm_fcd_binary_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to write byte sequences: output-port closed",
                  1, port);
    return -1;
  }

  if (bytes == NULL || size == 0)
    return 0;

  rslt = scm_port_write_bytes(port, bytes, size);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_fcd_flush_output_port(ScmObj port)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(false, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_fcd_output_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_fcd_error("failed to flush output-port: output-port closed", 1, port);
    return -1;
  }

  rslt = scm_port_flush(port);
  if (rslt < 0) return -1;

  return 0;
}
