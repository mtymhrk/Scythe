#include <arpa/inet.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/earray.h"
#include "scythe/marshal.h"
#include "scythe/chashtbl.h"
#include "scythe/vminst.h"

#define SCM_MARSHAL_ALIGN 4
#define SCM_MARSHAL_BUFFER_INIT_SIZE 256

enum { SCM_MARSHAL_NUM_TYPE_INT, SCM_MARSHAL_NUM_TYPE_STR };

typedef struct ScmMarshalObjHeaderRec {
  size_t size;
  uint8_t flags;
  size_t nsz;
  const char *name;
  ScmTypeInfo *info;
} ScmMarshalObjHeader;

#define SCM_MARSHAL_OBJ_FLG_SHARED 0x01

#define SCM_MARSHAL_SIZE_SIZE sizeof(uint32_t)
#define SCM_MARSHAL_OFFSET_SIZE sizeof(uint32_t)

typedef struct ScmMarshalObjStatRec {
  ScmObj obj;
  size_t pos;
  bool registered;
} ScmMarshalObjStat;

typedef struct ScmUnmarshalObjStatRec {
  ScmObj obj;
  size_t pos;
  bool shared;
  size_t idx;
  bool registered;
} ScmUnmarshalObjStat;

static ssize_t scm_marshal_obj(ScmObj obj, ScmMarshalObjStat *container,
                               ScmObj marshal);
static ScmObj scm_unmarshal_obj(ScmUnmarshalObjStat *container,
                                ScmObj unmarshal);


static inline size_t
scm_marshal_align(size_t x)
{
  return ((x % SCM_MARSHAL_ALIGN == 0) ?
          x : (x / SCM_MARSHAL_ALIGN) * SCM_MARSHAL_ALIGN + SCM_MARSHAL_ALIGN);
}

static void
scm_marshal_error(ScmObj obj, const char *fmt, ...)
{
  char msg[256];
  va_list arg;

  scm_assert(scm_obj_null_p(obj)
             || scm_obj_type(obj) == &SCM_MARSHAL_TYPE_INFO
             || scm_obj_type(obj) == &SCM_UNMARSHAL_TYPE_INFO);

  if (scm_obj_null_p(obj))
    snprintf(msg, sizeof(msg), "marshal/unmarshal error: %s", fmt);
  else if (scm_obj_type(obj) == &SCM_MARSHAL_TYPE_INFO)
    snprintf(msg, sizeof(msg), "marshal error: %s", fmt);
  else
    snprintf(msg, sizeof(msg), "unmarshal error: %s", fmt);
  msg[sizeof(msg) - 1] = '\0';

  va_start(arg, fmt);
  vsnprintf(msg, sizeof(msg), fmt, arg);
  va_end(arg);
  msg[sizeof(msg) - 1] = '\0';

  scm_fcd_error(msg, 0);
}


/****************************************************************************/
/* ScmMarshalBuffer                                                         */
/****************************************************************************/

static int
scm_marshal_buffer_init(ScmMarshalBuffer *buffer, const void *data, size_t size)
{
  scm_assert(buffer != NULL);

  buffer->type = '\0';
  if (data == NULL) {
    int r = eary_init(&buffer->buf.m.data,
                      sizeof(scm_byte_t), size);
    if (r < 0) return -1;
    buffer->type = 'm';
  }
  else {
    buffer->buf.u.data = data;
    buffer->buf.u.size = size;
    buffer->type = 'u';
  }
  buffer->cur = 0;
  return 0;
}

static void
scm_marshal_buffer_fin(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);

  if (buffer->type == 'm')
    eary_fin(&buffer->buf.m.data);
}

static inline void *
scm_marshal_buffer_head(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);
  scm_assert(buffer->type == 'm');
  return EARY_HEAD(&buffer->buf.m.data);
}

static inline const void *
scm_marshal_buffer_head_const(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);
  if (buffer->type == 'm')
    return EARY_HEAD(&buffer->buf.m.data);
  else
    return buffer->buf.u.data;
}

static inline size_t
scm_marshal_buffer_size(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);
  if (buffer->type == 'm')
    return EARY_SIZE(&buffer->buf.m.data);
  else
    return buffer->buf.u.size;
}

static inline size_t
scm_marshal_buffer_pos(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);
  return buffer->cur;
}

static inline void *
scm_marshal_buffer_pos2ptr(ScmMarshalBuffer *buffer, size_t pos)
{
  scm_assert(buffer != NULL);
  scm_assert(buffer->type == 'm');
  return (scm_byte_t *)scm_marshal_buffer_head(buffer) + pos;
}

static inline const void *
scm_marshal_buffer_pos2ptr_const(ScmMarshalBuffer *buffer, size_t pos)
{
  scm_assert(buffer != NULL);
  return (const scm_byte_t *)scm_marshal_buffer_head_const(buffer) + pos;
}

static inline void *
scm_marshal_buffer_ptr(ScmMarshalBuffer *buffer)
{
  return scm_marshal_buffer_pos2ptr(buffer, buffer->cur);
}

static inline const void *
scm_marshal_buffer_ptr_const(ScmMarshalBuffer *buffer)
{
  return scm_marshal_buffer_pos2ptr_const(buffer, buffer->cur);
}

static inline void
scm_marshal_buffer_shift(ScmMarshalBuffer *buffer, size_t pos)
{
  scm_assert(buffer != NULL);
  scm_assert(pos <= scm_marshal_buffer_size(buffer) - buffer->cur);
  buffer->cur += pos;
}

static inline void
scm_marshal_buffer_seek(ScmMarshalBuffer *buffer, size_t pos)
{
  scm_assert(buffer != NULL);
  scm_assert(pos <= scm_marshal_buffer_size(buffer));
  buffer->cur = pos;
}

static inline int
scm_marshal_buffer_prepare_to_push(ScmMarshalBuffer *buffer, size_t size)
{
  int err;
  scm_assert(buffer->type == 'm');
  scm_assert(size <= SSIZE_MAX - buffer->cur);
  EARY_SET(&buffer->buf.m.data, scm_byte_t, buffer->cur + size - 1, 0, err);
  return err;
}

static inline void *
scm_marshal_buffer_chuck(ScmMarshalBuffer *buffer)
{
  scm_assert(buffer != NULL);
  scm_assert(buffer->type == 'm');
  buffer->type = '\0';
  return eary_chuck_ary(&buffer->buf.m.data);
}

static inline const void *
scm_marshal_buffer_chuck_const(ScmMarshalBuffer *buffer)
{
  const void *x;

  scm_assert(buffer != NULL);

  x = NULL;
  if (buffer->type == 'm')
    x = eary_chuck_ary(&buffer->buf.m.data);
  else if (buffer->type == 'u')
    x = buffer->buf.u.data;

  buffer->type = '\0';
  return x;
}


/****************************************************************************/
/* input/output                                                             */
/****************************************************************************/

static int
scm_push_padding_for_alignment(ScmMarshalBuffer *output)
{
  size_t pad, pos;
  int r;

  scm_assert(output != NULL);

  pos = scm_marshal_align(scm_marshal_buffer_pos(output));
  pad = pos - scm_marshal_buffer_pos(output);

  if (pad == 0)
    return 0;

  r = scm_marshal_buffer_prepare_to_push(output, pad);
  if (r < 0) return -1;

  memset(scm_marshal_buffer_ptr(output), 0, pad);
  scm_marshal_buffer_shift(output, pad);
  return 0;
}

static int
scm_skip_padding_for_alignment(ScmMarshalBuffer *data)
{
  size_t pad, pos;

  scm_assert(data != NULL);

  pos = scm_marshal_align(scm_marshal_buffer_pos(data));
  pad = pos - scm_marshal_buffer_pos(data);
  if (pad == 0) {
    return 0;
  }
  else {
    scm_marshal_buffer_shift(data, pad);
    return 0;
  }
}

static int
scm_push_uint8(ScmMarshalBuffer *output, uint8_t val)
{
  int r;

  scm_assert(output != NULL);

  r = scm_marshal_buffer_prepare_to_push(output, sizeof(val));
  if (r < 0) return -1;

  *(typeof(val) *)scm_marshal_buffer_ptr(output) = val;
  scm_marshal_buffer_shift(output, sizeof(val));

  return 0;
}

static int
scm_write_uint8(ScmMarshalBuffer *output, size_t pos, uint8_t val)
{
  scm_assert(output != NULL);

  if (pos > scm_marshal_buffer_size(output) - sizeof(val)) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to write: uint8: out of range");
    return -1;
  }

  *(typeof(val) *)scm_marshal_buffer_pos2ptr(output, pos) = val;

  return 0;
}

static int
scm_read_uint8(ScmMarshalBuffer *input, uint8_t *val)
{
  scm_assert(input != NULL);
  scm_assert(val != NULL);

  if (scm_marshal_buffer_pos(input)
      > scm_marshal_buffer_size(input) - sizeof(*val)) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: uint8: out of range");
    return -1;
  }

  *val = *(const typeof(*val) *)scm_marshal_buffer_ptr_const(input);
  scm_marshal_buffer_shift(input, sizeof(*val));

  return 0;
}

static int
scm_peek_uint8(ScmMarshalBuffer *input, size_t pos, uint8_t *val)
{
  scm_assert(input != NULL);
  scm_assert(val != NULL);

  if (pos > scm_marshal_buffer_size(input) - sizeof(*val)) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: uint8: out of range");
    return -1;
  }

  *val = *(const typeof(*val) *)scm_marshal_buffer_pos2ptr_const(input, pos);
  return 0;
}

/* static int */
/* scm_push_uint16(ScmMarshalBuffer *output, uint16_t val) */
/* { */
/*   int r; */

/*   scm_assert(output != NULL); */
/*   scm_assert((uintptr_t)scm_marshal_buffer_ptr(output) % sizeof(val) == 0); */

/*   r = scm_marshal_buffer_prepare_to_push(output, sizeof(val)); */
/*   if (r < 0) return -1; */

/*   *(typeof(val) *)scm_marshal_buffer_ptr(output) = htons(val); */
/*   scm_marshal_buffer_shift(output, sizeof(val)); */

/*   return 0; */
/* } */

/* static int */
/* scm_write_uint16(ScmMarshalBuffer *output, size_t pos, uint16_t val) */
/* { */
/*   scm_assert(output != NULL); */

/*   if (pos > scm_marshal_buffer_size(output) - sizeof(val)) { */
/*     scm_capi_error("marshal error", 0); /\* TODO: write error message *\/ */
/*     return -1; */
/*   } */

/*   scm_assert((uintptr_t)scm_marshal_buffer_pos2ptr(output, pos) % sizeof(val) == 0); */

/*   *(typeof(val) *)scm_marshal_buffer_pos2ptr(output, pos) = htons(val); */

/*   return 0; */
/* } */


/* static int */
/* scm_read_uint16(ScmMarshalBuffer *input, uint16_t *val) */
/* { */
/*   scm_assert(input != NULL); */
/*   scm_assert(val != NULL); */
/*   scm_assert((uintptr_t)scm_marshal_buffer_ptr(input) % sizeof(*val) == 0); */

/*   if (scm_marshal_buffer_pos(input) */
/*       > scm_marshal_buffer_size(input) - sizeof(*val)) { */
/*     scm_capi_error("unmarshal error", 0); /\* TODO: write error message *\/ */
/*     return -1; */
/*   } */

/*   *val = htons(*(typeof(val))scm_marshal_buffer_ptr(input)); */
/*   scm_marshal_buffer_shift(input, sizeof(*val)); */

/*   return 0; */
/* } */

static int
scm_push_uint32(ScmMarshalBuffer *output, uint32_t val)
{
  int r;

  scm_assert(output != NULL);
  scm_assert((uintptr_t)scm_marshal_buffer_ptr(output) % sizeof(val) == 0);

  r = scm_marshal_buffer_prepare_to_push(output, sizeof(val));
  if (r < 0) return -1;

  *(typeof(val) *)scm_marshal_buffer_ptr(output) = htonl(val);
  scm_marshal_buffer_shift(output, sizeof(val));

  return 0;
}

static int
scm_write_uint32(ScmMarshalBuffer *output, size_t pos, uint32_t val)
{
  scm_assert(output != NULL);

  if (pos > scm_marshal_buffer_size(output) - sizeof(val)) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to write: uint32: out of range");
    return -1;
  }

  scm_assert((uintptr_t)scm_marshal_buffer_pos2ptr(output, pos) % sizeof(val) == 0);

  *(typeof(val) *)scm_marshal_buffer_pos2ptr(output, pos) = htonl(val);

  return 0;
}

static int
scm_read_uint32(ScmMarshalBuffer *input, uint32_t *val)
{
  scm_assert(input != NULL);
  scm_assert(val != NULL);
  scm_assert((uintptr_t)scm_marshal_buffer_ptr_const(input) % sizeof(*val)
             == 0);

  if (scm_marshal_buffer_pos(input)
      > scm_marshal_buffer_size(input) - sizeof(*val)) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: uint32: out of range");
    return -1;
  }

  *val = htonl(*(const typeof(*val) *)scm_marshal_buffer_ptr_const(input));
  scm_marshal_buffer_shift(input, sizeof(*val));

  return 0;
}

static int
scm_push_small_string(ScmMarshalBuffer *output, const char *str)
{
  size_t size;
  int r;

  scm_assert(output != NULL);
  scm_assert(str != NULL);

  size = strlen(str) + 1;
  if (size > UINT8_MAX) {
    scm_marshal_error(SCM_OBJ_NULL,
                      "failed to write: too long string: `%s'", str);
    return -1;
  }

  r = scm_push_uint8(output, (uint8_t)size);
  if (r < 0) return -1;

  r = scm_marshal_buffer_prepare_to_push(output, size);
  if (r < 0) return -1;

  memcpy(scm_marshal_buffer_ptr(output), str, size);
  scm_marshal_buffer_shift(output, size);

  return 0;
}

static int
scm_read_small_string(ScmMarshalBuffer *input, const char **ptr, size_t *size)
{
  uint8_t x;
  int r;

  scm_assert(input != NULL);
  scm_assert(ptr != NULL);
  scm_assert(size != NULL);

  r = scm_read_uint8(input, &x);
  if (r < 0) return -1;
  *size = (size_t)x;

  if (scm_marshal_buffer_pos(input) > scm_marshal_buffer_size(input) - *size) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: invalid string data");
    return -1;
  }

  *ptr = (const char *)scm_marshal_buffer_ptr_const(input);
  scm_marshal_buffer_shift(input, *size);

  return 0;
}

static int
scm_push_encoding(ScmMarshalBuffer *output, ScmEncoding *enc)
{
  scm_assert(output != NULL);
  scm_assert(enc != NULL);

  return scm_push_small_string(output, scm_enc_name(enc));
}

static int
scm_read_encoding(ScmMarshalBuffer *input, ScmEncoding **enc)
{
  const char *name;
  size_t size;
  int r;

  scm_assert(input != NULL);
  scm_assert(enc != NULL);

  r = scm_read_small_string(input, &name, &size);
  if (r < 0) return -1;

  *enc = scm_enc_find_enc(name);
  if (*enc == NULL) {
    scm_marshal_error(SCM_OBJ_NULL, "unsupported encoding: %s", name);
    return -1;
  }

  return 0;
}

static char *
scm_alloc_space(ScmMarshalBuffer *output, size_t size)
{
  char *p;
  int r;

  scm_assert(output != NULL);

  r = scm_marshal_buffer_prepare_to_push(output, size);
  if (r < 0) return NULL;

  p = scm_marshal_buffer_ptr(output);
  scm_marshal_buffer_shift(output, size);
  return p;
}

static int
scm_push_bytes(ScmMarshalBuffer *output, const void *ptr, size_t size)
{
  int r;

  scm_assert(output != NULL);

  r = scm_marshal_buffer_prepare_to_push(output, size);
  if (r < 0) return -1;

  memcpy(scm_marshal_buffer_ptr(output), ptr, size);
  scm_marshal_buffer_shift(output, size);

  return 0;
}

static int
scm_read_bytes(ScmMarshalBuffer *input, const void **ptr, size_t size)
{
  scm_assert(input != NULL);
  scm_assert(ptr != NULL);

  if (scm_marshal_buffer_pos(input)
      > scm_marshal_buffer_size(input) - size) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: byte seq: out of range");
    return -1;
  }

  *ptr = scm_marshal_buffer_ptr_const(input);
  scm_marshal_buffer_shift(input, size);

  return 0;
}

static int
scm_push_size(ScmMarshalBuffer *output, size_t size)
{
  scm_assert(output != NULL);

  if (size > UINT32_MAX) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to write: size: overflow");
    return -1;
  }

  return scm_push_uint32(output, (uint32_t)size);
}

static int
scm_write_size(ScmMarshalBuffer *output, size_t pos, size_t size)
{
  scm_assert(output != NULL);

  if (size > UINT32_MAX) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to write: size: overflow");
    return -1;
  }

  return scm_write_uint32(output, pos, (uint32_t)size);
}

static int
scm_read_size(ScmMarshalBuffer *input, size_t *size)
{
  uint32_t x;
  int r;

  scm_assert(input != NULL );
  scm_assert(size != NULL);

  r = scm_read_uint32(input, &x);
  if (r < 0) return -1;

#if UINT32_MAX > SIZE_MAX
  if (x > SIZE_MAX) {
    scm_marshal_error(SCM_OBJ_NULL, "failed to read: size: overflow");
    return -1;
  }
#endif  /* UINT32_MAX > SIZE_MAX */

  *size = (size_t)x;
  return 0;
}

static inline int
scm_push_offset(ScmMarshalBuffer *output, size_t offset)
{
  return scm_push_size(output, offset);
}

static inline int
scm_write_offset(ScmMarshalBuffer *output, size_t pos, size_t offset)
{
  return scm_write_size(output, pos, offset);
}

static inline int
scm_read_offset(ScmMarshalBuffer *input, size_t *offset)
{
  return scm_read_size(input, offset);
}

static int
scm_push_obj_header(ScmMarshalBuffer *output, const ScmMarshalObjHeader *header)
{
  int r;

  scm_assert(output);
  scm_assert(header->info != NULL);

  r = scm_push_size(output, header->size);
  if (r < 0) return -1;

  r = scm_push_uint8(output, header->flags);
  if (r < 0) return -1;

  r = scm_push_small_string(output, scm_type_info_name(header->info));
  if (r < 0) return -1;

  return scm_push_padding_for_alignment(output);
}

static int
scm_write_obj_header_size(ScmMarshalBuffer *output, size_t pos, size_t size)
{
  return scm_write_size(output, pos, size);
}

static int
scm_set_obj_header_flag(ScmMarshalBuffer *output, size_t pos, uint8_t flags)
{
  uint8_t flg;
  int r;

  r = scm_peek_uint8(output, pos + SCM_MARSHAL_SIZE_SIZE, &flg);
  if (r < 0) return -1;

  return scm_write_uint8(output, pos + SCM_MARSHAL_SIZE_SIZE, flg | flags);
}

static int
scm_read_obj_header(ScmMarshalBuffer *input, ScmMarshalObjHeader *header)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(header != NULL);

  r = scm_read_size(input, &header->size);
  if (r < 0) return -1;

  r = scm_read_uint8(input, &header->flags);
  if (r < 0) return -1;

  r = scm_read_small_string(input, &header->name, &header->nsz);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_header(ScmMarshalBuffer *output, const ScmMarshalHeader *header)
{
  int r;

  scm_assert(output != NULL);
  scm_assert(header != NULL);

  r = scm_push_size(output, header->size);
  if (r < 0) return -1;

  r = scm_push_size(output, (uint32_t)header->nr_obj);
  if (r < 0) return -1;

  r = scm_push_size(output, (uint32_t)header->nr_shared);
  if (r < 0) return -1;

  r = scm_push_offset(output, (uint32_t)header->obj_pos);
  if (r < 0) return -1;

  r = scm_push_offset(output, (uint32_t)header->shared_pos);
  if (r < 0) return -1;

  r = scm_push_encoding(output, header->enc);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_write_header(ScmMarshalBuffer *output, size_t pos,
                 const ScmMarshalHeader *header)
{
  int r;

  scm_assert(output != NULL);
  scm_assert(header != NULL);

  r = scm_write_size(output, pos, header->size);
  if (r < 0) return -1;

  r = scm_write_size(output,
                     pos + SCM_MARSHAL_SIZE_SIZE, header->nr_obj);
  if (r < 0) return -1;

  r = scm_write_size(output,
                     pos + SCM_MARSHAL_SIZE_SIZE * 2, header->nr_shared);
  if (r < 0) return -1;

  r = scm_write_offset(output,
                       pos + SCM_MARSHAL_SIZE_SIZE * 3, header->obj_pos);
  if (r < 0) return -1;

  r = scm_write_offset(output,
                       pos + SCM_MARSHAL_SIZE_SIZE * 3 + SCM_MARSHAL_OFFSET_SIZE,
                       header->shared_pos);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_header(ScmMarshalBuffer *input, ScmMarshalHeader *header)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(header != NULL);

  r = scm_read_size(input, &header->size);
  if (r < 0) return -1;

  r = scm_read_size(input, &header->nr_obj);
  if (r < 0) return -1;

  r = scm_read_size(input, &header->nr_shared);
  if (r < 0) return -1;

  r = scm_read_offset(input, &header->obj_pos);
  if (r < 0) return -1;

  r = scm_read_offset(input, &header->shared_pos);
  if (r < 0) return -1;

  r = scm_read_encoding(input, &header->enc);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}


/****************************************************************************/
/* ScmMarshal/ScmUnmarshal                                                  */
/****************************************************************************/

ScmTypeInfo SCM_MARSHAL_TYPE_INFO = {
  .name                = "marshal",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmMarshal),
  .gc_ini_func         = scm_marshal_gc_initialize,
  .gc_fin_func         = scm_marshal_gc_finalize,
  .gc_accept_func      = scm_marshal_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

ScmTypeInfo SCM_UNMARSHAL_TYPE_INFO = {
  .name                = "unmarshal",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmUnmarshal),
  .gc_ini_func         = scm_unmarshal_gc_initialize,
  .gc_fin_func         = scm_unmarshal_gc_finalize,
  .gc_accept_func      = scm_unmarshal_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static size_t
scm_marshal_hash_func(ScmCHashTblKey key)
{
  ScmObj obj = (ScmObj)key;
  if (scm_fcd_fixnum_p(obj))
    return (size_t)obj >> 1;
  else
    return (size_t)obj >> 2;
}

int
scm_marshal_initialize(ScmObj marshal)
{
  ScmMarshalHeader header;
  int r;

  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  r = eary_init(&SCM_MARSHAL(marshal)->shared, sizeof(size_t), 32);
  if (r < 0) return -1;

  r = eary_init(&SCM_MARSHAL(marshal)->top, sizeof(size_t), 8);
  if (r < 0) return -1;

  SCM_MARSHAL(marshal)->output = scm_fcd_malloc(sizeof(ScmMarshalBuffer));
  if (SCM_MARSHAL(marshal)->output == NULL) return -1;

  r = scm_marshal_buffer_init(SCM_MARSHAL(marshal)->output,
                              NULL, SCM_MARSHAL_BUFFER_INIT_SIZE);
  if (r < 0) return -1;

  SCM_MARSHAL(marshal)->obj2pos = scm_chash_tbl_new(marshal, 256,
                                                    SCM_CHASH_TBL_SCMOBJ,
                                                    SCM_CHASH_TBL_CVAL,
                                                    scm_marshal_hash_func,
                                                    scm_fcd_eq_p);
  if (SCM_MARSHAL(marshal)->obj2pos == NULL)
    return -1;

  header.size = 0;
  header.nr_obj = 0;
  header.nr_shared = 0;
  header.obj_pos = 0;
  header.shared_pos = 0;
  header.enc = scm_fcd_system_encoding();
  r = scm_push_header(SCM_MARSHAL(marshal)->output, &header);
  if (r < 0) return -1;

  return 0;
}

void
scm_marshal_finalize(ScmObj marshal)
{
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  if (SCM_MARSHAL(marshal)->obj2pos != NULL) {
    scm_chash_tbl_end(SCM_MARSHAL(marshal)->obj2pos);
    SCM_MARSHAL(marshal)->obj2pos = NULL;
  }

  if (SCM_MARSHAL(marshal)->output != NULL) {
    scm_marshal_buffer_fin(SCM_MARSHAL(marshal)->output);
    scm_fcd_free(SCM_MARSHAL(marshal)->output);
    SCM_MARSHAL(marshal)->output = NULL;
  }

  eary_fin(&SCM_MARSHAL(marshal)->top);
  eary_fin(&SCM_MARSHAL(marshal)->shared);
}

static inline ScmMarshalBuffer *
scm_marshal_output(ScmObj marshal)
{
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(!scm_marshal_terminated_p(marshal));
  return SCM_MARSHAL(marshal)->output;
}

static int
scm_marshal_pos(ScmObj marshal, ScmObj obj, size_t *pos)
{
  ScmCHashTblVal val;
  bool found;
  int r;

  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(!scm_marshal_terminated_p(marshal));
  scm_assert(scm_obj_not_null_p(obj));
  scm_assert(pos != NULL);

  r = scm_chash_tbl_get(SCM_MARSHAL(marshal)->obj2pos,
                        (ScmCHashTblKey)obj, &val, &found);
  if (r < 0) return -1;

  if (found) {
    *pos = val;
    EARY_PUSH(&SCM_MARSHAL(marshal)->shared, size_t, val, r);
    if (r < 0) return -1;
    return 1;
  }
  else {
    return 0;
  }
}

static int
scm_marshal_reg_pos(ScmObj marshal, ScmObj obj, size_t pos)
{
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(!scm_marshal_terminated_p(marshal));
  scm_assert(scm_obj_not_null_p(obj));

  return scm_chash_tbl_insert(SCM_MARSHAL(marshal)->obj2pos, obj, pos);
}

static int
scm_marshal_qsort_cmp(const void *x, const void *y)
{
  if (*(const size_t *)x < *(const size_t *)y)
    return -1;
  else if (*(const size_t *)x > *(const size_t *)y)
    return 1;
  else
    return 0;
}

static ssize_t
scm_marshal_push_shared_pos(ScmObj marshal)
{
  size_t prev;
  ssize_t n;
  int r;

  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(!scm_marshal_terminated_p(marshal));

  qsort(EARY_HEAD(&SCM_MARSHAL(marshal)->shared),
        EARY_SIZE(&SCM_MARSHAL(marshal)->shared),
        sizeof(size_t),
        scm_marshal_qsort_cmp);

  prev = 0;
  n = 0;
  for (size_t i = 0; i < EARY_SIZE(&SCM_MARSHAL(marshal)->shared); i++) {
    size_t x = ((size_t *)EARY_HEAD(&SCM_MARSHAL(marshal)->shared))[i];
    if (x == prev) continue;

    r = scm_push_offset(SCM_MARSHAL(marshal)->output, x);
    if (r < 0) return -1;

    n++;
    prev = x;
  }

  return (ssize_t)n;
}

int
scm_marshal_push_obj(ScmObj marshal, ScmObj obj)
{
  ssize_t pos;
  int r;

  SCM_REFSTK_INIT_REG(&marshal, &obj);

  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  pos = scm_marshal_obj(obj, NULL, marshal);
  if (pos < 0) return -1;

  EARY_PUSH(&SCM_MARSHAL(marshal)->top, size_t, pos, r);
  if (r < 0) return -1;

  return 0;
}

void *
scm_marshal_terminate(ScmObj marshal, size_t *size)
{
  ScmMarshalHeader header;
  ssize_t nr_shared;
  size_t pos;
  void *data;
  int r;

  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(!scm_marshal_terminated_p(marshal));

  header.nr_obj = EARY_SIZE(&SCM_MARSHAL(marshal)->top);
  header.obj_pos = scm_marshal_buffer_pos(SCM_MARSHAL(marshal)->output);

  for (size_t i = 0; i < header.nr_obj; i++) {
    EARY_GET(&SCM_MARSHAL(marshal)->top, size_t, i, pos);
    r = scm_push_offset(SCM_MARSHAL(marshal)->output, pos);
    if (r < 0) return NULL;
  }

  header.shared_pos = scm_marshal_buffer_pos(SCM_MARSHAL(marshal)->output);

  nr_shared = scm_marshal_push_shared_pos(marshal);
  if (nr_shared < 0) return NULL;
  header.nr_shared = (size_t)nr_shared;

  header.size = scm_marshal_buffer_pos(SCM_MARSHAL(marshal)->output);
  r = scm_write_header(SCM_MARSHAL(marshal)->output, 0, &header);
  if (r < 0) return NULL;

  if (size != NULL) *size = header.size;
  data = scm_marshal_buffer_chuck(SCM_MARSHAL(marshal)->output);

  scm_marshal_buffer_fin(SCM_MARSHAL(marshal)->output);
  scm_fcd_free(SCM_MARSHAL(marshal)->output);
  SCM_MARSHAL(marshal)->output = NULL;

  return data;
}

void
scm_marshal_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_MARSHAL_TYPE_INFO);

  SCM_MARSHAL(obj)->output = NULL;
  eary_init(&SCM_MARSHAL(obj)->shared, 0, 0);
  eary_init(&SCM_MARSHAL(obj)->top, 0, 0);
  SCM_MARSHAL(obj)->obj2pos = NULL;
}

void
scm_marshal_gc_finalize(ScmObj obj)
{
  scm_marshal_finalize(obj);
}

int
scm_marshal_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  scm_assert_obj_type(obj, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (SCM_MARSHAL(obj)->obj2pos != NULL)
    return scm_chash_tbl_gc_accept(SCM_MARSHAL(obj)->obj2pos,
                                   obj, mem, handler, true);
  else
    return SCM_GC_REF_HANDLER_VAL_INIT;
}

static int
scm_unmarshal_read_pos(ScmObj unmarshal)
{
  size_t pos;

  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  pos = scm_marshal_buffer_pos(SCM_UNMARSHAL(unmarshal)->input);

  if (SCM_UNMARSHAL(unmarshal)->mh.obj_pos
      > scm_marshal_buffer_size(SCM_UNMARSHAL(unmarshal)->input)) {
    scm_marshal_error(SCM_OBJ_NULL, "marshal header: invalid offset value");
    return -1;
  }

  scm_marshal_buffer_seek(SCM_UNMARSHAL(unmarshal)->input,
                          SCM_UNMARSHAL(unmarshal)->mh.obj_pos);
  for (size_t i = 0; i < SCM_UNMARSHAL(unmarshal)->mh.nr_obj; i++) {
    int r = scm_read_offset(SCM_UNMARSHAL(unmarshal)->input,
                            SCM_UNMARSHAL(unmarshal)->obj_pos + i);
    if (r < 0) return -1;
  }

  if (SCM_UNMARSHAL(unmarshal)->mh.shared_pos
      > scm_marshal_buffer_size(SCM_UNMARSHAL(unmarshal)->input)) {
    scm_marshal_error(SCM_OBJ_NULL, "marshal header: invalid offset value");
    return -1;
  }

  scm_marshal_buffer_seek(SCM_UNMARSHAL(unmarshal)->input,
                          SCM_UNMARSHAL(unmarshal)->mh.shared_pos);
  for (size_t i = 0; i < SCM_UNMARSHAL(unmarshal)->mh.nr_shared; i++) {
    int r = scm_read_offset(SCM_UNMARSHAL(unmarshal)->input,
                            SCM_UNMARSHAL(unmarshal)->shared_pos + i);
    if (r < 0) return -1;
  }

  scm_marshal_buffer_seek(SCM_UNMARSHAL(unmarshal)->input, pos);

  return 0;
}

static size_t
scm_unmarshal_extract_size(const void *data)
{
  scm_assert(data != NULL);
  return (size_t)htonl(*(const uint32_t *)data);
}

int
scm_unmarshal_initialize(ScmObj unmarshal, const void *data)
{
  size_t size;
  int r;

  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(data != NULL);

  size = scm_unmarshal_extract_size(data);

  SCM_UNMARSHAL(unmarshal)->input = scm_fcd_malloc(sizeof(ScmMarshalBuffer));
  if (SCM_UNMARSHAL(unmarshal)->input == NULL) return -1;

  r = scm_marshal_buffer_init(SCM_UNMARSHAL(unmarshal)->input, data, size);
  if (r < 0) return -1;

  r = scm_read_header(SCM_UNMARSHAL(unmarshal)->input,
                      &SCM_UNMARSHAL(unmarshal)->mh);
  if (r < 0) return -1;

  if (SCM_UNMARSHAL(unmarshal)->mh.nr_obj > 0) {
    if (SCM_UNMARSHAL(unmarshal)->mh.nr_obj > SIZE_MAX / sizeof(size_t)) {
      scm_marshal_error(SCM_OBJ_NULL, "too many marshaled object");
      return -1;
    }

    SCM_UNMARSHAL(unmarshal)->obj_pos
      = scm_fcd_malloc(sizeof(size_t) * SCM_UNMARSHAL(unmarshal)->mh.nr_obj);
    if (SCM_UNMARSHAL(unmarshal)->obj_pos == NULL) return -1;
  }

  if (SCM_UNMARSHAL(unmarshal)->mh.nr_shared > 0) {
    if (SCM_UNMARSHAL(unmarshal)->mh.nr_shared > SIZE_MAX / sizeof(size_t)
        || SCM_UNMARSHAL(unmarshal)->mh.nr_shared > SIZE_MAX / sizeof(ScmObj)) {
      scm_marshal_error(SCM_OBJ_NULL, "too many shared object");
      return -1;
    }

    SCM_UNMARSHAL(unmarshal)->shared_pos
      = scm_fcd_malloc(sizeof(size_t) * SCM_UNMARSHAL(unmarshal)->mh.nr_shared);
    if (SCM_UNMARSHAL(unmarshal)->shared_pos == NULL) return -1;

    SCM_UNMARSHAL(unmarshal)->shared_obj
      = scm_fcd_malloc(sizeof(ScmObj) * SCM_UNMARSHAL(unmarshal)->mh.nr_shared);
    if (SCM_UNMARSHAL(unmarshal)->shared_obj == NULL) return -1;

    for (size_t i = 0; i < SCM_UNMARSHAL(unmarshal)->mh.nr_shared; i++)
      SCM_UNMARSHAL(unmarshal)->shared_obj[i] = SCM_OBJ_NULL;
  }

  r = scm_unmarshal_read_pos(unmarshal);
  if (r < 0) return -1;

  if (SCM_UNMARSHAL(unmarshal)->mh.nr_obj > 0) {
    if (SCM_UNMARSHAL(unmarshal)->mh.nr_obj > SIZE_MAX / sizeof(ScmObj)) {
      scm_marshal_error(unmarshal, "too many marshaled object");
      return -1;
    }

    SCM_UNMARSHAL(unmarshal)->unmarshaled
      = scm_fcd_malloc(sizeof(ScmObj) * SCM_UNMARSHAL(unmarshal)->mh.nr_obj);

    if (SCM_UNMARSHAL(unmarshal)->unmarshaled == NULL) return -1;

    for (size_t i = 0; i < SCM_UNMARSHAL(unmarshal)->mh.nr_obj; i++)
      SCM_UNMARSHAL(unmarshal)->unmarshaled[i] = SCM_OBJ_NULL;
  }

  return 0;
}

void
scm_unmarshal_finalize(ScmObj unmarshal)
{
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  if (SCM_UNMARSHAL(unmarshal)->unmarshaled != NULL) {
    scm_fcd_free(SCM_UNMARSHAL(unmarshal)->unmarshaled);
    SCM_UNMARSHAL(unmarshal)->unmarshaled = NULL;
  }

  if (SCM_UNMARSHAL(unmarshal)->shared_obj != NULL) {
    scm_fcd_free(SCM_UNMARSHAL(unmarshal)->shared_obj);
    SCM_UNMARSHAL(unmarshal)->shared_obj = NULL;
  }

  if (SCM_UNMARSHAL(unmarshal)->shared_pos != NULL) {
    scm_fcd_free(SCM_UNMARSHAL(unmarshal)->shared_pos);
    SCM_UNMARSHAL(unmarshal)->shared_pos = NULL;
  }

  if (SCM_UNMARSHAL(unmarshal)->obj_pos != NULL) {
    scm_fcd_free(SCM_UNMARSHAL(unmarshal)->obj_pos);
    SCM_UNMARSHAL(unmarshal)->obj_pos = NULL;
  }

  if (SCM_UNMARSHAL(unmarshal)->input != NULL) {
    scm_marshal_buffer_fin(SCM_UNMARSHAL(unmarshal)->input);
    scm_fcd_free(SCM_UNMARSHAL(unmarshal)->input);
    SCM_UNMARSHAL(unmarshal)->input = NULL;
  }
}

static inline ScmMarshalBuffer *
scm_unmarshal_input(ScmObj unmarshal)
{
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  return SCM_UNMARSHAL(unmarshal)->input;
}

static inline ScmEncoding *
scm_unmarshal_encoding(ScmObj unmarshal)
{
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  return SCM_UNMARSHAL(unmarshal)->mh.enc;
}

static int
scm_unmarshal_bsearch_cmp(const void *x, const void *y)
{
  if (*(const size_t *)x < *(const size_t *)y)
    return -1;
  else if (*(const size_t *)x > *(const size_t *)y)
    return 1;
  else
    return 0;
}

static int
scm_unmarshal_shared_obj(ScmObj unmarshal, size_t pos,
                         size_t *idx, scm_csetter_t *shared)
{
  size_t *found;
  ptrdiff_t i;

  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(idx != NULL);
  scm_assert(shared != NULL);

  found = bsearch(&pos, SCM_UNMARSHAL(unmarshal)->shared_pos,
                  SCM_UNMARSHAL(unmarshal)->mh.nr_shared, sizeof(size_t),
                  scm_unmarshal_bsearch_cmp);
  if (found == NULL)
    goto not_found;

  i = found - SCM_UNMARSHAL(unmarshal)->shared_pos;
  scm_csetter_setq(shared, SCM_UNMARSHAL(unmarshal)->shared_obj[i]);
  *idx = (size_t)i;
  return 1;

 not_found:
  *idx = 0;
  scm_csetter_setq(shared, SCM_OBJ_NULL);
  return 0;
}

static int
scm_unmarshal_reg_shared_obj(ScmObj unmarshal, size_t pos,
                             size_t idx, ScmObj obj)
{
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(idx < SCM_UNMARSHAL(unmarshal)->mh.nr_shared);
  scm_assert(SCM_UNMARSHAL(unmarshal)->shared_pos[idx] == pos);
  scm_assert(scm_obj_not_null_p(obj));

  SCM_SLOT_SETQ(ScmUnmarshal, unmarshal, shared_obj[idx], obj);
  return 0;
}

ScmObj
scm_unmarshal_ref(ScmObj unmarshal, size_t idx)
{
  ScmObj o = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&unmarshal, &o);

  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(idx < SCM_UNMARSHAL(unmarshal)->mh.nr_obj);

  if (scm_obj_not_null_p(SCM_UNMARSHAL(unmarshal)->unmarshaled[idx]))
    return SCM_UNMARSHAL(unmarshal)->unmarshaled[idx];

  if (SCM_UNMARSHAL(unmarshal)->obj_pos[idx]
      > scm_marshal_buffer_size(SCM_UNMARSHAL(unmarshal)->input)) {
    scm_marshal_error(unmarshal,
                      "marshaled object offset: invalid offset value");
    return SCM_OBJ_NULL;
  }

  scm_marshal_buffer_seek(SCM_UNMARSHAL(unmarshal)->input,
                          SCM_UNMARSHAL(unmarshal)->obj_pos[idx]);
  o = scm_unmarshal_obj(NULL, unmarshal);
  if (scm_obj_null_p(o)) return SCM_OBJ_NULL;

  SCM_SLOT_SETQ(ScmUnmarshal, unmarshal, unmarshaled[idx], o);
  return o;
}

void
scm_unmarshal_gc_initialize(ScmObj obj, ScmObj mem)
{
  scm_assert_obj_type(obj, &SCM_UNMARSHAL_TYPE_INFO);

  SCM_UNMARSHAL(obj)->input = NULL;
  SCM_UNMARSHAL(obj)->obj_pos = NULL;
  SCM_UNMARSHAL(obj)->shared_pos = NULL;
  SCM_UNMARSHAL(obj)->shared_obj = NULL;
  SCM_UNMARSHAL(obj)->unmarshaled = NULL;
}

void
scm_unmarshal_gc_finalize(ScmObj obj)
{
  scm_unmarshal_finalize(obj);
}

int
scm_unmarshal_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler)
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  if (SCM_UNMARSHAL(obj)->shared_obj != NULL) {
    for (size_t i = 0; i < SCM_UNMARSHAL(obj)->mh.nr_shared; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                     SCM_UNMARSHAL(obj)->shared_obj[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  if (SCM_UNMARSHAL(obj)->unmarshaled != NULL) {
    for (size_t i = 0; i < SCM_UNMARSHAL(obj)->mh.nr_obj; i++) {
      rslt = SCM_GC_CALL_REF_HANDLER(handler, obj,
                                     SCM_UNMARSHAL(obj)->unmarshaled[i], mem);
      if (scm_gc_ref_handler_failure_p(rslt)) return rslt;
    }
  }

  return rslt;
}


/****************************************************************************/
/* Marshal/Unmarshal Handler                                                */
/****************************************************************************/


static int
scm_marshal_obj_atom(ScmMarshalObjStat *stat, ScmObj marshal,
                     int (*func)(ScmMarshalObjStat *stat, ScmObj marshal))
{
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  size_t end;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_obj_not_null_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  if (func != NULL) {
    r = func(stat, marshal);
    if (r < 0) return -1;
  }

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_eof(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_eof_object_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, NULL);
}

static int
scm_unmarshal_obj_eof(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  stat->obj = SCM_EOF_OBJ;
  return 0;
}

static int
scm_marshal_obj_bool_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_boolean_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  r = scm_push_uint8(output, scm_fcd_true_object_p(stat->obj) ? 1 : 0);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_bool(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_boolean_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_bool_internal);
}

static int
scm_unmarshal_obj_bool(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  uint8_t val;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_uint8(input, &val);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  stat->obj = (val == 1) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
  return 0;
}

static int
scm_marshal_obj_nil(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_nil_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, NULL);
}

static int
scm_unmarshal_obj_nil(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  stat->obj = SCM_NIL_OBJ;
  return 0;
}

static int
scm_marshal_obj_undef(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_undef_object_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, NULL);
}

static int
scm_unmarshal_obj_undef(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  stat->obj = SCM_UNDEF_OBJ;
  return 0;
}

static ScmObj
scm_external_representation(ScmObj obj)
{
  ScmObj port = SCM_OBJ_INIT, str = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&obj,
                      &port, &str);

  scm_assert(scm_obj_not_null_p(obj));

  port = scm_fcd_open_output_string();
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  r = scm_fcd_write(obj, port);
  if (r < 0) return SCM_OBJ_NULL;

  return scm_fcd_get_output_string(port);
}

static int
scm_marshal_obj_number_str(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  ScmObj str;
  char *p;
  size_t ext_sz;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_number_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&marshal,
                      &str);

  output = scm_marshal_output(marshal);

  str = scm_external_representation(stat->obj);
  if (scm_obj_null_p(str)) return -1;

  ext_sz = scm_fcd_string_bytesize(str);
  ext_sz++;

  r = scm_push_uint8(output, SCM_MARSHAL_NUM_TYPE_STR);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  r = scm_push_size(output, ext_sz);
  if (r < 0) return -1;

  p = scm_alloc_space(output, ext_sz);
  if (p == NULL) return -1;

  p = scm_fcd_string_to_cstr(str, p, ext_sz);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_unmarshal_obj_number_str(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  ScmEncoding *enc;
  const void *ext;
  size_t ext_sz;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);
  enc = scm_unmarshal_encoding(unmarshal);

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  r = scm_read_size(input, &ext_sz);
  if (r < 0) return -1;

  r = scm_read_bytes(input, &ext, ext_sz);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  stat->obj = scm_fcd_make_number_from_literal(ext, enc);
  if (scm_obj_null_p(stat->obj)) return -1;

  return 0;
}

static int
scm_marshal_obj_integer_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  scm_sword_t num;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_integer_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);

  r = scm_fcd_integer_to_sword(stat->obj, &num);
  if (r < 0) {
    scm_fcd_discard_raised_obj(); /* overflow/underflow エラーを握り潰す
                                   * XXX: エラー原因が overflow/underflow のみ前
                                   *      提の実装 */
    return scm_marshal_obj_number_str(stat, marshal);
  }

  if (num < INT32_MIN || INT32_MAX < num)
    return scm_marshal_obj_number_str(stat, marshal);

  r = scm_push_uint8(output, SCM_MARSHAL_NUM_TYPE_INT);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  r = scm_push_uint32(output, (uint32_t)num);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_integer(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_integer_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_integer_internal);
}

static int
scm_unmarshal_obj_integer(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  uint8_t type;
  int32_t num;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_uint8(input, &type);
  if (r < 0) return -1;

  if (type != SCM_MARSHAL_NUM_TYPE_INT)
    return scm_unmarshal_obj_number_str(stat, unmarshal);

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  r = scm_read_uint32(input, (uint32_t *)&num);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

#if SIZEOF_SCM_WORD_T >= 4

  stat->obj = scm_fcd_make_number_from_sword(num);
  if (scm_obj_null_p(stat->obj)) return -1;
  return 0;

#else  /* SIZEOF_SCM_WORD_T < 4 */

  if (SCM_SWORD_MIN <= num && num <= SCM_SWORD_MAX) {
    stat->obj = scm_fcd_make_number_from_sword(num);
  }
  else {
    char str[16];
    int l = snprintf(str, sizeof(str), "%ld", (long)num);
    stat->obj = scm_fcd_make_number_from_literal(str, (size_t)l);
  }

  if (scm_obj_null_p(stat->obj)) return -1;
  return 0;

#endif  /* SIZEOF_SCM_WORD_T >= 4 */

}

static int
scm_marshal_obj_pair(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  ssize_t pos_car, pos_cdr;
  size_t end, cont;
  char *p;
  int r;

  SCM_REFSTK_INIT_REG(&marshal,
                      &car, &cdr);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_pair_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  cont = scm_marshal_buffer_pos(output);
  p = scm_alloc_space(output, SCM_MARSHAL_OFFSET_SIZE * 2);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);
  if (r < 0) return -1;

  car = scm_fcd_car(stat->obj);
  pos_car = scm_marshal_obj(car, stat, marshal);
  if (pos_car < 0) return -1;

  cdr = scm_fcd_cdr(stat->obj);
  pos_cdr = scm_marshal_obj(cdr, stat, marshal);
  if (pos_cdr < 0) return -1;

  r = scm_write_offset(output, cont, (size_t)pos_car);
  if (r < 0) return -1;

  r = scm_write_offset(output, cont + SCM_MARSHAL_OFFSET_SIZE, (size_t)pos_cdr);
  if (r < 0) return -1;

  return 0;
}

static int
scm_unmarshal_obj_pair(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmObj car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  size_t pos_car, pos_cdr;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &car, &cdr);

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_offset(input, &pos_car);
  if (r < 0) return -1;

  r = scm_read_offset(input, &pos_cdr);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  if (pos_car > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal, "pair: car: invalid offset value");
    return -1;
  }

  if (pos_cdr > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal, "pair: cdr: invalid offset value");
    return -1;
  }

  stat->obj = scm_fcd_cons(SCM_EOF_OBJ, SCM_EOF_OBJ);
  if (scm_obj_null_p(stat->obj)) return -1;

  scm_marshal_buffer_seek(input, pos_car);
  car = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(car)) return -1;

  scm_marshal_buffer_seek(input, pos_cdr);
  cdr = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(cdr)) return -1;

  scm_fcd_set_car_i(stat->obj, car);
  scm_fcd_set_cdr_i(stat->obj, cdr);

  return 0;
}

static int
scm_marshal_obj_symbol_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj str;
  ScmMarshalBuffer *output;
  size_t size;
  char *p;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_symbol_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&marshal,
                      &str);


  output = scm_marshal_output(marshal);

  str = scm_fcd_symbol_to_string(stat->obj);
  if (scm_obj_null_p(str)) return -1;

  size = scm_fcd_string_bytesize(str);
  size++;

  r = scm_push_size(output, size);
  if (r < 0) return -1;

  p = scm_alloc_space(output, size);
  if (p == NULL) return -1;

  p = scm_fcd_string_to_cstr(str, p, size);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_symbol(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_symbol_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_symbol_internal);
}

static int
scm_unmarshal_obj_symbol(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  ScmEncoding *enc;
  size_t size;
  const void *p;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);
  enc = scm_unmarshal_encoding(unmarshal);

  r = scm_read_size(input, &size);
  if (r < 0) return -1;

  r = scm_read_bytes(input, &p, size);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  stat->obj = scm_fcd_make_symbol_from_bin(p, size - 1, enc);
  if (scm_obj_null_p(stat->obj)) return -1;

  return 0;
}

static int
scm_marshal_obj_char_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  scm_char_t chr;
  ScmEncoding *enc;
  ssize_t w;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_char_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&marshal);

  output = scm_marshal_output(marshal);

  w = scm_fcd_char_to_cchr(stat->obj, &chr);
  enc = scm_fcd_char_encoding(stat->obj);

  r = scm_push_encoding(output, enc);
  if (r < 0) return -1;

  r = scm_push_uint8(output, (uint8_t)w);
  if (r < 0) return -1;

  r = scm_push_bytes(output, chr.bytes, (size_t)w);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_char(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_char_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_char_internal);
}

static int
scm_unmarshal_obj_char(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  scm_char_t chr;
  ScmEncoding *enc;
  const void *p;
  uint8_t w;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_encoding(input, &enc);
  if (r < 0) return -1;

  r = scm_read_uint8(input, &w);
  if (r < 0) return -1;

  if (w > sizeof(chr)) {
    scm_marshal_error(unmarshal, "char: invalid character width");
    return -1;
  }

  r = scm_read_bytes(input, &p, (size_t)w);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  memcpy(chr.bytes, p, w);

  stat->obj = scm_fcd_make_char(&chr, enc);
  if (scm_obj_null_p(stat->obj)) return -1;

  return 0;
}

static int
scm_marshal_obj_string_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  ScmEncoding *enc;
  void *p;
  size_t size;
  int r;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_string_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&marshal);

  output = scm_marshal_output(marshal);

  enc = scm_fcd_string_encoding(stat->obj);
  size = scm_fcd_string_bytesize(stat->obj);
  size++;

  r = scm_push_encoding(output, enc);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  r = scm_push_size(output, (size_t)size);
  if (r < 0) return -1;

  p = scm_alloc_space(output, (size_t)size);
  if (p == NULL) return -1;

  p = scm_fcd_string_to_cstr(stat->obj, p, (size_t)size);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_string(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_string_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_string_internal);
}

static int
scm_unmarshal_obj_string(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  ScmEncoding *enc;
  const void *p;
  size_t size;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_encoding(input, &enc);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  r = scm_read_size(input, &size);
  if (r < 0) return -1;

  r = scm_read_bytes(input, &p, size);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  stat->obj = scm_fcd_make_string_from_bin(p, size - 1, enc);
  if (scm_obj_null_p(stat->obj)) return -1;

  return 0;
}

static int
scm_marshal_obj_vector(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj elm = SCM_OBJ_INIT;
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  size_t end, cont, len;
  char *p;
  int r;

  SCM_REFSTK_INIT_REG(&marshal,
                      &elm);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_vector_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  len = scm_fcd_vector_length(stat->obj);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  r = scm_push_size(output, len);
  if (r < 0) return -1;

  cont = scm_marshal_buffer_pos(output);

  if (len > SIZE_MAX / SCM_MARSHAL_OFFSET_SIZE) {
    scm_marshal_error(marshal, "vector: too big vector");
    return -1;
  }

  p = scm_alloc_space(output, SCM_MARSHAL_OFFSET_SIZE * len);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);
  if (r < 0) return -1;

  for (size_t i = 0; i < len; i++) {
    ssize_t pos;
    elm = scm_fcd_vector_ref(stat->obj, i);
    pos = scm_marshal_obj(elm, stat, marshal);
    if (pos < 0) return -1;

    r = scm_write_offset(output,
                         cont + SCM_MARSHAL_OFFSET_SIZE * i, (size_t)pos);
    if (r < 0) return -1;
  }

  return 0;
}

static int
scm_unmarshal_obj_vector(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmObj elm = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  size_t len;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &elm);

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);
  r = scm_read_size(input, &len);
  if (r < 0) return -1;

  stat->obj = scm_fcd_make_vector(len, SCM_OBJ_NULL);
  if (scm_obj_null_p(stat->obj)) return -1;

  for (size_t i = 0; i < len; i++) {
    size_t cur, pos;
    int r;

    r = scm_read_offset(input, &pos);
    if (r < 0) return -1;

    if (pos > scm_marshal_buffer_size(input)) {
      scm_marshal_error(unmarshal, "vector: invalid offset value");
      return -1;
    }

    cur = scm_marshal_buffer_pos(input);
    scm_marshal_buffer_seek(input, pos);

    elm = scm_unmarshal_obj(stat, unmarshal);
    if (scm_obj_null_p(elm)) return -1;

    scm_fcd_vector_set_i(stat->obj, i, elm);
    scm_marshal_buffer_seek(input, cur);
  }

  return 0;
}

static int
scm_marshal_obj_bytevector_internal(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmMarshalBuffer *output;
  size_t len;
  char *p;
  int r;

  SCM_REFSTK_INIT_REG(&marshal);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_bytevector_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);

  len = scm_fcd_bytevector_length(stat->obj);
  r = scm_push_size(output, len);
  if (r < 0) return -1;

  p = scm_alloc_space(output, len);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  p = scm_fcd_bytevector_to_cv(stat->obj, p, len);
  if (p == NULL) return -1;

  return 0;
}

static int
scm_marshal_obj_bytevector(ScmMarshalObjStat *stat, ScmObj marshal)
{
  scm_assert(stat != NULL);
  scm_assert(scm_fcd_bytevector_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return scm_marshal_obj_atom(stat, marshal, scm_marshal_obj_bytevector_internal);
}

static int
scm_unmarshal_obj_bytevector(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  size_t len;
  const void *p;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_size(input, &len);
  if (r < 0) return -1;

  r = scm_read_bytes(input, &p, len);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  stat->obj = scm_fcd_make_bytevector_from_cv(p, len);
  if (scm_obj_null_p(stat->obj)) return -1;

  return 0;
}

static int
scm_marshal_obj_qqtmplnode(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj qqtn_obj = SCM_OBJ_INIT;
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  ssize_t pos_obj;
  size_t end, cont;
  char *p;
  int r, qqtn_kind;

  SCM_REFSTK_INIT_REG(&marshal,
                      &qqtn_obj);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_qqtmplnode_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  scm_fcd_qqtmplnode_get_contents_for_marshal(stat->obj,
                                              &qqtn_kind,
                                              SCM_CSETTER_L(qqtn_obj));

#if INT_MIN < INT32_MIN || INT32_MAX < INT_MAX
  if (qqtn_kind < INT32_MIN || INT32_MAX < qqtn_kind) {
    scm_marshal_error(marshal,
                      "failed to write: qq template node: kind: overflow");
    return -1;
  }
#endif  /* INT_MIN < INT32_MIN || INT32_MAX < INT_MAX */

  r = scm_push_uint32(output, (uint32_t)qqtn_kind);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  cont = scm_marshal_buffer_pos(output);
  p = scm_alloc_space(output, SCM_MARSHAL_OFFSET_SIZE);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);

  pos_obj = scm_marshal_obj(qqtn_obj, stat, marshal);
  if (pos_obj < 0) return -1;

  r = scm_write_offset(output, cont, (size_t)pos_obj);
  if (r < 0) return -1;

  return 0;
}

static int
scm_unmarshal_obj_qqtmplnode(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmObj qqtn_obj = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  uint32_t qqtn_kind;
  size_t pos_obj;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &qqtn_obj);

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_uint32(input, &qqtn_kind);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  r = scm_read_offset(input, &pos_obj);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  if (pos_obj > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal, "qq template node: invalid offset value");
    return -1;
  }

  stat->obj = scm_fcd_make_qqtmplnode_for_unmarshal();
  if (scm_obj_null_p(stat->obj)) return -1;

  scm_marshal_buffer_seek(input, pos_obj);
  qqtn_obj = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(qqtn_obj)) return -1;

  r = scm_fcd_qqtmplnode_setup_for_unmarshal(stat->obj,
                                             (int)qqtn_kind, qqtn_obj);
  if (r < 0) return -1;

  return 0;
}

static int
scm_marshal_obj_qqtmpl(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj tmpl = SCM_OBJ_INIT, compiled = SCM_OBJ_INIT, expr = SCM_OBJ_INIT;
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  ssize_t pos_tmpl, pos_compiled, pos_expr;
  size_t end, cont;
  char *p;
  int r;

  SCM_REFSTK_INIT_REG(&marshal,
                      &tmpl, &compiled, &expr);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_qqtmpl_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  output = scm_marshal_output(marshal);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  cont = scm_marshal_buffer_pos(output);
  p = scm_alloc_space(output, SCM_MARSHAL_OFFSET_SIZE * 3);
  if (p == NULL) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);
  if (r < 0) return -1;

  r = scm_fcd_qqtmpl_get_contents_for_marshal(stat->obj,
                                              SCM_CSETTER_L(tmpl),
                                              SCM_CSETTER_L(compiled),
                                              SCM_CSETTER_L(expr));
  if (r < 0) return -1;

  pos_tmpl = scm_marshal_obj(tmpl, stat, marshal);
  if (pos_tmpl < 0) return -1;

  pos_compiled = scm_marshal_obj(compiled, stat, marshal);
  if (pos_compiled < 0) return -1;

  pos_expr = scm_marshal_obj(expr, stat, marshal);
  if (pos_expr < 0) return -1;

  r = scm_write_offset(output, cont, (size_t)pos_tmpl);
  if (r < 0) return -1;
  cont += SCM_MARSHAL_OFFSET_SIZE;

  r = scm_write_offset(output, cont, (size_t)pos_compiled);
  if (r < 0) return -1;
  cont += SCM_MARSHAL_OFFSET_SIZE;

  r = scm_write_offset(output, cont, (size_t)pos_expr);
  if (r < 0) return -1;
  cont += SCM_MARSHAL_OFFSET_SIZE;

  return 0;
}

static int
scm_unmarshal_obj_qqtmpl(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmObj tmpl = SCM_OBJ_INIT, compiled = SCM_OBJ_INIT, expr = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  size_t pos_tmpl, pos_compiled, pos_expr;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &tmpl, &compiled, &expr);

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_offset(input, &pos_tmpl);
  if (r < 0) return -1;

  r = scm_read_offset(input, &pos_compiled);
  if (r < 0) return -1;

  r = scm_read_offset(input, &pos_expr);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  if (pos_tmpl > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal,
                      "qq template: template: invalid offset values");
    return -1;
  }

  if (pos_compiled > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal,
                      "qq template: compiled template: invalid offset values");
    return -1;
  }

  if (pos_expr > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal,
                      "qq template: unqouted expressions: invalid offset values");
    return -1;
  }

  stat->obj = scm_fcd_make_qqtmpl_for_unmarshal();
  if (scm_obj_null_p(stat->obj)) return -1;

  scm_marshal_buffer_seek(input, pos_tmpl);
  tmpl = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(tmpl)) return -1;

  scm_marshal_buffer_seek(input, pos_compiled);
  compiled = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(compiled)) return -1;

  scm_marshal_buffer_seek(input, pos_expr);
  expr = scm_unmarshal_obj(stat, unmarshal);
  if (scm_obj_null_p(expr)) return -1;

  r = scm_fcd_qqtmpl_setup_for_unmarshal(stat->obj, tmpl, compiled, expr);
  if (r < 0) return -1;

  return 0;
}

#define SCM_MARSHAL_ISEQ_OP_SIZE sizeof(uint32_t)
#define SCM_MARSHAL_ISEQ_OPD_SI_SIZE sizeof(uint32_t)
#define SCM_MARSHAL_ISEQ_INST_SIZE_NOOPD        \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE))
#define SCM_MARSHAL_ISEQ_INST_SIZE_OBJ                                  \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE + SCM_MARSHAL_OFFSET_SIZE))
#define SCM_MARSHAL_ISEQ_INST_SIZE_OBJ_OBJ                              \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE + SCM_MARSHAL_OFFSET_SIZE * 2))
#define SCM_MARSHAL_ISEQ_INST_SIZE_SI                                   \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE + SCM_MARSHAL_ISEQ_OPD_SI_SIZE))
#define SCM_MARSHAL_ISEQ_INST_SIZE_SI_SI      \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE \
                     + SCM_MARSHAL_ISEQ_OPD_SI_SIZE * 2))
#define SCM_MARSHAL_ISEQ_INST_SIZE_SI_SI_OBJ            \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE           \
                     + SCM_MARSHAL_ISEQ_OPD_SI_SIZE * 2 \
                     + SCM_MARSHAL_OFFSET_SIZE))
#define SCM_MARSHAL_ISEQ_INST_SIZE_IOF                                  \
  (scm_marshal_align(SCM_MARSHAL_ISEQ_OP_SIZE + SCM_MARSHAL_ISEQ_OPD_SI_SIZE))


static int
scm_push_inst_op(ScmMarshalBuffer *output, scm_opcode_t op)
{
  scm_assert(output != NULL);

#if INT_MIN < INT32_MIN || INT32_MAX < INT_MAX
  if (op < INT32_MIN || INT32_MAX < op) {
    scm_marshal_error(SCM_OBJ_NULL,
                      "failed to write: vm instruction: operator: overflow");
    return -1;
  }
#endif  /* INT_MIN < INT32_MIN || INT32_MAX < INT_MAX */

  return scm_push_uint32(output, (uint32_t)op);
}

static int
scm_read_inst_op(ScmMarshalBuffer *input, scm_opcode_t *op)
{
  uint32_t x;
  int r;

  scm_assert(input != NULL);
  scm_assert(op != NULL);

  r = scm_read_uint32(input, &x);
  if (r < 0) return -1;

  *op = (scm_opcode_t)x;
  return 0;
}

static int
scm_push_operand_si(ScmMarshalBuffer *output, int si)
{
  scm_assert(output != NULL);

#if (INT_MIN < INT32_MIN || INT32_MAX < INT_MAX)
  if (si < INT32_MIN || INT32_MAX < si) {
    scm_marshal_error(SCM_OBJ_NULL,
                      "failed to write: vm instruction: operand: overflow");
    return -1;
  }
#endif  /* INT_MIN < INT32_MIN || INT32_MAX < INT_MAX */

  return scm_push_uint32(output, (uint32_t)si);
}

static int
scm_read_operand_si(ScmMarshalBuffer *input, int *si)
{
  int32_t x;
  int r;

  scm_assert(input != NULL);

  r = scm_read_uint32(input, (uint32_t *)&x);
  if (r < 0) return -1;

#if INT32_MIN < INT_MIN || INT_MAX < INT32_MAX
  if (x < INT_MIN || INT_MAX < x) {
    scm_marshal_error(SCM_OBJ_NULL,
                      "failed to read: vm instruction: operand: overflow");
    return -1;
  }
#endif  /* INT32_MIN < INT_MIN || INT_MAX < INT32_MAX */

  *si = (int)x;
  return 0;
}

static int
scm_push_inst_noopd(ScmMarshalBuffer *output, scm_opcode_t op)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_noopd(ScmMarshalBuffer *input)
{
  int r;

  scm_assert(input != NULL);

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_obj(ScmMarshalBuffer *output, scm_opcode_t op, size_t pos)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_offset(output, pos);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_write_inst_opd_obj(ScmMarshalBuffer *output, size_t pos, size_t opd)
{
  scm_assert(output != NULL);
  return scm_write_offset(output, pos + SCM_MARSHAL_ISEQ_OP_SIZE, opd);
}

static int
scm_read_inst_opd_obj(ScmMarshalBuffer *input, size_t *pos)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(pos != NULL);

  r = scm_read_offset(input, pos);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_obj_obj(ScmMarshalBuffer *output,
                      scm_opcode_t op, size_t pos1, size_t pos2)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_offset(output, pos1);
  if (r < 0) return -1;

  r = scm_push_offset(output, pos2);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_write_inst_opd_obj_obj(ScmMarshalBuffer *output, size_t pos,
                           size_t opd1, size_t opd2)
{
  int r;

  scm_assert(output != NULL);

  pos += SCM_MARSHAL_ISEQ_OP_SIZE;
  r = scm_write_offset(output, pos, opd1);
  if (r < 0) return -1;

  pos += SCM_MARSHAL_OFFSET_SIZE;
  r = scm_write_offset(output, pos, opd2);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_obj_obj(ScmMarshalBuffer *input, size_t *pos1, size_t *pos2)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(pos1 != NULL);
  scm_assert(pos2 != NULL);

  r = scm_read_offset(input, pos1);
  if (r < 0) return -1;

  r = scm_read_offset(input, pos2);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_si(ScmMarshalBuffer *output, scm_opcode_t op, int si)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_si(ScmMarshalBuffer *input, int *si)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(si != NULL);

  r = scm_read_operand_si(input, si);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_si_si(ScmMarshalBuffer *output, scm_opcode_t op, int si1, int si2)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si1);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si2);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_si_si(ScmMarshalBuffer *input, int *si1, int *si2)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(si1 != NULL);
  scm_assert(si2 != NULL);

  r = scm_read_operand_si(input, si1);
  if (r < 0) return -1;

  r = scm_read_operand_si(input, si2);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_si_si_obj(ScmMarshalBuffer *output,
                        scm_opcode_t op, int si1, int si2, size_t pos)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si1);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si2);
  if (r < 0) return -1;

  r = scm_push_offset(output, pos);
  if (r < 0) return -1;

  return 0;
}

static int
scm_write_inst_opd_si_si_obj(ScmMarshalBuffer *output,
                             size_t pos, int opd1, int opd2, size_t opd3)
{
  int r;

  scm_assert(output != NULL);

  pos += SCM_MARSHAL_ISEQ_OP_SIZE;
  /* r = scm_write_inst_opd_si(output, pos, opd1); */
  /* if (r < 0) return -1; */

  pos += SCM_MARSHAL_ISEQ_OPD_SI_SIZE;
  /* r = scm_write_inst_opd_si(output, pos, opd2); */
  /* if (r < 0) return -1; */

  pos += SCM_MARSHAL_ISEQ_OPD_SI_SIZE;
  r = scm_write_offset(output, pos, opd3);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_si_si_obj(ScmMarshalBuffer *input,
                            int *si1, int *si2, size_t *pos)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(si1 != NULL);
  scm_assert(si2 != NULL);
  scm_assert(pos != NULL);

  r = scm_read_operand_si(input, si1);
  if (r < 0) return -1;

  r = scm_read_operand_si(input, si2);
  if (r < 0) return -1;

  r = scm_read_offset(input, pos);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

static int
scm_push_inst_iof(ScmMarshalBuffer *output, scm_opcode_t op, int si)
{
  int r;

  scm_assert(output != NULL);

  r = scm_push_inst_op(output, op);
  if (r < 0) return -1;

  r = scm_push_operand_si(output, si);
  if (r < 0) return -1;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) return -1;

  return 0;
}

static int
scm_read_inst_opd_iof(ScmMarshalBuffer *input, int *si)
{
  int r;

  scm_assert(input != NULL);
  scm_assert(si != NULL);

  r = scm_read_operand_si(input, si);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  return 0;
}

int
scm_marshal_iseq_br_bsearch_cmp(const void *x, const void *y)
{
  if (*(const size_t *)x > *(const size_t *)y)
    return 1;
  else if (*(const size_t *)x < *(const size_t *)y)
    return -1;
  else
    return 0;
}

static ssize_t
scm_marshal_obj_iseq_inst(ScmMarshalObjStat *stat, ScmObj marshal,
                          ScmObj disasm, EArray *label_pos)
{
  ScmMarshalBuffer *output;
  const ScmDisasmToken *tk;
  size_t cnt;
  int r;

  SCM_REFSTK_INIT_REG(&marshal, &disasm);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(scm_fcd_disassembler_p(disasm));
  scm_assert(label_pos != NULL);

  output = scm_marshal_output(marshal);

  cnt = 0;
  while ((tk = scm_fcd_disassembler_token(disasm)) != NULL
         && tk->type != SCM_DISASM_TK_END) {

    r = scm_fcd_disassembler_cnv_to_marshalable(disasm);
    if (r < 0) return -1;

    switch (tk->type) {
    case SCM_DISASM_TK_INST:

      switch (tk->inst.fmt) {
      case SCM_OPFMT_NOOPD:
        r = scm_push_inst_noopd(output, tk->inst.i.noopd.op);
        break;
      case SCM_OPFMT_OBJ:
        r = scm_push_inst_obj(output, tk->inst.i.obj.op, 0);
        break;
      case SCM_OPFMT_OBJ_OBJ:
        r = scm_push_inst_obj_obj(output, tk->inst.i.obj_obj.op, 0, 0);
        break;
      case SCM_OPFMT_SI:
        r = scm_push_inst_si(output, tk->inst.i.si.op, tk->inst.i.si.opd1);
        break;
      case SCM_OPFMT_SI_SI:
        r = scm_push_inst_si_si(output, tk->inst.i.si_si.op,
                                tk->inst.i.si_si.opd1, tk->inst.i.si_si.opd2);
        break;
      case SCM_OPFMT_SI_SI_OBJ:
        r = scm_push_inst_si_si_obj(output, tk->inst.i.si_si_obj.op,
                                    tk->inst.i.si_si_obj.opd1,
                                    tk->inst.i.si_si_obj.opd2,
                                    0);
        break;
      case SCM_OPFMT_IOF:
        if (tk->label_id > INT_MAX) {
          scm_marshal_error(marshal, "iseq: too many branch destination");
          return -1;
        }
        r = scm_push_inst_iof(output, tk->inst.i.iof.op, (int)tk->label_id);
        break;
      default:
        scm_marshal_error(marshal, "iseq: unknown instruction format");
        return -1;
        break;
      }

      if (r < 0) return -1;
      cnt++;
      break;
    case SCM_DISASM_TK_LABEL:
      EARY_PUSH(label_pos, size_t, scm_marshal_buffer_pos(output), r);
      if (r < 0) return -1;
      break;
    default:
      r = -1;
      break;
    }

    r = scm_fcd_disassembler_next(disasm);
    if (r < 0) return -1;
  }

  if (tk == NULL) return -1;

  return (ssize_t)cnt;
}

static int
scm_marshal_obj_iseq_body(ScmMarshalObjStat *stat, ScmObj marshal,
                          ScmObj disasm, size_t *inst_start)
{
  ScmMarshalBuffer *output;
  EArray label_pos;
  size_t pos, cont, idx, *itr;
  ssize_t nr_inst;
  int r;

  SCM_REFSTK_INIT_REG(&marshal, &disasm);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(scm_fcd_disassembler_p(disasm));
  scm_assert(inst_start != NULL);

  r = eary_init(&label_pos, sizeof(size_t), 0);
  if (r < 0) return -1;

  output = scm_marshal_output(marshal);
  cont = scm_marshal_buffer_pos(output);

  r = scm_push_size(output, 0);
  if (r < 0) goto err;

  r = scm_push_size(output, 0);
  if (r < 0) goto err;

  r = scm_push_offset(output, 0);
  if (r < 0) goto err;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) goto err;

  *inst_start = scm_marshal_buffer_pos(output);
  nr_inst = scm_marshal_obj_iseq_inst(stat, marshal, disasm, &label_pos);
  if (nr_inst < 0) goto err;

  pos = scm_marshal_buffer_pos(output);
  EARY_FOR_EACH(&label_pos, idx, itr) {
    r = scm_push_offset(output, *itr);
    if (r < 0) goto err;
  }

  r = scm_push_padding_for_alignment(output);
  if (r < 0) goto err;

  r = scm_write_size(output, cont, EARY_SIZE(&label_pos));
  if (r < 0) goto err;

  cont += SCM_MARSHAL_SIZE_SIZE;
  r = scm_write_size(output, cont, (size_t)nr_inst);
  if (r < 0) goto err;

  cont += SCM_MARSHAL_SIZE_SIZE;
  r = scm_write_offset(output, cont, pos);
  if (r < 0) goto err;

  r = scm_push_padding_for_alignment(output);
  if (r < 0) goto err;

  eary_fin(&label_pos);
  return 0;

 err:
  eary_fin(&label_pos);
  return -1;
}

static int
scm_marshal_obj_iseq_opd(ScmMarshalObjStat *stat, ScmObj marshal, ScmObj disasm,
                         size_t inst_start)
{
  ScmMarshalBuffer *output;
  const ScmDisasmToken *tk;
  size_t pos;

  SCM_REFSTK_INIT_REG(&marshal, &disasm);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  scm_assert(scm_fcd_disassembler_p(disasm));

  output = scm_marshal_output(marshal);

  pos = inst_start;
  while ((tk = scm_fcd_disassembler_token(disasm)) != NULL
         && tk->type != SCM_DISASM_TK_END) {
    ssize_t oft1, oft2;
    int r;

    r = scm_fcd_disassembler_cnv_to_marshalable(disasm);
    if (r < 0) return -1;

    if (tk->type == SCM_DISASM_TK_INST) {
      switch (tk->inst.fmt) {
      case SCM_OPFMT_NOOPD:
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_NOOPD;
        break;
      case SCM_OPFMT_OBJ:
        oft1 = scm_marshal_obj(tk->inst.i.obj.opd1, stat, marshal);
        if (oft1 < 0) return -1;
        r = scm_write_inst_opd_obj(output, pos, (size_t)oft1);
        if (r < 0) return -1;
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_OBJ;
        break;
      case SCM_OPFMT_OBJ_OBJ:
        oft1 = scm_marshal_obj(tk->inst.i.obj_obj.opd1, stat, marshal);
        if (oft1 < 0) return -1;
        oft2 = scm_marshal_obj(tk->inst.i.obj_obj.opd2, stat, marshal);
        if (oft2 < 0) return -1;
        r = scm_write_inst_opd_obj_obj(output, pos, (size_t)oft1, (size_t)oft2);
        if (r < 0) return -1;
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_OBJ_OBJ;
        break;
      case SCM_OPFMT_SI:
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_SI;
        break;
      case SCM_OPFMT_SI_SI:
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_SI_SI;
        break;
      case SCM_OPFMT_SI_SI_OBJ:
        oft1 = scm_marshal_obj(tk->inst.i.si_si_obj.opd3, stat, marshal);
        if (oft1 < 0) return -1;
        r = scm_write_inst_opd_si_si_obj(output, pos,
                                         tk->inst.i.si_si_obj.opd1,
                                         tk->inst.i.si_si_obj.opd2,
                                         (size_t)oft1);
        if (r < 0) return -1;
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_SI_SI_OBJ;
        break;
      case SCM_OPFMT_IOF:
        pos += SCM_MARSHAL_ISEQ_INST_SIZE_IOF;
        break;
      default:
        scm_marshal_error(marshal, "iseq: unknown instruction format");
        break;
      }
    }

    r = scm_fcd_disassembler_next(disasm);
    if (r < 0) return -1;
  }

  if (tk == NULL) return -1;

  return 0;
}

static int
scm_marshal_obj_iseq(ScmMarshalObjStat *stat, ScmObj marshal)
{
  ScmObj disasm = SCM_OBJ_INIT;
  ScmMarshalBuffer *output;
  ScmMarshalObjHeader header;
  size_t inst_start, end;
  int r;

  SCM_REFSTK_INIT_REG(&marshal,
                      &disasm);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  disasm = scm_fcd_make_disassembler(stat->obj);
  if (scm_obj_null_p(disasm)) return -1;

  output = scm_marshal_output(marshal);
  stat->pos = scm_marshal_buffer_pos(output);

  header.size = 0;
  header.flags = 0;
  header.info = scm_obj_type(stat->obj);
  r = scm_push_obj_header(output, &header);
  if (r < 0) return -1;

  r = scm_marshal_obj_iseq_body(stat, marshal, disasm, &inst_start);
  if (r < 0) return -1;

  end = scm_marshal_buffer_pos(output);

  r = scm_write_obj_header_size(output, stat->pos, end - stat->pos);
  if (r < 0) return -1;

  scm_fcd_disassembler_rewind(disasm);
  r = scm_marshal_obj_iseq_opd(stat, marshal, disasm, inst_start);
  if (r < 0) return -1;

  return 0;
}

static int
scm_unmarshal_obj_iseq_label_pos(ScmUnmarshalObjStat *stat,
                                 ScmObj unmarshal, size_t pos,
                                 size_t *label_pos, size_t nr_labels)
{
  ScmMarshalBuffer *input;
  size_t cur;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(nr_labels == 0 || label_pos != NULL);

  input = scm_unmarshal_input(unmarshal);

  if (pos > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal, "iseq: invalid offset value");
    return -1;
  }

  cur = scm_marshal_buffer_pos(input);
  scm_marshal_buffer_seek(input, pos);

  for (size_t i = 0; i < nr_labels; i++) {
    int r = scm_read_offset(input, label_pos + i);
    if (r < 0) return -1;
  }

  scm_marshal_buffer_seek(input, cur);
  return 0;
}

static ScmObj
scm_unmarshal_obj_iseq_opd(ScmUnmarshalObjStat *stat, ScmObj unmarshal,
                           size_t opd_pos)
{
  ScmObj opd = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  size_t pos;

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  input = scm_unmarshal_input(unmarshal);

  if (opd_pos > scm_marshal_buffer_size(input)) {
    scm_marshal_error(unmarshal, "iseq: operand: invalid offset value");
    return SCM_OBJ_NULL;;
  }

  pos = scm_marshal_buffer_pos(input);
  scm_marshal_buffer_seek(input, opd_pos);
  opd = scm_unmarshal_obj(stat, unmarshal);
  scm_marshal_buffer_seek(input, pos);
  return opd;
}

static int
scm_unmarshal_obj_iseq_internal(ScmUnmarshalObjStat *stat,
                                ScmObj unmarshal, ScmObj asmb,
                                size_t nr_inst, size_t nr_labels, size_t br_pos)
{
  ScmObj opd1 = SCM_OBJ_INIT, opd2 = SCM_OBJ_INIT;
  ScmMarshalBuffer *input;
  size_t label_pos[nr_labels], label_id;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal, &asmb,
                      &opd1, &opd2);

  scm_assert(stat != NULL);
  scm_assert(scm_fcd_iseq_p(stat->obj));
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  scm_assert(scm_fcd_assembler_p(asmb));

  input = scm_unmarshal_input(unmarshal);

  r = scm_unmarshal_obj_iseq_label_pos(stat, unmarshal,
                                       br_pos, label_pos, nr_labels);
  if (r < 0) return -1;

  for (size_t id = 0; id < nr_labels; id++) {
    r = scm_fcd_assembler_register_label_id(asmb, id);
    if (r < 0) return -1;
  }

  label_id = 0;
  for (size_t i = 0; i < nr_inst; i++) {
    scm_opcode_t op;
    size_t pos1, pos2;
    int si1, si2;

    if (label_id < nr_labels &&
        scm_marshal_buffer_pos(input) == label_pos[label_id]) {
      r = scm_fcd_assembler_push(asmb, SCM_ASM_PI_LABEL, label_id);
      if (r < 0) return -1;
      label_id++;
    }

    r = scm_read_inst_op(input, &op);
    if (r < 0) return -1;

    switch (scm_opfmt_table[op]) {
    case SCM_OPFMT_NOOPD:
      r = scm_read_inst_opd_noopd(input);
      if (r < 0) return -1;
      r = scm_fcd_assembler_push(asmb, op);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_OBJ:
      r = scm_read_inst_opd_obj(input, &pos1);
      if (r < 0) return -1;
      opd1 = scm_unmarshal_obj_iseq_opd(stat, unmarshal, pos1);
      if (scm_obj_null_p(opd1)) return -1;
      r = scm_fcd_assembler_push(asmb, op, opd1);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_OBJ_OBJ:
      r = scm_read_inst_opd_obj_obj(input, &pos1, &pos2);
      if (r < 0) return -1;
      opd1 = scm_unmarshal_obj_iseq_opd(stat, unmarshal, pos1);
      if (scm_obj_null_p(opd1)) return -1;
      opd2 = scm_unmarshal_obj_iseq_opd(stat, unmarshal, pos2);
      if (scm_obj_null_p(opd2)) return -1;
      r = scm_fcd_assembler_push(asmb, op, opd1, opd2);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_SI:
      r = scm_read_inst_opd_si(input, &si1);
      if (r < 0) return -1;
      r = scm_fcd_assembler_push(asmb, op, si1);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_SI_SI:
      r = scm_read_inst_opd_si_si(input, &si1, &si2);
      if (r < 0) return -1;
      r = scm_fcd_assembler_push(asmb, op, si1, si2);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_SI_SI_OBJ:
      r = scm_read_inst_opd_si_si_obj(input, &si1, &si2, &pos1);
      if (r < 0) return -1;
      opd1 = scm_unmarshal_obj_iseq_opd(stat, unmarshal, pos1);
      if (scm_obj_null_p(opd1)) return -1;
      r = scm_fcd_assembler_push(asmb, op, si1, si2, opd1);
      if (r < 0) return -1;
      break;
    case SCM_OPFMT_IOF:
      r = scm_read_inst_opd_iof(input, &si1);
      if (r < 0) return -1;
      r = scm_fcd_assembler_push(asmb, op, true, (size_t)si1);
      if (r < 0) return -1;
      break;
    default:
      scm_marshal_error(unmarshal, "iseq: unknown instruction format");
      return -1;
      break;
    }
  }

  if (label_id < nr_labels &&
      scm_marshal_buffer_pos(input) == label_pos[label_id]) {
    r = scm_fcd_assembler_push(asmb, SCM_ASM_PI_LABEL, label_id);
    if (r < 0) return -1;
    label_id++;
  }

  scm_assert(label_id == nr_labels);

  return 0;
}

static int
scm_unmarshal_obj_iseq(ScmUnmarshalObjStat *stat, ScmObj unmarshal)
{
  ScmMarshalBuffer *input;
  ScmObj asmb = SCM_OBJ_INIT;
  size_t nr_branch, nr_inst, br_pos;
  int r;

  scm_assert(stat != NULL);
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &asmb);

  stat->obj = scm_fcd_make_iseq();
  if (scm_obj_null_p(stat->obj)) return -1;

  asmb = scm_fcd_make_assembler(stat->obj);
  if (scm_obj_null_p(asmb)) return -1;

  input = scm_unmarshal_input(unmarshal);

  r = scm_read_size(input, &nr_branch);
  if (r < 0) return -1;

  r = scm_read_size(input, &nr_inst);
  if (r < 0) return -1;

  r = scm_read_offset(input, &br_pos);
  if (r < 0) return -1;

  r = scm_skip_padding_for_alignment(input);
  if (r < 0) return -1;

  r = scm_unmarshal_obj_iseq_internal(stat, unmarshal, asmb,
                                      nr_inst, nr_branch, br_pos);
  if (r < 0) return -1;

  r = scm_fcd_assembler_commit(asmb);
  if (r < 0) return -1;

  return 0;
}

struct marshal_handler {
  const char *name;
  int (*marshal)(ScmMarshalObjStat *, ScmObj);
  int (*unmarshal)(ScmUnmarshalObjStat *, ScmObj);
};

static const struct marshal_handler *
scm_marshal_handler(const char *name)
{
  static const struct marshal_handler tbl[] = {
    { "eof", scm_marshal_obj_eof, scm_unmarshal_obj_eof},
    { "boolean", scm_marshal_obj_bool, scm_unmarshal_obj_bool },
    { "nil", scm_marshal_obj_nil, scm_unmarshal_obj_nil },
    { "undef", scm_marshal_obj_undef, scm_unmarshal_obj_undef },
    { "pair", scm_marshal_obj_pair, scm_unmarshal_obj_pair },
    { "fixnum", scm_marshal_obj_integer, scm_unmarshal_obj_integer },
    { "bixnum", scm_marshal_obj_integer, scm_unmarshal_obj_integer },
    { "symbol", scm_marshal_obj_symbol, scm_unmarshal_obj_symbol },
    { "char", scm_marshal_obj_char, scm_unmarshal_obj_char },
    { "string", scm_marshal_obj_string, scm_unmarshal_obj_string },
    { "vector", scm_marshal_obj_vector, scm_unmarshal_obj_vector },
    { "bytevector", scm_marshal_obj_bytevector, scm_unmarshal_obj_bytevector },
    { "qq-tmpl-node", scm_marshal_obj_qqtmplnode, scm_unmarshal_obj_qqtmplnode },
    { "qq-template", scm_marshal_obj_qqtmpl, scm_unmarshal_obj_qqtmpl },
    { "iseq", scm_marshal_obj_iseq, scm_unmarshal_obj_iseq },
  };

  scm_assert(name != NULL);

  for (size_t i = 0; i < sizeof(tbl)/sizeof(tbl[0]); i++) {
    if (strcmp(tbl[i].name, name) == 0)
      return &tbl[i];
  }

  return NULL;
}

static ssize_t
scm_marshal_obj(ScmObj obj, ScmMarshalObjStat *container, ScmObj marshal)
{
  const struct marshal_handler *handler;
  ScmMarshalObjStat stat = { .obj = obj, .pos = 0, .registered = false };
  size_t pos;
  int r;

  SCM_REFSTK_INIT_REG(&obj, &marshal,
                      &stat.obj);

  scm_assert(scm_obj_not_null_p(obj));
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);

  if (container != NULL && !container->registered) {
    r = scm_marshal_reg_pos(marshal, container->obj, container->pos);
    if (r < 0) return -1;
    container->registered = true;
  }

  r = scm_marshal_pos(marshal, obj, &pos);
  if (r < 0) return -1;

  if (r == 1) {
    r = scm_set_obj_header_flag(scm_marshal_output(marshal),
                                pos, SCM_MARSHAL_OBJ_FLG_SHARED);
    if (r < 0) return -1;
    return (ssize_t)pos;
  }

  handler = scm_marshal_handler(scm_obj_type_name(obj));
  if (handler == NULL) {
    scm_marshal_error(marshal,
                      "unsupported object: %s", scm_obj_type_name(obj));
    return -1;
  }

  r = handler->marshal(&stat, marshal);
  if (r < 0) return -1;

  if (!stat.registered) {
    r = scm_marshal_reg_pos(marshal, stat.obj, stat.pos);
    if (r < 0) return -1;
    stat.registered = true;
  }

  return (ssize_t)stat.pos;
}

static ScmObj
scm_unmarshal_obj(ScmUnmarshalObjStat *container, ScmObj unmarshal)
{
  ScmObj obj = SCM_OBJ_INIT;
  const struct marshal_handler *handler;
  ScmMarshalObjHeader obj_header;
  ScmUnmarshalObjStat stat = { .obj = SCM_OBJ_INIT, .pos = 0,
                               .shared = false, .registered = false };
  ScmMarshalBuffer* input;
  int r;

  SCM_REFSTK_INIT_REG(&unmarshal,
                      &stat.obj, &obj);

  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);

  if (container != NULL && container->shared && !container->registered) {
    scm_assert(scm_obj_not_null_p(container->obj));
    r = scm_unmarshal_reg_shared_obj(unmarshal, container->pos, container->idx,
                                     container->obj);
    if (r < 0) return SCM_OBJ_NULL;
    container->registered = true;
  }

  input = scm_unmarshal_input(unmarshal);
  stat.pos = scm_marshal_buffer_pos(input);
  r = scm_read_obj_header(input, &obj_header);
  if (r < 0) return SCM_OBJ_NULL;

  if (obj_header.flags & SCM_MARSHAL_OBJ_FLG_SHARED) {
    stat.shared = true;
    r = scm_unmarshal_shared_obj(unmarshal,
                                 stat.pos, &stat.idx, SCM_CSETTER_L(obj));
    if (r < 0) return SCM_OBJ_NULL;
    if (scm_obj_not_null_p(obj))
      return obj;
  }

  handler = scm_marshal_handler(obj_header.name);
  if (handler == NULL) {
    scm_marshal_error(unmarshal, "unsupported type name: %s", obj_header.name);
    return SCM_OBJ_NULL;
  }

  r = handler->unmarshal(&stat, unmarshal);
  if (r < 0) return SCM_OBJ_NULL;
  scm_assert(scm_obj_not_null_p(stat.obj));

  if (stat.shared && !stat.registered) {
    r = scm_unmarshal_reg_shared_obj(unmarshal, stat.pos, stat.idx, stat.obj);
    if (r < 0) return SCM_OBJ_NULL;
    stat.registered = true;
  }

  return stat.obj;
}


/****************************************************************************/
/* Marshal/Unmarshal (interface)                                            */
/****************************************************************************/

bool
scm_fcd_marshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MARSHAL_TYPE_INFO);
}

ScmObj
scm_fcd_marshal_new(SCM_MEM_TYPE_T mtype)
{
  ScmObj marshal = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&marshal);

  marshal = scm_fcd_mem_alloc(&SCM_MARSHAL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(marshal)) return SCM_OBJ_NULL;

  r = scm_marshal_initialize(marshal);
  if (r < 0) return SCM_OBJ_NULL;

  return marshal;
}

ScmObj
scm_fcd_make_marshal(void)
{
  return scm_fcd_marshal_new(SCM_MEM_HEAP);
}

bool
scm_fcd_marshal_terminated_p(ScmObj marshal)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  return scm_marshal_terminated_p(marshal);
}

int
scm_fcd_marshal_push(ScmObj marshal, ScmObj obj)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  scm_assert(scm_marshal_terminated_p(marshal));
  scm_assert(scm_obj_not_null_p(obj));
  return scm_marshal_push_obj(marshal, obj);
}

void *
scm_fcd_marshal_terminate(ScmObj marshal, size_t *size)
{
  scm_assert(scm_fcd_marshal_p(marshal));
  scm_assert(scm_marshal_terminated_p(marshal));
  return scm_marshal_terminate(marshal, size);
}

bool
scm_fcd_unmarshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_UNMARSHAL_TYPE_INFO);
}

ScmObj
scm_fcd_unmarshal_new(SCM_MEM_TYPE_T mtype, const void *data)
{
  ScmObj unmarshal = SCM_OBJ_INIT;
  int r;

  scm_assert(data != NULL);

  unmarshal = scm_fcd_mem_alloc(&SCM_UNMARSHAL_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(unmarshal)) return SCM_OBJ_NULL;

  r = scm_unmarshal_initialize(unmarshal, data);
  if (r < 0) return SCM_OBJ_NULL;

  return unmarshal;
}

ScmObj
scm_fcd_make_unmarshal(const void *data)
{
  scm_assert(data != NULL);
  return scm_fcd_unmarshal_new(SCM_MEM_HEAP, data);
}

size_t
scm_fcd_unmarshal_num(ScmObj unmarshal)
{
  scm_assert(scm_fcd_unmarshal_p(unmarshal));
  return scm_unmarshal_num_of_objs(unmarshal);
}

ScmObj
scm_fcd_unmarshal_ref(ScmObj unmarshal, size_t idx)
{
  scm_assert(scm_fcd_unmarshal_p(unmarshal));
  scm_assert(idx < scm_unmarshal_num_of_objs(unmarshal));
  return scm_unmarshal_ref(unmarshal, idx);
}

static void *
marshal_va_internal(size_t *size, size_t nr_obj, va_list args)
{
  ScmObj marshal = SCM_OBJ_INIT, obj[nr_obj];

  for (size_t i = 0; i < nr_obj; i++) obj[i] = va_arg(args, ScmObj);

  SCM_REFSTK_INIT_REG(&marshal);
  SCM_REFSTK_REG_ARY(obj, nr_obj);

  marshal = scm_fcd_marshal_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(marshal)) return NULL;

  for (size_t i = 0; i < nr_obj; i++) {
    int r = scm_marshal_push_obj(marshal, obj[i]);
    if (r < 0) return NULL;
  }

  return scm_marshal_terminate(marshal, size);
}

void *
scm_fcd_marshal_va(size_t *size, va_list args)
{
  ScmObj o = SCM_OBJ_INIT;
  size_t n;
  void *p;
  va_list copy;

  va_copy(copy, args);

  n = 0;
  while (true) {
    o = va_arg(args, ScmObj);
    if (scm_obj_null_p(o)) break;
    n++;
  }

  p = marshal_va_internal(size, n, copy);
  va_end(copy);

  return p;
}

void *
scm_fcd_marshal(size_t *size, ...)
{
  void *data;
  va_list args;

  va_start(args, size);
  data = scm_fcd_marshal_va(size, args);
  va_end(args);

  return data;
}
