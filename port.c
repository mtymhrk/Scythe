#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "api.h"
#include "port.h"
#include "encoding.h"
#include "impl_utils.h"

/* Note: size_t 型の引数が SSIZE_MAX 以下であることを assert でチェックしてい
 *       るのは、read/write の戻り値が ssize_t 型であるため。 */

#define BIT_IS_SETED(val, bit) (((val) & (bit)) ? true : false)

#define SCM_STRINGIO_INIT_BUF_SIZE 64
#define SCM_PORT_DEFAULT_BUF_SIZE 256


ScmTypeInfo SCM_PORT_TYPE_INFO = {
  .pp_func             = NULL,
  .obj_size            = sizeof(ScmPort),
  .gc_ini_func         = NULL,
  .gc_fin_func         = scm_port_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
};

void
scm_io_initialize(ScmIO *io,
                  ScmIOFinFunc fin,
                  ScmIOReadFunc read,
                  ScmIOWriteFunc write,
                  ScmIOSeekFunc seek,
                  ScmIOCloseFunc close,
                  ScmIOReadyPFunc readyp,
                  ScmIOBuffModeFunc buf_mode,
                  ScmIOBlkSizeFunc blk_size)
{
  scm_assert(io != NULL);

  io->fin_func = fin;
  io->read_func = read;
  io->write_func = write;
  io->seek_func = seek;
  io->close_func = close;
  io->ready_p_func = readyp;
  io->default_buf_mode_func = buf_mode;
  io->blk_size_func = blk_size;
}


void
scm_io_end(ScmIO *io)
{
  scm_assert(io != NULL);

  if (io->fin_func != NULL)
    io->fin_func(io);
}

ssize_t
scm_io_read(ScmIO *io, void *buf, size_t size)
{
  scm_assert(io != NULL);

  return (io->read_func!= NULL) ? io->read_func(io, buf, size) : -1;
}

ssize_t
scm_io_write(ScmIO *io, const void *buf, size_t size)
{
  scm_assert(io != NULL);

  return (io->write_func != NULL) ? io->write_func(io, buf, size) : -1;
}


bool
scm_io_seek(ScmIO *io, off_t offset, int whence)
{
  scm_assert(io != NULL);

  return (io->seek_func != NULL) ? io->seek_func(io, offset, whence) : -1;
}

int
scm_io_close(ScmIO *io)
{
  scm_assert(io != NULL);

  return (io->close_func != NULL) ? io->close_func(io) : -1;
}

int
scm_io_ready_p(ScmIO *io)
{
  scm_assert(io != NULL);

  return (io->ready_p_func != NULL) ? io->ready_p_func(io) : 1;
}

SCM_PORT_BUF_MODE
scm_io_default_buffer_mode(ScmIO *io)
{
  scm_assert(io != NULL);

  if (io->default_buf_mode_func != NULL)
    return io->default_buf_mode_func(io);
  else
    return SCM_PORT_BUF_FULL;
}

ssize_t
scm_io_block_size(ScmIO *io)
{
  assert(io != NULL);
  return (io->blk_size_func != NULL) ? io->blk_size_func(io) : -1;
}

ScmFileIO *
scm_fileio_new(int fd)
{
  ScmFileIO *fileio;

  fileio = scm_capi_malloc(sizeof(ScmFileIO));
  if (fileio == NULL) return NULL;

  scm_io_initialize((ScmIO *)fileio,
                    (ScmIOFinFunc)scm_fileio_end,
                    (ScmIOReadFunc)scm_fileio_read,
                    (ScmIOWriteFunc)scm_fileio_write,
                    (ScmIOSeekFunc)scm_fileio_seek,
                    (ScmIOCloseFunc)scm_fileio_close,
                    (ScmIOReadyPFunc)scm_fileio_ready_p,
                    (ScmIOBuffModeFunc)scm_fileio_default_buffer_mode,
                    (ScmIOBlkSizeFunc)scm_fileio_block_size);
  fileio->fd = fd;

  return fileio;
}

void
scm_fileio_end(ScmFileIO *fileio)
{
  scm_assert(fileio != NULL);

  scm_capi_free(fileio);
}

ScmFileIO *
scm_fileio_open(const char *pathname, int flags, mode_t mode)
{
  int fd;

  scm_assert(pathname != NULL);

  SCM_SYSCALL(fd, open(pathname, flags, mode));
  if (fd < 0) return NULL;

  return scm_fileio_new(fd);
}

ssize_t
scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size)
{
  ssize_t n;

  scm_assert(fileio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  SCM_SYSCALL(n, read(fileio->fd, buf, size));

  return n;
}

ssize_t
scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size)
{
  ssize_t n;

  scm_assert(fileio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  SCM_SYSCALL(n, write(fileio->fd, buf, size));

  return n;
}

int
scm_fileio_ready_p(ScmFileIO *fileio)
{
  int n;
  fd_set readfds;
  struct timeval tout;

  scm_assert(fileio != NULL);

  FD_ZERO(&readfds);
  FD_SET(fileio->fd, &readfds);
  tout.tv_sec = 0;
  tout.tv_usec = 0;

  SCM_SYSCALL(n, select(fileio->fd + 1, &readfds, NULL, NULL, &tout));
  if (n < 0) return -1;

  return (n > 0) ? 1 : 0;
}

off_t
scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence)
{
  off_t n;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(n, lseek(fileio->fd, offset, whence));

  return n;
}

int
scm_fileio_close(ScmFileIO *fileio)
{
  int n;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(n, close(fileio->fd));

  return n;
}

SCM_PORT_BUF_MODE
scm_fileio_default_buffer_mode(ScmFileIO *fileio)
{
  struct stat st;
  int ret;

  scm_assert(fileio != NULL);

  if (isatty(fileio->fd))
    return SCM_PORT_BUF_MODEST;

  SCM_SYSCALL(ret, fstat(fileio->fd, &st));
  if (ret < 0) return SCM_PORT_BUF_FULL;

  if (S_ISFIFO(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else if (S_ISSOCK(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else if (S_ISCHR(st.st_mode))
    return SCM_PORT_BUF_MODEST;
  else
    return SCM_PORT_BUF_FULL;
}

ssize_t
scm_fileio_block_size(ScmFileIO *fileio)
{
  int rslt;
  struct stat stat;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(rslt, fstat(fileio->fd, &stat));
  if (rslt < 0)
    return -1;
  else
    return stat.st_blksize;
}

int
scm_stringio_expand_buffer(ScmStringIO *strio, size_t needed_size)
{
  size_t new_size;

  scm_assert(strio != NULL);
  scm_assert(needed_size <= SSIZE_MAX);

  if (needed_size < strio->capacity) return 0;

  for (new_size = SCM_STRINGIO_INIT_BUF_SIZE;
       new_size < needed_size && new_size < SSIZE_MAX / 2;
       new_size *= 2)
    ;

  if (new_size < needed_size) new_size = SSIZE_MAX;

  if (strio->string == NULL) {
    strio->string = scm_capi_malloc(new_size);
    strio->capacity = new_size;
  }
  else if (new_size > strio->capacity) {
    char *new_buffer = scm_capi_realloc(strio->string, new_size);
    if (new_buffer == NULL) return -1;
    strio->string = new_buffer;
    strio->capacity = new_size;
  }

  return 0;
}


ScmStringIO *
scm_stringio_new(const char *str, size_t len)
{
  ScmStringIO *strio;
  int ret;

  scm_assert(len <= SSIZE_MAX);

  strio = scm_capi_malloc(sizeof(ScmStringIO));
  if (strio == NULL) return NULL;

  scm_io_initialize((ScmIO *)strio,
                    (ScmIOFinFunc)scm_stringio_end,
                    (ScmIOReadFunc)scm_stringio_read,
                    (ScmIOWriteFunc)scm_stringio_write,
                    (ScmIOSeekFunc)scm_stringio_seek,
                    (ScmIOCloseFunc)scm_stringio_close,
                    (ScmIOReadyPFunc)scm_stringio_ready_p,
                    (ScmIOBuffModeFunc)scm_stringio_default_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL);
  strio->string = NULL;
  strio->capacity = 0;
  strio->length = 0;
  strio->pos = 0;

  if (str != NULL) {
    ret = scm_stringio_expand_buffer(strio, len);
    if (ret < 0) goto err;
    memcpy(strio->string, str, len);
    strio->length = len;
  }
  else {
    ret = scm_stringio_expand_buffer(strio, 0);
    if (ret < 0) goto err;
  }

  return strio;

 err:
  scm_capi_free(strio);
  return NULL;
}

void
scm_stringio_end(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  scm_capi_free(strio->string);
  scm_capi_free(strio);
}

ssize_t
scm_stringio_read(ScmStringIO *strio, void *buf, size_t size)
{
  size_t n;
  size_t rest;

  scm_assert(strio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  rest = strio->length - strio->pos;
  if (rest <= 0) return 0;

  n = (size <= rest) ? size : rest;
  scm_assert(n <= SSIZE_MAX);

  memcpy(buf, strio->string + strio->pos, n);
  strio->pos += n;

  return (ssize_t)n;
}

ssize_t
scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size)
{
  scm_assert(strio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (size > SSIZE_MAX - strio->pos) return -1;

  scm_stringio_expand_buffer(strio, strio->pos + size);
  memcpy(strio->string + strio->pos, buf, size);
  strio->pos += size;
  if (strio->pos > strio->length)
    strio->length = strio->pos;

  return (ssize_t)size;
}


int
scm_stringio_ready_p(ScmStringIO *strio)
{
  assert(strio != NULL);

  /* eof 相当のケースでも読取可能と返す */
  return (strio->pos <= strio->length) ? 1 : 0;
}

off_t
scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence)
{
  ssize_t pos;

  assert(strio != NULL);

  switch (whence) {
  case SEEK_SET:
    pos = offset;
    break;
  case SEEK_CUR:
    pos = (ssize_t)strio->pos + offset;
    break;
  case SEEK_END:
    pos = (ssize_t)strio->length + offset;
    break;
  default:
    return -1;
  }

  if (pos < 0) return -1;

  scm_stringio_expand_buffer(strio, (size_t)pos);
  strio->pos = (size_t)pos;

  return pos;
}

int
scm_stringio_close(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  /* nothing to do  */

  return 0;
}

SCM_PORT_BUF_MODE
scm_stringio_default_buffer_mode(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return SCM_PORT_BUF_NONE;
}

char *
scm_stringio_buffer(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return strio->string;
}

size_t
scm_stringio_length(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return strio->length;
}

scm_local_func int
scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_MODE buf_mode)
{
  ssize_t s;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT)
    SCM_PORT(port)->buffer_mode =
      scm_io_default_buffer_mode(SCM_PORT(port)->io);
  else
    SCM_PORT(port)->buffer_mode = buf_mode;

  SCM_PORT(port)->pos = 0;
  SCM_PORT(port)->used = 0;
  switch (SCM_PORT(port)->buffer_mode) {
  case SCM_PORT_BUF_FULL:   /* fall through */
  case SCM_PORT_BUF_LINE:   /* fall through */
  case SCM_PORT_BUF_MODEST: /* fall through */
    s = scm_io_block_size(SCM_PORT(port)->io);
    if (s >= 0)
      SCM_PORT(port)->capacity = (size_t)s;
    else
      SCM_PORT(port)->capacity = SCM_PORT_DEFAULT_BUF_SIZE;
    break;
  case SCM_PORT_BUF_NONE:
    SCM_PORT(port)->capacity = 0;
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    SCM_PORT(port)->capacity = 0; /* must not happen */
    break;
  }

  if (SCM_PORT(port)->capacity == 0) {
    SCM_PORT(port)->buffer = NULL;
    return 0;
  }

  SCM_PORT(port)->buffer = scm_capi_malloc(SCM_PORT(port)->capacity);
  if (SCM_PORT(port)->buffer == NULL)
    return -1;

  return 0;
}

scm_local_inline bool
scm_port_buffer_empty_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return (SCM_PORT(port)->pos == SCM_PORT(port)->used);
}

scm_local_inline void
scm_port_clear_buffer(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  SCM_PORT(port)->used = 0;
  SCM_PORT(port)->pos = 0;
}


scm_local_func ssize_t
scm_port_read_from_buffer(ScmObj port, void *buf, size_t size)
{
  size_t n;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);
  scm_assert(SCM_PORT(port)->buffer_mode != SCM_PORT_BUF_NONE);

  n = SCM_PORT(port)->used - SCM_PORT(port)->pos;
  n = (n < size) ? n : size;

  memcpy(buf, SCM_PORT(port)->buffer + SCM_PORT(port)->pos, n);
  SCM_PORT(port)->pos += n;

  return (ssize_t)n;
}

scm_local_func ssize_t
scm_port_read_into_buffer(ScmObj port)
{
  ssize_t n;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(SCM_PORT(port)->buffer_mode != SCM_PORT_BUF_NONE);

  if (SCM_PORT(port)->eof_received_p) return 0;

  n = scm_io_read(SCM_PORT(port)->io,
                  SCM_PORT(port)->buffer, SCM_PORT(port)->capacity);
  if (n < 0) return -1; // error

  if (n == 0) SCM_PORT(port)->eof_received_p = true;

  SCM_PORT(port)->pos = 0;
  SCM_PORT(port)->used = (size_t)n;

  return n;
}

scm_local_func ssize_t
scm_port_size_up_to_lf(ScmObj port, const void *buf, size_t size)
{
  /* provisional implementation */
  /* 現状、LF 改行にしか対応していない */

  const ScmEncVirtualFunc *vf;
  ScmStrItr iter;
  scm_char_t lf;
  ssize_t len;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  vf = SCM_ENCODING_VFUNC(SCM_PORT(port)->encoding);
  iter = scm_str_itr_begin((void *)buf, size, vf->char_width);
  lf = SCM_ENCODING_CONST_LF_CHAR(SCM_PORT(port)->encoding);

  len = 0;
  while (!SCM_STR_ITR_IS_END(&iter)) {
    ssize_t w = SCM_STR_ITR_WIDTH(&iter);
    if (w < 0)
      return -1;       /* TODO: error handling (illegal sequence) */
    else if (w == 0)
      return 0;

    len += w;
    if (memcmp(lf.bytes, SCM_STR_ITR_PTR(&iter), (size_t)w) == 0)
      return len;

    iter = scm_str_itr_next(&iter);
    if (SCM_STR_ITR_IS_ERR(&iter))
      return -1; /* TODO: error handling (illegal sequence) */
  }

  return 0;
}

scm_local_inline uint8_t *
scm_port_pushback_buff_head(ScmObj port)
{
  return (SCM_PORT(port)->pushback
          + (SCM_PORT_PUSHBACK_BUFF_SIZE - SCM_PORT(port)->pb_used));
}

scm_local_inline size_t
scm_port_pushback_buff_unused(ScmObj port)
{
  return SCM_PORT_PUSHBACK_BUFF_SIZE - SCM_PORT(port)->pb_used;
}

scm_local_func ssize_t
scm_port_read_from_pushback_buf(ScmObj port, void *buf, size_t size)
{
  size_t n;
  void *head;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  n = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;
  head = scm_port_pushback_buff_head(port);
  memcpy(buf, head, n);
  SCM_PORT(port)->pb_used -= n;

  return (ssize_t)n;
}

scm_local_func ssize_t
scm_port_read_line_from_pushback_buf(ScmObj port,
                                     void *buf, size_t size, bool *lf_exists_p)
{
  size_t n;
  ssize_t lf;
  void *head;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  n = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;
  head = scm_port_pushback_buff_head(port);

  lf = scm_port_size_up_to_lf(port, head, n);
  if (lf < 0) return -1;

  if (lf > 0) {
    n = (size_t)lf;
    *lf_exists_p = true;
  }
  else {
    *lf_exists_p = false;
  }

  memcpy(buf, head, n);
  SCM_PORT(port)->pb_used -= n;

  return (ssize_t)n;
}

scm_local_func ssize_t
scm_port_read_char_from_pushback_buf(ScmObj port, scm_char_t *chr)
{
  const ScmEncVirtualFunc *vf;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  vf = SCM_ENCODING_VFUNC(SCM_PORT(port)->encoding);

  ssize_t rslt = vf->char_width(scm_port_pushback_buff_head(port),
                                SCM_PORT(port)->pb_used);

  if (rslt < 0) {
    /* TODO: error handling (illegal sequnce) */
    return -1;
  }
  else if (rslt == 0) {
    return 0;
  }
  else {
    scm_port_read_from_pushback_buf(port, chr->bytes, (size_t)rslt);
    return rslt;
  }
}

enum { WAIT_ALL, DONT_WAIT, BREAK_LF };

scm_local_func ssize_t
scm_port_read_buf(ScmObj port, void *buf, size_t size, int mode)
{
  size_t nr;
  ssize_t lf;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);
  scm_assert(SCM_PORT(port)->buffer_mode != SCM_PORT_BUF_NONE);

  nr = 0;
  do {
    size_t n;
    ssize_t rslt;

    if (scm_port_buffer_empty_p(port)) {
      rslt = scm_port_read_into_buffer(port);
      if (rslt < 0) return -1; // error
      if (rslt == 0) break;
    }

    n = size - nr;
    lf = 0;

    if (mode == BREAK_LF) {
      if (SCM_PORT(port)->used - SCM_PORT(port)->pos < n)
        n = SCM_PORT(port)->used - SCM_PORT(port)->pos;

      lf = scm_port_size_up_to_lf(port,
                                  SCM_PORT(port)->buffer + SCM_PORT(port)->pos,
                                  n);
      if (lf < 0) return -1;
      if (lf > 0) n = (size_t)lf;
    }

    rslt = scm_port_read_from_buffer(port, (uint8_t *)buf + nr, (size_t)n);
    if (rslt < 0) return -1; // error
    nr += (size_t)rslt;

    if (nr >= size) break;

  } while (mode == WAIT_ALL
           || (mode == DONT_WAIT && scm_port_ready_p(port))
           || (mode == BREAK_LF && lf == 0));

  return (ssize_t)nr;
}

scm_local_func ssize_t
scm_port_read_line_nonbuf(ScmObj port, void *buf, size_t size)
{
  /* provisional implementation */
  /* 現状、LF 改行にしか対応していない */
  const ScmEncVirtualFunc *vf;
  scm_char_t lf;
  uint8_t *p;
  size_t len, pl;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);
  scm_assert(SCM_PORT(port)->buffer_mode == SCM_PORT_BUF_NONE);

  if (SCM_PORT(port)->eof_received_p) return 0;

  vf = SCM_ENCODING_VFUNC(SCM_PORT(port)->encoding);
  lf = SCM_ENCODING_CONST_LF_CHAR(SCM_PORT(port)->encoding);

  p = buf;
  pl = 0;
  for (len = 0; len < size; len++) {
    uint8_t c;
    ssize_t rslt = scm_io_read(SCM_PORT(port)->io, &c, sizeof(c));
    if (rslt < 0) return -1;
    if (rslt == 0) {
      SCM_PORT(port)->eof_received_p = true;
      break;
    }

    p[pl++] = c;

    ssize_t w = vf->char_width(p, pl);
    if (w < 0) {
      return -1;                /* TODO: error handling (illegal sequnce) */
    }
    else if (w > 0) {
      if (memcmp(lf.bytes, p, (size_t)w) == 0)
        return (ssize_t)len + 1;
      p += pl;
      pl = 0;
    }
  }

  return (ssize_t)len;
}

scm_local_func ssize_t
scm_port_read_nonbuf(ScmObj port, void *buf, size_t size)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);
  scm_assert(SCM_PORT(port)->buffer_mode == SCM_PORT_BUF_NONE);

  if (SCM_PORT(port)->eof_received_p)
    return 0;

  return scm_io_read(SCM_PORT(port)->io, buf, size);
}

scm_local_func ssize_t
scm_port_read_into_pushback_buf(ScmObj port, size_t size)
{
  ssize_t ret;
  uint8_t buf[SCM_PORT_PUSHBACK_BUFF_SIZE];

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_readable_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= scm_port_pushback_buff_unused(port));

  memcpy(buf, scm_port_pushback_buff_head(port), SCM_PORT(port)->pb_used);

  switch (SCM_PORT(port)->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE:
    ret = scm_port_read_buf(port,
                            buf + SCM_PORT(port)->pb_used, size, WAIT_ALL);
    break;
  case SCM_PORT_BUF_MODEST:
    ret = scm_port_read_buf(port,
                            buf + SCM_PORT(port)->pb_used, size, DONT_WAIT);
    break;
  case SCM_PORT_BUF_NONE:
    ret = scm_port_read_nonbuf(port,
                               buf + SCM_PORT(port)->pb_used, size);
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    ret = -1; /* must not happen */
    break;
  }

  if (ret < 0 || ret == 0) return ret;

  memcpy(scm_port_pushback_buff_head(port) - (size_t)ret,
         buf, SCM_PORT(port)->pb_used + (size_t)ret);
  SCM_PORT(port)->pb_used += (size_t)ret;

  return ret;
}

scm_local_func ssize_t
scm_port_write_buf(ScmObj port, const void *buf, size_t size)
{
  size_t i, n;
  ssize_t lf;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);
  scm_assert(scm_port_writable_p(port));
  scm_assert(!scm_port_closed_p(port));

  for (i = 0; i < size; i += n) {
    n = SCM_PORT(port)->capacity - SCM_PORT(port)->pos;
    n = (n < size - i) ? n : size - i;

    lf = 0;
    if (SCM_PORT(port)->buffer_mode == SCM_PORT_BUF_LINE) {
      lf = scm_port_size_up_to_lf(port, (const uint8_t *)buf + i, n);
      if (lf < 0) return -1;
      if (lf > 0) n = (size_t)lf;
    }

    memcpy(SCM_PORT(port)->buffer + SCM_PORT(port)->pos,
           (const uint8_t *)buf + i, n);
    SCM_PORT(port)->pos += n;
    if (SCM_PORT(port)->pos > SCM_PORT(port)->used)
      SCM_PORT(port)->used = SCM_PORT(port)->pos;

    if (lf > 0 || SCM_PORT(port)->used >= SCM_PORT(port)->capacity)
      scm_port_flush(port);
  }

  return (ssize_t)i;
}

void
scm_port_initialize(ScmObj port, ScmIO *io,
                    SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode,
                    SCM_ENCODING_T enc)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  SCM_PORT(port)->attr = attr;
  SCM_PORT(port)->io = io;
  SCM_PORT(port)->buffer_mode = SCM_PORT_BUF_NONE;
  SCM_PORT(port)->buffer = NULL;
  SCM_PORT(port)->capacity = 0;
  SCM_PORT(port)->pos = 0;
  SCM_PORT(port)->used = 0;
  SCM_PORT(port)->closed_p = false;
  SCM_PORT(port)->eof_received_p = false;
  SCM_PORT(port)->pb_used = 0;
  SCM_PORT(port)->encoding = enc;

  scm_port_init_buffer(port, buf_mode); /* TODO: caller へのエラーの伝搬 */
}

void
scm_port_finalize(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  scm_port_flush(port);
  scm_port_close(port);
  scm_io_end(SCM_PORT(port)->io);
  scm_capi_free(SCM_PORT(port)->buffer);
}

ScmObj
scm_port_new(SCM_CAPI_MEM_TYPE_T mtype,
             ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode,
             SCM_ENCODING_T enc)
{
  ScmObj port = SCM_OBJ_INIT;

  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  port = scm_capi_mem_alloc(&SCM_PORT_TYPE_INFO, mtype);

  scm_port_initialize(port, io, attr, buf_mode, enc);

  return port;
}

ScmObj
scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_new(SCM_CAPI_MEM_HEAP,
                      io, attr | SCM_PORT_ATTR_READABLE, buf_mode,
                      SCM_ENCODING_ASCII);
}

ScmObj
scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_new(SCM_CAPI_MEM_HEAP,
                      io, attr | SCM_PORT_ATTR_WRITABLE, buf_mode,
                      SCM_ENCODING_ASCII);
}

ScmObj
scm_port_open_input_fd(int fd,  SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  scm_assert(fd >= 0);

  io = (ScmIO *)scm_fileio_new(fd);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_input(io,
                             SCM_PORT_ATTR_FILE,
                             buf_mode);
}

ScmObj
scm_port_open_output_fd(int fd, SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  scm_assert(fd >= 0);

  io = (ScmIO *)scm_fileio_new(fd);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_output(io,
                              SCM_PORT_ATTR_FILE,
                              buf_mode);
}


ScmObj
scm_port_open_input_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  scm_assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_RDONLY, 0);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_input(io,
                             SCM_PORT_ATTR_FILE,
                             buf_mode);
}

ScmObj
scm_port_open_output_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  ScmIO *io;

  scm_assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_WRONLY | O_CREAT, 00644);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_output(io,
                              SCM_PORT_ATTR_FILE,
                              buf_mode);
}

ScmObj
scm_port_open_input_string(const void *string, size_t size)
{
  ScmIO *io;

  scm_assert(string != NULL);

  io = (ScmIO *)scm_stringio_new(string, size);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_input(io,
                             SCM_PORT_ATTR_STRING,
                             SCM_PORT_BUF_DEFAULT);
}

ScmObj
scm_port_open_output_string(void)
{
  ScmIO *io;

  io = (ScmIO *)scm_stringio_new(NULL, 0);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_output(io,
                              SCM_PORT_ATTR_STRING,
                              SCM_PORT_BUF_DEFAULT);
}

bool
scm_port_readable_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_READABLE);
}

bool
scm_port_writable_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_WRITABLE);
}

bool
scm_port_file_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_FILE);
}

bool
scm_port_string_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_STRING);
}

bool
scm_port_closed_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return SCM_PORT(port)->closed_p;
}

bool
scm_port_ready_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_readable_p(port));

  if (SCM_PORT(port)->pb_used > 0) {
    return true;
  }
  else if (scm_port_buffer_empty_p(port)) {
    if (SCM_PORT(port)->eof_received_p) {
      return true;
    }
    else {
      int ret = scm_io_ready_p(SCM_PORT(port)->io);
      if (ret < 0)
        ;                         /* TODO: error handling */
      return (ret == 0) ? false : true;
    }
  }
  else {
    return true;
  }
}

int
scm_port_flush(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_writable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  if (SCM_PORT(port)->used > 0) {
    scm_io_write(SCM_PORT(port)->io,
                 SCM_PORT(port)->buffer, SCM_PORT(port)->used);
    SCM_PORT(port)->used = 0;
    SCM_PORT(port)->pos = 0;
  }

  return 0;
}

int
scm_port_close(ScmObj port)
{
  int ret;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) return 0;

  if (scm_port_writable_p(port))
    scm_port_flush(port);

  ret = scm_io_close(SCM_PORT(port)->io);
  if (ret < 0) return ret;

  SCM_PORT(port)->closed_p = true;
  return 0;
}


ssize_t
scm_port_read(ScmObj port, void *buf, size_t size)
{
  void *bp;
  size_t sz;
  ssize_t ret, pb_nr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  pb_nr = scm_port_read_from_pushback_buf(port, buf, size);
  if (pb_nr < 0) return -1;
  if ((size_t)pb_nr >= size) return pb_nr;

  bp = (uint8_t *)buf + pb_nr;
  sz = size - (size_t)pb_nr;

  switch (SCM_PORT(port)->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE:
    ret = scm_port_read_buf(port, bp, sz, WAIT_ALL);
    break;
  case SCM_PORT_BUF_MODEST:
    ret = scm_port_read_buf(port, bp, sz, DONT_WAIT);
    break;
  case SCM_PORT_BUF_NONE:
    ret = scm_port_read_nonbuf(port, bp, sz);
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    ret = -1; /* must not happen */
    break;
  }

  if (ret == 0)
    SCM_PORT(port)->eof_received_p = false;

  return (ret < 0) ? ret : ret + pb_nr;
}

ssize_t
scm_port_read_line(ScmObj port, void *buf, size_t size)
{
  /* provitional impelmentation */
  /* 現状、ascii コードの LF 改行にしか対応していない */
  void *bp;
  size_t sz;
  ssize_t ret, pb_nr;
  bool lf_exists_p = false;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  pb_nr = scm_port_read_line_from_pushback_buf(port, buf, size, &lf_exists_p);
  if (pb_nr < 0) return -1;
  if (lf_exists_p || (size_t)pb_nr >= size) return pb_nr;

  bp = (uint8_t *)buf + pb_nr;
  sz = size - (size_t)pb_nr;

  switch (SCM_PORT(port)->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE: /* fall through */
  case SCM_PORT_BUF_MODEST:
    ret = scm_port_read_buf(port, bp, sz, BREAK_LF);
    break;
  case SCM_PORT_BUF_NONE:
    ret = scm_port_read_line_nonbuf(port, bp, sz);
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    ret = -1; /* must not happen */
    break;
  }

  if (ret == 0)
    SCM_PORT(port)->eof_received_p = false;

  return (ret < 0) ? ret : ret + pb_nr;
}

ssize_t
scm_port_read_char(ScmObj port, scm_char_t *chr)
{
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  while ((rslt = scm_port_read_char_from_pushback_buf(port, chr)) == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;
    else if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0)
        return -1;                /* TODO: error handling (illegal sequence) */
      else
        return 0;
    }
  }

  return rslt;
}

ssize_t
scm_port_pushback(ScmObj port, const void *buf, size_t size)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;
  if (size > scm_port_pushback_buff_unused(port)) return -1;

  memcpy(scm_port_pushback_buff_head(port) - size, buf, size);
  SCM_PORT(port)->pb_used += size;

  return (ssize_t)size;
}

ssize_t
scm_port_pushback_char(ScmObj port, const scm_char_t *chr)
{
  const ScmEncVirtualFunc *vf;
  ssize_t w;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  vf = SCM_ENCODING_VFUNC(SCM_PORT(port)->encoding);
  w = vf->char_width(chr->bytes, sizeof(scm_char_t));

  if (w <= 0)
    return -1;                  /* TODO: error handling (illegal sequnce) */

  memcpy(scm_port_pushback_buff_head(port) - (size_t)w, chr->bytes, (size_t)w);
  SCM_PORT(port)->pb_used += (size_t)w;

  return w;
}

ssize_t
scm_port_peek(ScmObj port, void *buf, size_t size)
{
  size_t npeek;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;
  if (size > SCM_PORT_PUSHBACK_BUFF_SIZE) return -1;

  if (size > SCM_PORT(port)->pb_used) {
    ssize_t ret =
      scm_port_read_into_pushback_buf(port, size - SCM_PORT(port)->pb_used);
    if (ret < 0) return -1;
  }

  npeek = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;

  memcpy(buf, scm_port_pushback_buff_head(port), npeek);

  return (ssize_t)npeek;
}

ssize_t
scm_port_peek_char(ScmObj port, scm_char_t *chr)
{
  const ScmEncVirtualFunc *vf;
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_readable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  vf = SCM_ENCODING_VFUNC(SCM_PORT(port)->encoding);

  while ((rslt = vf->char_width(scm_port_pushback_buff_head(port),
                                SCM_PORT(port)->pb_used)) == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;
    else if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0)
        return -1;                /* TODO: error handling (illegal sequence) */
      else
        return 0;
    }
  }

  if (rslt < 0) return rslt;

  memcpy(chr->bytes, scm_port_pushback_buff_head(port), (size_t)rslt);

  return rslt;
}

ssize_t
scm_port_write(ScmObj port, const void *buf, size_t size)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (!scm_port_writable_p(port)) return -1;
  if (scm_port_closed_p(port)) return -1;

  switch (SCM_PORT(port)->buffer_mode) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE: /* fall through */
  case SCM_PORT_BUF_MODEST:
    return scm_port_write_buf(port, buf, size);
    break;
  case SCM_PORT_BUF_NONE:
    return scm_io_write(SCM_PORT(port)->io, buf, size);
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    return -1; /* must not happen */
    break;
  }
}

int
scm_port_seek(ScmObj port, off_t offset, int whence)
{
  off_t off, rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) return -1;

  if (scm_port_writable_p(port))
    scm_port_flush(port);

  switch (whence) {
  case SEEK_SET:
    rslt = scm_io_seek(SCM_PORT(port)->io, offset, whence);
    break;
  case SEEK_CUR:
    off = offset - (ssize_t)(SCM_PORT(port)->used - SCM_PORT(port)->pos);
    rslt = scm_io_seek(SCM_PORT(port)->io, off, whence);
    break;
  case SEEK_END:
    rslt = scm_io_seek(SCM_PORT(port)->io, offset, whence);
    break;
  default:
    return -1;
    break;
  }

  if (rslt < 0) return (int)rslt;

  if (!scm_port_writable_p(port))
    scm_port_clear_buffer(port);

  SCM_PORT(port)->eof_received_p = false;

  return 0;
}

void *
scm_port_string_buffer(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) return NULL;

  return (void *)scm_stringio_buffer((ScmStringIO *)SCM_PORT(port)->io);
}

ssize_t
scm_port_string_buffer_length(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) return -1;

  return (ssize_t)scm_stringio_length((ScmStringIO *)SCM_PORT(port)->io);
}

void
scm_port_gc_finalize(ScmObj obj)
{
  scm_port_finalize(obj);
}
