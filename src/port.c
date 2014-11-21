#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <iconv.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/reference.h"
#include "scythe/api.h"
#include "scythe/port.h"
#include "scythe/encoding.h"
#include "scythe/impl_utils.h"

/* Note: size_t 型の引数が SSIZE_MAX 以下であることを assert でチェックしてい
 *       るのは、read/write の戻り値が ssize_t 型であるため。 */

#define BIT_IS_SET(val, bit) (((val) & (bit)) ? true : false)

#define SCM_STRINGIO_INIT_BUF_SIZE 64
#define SCM_CHARCONVIO_UC_BUF_SIZE 64
#define SCM_CHARCONVIO_CO_BUF_SIZE 8
#define SCM_PORT_DEFAULT_BUF_SIZE 256


ScmTypeInfo SCM_PORT_TYPE_INFO = {
  .name                = "port",
  .flags               = SCM_TYPE_FLG_MMO,
  .obj_print_func      = NULL,
  .obj_size            = sizeof(ScmPort),
  .gc_ini_func         = scm_port_gc_initialize,
  .gc_fin_func         = scm_port_gc_finalize,
  .gc_accept_func      = NULL,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

void
scm_io_initialize(ScmIO *io,
                  ScmIOFinFunc fin,
                  ScmIOReadFunc read,
                  ScmIOWriteFunc write,
                  ScmIOSeekFunc seek,
                  ScmIOPosFunc pos,
                  ScmIOCloseFunc close,
                  ScmIOReadyPFunc readyp,
                  ScmIOBuffModeFunc buf_mode,
                  ScmIOBlkSizeFunc blk_size,
                  ScmIOFlushFunc flush,
                  ScmIOClearFunc clear,
                  ScmIOLowerFunc lower)
{
  scm_assert(io != NULL);

  io->fin_func = fin;
  io->read_func = read;
  io->write_func = write;
  io->seek_func = seek;
  io->pos_func = pos;
  io->close_func = close;
  io->ready_p_func = readyp;
  io->default_buf_mode_func = buf_mode;
  io->blk_size_func = blk_size;
  io->flush_func = flush;
  io->clear_func = clear;
  io->lower_func = lower;
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

  return ((io->read_func!= NULL) ?
          io->read_func(io, buf, size) : (ssize_t)size);
}

ssize_t
scm_io_write(ScmIO *io, const void *buf, size_t size)
{
  scm_assert(io != NULL);

  return ((io->write_func != NULL) ?
          io->write_func(io, buf, size) : (ssize_t) size);
}


off_t
scm_io_seek(ScmIO *io, off_t offset, int whence)
{
  scm_assert(io != NULL);

  return (io->seek_func != NULL) ? io->seek_func(io, offset, whence) : 0;
}

off_t
scm_io_pos(ScmIO *io)
{
  scm_assert(io != NULL);

  return (io->pos_func != NULL) ? io->pos_func(io) : 0;
}

int
scm_io_close(ScmIO *io)
{
  scm_assert(io != NULL);

  return (io->close_func != NULL) ? io->close_func(io) : 0;
}

int
scm_io_ready_p(ScmIO *io)
{
  scm_assert(io != NULL);

  return (io->ready_p_func != NULL) ? io->ready_p_func(io) : 1;
}

int
scm_io_buffer_mode(ScmIO *io, SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode)
{
  scm_assert(io != NULL);

  if (io->default_buf_mode_func != NULL)
    return io->default_buf_mode_func(io, im, mode);
  else {
    *mode = SCM_PORT_BUF_FULL;
    return 0;
  }
}

ssize_t
scm_io_block_size(ScmIO *io)
{
  scm_assert(io != NULL);
  return (io->blk_size_func != NULL) ? io->blk_size_func(io) : 0;
}

int
scm_io_flush(ScmIO *io)
{
  scm_assert(io != NULL);
  return (io->flush_func != NULL) ? io->flush_func(io) : 0;
}

int
scm_io_clear(ScmIO *io)
{
  scm_assert(io != NULL);
  return (io->clear_func != NULL) ? io->clear_func(io) : 0;
}

ScmIO *
scm_io_lower(ScmIO *io)
{
  scm_assert(io != NULL);
  return (io->lower_func != NULL) ? io->lower_func(io) : NULL;
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
                    (ScmIOPosFunc)scm_fileio_pos,
                    (ScmIOCloseFunc)scm_fileio_close,
                    (ScmIOReadyPFunc)scm_fileio_ready_p,
                    (ScmIOBuffModeFunc)scm_fileio_buffer_mode,
                    (ScmIOBlkSizeFunc)scm_fileio_block_size,
                    (ScmIOFlushFunc)NULL,
                    (ScmIOClearFunc)NULL,
                    (ScmIOLowerFunc)NULL);
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
  if (fd < 0) {
    /* TODO; change error message */
    scm_capi_file_error("system call error: open", 0);
    return NULL;
  }

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
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: read", 0);
    return n;
  }

  return n;
}

ssize_t
scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size)
{
  ssize_t n;
  size_t rest;
  const char *p;

  scm_assert(fileio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  rest = size;
  p = buf;
  while (rest > 0) {
    SCM_SYSCALL(n, write(fileio->fd, p, rest));
    if (n < 0) {
      /* TODO; change error message */
      scm_capi_error("system call error: write", 0);
      return n;
    }

    rest -= (size_t)n;
    p += n;
  }

  return (ssize_t)(size - rest);
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
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: select", 0);
    return -1;
  }

  return (n > 0) ? 1 : 0;
}

off_t
scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence)
{
  off_t n;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(n, lseek(fileio->fd, offset, whence));
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: lseek", 0);
    return n;
  }

  return n;
}

off_t
scm_fileio_pos(ScmFileIO *fileio)
{
  off_t n;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(n, lseek(fileio->fd, 0, SEEK_CUR));
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: lseek", 0);
    return n;
  }

  return n;
}

int
scm_fileio_close(ScmFileIO *fileio)
{
  int n;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(n, close(fileio->fd));
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: close", 0);
    return n;
  }

  return n;
}

int
scm_fileio_buffer_mode(ScmFileIO *fileio, SCM_IO_MODE_T im,
                       SCM_PORT_BUF_T *mode)
{
  struct stat st;
  int ret;

  scm_assert(fileio != NULL);
  scm_assert(mode != NULL);

  if (isatty(fileio->fd)) {
    *mode = (im == SCM_IO_MODE_READ) ? SCM_PORT_BUF_FULL : SCM_PORT_BUF_LINE;
    return 0;
  }

  SCM_SYSCALL(ret, fstat(fileio->fd, &st));
  if (ret < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: fstat", 0);
    return -1;
  }

  if (S_ISFIFO(st.st_mode))
    *mode = (im == SCM_IO_MODE_READ) ? SCM_PORT_BUF_FULL : SCM_PORT_BUF_NONE;
  else if (S_ISSOCK(st.st_mode))
    *mode = (im == SCM_IO_MODE_READ) ? SCM_PORT_BUF_MODEST : SCM_PORT_BUF_NONE;
  else if (S_ISCHR(st.st_mode))
    *mode = (im == SCM_IO_MODE_READ) ? SCM_PORT_BUF_FULL : SCM_PORT_BUF_NONE;
  else
    *mode = SCM_PORT_BUF_FULL;

  return 0;
}

ssize_t
scm_fileio_block_size(ScmFileIO *fileio)
{
  int rslt;
  struct stat stat;

  scm_assert(fileio != NULL);

  SCM_SYSCALL(rslt, fstat(fileio->fd, &stat));
  if (rslt < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: fstat", 0);
    return -1;
  }

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
                    (ScmIOPosFunc)scm_stringio_pos,
                    (ScmIOCloseFunc)scm_stringio_close,
                    (ScmIOReadyPFunc)scm_stringio_ready_p,
                    (ScmIOBuffModeFunc)scm_stringio_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL,
                    (ScmIOFlushFunc)NULL,
                    (ScmIOClearFunc)NULL,
                    (ScmIOLowerFunc)NULL);
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
  int rslt;

  scm_assert(strio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (size > SSIZE_MAX - strio->pos) return -1;

  rslt = scm_stringio_expand_buffer(strio, strio->pos + size);
  if (rslt < 0) return -1;
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
    if (SSIZE_MAX - (ssize_t)strio->pos < offset) {
      scm_capi_error("failed to seek: offset is out of range", 0);
      return -1;
    }
    pos = (ssize_t)strio->pos + offset;
    break;
  case SEEK_END:
    if (SSIZE_MAX - (ssize_t)strio->length < offset) {
      scm_capi_error("failed to seek: offset is out of range", 0);
      return -1;
    }
    pos = (ssize_t)strio->length + offset;
    break;
  default:
    scm_assert(0);
    break;
  }

  if (pos < 0) {
    /* TODO; change error message */
    scm_capi_error("offset is out of range", 0);
    return -1;
  }

  if (scm_stringio_expand_buffer(strio, (size_t)pos) < 0)
    return -1;

  strio->pos = (size_t)pos;

  return pos;
}

off_t
scm_stringio_pos(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return (off_t)strio->pos;
}

int
scm_stringio_close(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  /* nothing to do  */

  return 0;
}

int
scm_stringio_buffer_mode(ScmStringIO *strio, SCM_IO_MODE_T im,
                         SCM_PORT_BUF_T *mode)
{
  scm_assert(strio != NULL);
  scm_assert(mode != NULL);

  *mode = SCM_PORT_BUF_NONE;

  return 0;
}

extern inline char *
scm_stringio_buffer(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return strio->string;
}

extern inline size_t
scm_stringio_length(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  return strio->length;
}

char *
scm_stringio_chuck_buffer(ScmStringIO *strio)
{
  char *p;

  scm_assert(strio != NULL);

  p = strio->string;

  strio->string = NULL;
  strio->capacity = 0;
  strio->length = 0;
  strio->pos = 0;

  return p;
}


static int
scm_bufferedio_init_buffer(ScmBufferedIO *bufio, ScmIO *source)
{
  ssize_t s;

  scm_assert(bufio != NULL);
  scm_assert(source != NULL);

  bufio->pos = 0;
  bufio->used = 0;

  s = scm_io_block_size(source);
  if (s > 0)
    bufio->capacity = (size_t)s;
  else if (s == 0)
    bufio->capacity = SCM_PORT_DEFAULT_BUF_SIZE;
  else
    return -1;

  bufio->buffer = scm_capi_malloc(bufio->capacity);
  if (bufio->buffer == NULL) return -1;

  return 0;
}


ScmBufferedIO *
scm_bufferedio_new(ScmIO *io)
{
  ScmBufferedIO *bufio;
  int r;

  bufio = scm_capi_malloc(sizeof(ScmBufferedIO));
  if (bufio == NULL) return NULL;

  scm_io_initialize((ScmIO *)bufio,
                    (ScmIOFinFunc)scm_bufferedio_end,
                    (ScmIOReadFunc)scm_bufferedio_read,
                    (ScmIOWriteFunc)scm_bufferedio_write,
                    (ScmIOSeekFunc)scm_bufferedio_seek,
                    (ScmIOPosFunc)scm_bufferedio_pos,
                    (ScmIOCloseFunc)scm_bufferedio_close,
                    (ScmIOReadyPFunc)scm_bufferedio_ready_p,
                    (ScmIOBuffModeFunc)scm_bufferedio_buffer_mode,
                    (ScmIOBlkSizeFunc)scm_bufferedio_block_size,
                    (ScmIOFlushFunc)scm_bufferedio_flush,
                    (ScmIOClearFunc)scm_bufferedio_clear,
                    (ScmIOLowerFunc)scm_bufferedio_lower);

  bufio->io = io;
  bufio->eof_received_p = false;
  bufio->stat = SCM_BUFFEREDIO_ST_NONE;

  r = scm_bufferedio_init_buffer(bufio, io);
  if (r < 0) {
    scm_capi_free(bufio);
    return NULL;
  }

  return bufio;
}

void
scm_bufferedio_end(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  scm_io_end(bufio->io);

  scm_capi_free(bufio->buffer);
  scm_capi_free(bufio);
}

ssize_t
scm_bufferedio_read(ScmBufferedIO *bufio, void *buf, size_t size)
{
  size_t nread, rest;
  ssize_t rslt;

  scm_assert(bufio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  nread = 0;
  rest = size;

  if (size == 0)
    return 0;

  if (bufio->stat == SCM_BUFFEREDIO_ST_WRITE) {
    int r = scm_bufferedio_flush(bufio);
    if (r < 0) return -1;
  }

  if (bufio->eof_received_p)
    goto end;

  if (bufio->used > bufio->pos) {
    size_t n;
    if (size > bufio->used - bufio->pos)
      n = bufio->used - bufio->pos;
    else
      n = size;
    memcpy(buf, bufio->buffer + bufio->pos, n);
    nread += n;
    rest -= n;

    bufio->pos += n;
    if (bufio->pos >= bufio->used)
      bufio->pos = bufio->used = 0;

    if (nread >= size) goto end;
  }

  if (rest >= bufio->capacity) {
    size_t n = bufio->capacity * (rest / bufio->capacity);
    rslt = scm_io_read(bufio->io, (char *)buf + nread, n);
    if (rslt < 0)
      return -1;
    else if (rslt == 0)
      bufio->eof_received_p = true;

    nread += (size_t)rslt;
    rest -= (size_t)rslt;

    if (nread >= size || (size_t)rslt < n) goto end;
  }

  rslt = scm_io_read(bufio->io, bufio->buffer, bufio->capacity);
  if (rslt < 0)
    return -1;
  else if (rslt == 0)
    bufio->eof_received_p = true;
  else {
    size_t n;
    if (rest > (size_t)rslt)
      n = (size_t)rslt;
    else
      n = rest;
    memcpy((char *)buf + nread, bufio->buffer, n);
    bufio->used = (size_t)rslt;
    bufio->pos = n;
    nread += n;
    rest -= n;
  }

 end:
  if (nread == 0 && bufio->eof_received_p)
    bufio->eof_received_p = false;

  if (bufio->used > bufio->pos)
    bufio->stat = SCM_BUFFEREDIO_ST_READ;

  return (ssize_t)nread;
}

ssize_t
scm_bufferedio_write(ScmBufferedIO *bufio, void *buf, size_t size)
{
  size_t n, nwrite, rest;
  ssize_t rslt;

  scm_assert(bufio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  nwrite = 0;
  rest = size;

  if (bufio->stat == SCM_BUFFEREDIO_ST_READ) {
    off_t r = scm_bufferedio_seek(bufio, 0, SEEK_CUR);
    if (r < 0) return -1;
  }

  if (bufio->pos > 0) {
    if (rest > bufio->capacity - bufio->pos)
      n = bufio->capacity - bufio->pos;
    else
      n = rest;

    memcpy(bufio->buffer + bufio->pos, (char *)buf + nwrite, n);
    bufio->pos += n;
    if (bufio->pos > bufio->used)
      bufio->used = bufio->pos;
    nwrite += n;
    rest -= n;

    if (bufio->capacity == bufio->pos) {
      rslt = scm_bufferedio_flush(bufio);
      if (rslt < 0) return -1;
    }

    if (nwrite >= size) goto end;
  }

  if (rest >= bufio->capacity) {
    n = bufio->capacity * (rest / bufio->capacity);
    rslt = scm_io_write(bufio->io, (char *)buf + nwrite, n);
    if (rslt < 0) return -1;

    nwrite += (size_t)rslt;
    rest -= (size_t)rslt;

    if (nwrite >= size || (size_t)rslt < n) goto end;
  }

  memcpy(bufio->buffer + bufio->pos, (char *)buf + nwrite, rest);
  bufio->pos += rest;
  if (bufio->pos > bufio->used)
    bufio->used = bufio->pos;
  nwrite += rest;
  rest = 0;

 end:
  bufio->eof_received_p = false;

  if (bufio->pos > 0)
    bufio->stat = SCM_BUFFEREDIO_ST_WRITE;

  return (ssize_t)nwrite;
}

int
scm_bufferedio_ready_p(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  if (bufio->pos > 0)
    return 1;
  else if (bufio->eof_received_p)
    return 1;
  else
    return scm_io_ready_p(bufio->io);
}

off_t
scm_bufferedio_seek(ScmBufferedIO *bufio, off_t offset, int whence)
{
  off_t off;
  int rslt;

  if (bufio->stat == SCM_BUFFEREDIO_ST_WRITE) {
    rslt = scm_bufferedio_flush(bufio);
    if (rslt < 0) return -1;
  }

  if (whence == SEEK_CUR && bufio->stat == SCM_BUFFEREDIO_ST_READ)
    off = offset - (off_t)(bufio->used - bufio->pos);
  else
    off = offset;

  off = scm_io_seek(bufio->io, off, whence);
  if (off < 0) return -1;

  rslt = scm_bufferedio_clear(bufio);
  if (rslt < 0) return -1;

  return off;
}

off_t
scm_bufferedio_pos(ScmBufferedIO *bufio)
{
  off_t pos;

  scm_assert(bufio != NULL);

  pos = scm_io_pos(bufio->io);
  if (pos < 0) return -1;

  if (bufio->stat == SCM_BUFFEREDIO_ST_READ)
    return pos - (off_t)(bufio->used - bufio->pos);
  else if (bufio->stat == SCM_BUFFEREDIO_ST_WRITE)
    return pos + (off_t)bufio->pos;
  else
    return pos;
}

int
scm_bufferedio_close(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  return scm_io_close(bufio->io);
}

int
scm_bufferedio_buffer_mode(ScmBufferedIO *bufio,
                           SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode)
{
  scm_assert(bufio != NULL);
  scm_assert(mode != NULL);

  return scm_io_buffer_mode(bufio->io, im, mode);
}

ssize_t
scm_bufferedio_block_size(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  return scm_io_block_size(bufio->io);
}

int
scm_bufferedio_flush(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  if (bufio->stat == SCM_BUFFEREDIO_ST_WRITE) {
    ssize_t rslt = scm_io_write(bufio->io, bufio->buffer, bufio->pos);
    if (rslt < 0) return -1;
    bufio->used = 0;
    bufio->pos = 0;
  }

  bufio->stat = SCM_BUFFEREDIO_ST_NONE;

  return scm_io_flush(bufio->io);
}

int
scm_bufferedio_clear(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  bufio->pos = bufio->used = 0;
  bufio->eof_received_p = false;
  bufio->stat = SCM_BUFFEREDIO_ST_NONE;

  return scm_io_clear(bufio->io);
}

ScmIO *
scm_bufferedio_lower(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  return bufio->io;
}

static int
scm_charconvio_init(ScmCharConvIO *ccio,
                    const char *incode, const char *extcode)
{
  scm_assert(ccio != NULL);
  scm_assert(incode != NULL);
  scm_assert(extcode != NULL);

  ccio->rcd = iconv_open(incode, extcode);
  if (ccio->rcd == (void *)-1) return -1;

  ccio->wcd = iconv_open(extcode, incode);
  if (ccio->wcd == (void *)-1) {
    iconv_close(ccio->rcd);
    return -1;
  }

  ccio->unconverted = scm_capi_malloc(SCM_CHARCONVIO_UC_BUF_SIZE);
  if (ccio->unconverted == NULL) {
    iconv_close(ccio->rcd);
    iconv_close(ccio->wcd);
    return -1;
  }

  ccio->converted = scm_capi_malloc(SCM_CHARCONVIO_CO_BUF_SIZE);
  if (ccio->converted == NULL) {
    iconv_close(ccio->rcd);
    iconv_close(ccio->wcd);
    scm_capi_free(ccio->unconverted);
    return -1;
  }

  ccio->uc_used = 0;
  ccio->uc_pos = 0;
  ccio->uc_incomplete_p = false;
  ccio->co_used = 0;
  ccio->co_pos = 0;
  ccio->co_ucsize = 0;
  ccio->eof_received_p = false;

  return 0;
}

static ssize_t
scm_charconvio_read_from_io(ScmCharConvIO *ccio)
{
  ssize_t rslt;

  scm_assert(ccio != NULL);

  if (ccio->eof_received_p)
    return 0;

  rslt = scm_io_read(ccio->io,
                     ccio->unconverted + ccio->uc_used,
                     SCM_CHARCONVIO_UC_BUF_SIZE - ccio->uc_used);
  if (rslt < 0) return -1;

  if (rslt == 0) {
    ccio->eof_received_p = true;
    if (ccio->uc_incomplete_p) return -1;
       /* 文字コードシーケンスが不完全なまま EOF を read */
  }
  else {
    ccio->uc_used += (size_t)rslt;
    ccio->uc_incomplete_p = false;
  }

  return rslt;
}

static ssize_t
scm_charconvio_conv_read(ScmCharConvIO *ccio, void *buf, size_t size,
                         size_t *cnsm, int *err)
{
  char *inb, *outb;
  size_t ins, outs;
  size_t rslt;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  inb = ccio->unconverted + ccio->uc_pos;
  ins = ccio->uc_used - ccio->uc_pos;

  outb = buf;
  outs = size;

  rslt = iconv(ccio->rcd, &inb, &ins, &outb, &outs);
  if (rslt == (size_t)-1) {
    if (err != NULL) *err = errno;

    if (errno != EINVAL && errno != E2BIG)
      return -1;

    if (cnsm != NULL) *cnsm = ccio->uc_used - ccio->uc_pos - ins;

    ccio->uc_pos += ccio->uc_used - ccio->uc_pos - ins;
    if (errno == EINVAL) {
      size_t new_used = ccio->uc_used - ccio->uc_pos;
      memmove(ccio->unconverted, ccio->unconverted + ccio->uc_pos, new_used);
      ccio->uc_pos = 0;
      ccio->uc_used = new_used;
      ccio->uc_incomplete_p = true;
    }
  }
  else {
    if (err != NULL) *err = 0;
    if (cnsm != NULL) *cnsm = ccio->uc_used - ccio->uc_pos;
    ccio->uc_pos = ccio->uc_used = 0;
  }

  return (ssize_t)(size - outs);
}

static int
scm_charconvio_read_into_cnvd(ScmCharConvIO *ccio)
{
  size_t cnsm;
  ssize_t rslt;
  int err;

  scm_assert(ccio != NULL);

  rslt = scm_charconvio_conv_read(ccio,
                                  ccio->converted, SCM_CHARCONVIO_CO_BUF_SIZE,
                                  &cnsm, &err);
  if (rslt < 0) return -1;

  /* 変換誤の 1 文字のサイズが ccio->convereted のサイズを越えるケースは想
     定外 */
  scm_assert(rslt > 0);

  ccio->co_used = (size_t)rslt;
  ccio->co_pos = 0;
  ccio->co_ucsize = cnsm;

  return 0;
}

static ssize_t
scm_charconvio_write_aux(ScmCharConvIO *ccio, const void *buf, size_t size)
{
  char outbuf[256];
  size_t rslt, ins, outs;
  ssize_t nw;
  const char *inb;
  char *outb;
  bool cont;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  inb = buf;
  ins = size;

  cont = true;

  while (cont) {
    outb = outbuf;
    outs = sizeof(outbuf);

    rslt = iconv(ccio->wcd, &inb, &ins, &outb, &outs);
    if (rslt == (size_t)-1) {
      if (errno == EILSEQ) {
        return -1;                /* illegal sequence */
      }
      else if (errno == EINVAL) {
        cont = false;
      }
      else if (errno == E2BIG) {
        /* 変換後 1 文字のバイト数が outbuf のサイズを越えるケースは想定外 */
        scm_assert(outb != outbuf);
        ;                       /* nothing to do */
      }
      else {
        return -1;              /* must not happen */
      }
    }
    else {
      cont = false;
    }

    nw = scm_io_write(ccio->io, outbuf, (size_t)(outb - outbuf));
    if (nw < 0) return -1;
  }

  return (ssize_t)(size - ins);
}

ScmCharConvIO *
scm_charconvio_new(ScmIO *io, const char *incode, const char *extcode)
{
  ScmCharConvIO *ccio;
  int rslt;

  ccio = scm_capi_malloc(sizeof(ScmCharConvIO));
  if (ccio == NULL) return NULL;

  scm_io_initialize((ScmIO *)ccio,
                    (ScmIOFinFunc)scm_charconvio_end,
                    (ScmIOReadFunc)scm_charconvio_read,
                    (ScmIOWriteFunc)scm_charconvio_write,
                    (ScmIOSeekFunc)NULL,
                    (ScmIOPosFunc)NULL,
                    (ScmIOCloseFunc)scm_charconvio_close,
                    (ScmIOReadyPFunc)scm_charconvio_ready_p,
                    (ScmIOBuffModeFunc)scm_charconvio_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL,
                    (ScmIOFlushFunc)scm_charconvio_flush,
                    (ScmIOClearFunc)scm_charconvio_clear,
                    (ScmIOLowerFunc)scm_charconvio_lower);

  ccio->io = io;

  rslt = scm_charconvio_init(ccio, incode, extcode);
  if (rslt < 0) {
    scm_capi_free(ccio);
    return NULL;
  }

  return ccio;
}

void
scm_charconvio_end(ScmCharConvIO *ccio)
{
  scm_assert(ccio != NULL);

  scm_capi_free(ccio->unconverted);
  scm_capi_free(ccio->converted);
  scm_capi_free(ccio);
}

ssize_t
scm_charconvio_read(ScmCharConvIO *ccio, void *buf, size_t size)
{
  size_t nread;
  ssize_t rslt;
  int err;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  if (size == 0)
    return 0;

  if ((ccio->eof_received_p && ccio->co_used == 0 && ccio->uc_used == 0)
      || size == 0) {
    ccio->eof_received_p = false;
    return 0;
  }

  nread = 0;

  if (ccio->co_used != 0) {
    size_t s = size;
    if (size > ccio->co_used - ccio->co_pos)
      s = ccio->co_used - ccio->co_pos;
    memcpy(buf, ccio->converted + ccio->co_pos, s);
    nread += s;

    ccio->co_pos += s;
    if (ccio->co_pos >= ccio->co_used)
      ccio->co_pos = ccio->co_used = ccio->co_ucsize = 0;
  }

  err = 0;

  while (nread < size) {
    if (ccio->uc_used == 0 || ccio->uc_incomplete_p) {
      rslt = scm_charconvio_read_from_io(ccio);
      if (rslt < 0) return -1;
      if (rslt == 0) break;
    }

    rslt = scm_charconvio_conv_read(ccio, (char *)buf + nread, size - nread,
                                    NULL, &err);
    if (rslt < 0) return -1;

    nread += (size_t)rslt;

    if (err == E2BIG) break;
  }

  if (err == E2BIG && nread < size) {
    size_t s;
    int r = scm_charconvio_read_into_cnvd(ccio);
    if (r < 0) return -1;

    s = size - nread;
    memcpy((char *)buf + nread, ccio->converted, s);
    ccio->co_pos += s;
    nread += s;
  }

  if (nread == 0)
    ccio->eof_received_p = false;

  return (ssize_t)nread;
}

ssize_t
scm_charconvio_write(ScmCharConvIO *ccio, const void *buf, size_t size)
{
  ssize_t nwrite;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  if (ccio->uc_used > 0 || ccio->co_ucsize > 0) {
    off_t r = scm_io_seek(ccio->io,
                          -(off_t)(ccio->co_ucsize
                                   + (ccio->uc_used - ccio->uc_pos)),
                          SEEK_CUR);
    if (r < 0) return -1;

    ccio->uc_pos = ccio->uc_used = 0;
    ccio->uc_incomplete_p = false;
    ccio->co_pos = ccio->co_used = ccio->co_ucsize = 0;
    ccio->eof_received_p  = false;
  }

  nwrite = scm_charconvio_write_aux(ccio, buf, size);
  if (nwrite < 0) return -1;

  return nwrite;
}

int
scm_charconvio_ready_p(ScmCharConvIO *ccio)
{
  scm_assert(ccio != NULL);

  if (ccio->co_used > 0)
    return 1;
  else if (ccio->uc_incomplete_p)
    /* BUG:
       完全に 1 文字読み込むには 2 byte 必要で、l byte だけ読み込めるような
       ケースだと、嘘を返す(ready_p では 1 を返すが read で停止する)ことにな
       る */
    return scm_io_ready_p(ccio->io);
  else if (ccio->uc_used > 0 || ccio->eof_received_p)
    return 1;
  else
    return scm_io_ready_p(ccio->io);
}

int
scm_charconvio_close(ScmCharConvIO *ccio)
{
  int r, ret;

  scm_assert(ccio != NULL);

  ret = 0;

  r = iconv_close(ccio->rcd);
  if (r < 0 && ret == 0) return ret = -1;

  r = iconv_close(ccio->wcd);
  if (r < 0 && ret == 0) return ret = -1;

  r = scm_io_close(ccio->io);
  if (r < 0 && ret == 0) return ret = -1;

  return ret;
}

int
scm_charconvio_buffer_mode(ScmCharConvIO *ccio,
                           SCM_IO_MODE_T im, SCM_PORT_BUF_T *mode)
{
  scm_assert(ccio != NULL);
  scm_assert(mode != NULL);

  return scm_io_buffer_mode(ccio->io, im, mode);
}

int
scm_charconvio_flush(ScmCharConvIO *ccio)
{
  scm_assert(ccio != NULL);

  return scm_io_flush(ccio->io);
}

int
scm_charconvio_clear(ScmCharConvIO *ccio)
{
  size_t r;

  scm_assert(ccio != NULL);

  r = iconv(ccio->rcd, NULL, NULL, NULL, NULL);
  if (r == (size_t)-1) return -1;

  r = iconv(ccio->wcd, NULL, NULL, NULL, NULL);
  if (r == (size_t)-1) return -1;

  ccio->uc_pos = ccio->uc_used = 0;
  ccio->uc_incomplete_p = false;
  ccio->co_pos = ccio->co_used = ccio->co_ucsize = 0;
  ccio->eof_received_p  = false;

  return 0;
}

ScmIO *
scm_charconvio_lower(ScmCharConvIO *ccio)
{
  scm_assert(ccio != NULL);

  return ccio->io;
}

static int
scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_T buf_mode)
{
  int rslt;
  SCM_IO_MODE_T im;
  ScmIO *bufio;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT) {
    im = (BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT) ?
          SCM_IO_MODE_READ : SCM_IO_MODE_WRITE);
    rslt = scm_io_buffer_mode(SCM_PORT(port)->io, im,
                              &SCM_PORT(port)->buf_mode);
    if (rslt < 0) return -1;
  }
  else
    SCM_PORT(port)->buf_mode = buf_mode;

  if (SCM_PORT(port)->buf_mode != SCM_PORT_BUF_NONE) {
    bufio = (ScmIO *)scm_bufferedio_new(SCM_PORT(port)->io);
    if (bufio == NULL) return -1;
    SCM_PORT(port)->io = bufio;
    SCM_PORT(port)->attr |= SCM_PORT_ATTR_BUFFERED;
  }

  return 0;
}

static int
scm_port_init_encode(ScmObj port)
{
  ScmEncoding *enc;
  const char *icode, *ecode;
  ScmIO *io;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (SCM_PORT(port)->encoding[0] == '\0'
      && BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_BINARY))
    return 0;

  enc = scm_enc_find_enc(SCM_PORT(port)->encoding);
  if (SCM_PORT(port)->inn_enc == enc)
    return 0;

  icode = scm_enc_name(SCM_PORT(port)->inn_enc);
  ecode = SCM_PORT(port)->encoding;
  if (icode == NULL || ecode == NULL)
    return -1;

  io = (ScmIO *)scm_charconvio_new(SCM_PORT(port)->io, icode, ecode);
  if (io == NULL) return -1;

  SCM_PORT(port)->io = io;
  SCM_PORT(port)->attr |= SCM_PORT_ATTR_CHARCONV;

  return 0;
}

static ssize_t
scm_port_size_up_to_rearmost_lf(ScmObj port, const void *buf, size_t size)
{
  ScmStrItr iter;
  ssize_t len, found, lf_w, cr_w;
  scm_char_t lf, cr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  scm_str_itr_begin((void *)buf, size, SCM_PORT(port)->inn_enc, &iter);

  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\n', &lf);
  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\r', &cr);

  lf_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, lf.bytes, sizeof(lf));
  cr_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, cr.bytes, sizeof(cr));

  len = 0;
  found = 0;

  while (!scm_str_itr_end_p(&iter)) {
    ssize_t w = scm_str_itr_width(&iter);
    if (w < 0) {
      /* TODO; change error message */
      scm_capi_error("illegal byte sequence", 0);
      return -1;       /* TODO: error handling (illegal sequence) */
    }
    else if (w == 0)
      return 0;

    len += w;
    if ((lf_w == w
         && memcmp(lf.bytes, scm_str_itr_ptr(&iter), (size_t)w) == 0)
        || (cr_w == w
            && memcmp(cr.bytes, scm_str_itr_ptr(&iter), (size_t)w) == 0))
      found = len;

    scm_str_itr_next(&iter);
    if (scm_str_itr_err_p(&iter)) {
      /* TODO; change error message */
      scm_capi_error("illegal byte sequence", 0);
      return -1; /* TODO: error handling (illegal sequence) */
    }
  }

  return found;
}

static inline uint8_t *
scm_port_pushback_buff_head(ScmObj port)
{
  return (SCM_PORT(port)->pushback
          + (SCM_PORT_PUSHBACK_BUFF_SIZE - SCM_PORT(port)->pb_used));
}

static inline size_t
scm_port_pushback_buff_unused(ScmObj port)
{
  return SCM_PORT_PUSHBACK_BUFF_SIZE - SCM_PORT(port)->pb_used;
}

static ssize_t
scm_port_read_from_pushback_buf(ScmObj port, void *buf, size_t size)
{
  size_t n;
  void *head;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  n = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;
  head = scm_port_pushback_buff_head(port);
  memcpy(buf, head, n);
  SCM_PORT(port)->pb_used -= n;

  return (ssize_t)n;
}

static int
scm_port_pushback_buf_char_ready(ScmObj port, bool *rslt)
{
  ssize_t width;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(scm_port_textual_port_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) {
    if (rslt != NULL) *rslt = false;
    return 0;
  }

  width = scm_enc_char_width(SCM_PORT(port)->inn_enc,
                             scm_port_pushback_buff_head(port),
                             SCM_PORT(port)->pb_used);

  if (width < 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return -1;
  }

  if (rslt != NULL) *rslt = (width == 0) ? false : true;

  return 0;
}

static ssize_t
scm_port_read_char_from_pushback_buf(ScmObj port, scm_char_t *chr)
{
  ssize_t width;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(scm_port_textual_port_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  width = scm_enc_char_width(SCM_PORT(port)->inn_enc,
                             scm_port_pushback_buff_head(port),
                             SCM_PORT(port)->pb_used);

  if (width < 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return -1;
  }
  else if (width == 0) {
    return 0;
  }
  else {
    scm_port_read_from_pushback_buf(port, chr->bytes, (size_t)width);
    return width;
  }
}

static ssize_t
scm_port_read_from_io(ScmObj port, void *buf, size_t size)
{
  ssize_t n;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);

  if (SCM_PORT(port)->eof_received_p)
    return 0;

  n = scm_io_read(SCM_PORT(port)->io, buf, size);
  if (n < 0)
    return -1;
  else if (n == 0)
    SCM_PORT(port)->eof_received_p = true;

  return n;
}

static ssize_t
scm_port_read_into_pushback_buf(ScmObj port, size_t size)
{
  ssize_t ret;
  uint8_t buf[SCM_PORT_PUSHBACK_BUFF_SIZE];

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= scm_port_pushback_buff_unused(port));

  memcpy(buf, scm_port_pushback_buff_head(port), SCM_PORT(port)->pb_used);

  ret = scm_port_read_from_io(port, buf + SCM_PORT(port)->pb_used, size);
  if (ret < 0 || ret == 0) return ret;

  memcpy(scm_port_pushback_buff_head(port) - (size_t)ret,
         buf, SCM_PORT(port)->pb_used + (size_t)ret);
  SCM_PORT(port)->pb_used += (size_t)ret;

  return ret;
}

static ssize_t
scm_port_read(ScmObj port, void *buf, size_t size)
{
  ssize_t ret, pb_nr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  pb_nr = scm_port_read_from_pushback_buf(port, buf, size);
  if (pb_nr < 0) return -1;
  if ((size_t)pb_nr >= size) return pb_nr;

  ret = scm_port_read_from_io(port, (char *)buf + pb_nr, size - (size_t)pb_nr);
  if (ret < 0)
    return -1;

  return ret + pb_nr;
}

static ssize_t
scm_port_read_char_inter(ScmObj port, scm_char_t *chr)
{
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(scm_port_textual_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(chr != NULL);

  while ((rslt = scm_port_read_char_from_pushback_buf(port, chr)) == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;
    else if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0) {
        return -1;                /* TODO: error handling (illegal sequence) */
      }
      else {
        return 0;
      }
    }
  }

  return rslt;
}

static ssize_t
scm_port_write(ScmObj port, const void *buf, size_t size)
{
  size_t sz;
  ssize_t n, nw;
  int r;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(scm_port_output_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(size <= SSIZE_MAX);

  sz = size;
  nw = 0;

  if (SCM_PORT(port)->buf_mode == SCM_PORT_BUF_LINE) {
    ssize_t lf = scm_port_size_up_to_rearmost_lf(port, buf, size);
    if (lf < 0)
      return -1;

    if (lf > 0) {
      n = scm_io_write(SCM_PORT(port)->io, buf, (size_t)lf);
      if (n < 0) return -1;
      else if (n < lf) return n;

      r = scm_io_flush(SCM_PORT(port)->io);
      if (r < 0) return -1;

      sz -= (size_t)n;
      nw += n;
    }
  }

  n = scm_io_write(SCM_PORT(port)->io, (const char *)buf + nw, sz);
  if (n < 0) return -1;

  return nw + n;
}

static int
scm_port_analy_modestr(const char *mode,
                       SCM_PORT_ATTR *attr, SCM_PORT_OFLG *oflg)
{
  SCM_PORT_ATTR a;
  SCM_PORT_OFLG o;
  int i;

  switch (mode[0]) {
  case 'r':
    a = SCM_PORT_ATTR_INPUT;
    o = 0;
    break;
  case 'w':
    a = SCM_PORT_ATTR_OUTPUT;
    o = SCM_PORT_OFLG_TRUNC | SCM_PORT_OFLG_CREATE;
    break;
  case 'a':
    a = SCM_PORT_ATTR_OUTPUT;
    o = SCM_PORT_OFLG_APPEND | SCM_PORT_OFLG_CREATE;
    break;
  default:
    scm_capi_error("unknown mode", 0);
    return -1;
  }

  for (i = 1; mode[i] != '\0'; i++) {
    switch(mode[i]) {
    case '+':
      a |= SCM_PORT_ATTR_INPUT | SCM_PORT_ATTR_OUTPUT;
      break;
    case 'x':
      o |= SCM_PORT_OFLG_EXCL;
      break;
    case 'b':
      a |= SCM_PORT_ATTR_BINARY;
      break;
    default:
      scm_capi_error("unknown mode", 0);
      return -1;
    }
  }

  if (!BIT_IS_SET(a, SCM_PORT_ATTR_BINARY))
    a |= SCM_PORT_ATTR_TEXTUAL;

  if (attr != NULL) *attr |= a;
  if (oflg != NULL) *oflg |= o;

  return 0;
}

int
scm_port_initialize(ScmObj port, ScmIO *io,
                    SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                    ScmEncoding *inn_enc, const char *enc)
{
  int rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);
  scm_assert(inn_enc != NULL);

  SCM_PORT(port)->attr = attr;
  SCM_PORT(port)->io = io;
  SCM_PORT(port)->buf_mode = SCM_PORT_BUF_NONE;
  SCM_PORT(port)->closed_p = false;
  SCM_PORT(port)->eof_received_p = false;
  SCM_PORT(port)->pb_used = 0;
  SCM_PORT(port)->inn_enc = inn_enc;

  SCM_PORT(port)->encoding[0] = '\0';
  if (enc != NULL) {
    size_t len = strlen(enc);
    if (len >= SCM_PORT_ENCODING_NAME_SIZE)
      len = SCM_PORT_ENCODING_NAME_SIZE - 1;
    memcpy(SCM_PORT(port)->encoding, enc, len);
    SCM_PORT(port)->encoding[len] = '\0';
  }

  rslt = scm_port_init_buffer(port, buf_mode);
  if (rslt < 0) goto err;

  rslt =  scm_port_init_encode(port);
  if (rslt < 0) goto err;

  return 0;

 err:
  SCM_PORT(port)->io = NULL;
  return -1;
}

void
scm_port_finalize(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (SCM_PORT(port)->io == NULL) return;

  if (!scm_port_closed_p(port)) {
    if (scm_port_output_port_p(port))
      scm_port_flush(port);
    scm_io_close(SCM_PORT(port)->io);
  }

  scm_io_end(SCM_PORT(port)->io);
  SCM_PORT(port)->io = NULL;
}

ScmObj
scm_port_new(SCM_MEM_TYPE_T mtype,
             ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
             ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  int rslt;

  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);
  scm_assert(inn_enc != NULL);

  port = scm_capi_mem_alloc(&SCM_PORT_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  rslt = scm_port_initialize(port, io, attr, buf_mode, inn_enc, enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  return port;
}

ScmObj
scm_port_open_fd_inter(int fd, SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                       ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmIO *io;

  scm_assert(fd >= 0);
  scm_assert((attr & (SCM_PORT_ATTR_BINARY | SCM_PORT_ATTR_TEXTUAL)) != 0);
  scm_assert(!(BIT_IS_SET(attr, SCM_PORT_ATTR_BINARY)
               && BIT_IS_SET(attr, SCM_PORT_ATTR_TEXTUAL)));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_STRING));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_BUFFERED));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_CHARCONV));
  scm_assert(inn_enc != NULL);

  io = (ScmIO *)scm_fileio_new(fd);
  if (io == NULL) return SCM_OBJ_NULL;

  port = scm_port_new(SCM_MEM_HEAP, io,
                      attr | SCM_PORT_ATTR_FILE, buf_mode, inn_enc, enc);
  if (scm_obj_null_p(port))
    scm_io_end(io);

  return port;
}

ScmObj
scm_port_open_fd(int fd, const char *mode, SCM_PORT_BUF_T buf_mode,
                 ScmEncoding *inn_enc, const char *enc)
{
  SCM_PORT_ATTR attr;
  int rslt;

  scm_assert(fd >= 0);
  scm_assert(mode != NULL);
  scm_assert(inn_enc != NULL);

  attr = 0;

  rslt = scm_port_analy_modestr(mode, &attr, NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_port_open_fd_inter(fd, attr, buf_mode, inn_enc, enc);
}

ScmObj
scm_port_open_file_inter(const char *path,
                         SCM_PORT_ATTR attr, SCM_PORT_OFLG oflg,
                         SCM_PORT_BUF_T buf_mode, mode_t perm,
                         ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmIO *io;
  SCM_PORT_ATTR a;
  int flags;

  scm_assert(path != NULL);
  scm_assert((attr & (SCM_PORT_ATTR_BINARY | SCM_PORT_ATTR_TEXTUAL)) != 0);
  scm_assert(!(BIT_IS_SET(attr, SCM_PORT_ATTR_BINARY)
               && BIT_IS_SET(attr, SCM_PORT_ATTR_TEXTUAL)));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_STRING));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_BUFFERED));
  scm_assert(!BIT_IS_SET(attr, SCM_PORT_ATTR_CHARCONV));
  scm_assert(inn_enc != NULL);

  a = attr & (SCM_PORT_ATTR_INPUT | SCM_PORT_ATTR_OUTPUT);
  if (a == SCM_PORT_ATTR_INPUT)
    flags = O_RDONLY;
  else if (a ==  SCM_PORT_ATTR_OUTPUT)
    flags = O_WRONLY;
  else
    flags = O_RDWR;

  if (BIT_IS_SET(oflg, SCM_PORT_OFLG_TRUNC))  flags |= O_TRUNC;
  if (BIT_IS_SET(oflg, SCM_PORT_OFLG_APPEND)) flags |= O_APPEND;
  if (BIT_IS_SET(oflg, SCM_PORT_OFLG_CREATE)) flags |= O_CREAT;
  if (BIT_IS_SET(oflg, SCM_PORT_OFLG_EXCL))  flags |= O_EXCL;

  io = (ScmIO *)scm_fileio_open(path, flags, perm);
  if (io == NULL) return SCM_OBJ_NULL;

  port = scm_port_new(SCM_MEM_HEAP, io,
                      attr | SCM_PORT_ATTR_FILE, buf_mode, inn_enc, enc);
  if (scm_obj_null_p(port))
    scm_io_end(io);

  return port;
}

ScmObj
scm_port_open_file(const char *path, const char *mode,
                   SCM_PORT_BUF_T buf_mode, mode_t perm,
                   ScmEncoding *inn_enc, const char *enc)
{
  SCM_PORT_ATTR attr;
  SCM_PORT_OFLG oflg;
  int rslt;

  scm_assert(path != NULL);
  scm_assert(mode != NULL);
  scm_assert(inn_enc != NULL);

  attr = 0;
  oflg = 0;

  rslt = scm_port_analy_modestr(mode, &attr, &oflg);
  if (rslt < 0) return SCM_OBJ_NULL;

  return scm_port_open_file_inter(path, attr, oflg,
                                  buf_mode, perm, inn_enc, enc);
}

ScmObj
scm_port_open_string(const void *string, size_t size,
                     const char *mode, ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmIO *io;
  SCM_PORT_ATTR attr;
  int rslt;

  scm_assert(mode != NULL);
  scm_assert(inn_enc != NULL);

  attr = SCM_PORT_ATTR_STRING;

  rslt = scm_port_analy_modestr(mode, &attr, NULL);
  if (rslt < 0) return SCM_OBJ_NULL;

  io = (ScmIO *)scm_stringio_new(string, size);
  if (io == NULL) return SCM_OBJ_NULL;

  port = scm_port_new(SCM_MEM_HEAP, io,
                      attr, SCM_PORT_BUF_DEFAULT, inn_enc, enc);
  if (scm_obj_null_p(port))
    scm_io_end(io);

  return port;
}

bool
scm_port_input_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT);
}

bool
scm_port_output_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_OUTPUT);
}

bool
scm_port_textual_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_TEXTUAL);
}

bool
scm_port_binary_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_BINARY);
}

bool
scm_port_buffered_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_BUFFERED);
}

bool
scm_port_code_converted_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_CHARCONV);
}

bool
scm_port_file_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_FILE);
}

bool
scm_port_string_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SET(SCM_PORT(port)->attr, SCM_PORT_ATTR_STRING);
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
  scm_assert(scm_port_input_port_p(port));

  if (SCM_PORT(port)->pb_used > 0) {
    return true;
  }
  else if (SCM_PORT(port)->eof_received_p) {
    return true;
  }
  else {
    int ret = scm_io_ready_p(SCM_PORT(port)->io);
    if (ret < 0) return false;
    return (ret == 0) ? false : true;
  }
}

int
scm_port_char_ready(ScmObj port, bool *rslt)
{
  bool ready_p;
  int ret;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  ret = scm_port_pushback_buf_char_ready(port, &ready_p);
  if (ret < 0) return -1;

  while (!ready_p) {
    ssize_t r;

    if (!scm_port_ready_p(port)) break;

    r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0) {
      return -1;
    }

    if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0) {
        scm_capi_error("failed to peek a character: illegal byte sequence", 0);
        return -1;
      }

      ready_p = true;
    }
    else {
      ret = scm_port_pushback_buf_char_ready(port, &ready_p);
      if (ret < 0) return -1;
    }
  }

  if (rslt != NULL) *rslt = ready_p;

  return 0;
}

ScmEncoding *
scm_port_internal_enc(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  return SCM_PORT(port)->inn_enc;
}

const char *
scm_port_external_enc(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  return SCM_PORT(port)->encoding;
}

int
scm_port_flush(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_output_port_p(port)) {
    scm_capi_error("port is not output port", 0);
    return -1;
  }

  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  return scm_io_flush(SCM_PORT(port)->io);
}

int
scm_port_close(ScmObj port)
{
  int rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) return 0;

  if (scm_port_output_port_p(port)) {
    rslt = scm_port_flush(port);
    if (rslt < 0) return -1;
  }

  rslt = scm_io_close(SCM_PORT(port)->io);
  if (rslt < 0) return -1;

  SCM_PORT(port)->closed_p = true;
  return 0;
}

ssize_t
scm_port_read_bytes(ScmObj port, void *buf, size_t size)
{
  ssize_t nr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  if (size == 0)
    return 0;

  nr = scm_port_read(port, buf, size);

  if (nr == 0)
    SCM_PORT(port)->eof_received_p = false;

  return nr;
}

ssize_t
scm_port_read_char(ScmObj port, scm_char_t *chr)
{
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  rslt = scm_port_read_char_inter(port, chr);

  if (rslt == 0)
    SCM_PORT(port)->eof_received_p = false;

  return rslt;
}

ssize_t
scm_port_read_line(ScmObj port, ScmIO *io)
{
  ssize_t ret, nr, lf_w, cr_w;
  scm_char_t chr;
  scm_char_t lf, cr;
  bool cr_exists_p;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\n', &lf);
  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\r', &cr);

  lf_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, lf.bytes, sizeof(lf));
  cr_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, cr.bytes, sizeof(cr));

  cr_exists_p = false;
  nr = 0;

  while (true) {
    ret = scm_port_read_char_inter(port, &chr);
    if (ret < 0) return -1;
    else if (ret == 0) break;

    nr++;

    if (ret == lf_w && memcmp(lf.bytes, chr.bytes, (size_t)lf_w) == 0) {
      break;
    }
    else if (ret == cr_w && memcmp(cr.bytes, chr.bytes, (size_t)cr_w) == 0) {
      cr_exists_p = true;
      break;
    }

    if (io != NULL) {
      ret = scm_io_write(io, chr.bytes, (size_t)ret);
      if (ret < 0) return -1;
    }
  }

  if (cr_exists_p) {
    ret = scm_port_peek_char(port, &chr);
    if (ret < 0) return -1;

    if (ret == lf_w && memcmp(lf.bytes, chr.bytes, (size_t)lf_w) == 0) {
      ret = scm_port_read(port, &chr, (size_t)ret);
      if (ret < 0) return -1;
      nr++;
    }
  }

  if (nr == 0)
    SCM_PORT(port)->eof_received_p = false;

  return nr;
}

ssize_t
scm_port_read_string(size_t n, ScmObj port, ScmIO *io)
{
  size_t i;
  ssize_t ret;
  scm_char_t chr;

  scm_assert(n < SSIZE_MAX);
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  if (n == 0)
    return 0;

  for (i = 0; i < n; i++) {
    ret = scm_port_read_char_inter(port, &chr);
    if (ret < 0) return -1;
    else if (ret == 0) break;

    if (io != NULL) {
      ret = scm_io_write(io, chr.bytes, (size_t)ret);
      if (ret < 0) return -1;
    }
  }

  if (i == 0)
    SCM_PORT(port)->eof_received_p = false;

  return (ssize_t)i;
}

ssize_t
scm_port_pushback_bytes(ScmObj port, const void *buf, size_t size)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }
  if (size > scm_port_pushback_buff_unused(port)) {
    /* TODO; change error message */
    scm_capi_error("pushback buffer overflow", 0);
    return -1;
  }

  memcpy(scm_port_pushback_buff_head(port) - size, buf, size);
  SCM_PORT(port)->pb_used += size;

  return (ssize_t)size;
}

ssize_t
scm_port_pushback_char(ScmObj port, const scm_char_t *chr)
{
  ssize_t w;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  w = scm_enc_char_width(SCM_PORT(port)->inn_enc,
                         chr->bytes, sizeof(scm_char_t));
  if (w <= 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return -1;
  }

  memcpy(scm_port_pushback_buff_head(port) - (size_t)w, chr->bytes, (size_t)w);
  SCM_PORT(port)->pb_used += (size_t)w;

  return w;
}

ssize_t
scm_port_peek_bytes(ScmObj port, void *buf, size_t size)
{
  size_t npeek;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }
  if (size > SCM_PORT_PUSHBACK_BUFF_SIZE) {
    scm_capi_error("pushback buffer overflow", 0);
    return -1;
  }

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
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_capi_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  while ((rslt = scm_enc_char_width(SCM_PORT(port)->inn_enc,
                                    scm_port_pushback_buff_head(port),
                                    SCM_PORT(port)->pb_used))
         == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;
    else if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0) {
        /* TODO; change error message */
        scm_capi_error("illegal byte sequence", 0);
        return -1;
      }
      else
        return 0;
    }
  }

  if (rslt < 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return rslt;
  }

  memcpy(chr->bytes, scm_port_pushback_buff_head(port), (size_t)rslt);

  return rslt;
}

ssize_t
scm_port_write_bytes(ScmObj port, const void *buf, size_t size)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (!scm_port_output_port_p(port)) {
    scm_capi_error("port is not output port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  return scm_port_write(port, buf, size);
}

ssize_t
scm_port_write_char(ScmObj port, scm_char_t chr)
{
  ssize_t s;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_output_port_p(port)) {
    scm_capi_error("port is not output port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_capi_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }

  s = scm_enc_char_width(SCM_PORT(port)->inn_enc, chr.bytes, sizeof(chr));
  if (s < 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return -1;
  }

  return scm_port_write(port, chr.bytes, (size_t)s);
}

off_t
scm_port_seek(ScmObj port, off_t offset, int whence)
{
  off_t ret;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 0);
    return -1;
  }
  else if (scm_port_code_converted_port_p(port)) {
    scm_capi_error("seek operation of the port is not supported", 0);
    return -1;
  }

  if (scm_port_output_port_p(port))
    scm_port_flush(port);

  if (whence == SEEK_CUR)
    offset -= (off_t)SCM_PORT(port)->pb_used;

  ret = scm_io_seek(SCM_PORT(port)->io, offset, whence);
  if (ret < 0) return -1;

  if (scm_port_input_port_p(port)) {
    int r = scm_io_clear(SCM_PORT(port)->io);
    if (r < 0) return -1;
  }

  SCM_PORT(port)->eof_received_p = false;
  SCM_PORT(port)->pb_used = 0;

  return ret;
}

off_t
scm_port_pos(ScmObj port)
{
  off_t pos;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) {
    scm_capi_error("port is already closed", 1, port);
    return -1;
  }
  else if (scm_port_code_converted_port_p(port)) {
    scm_capi_error("pos operation to code-converted port is not supported",
                   1, port);
    return -1;
  }

  pos = scm_io_pos(SCM_PORT(port)->io);
  if (pos < 0) return -1;

  return pos - (off_t)SCM_PORT(port)->pb_used;
}

const void *
scm_port_string_buffer(ScmObj port)
{
  ScmIO *io, *l;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) {
    scm_capi_error("port is not string port", 0);
    return NULL;
  }

  for (io = SCM_PORT(port)->io; (l = scm_io_lower(io)) != NULL; io = l);

  return (void *)scm_stringio_buffer((ScmStringIO *)io);
}

ssize_t
scm_port_string_buffer_length(ScmObj port)
{
  ScmIO *io, *l;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) {
    scm_capi_error("port is not string port", 0);
    return -1;
  }

  for (io = SCM_PORT(port)->io; (l = scm_io_lower(io)) != NULL; io = l);

  return (ssize_t)scm_stringio_length((ScmStringIO *)io);
}

void
scm_port_gc_initialize(ScmObj obj, ScmObj mem)
{
  SCM_PORT(obj)->io = NULL;
}

void
scm_port_gc_finalize(ScmObj obj)
{
  scm_port_finalize(obj);
}
