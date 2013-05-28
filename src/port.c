#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <iconv.h>
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
#define SCM_CHARCONVIO_DEFAULT_BUF_SIZE 64
#define SCM_PORT_DEFAULT_BUF_SIZE 256


ScmTypeInfo SCM_PORT_TYPE_INFO = {
  .name                = "port",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = NULL,
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
                  ScmIOCloseFunc close,
                  ScmIOReadyPFunc readyp,
                  ScmIOBuffModeFunc buf_mode,
                  ScmIOBlkSizeFunc blk_size,
                  ScmIOFlushFunc flush,
                  ScmIOClearFunc clear)
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
  io->flush_func = flush;
  io->clear_func = clear;
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
                    (ScmIOBuffModeFunc)scm_fileio_buffer_mode,
                    (ScmIOBlkSizeFunc)scm_fileio_block_size,
                    (ScmIOFlushFunc)NULL,
                    (ScmIOClearFunc)NULL);
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
    scm_capi_error("system call error: open", 0);
    return NULL;      /* [ERR]: port: open errr: errno */
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
    return n;          /* [ERR]: port: read err: errno */
  }

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
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: write", 0);
    return n;          /* [ERR]: port: write err: errno */
  }

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
  if (n < 0) {
    /* TODO; change error message */
    scm_capi_error("system call error: select", 0);
    return -1;         /* [ERR]: port: file access err: errno */
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
    return n;          /* [ERR]: port: seek err: errno */
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
    return n;          /* [ERR]: port: close err: errno */
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
    return -1;       /* [ERR]: port: fstat err: errno */
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
    return -1;                  /* [ERR]: port: fstat err: errno */
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
    if (new_buffer == NULL) return -1; /* [ERR]: [through]  */
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
  if (strio == NULL) return NULL; /* [ERR]: [through] */

  scm_io_initialize((ScmIO *)strio,
                    (ScmIOFinFunc)scm_stringio_end,
                    (ScmIOReadFunc)scm_stringio_read,
                    (ScmIOWriteFunc)scm_stringio_write,
                    (ScmIOSeekFunc)scm_stringio_seek,
                    (ScmIOCloseFunc)scm_stringio_close,
                    (ScmIOReadyPFunc)scm_stringio_ready_p,
                    (ScmIOBuffModeFunc)scm_stringio_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL,
                    (ScmIOFlushFunc)NULL,
                    (ScmIOClearFunc)NULL);
  strio->string = NULL;
  strio->capacity = 0;
  strio->length = 0;
  strio->pos = 0;

  if (str != NULL) {
    ret = scm_stringio_expand_buffer(strio, len);
    if (ret < 0) goto err;      /* [ERR]: [through] */
    memcpy(strio->string, str, len);
    strio->length = len;
  }
  else {
    ret = scm_stringio_expand_buffer(strio, 0);
    if (ret < 0) goto err;     /* [ERR]: [through] */
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
  if (rslt < 0) return -1;       /* [ERR]: [through] */
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
    if (SSIZE_MAX - (ssize_t)strio->pos < offset) return -1; /* [ERR]: port: file offset is out of range */
    pos = (ssize_t)strio->pos + offset;
    break;
  case SEEK_END:
    if (SSIZE_MAX - (ssize_t)strio->length < offset) return -1; /* [ERR]: port: file offset is out of range */
    pos = (ssize_t)strio->length + offset;
    break;
  default:
    scm_assert(0);
    break;
  }

  if (pos < 0) {
    /* TODO; change error message */
    scm_capi_error("offset is out of range", 0);
    return -1;       /* [ERR]: port: file offset is out of range */
  }

  if (scm_stringio_expand_buffer(strio, (size_t)pos) < 0)
    return -1;                  /* [ERR]: [through] */

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

scm_local_func int
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
                    (ScmIOCloseFunc)scm_bufferedio_close,
                    (ScmIOReadyPFunc)scm_bufferedio_ready_p,
                    (ScmIOBuffModeFunc)scm_bufferedio_buffer_mode,
                    (ScmIOBlkSizeFunc)scm_bufferedio_block_size,
                    (ScmIOFlushFunc)scm_bufferedio_flush,
                    (ScmIOClearFunc)scm_bufferedio_clear);

  bufio->io = io;
  bufio->eof_received_p = false;

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

  if (bufio->eof_received_p) {
    bufio->eof_received_p = false;
    return 0;
  }

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

    if (nwrite >= size) return (ssize_t)nwrite;
  }

  if (rest >= bufio->capacity) {
    n = bufio->capacity * (rest / bufio->capacity);
    rslt = scm_io_write(bufio->io, (char *)buf + nwrite, n);
    if (rslt < 0) return -1;

    nwrite += (size_t)rslt;
    rest -= (size_t)rslt;

    if (nwrite >= size || (size_t)rslt < n) return (ssize_t)nwrite;
  }

  memcpy(bufio->buffer + bufio->pos, (char *)buf + nwrite, rest);
  bufio->pos += rest;
  if (bufio->pos > bufio->used)
    bufio->used = bufio->pos;
  nwrite += rest;
  rest = 0;

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

  if (whence == SEEK_CUR)
    off = offset - (off_t)(bufio->used - bufio->pos);
  else
    off = offset;

  off = scm_io_seek(bufio->io, off, whence);
  if (rslt < 0) return (int)rslt;

  rslt = scm_bufferedio_clear(bufio);
  if (rslt < 0) return -1;

  return off;
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

  if (bufio->used > 0) {
    ssize_t rslt = scm_io_write(bufio->io, bufio->buffer, bufio->pos);
    if (rslt < 0) return -1;
    bufio->used = 0;
    bufio->pos = 0;
  }

  return scm_io_flush(bufio->io);
}

int
scm_bufferedio_clear(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  bufio->pos = bufio->used = 0;
  bufio->eof_received_p = false;

  return scm_io_clear(bufio->io);
}

scm_local_func int
scm_charconvio_init(ScmCharConvIO *ccio,
                    const char *tocode, const char *fromcode)
{
  scm_assert(ccio != NULL);
  scm_assert(tocode != NULL);
  scm_assert(fromcode != NULL);

  ccio->icd = iconv_open(tocode, fromcode);
  if (ccio->icd == (void *)-1) return -1;

  ccio->unconverted = scm_capi_malloc(SCM_CHARCONVIO_DEFAULT_BUF_SIZE);
  if (ccio->unconverted == NULL) {
    iconv_close(ccio->icd);
    return -1;
  }

  ccio->converted = scm_capi_malloc(SCM_CHARCONVIO_DEFAULT_BUF_SIZE);
  if (ccio->converted == NULL) {
    iconv_close(ccio->icd);
    scm_capi_free(ccio->unconverted);
    return -1;
  }

  ccio->uc_capacity = SCM_CHARCONVIO_DEFAULT_BUF_SIZE;
  ccio->uc_used = 0;
  ccio->uc_pos = 0;
  ccio->uc_incomplete_p = false;
  ccio->co_capacity = SCM_CHARCONVIO_DEFAULT_BUF_SIZE;
  ccio->co_used = 0;
  ccio->co_pos = 0;
  ccio->eof_received_p = false;

  return 0;
}

scm_local_func ssize_t
scm_charconvio_read_from_io(ScmCharConvIO *ccio)
{
  ssize_t rslt;

  scm_assert(ccio != NULL);

  if (ccio->eof_received_p)
    return 0;

  rslt = scm_io_read(ccio->io,
                     ccio->unconverted + ccio->uc_used,
                     ccio->uc_capacity - ccio->uc_used);
  if (rslt < 0) return -1;

  if (rslt == 0) {
    ccio->eof_received_p = true;
    if (ccio->uc_incomplete_p) return -1;
       /* 文字コードシーケンスが不完全なまま EOF を read */
  }
  else {
    ccio->uc_used += (size_t)rslt;
  }

  return rslt;
}

scm_local_func ssize_t
scm_charconvio_conv_read(ScmCharConvIO *ccio, void *buf, size_t size, int *err)
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

  rslt = iconv(ccio->icd, &inb, &ins, &outb, &outs);
  if (rslt == (size_t)-1) {
    if (err != NULL) *err = errno;

    if (errno != EINVAL && errno != E2BIG)
      return -1;

    ccio->uc_pos += ccio->uc_used - ccio->uc_pos - ins;
    ccio->uc_incomplete_p = (errno == EINVAL) ? true : false;
  }
  else {
    if (err != NULL) *err = 0;
    ccio->uc_pos = ccio->uc_used = 0;
  }

  return (ssize_t)(size - outs);
}

scm_local_func ssize_t
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

    rslt = iconv(ccio->icd, &inb, &ins, &outb, &outs);
    if (rslt == (size_t)-1) {
      if (errno == EILSEQ) {
        return -1;                /* illegal sequence */
      }
      else if (errno == EINVAL) {
        scm_assert(ccio->uc_capacity > ins);
        if (ccio->unconverted != inb) {
          memcpy(ccio->unconverted, inb, ins);
          ccio->uc_used = ins;
          ccio->uc_pos = 0;
        }
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
      ccio->uc_used = 0;
      ccio->uc_pos = 0;
      cont = false;
    }

    nw = scm_io_write(ccio->io, outbuf, (size_t)(outb - outbuf));
    if (nw < 0) return -1;
  }

  return (ssize_t)(size - ins);
}

ScmCharConvIO *
scm_charconvio_new(ScmIO *io, const char *tocode, const char *fromcode)
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
                    (ScmIOCloseFunc)scm_charconvio_close,
                    (ScmIOReadyPFunc)scm_charconvio_ready_p,
                    (ScmIOBuffModeFunc)scm_charconvio_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL,
                    (ScmIOFlushFunc)scm_charconvio_flush,
                    (ScmIOClearFunc)scm_charconvio_clear);

  ccio->io = io;

  rslt = scm_charconvio_init(ccio, tocode, fromcode);
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
  int err, ready;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

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
    if (ccio->co_pos >= ccio->co_used) ccio->co_pos = ccio->co_used = 0;
  }

  err = 0;

  while (nread < size) {
    ready = 0;
    if (nread == 0 || (ready = scm_io_ready_p(ccio->io))) {
      if (ready < 0) return -1;

      ssize_t n = scm_charconvio_read_from_io(ccio);
      if (n < 0) return -1;
      if (n == 0) break;
    }
    else {
      break;
    }

    rslt = scm_charconvio_conv_read(ccio,
                                    (char *)buf + nread, size - nread, &err);
    if (rslt < 0) return -1;

    nread += (size_t)rslt;

    if (err == E2BIG) break;
  }

  if (err == E2BIG) {
    size_t s;
    rslt = scm_charconvio_conv_read(ccio,
                                    ccio->converted, ccio->co_capacity, &err);
    if (rslt < 0) return -1;

    /* 変換誤の 1 文字のサイズが ccio->convereted のサイズを越えるケースは想
       定外 */
    scm_assert(err != E2BIG);

    ccio->co_used = (size_t)rslt;

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
  ssize_t nw, nwrite;
  size_t i;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  nwrite = 0;

  i = 0;
  while (ccio->uc_used > 0 && i < size) {

    /* 変換前の 1 文字のサイズが ccio->buffer のサイズ を越えるケースは想定外 */
    scm_assert(ccio->uc_used < ccio->uc_capacity - 1);

    ccio->unconverted[ccio->uc_used++] = *((const char *)buf + i);
    nw = scm_charconvio_write_aux(ccio, ccio->unconverted, ccio->uc_used);
    if (nw < 0) return -1;
    nwrite += nw;
  }

  nw = scm_charconvio_write_aux(ccio, (const char *)buf + i, size - i);
  if (nw < 0) return -1;

  return nwrite + nw;
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
  int r;

  scm_assert(ccio != NULL);

  r = iconv_close(ccio->icd);
  if (r < 0) return -1;

  return scm_io_close(ccio->io);
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

  r = iconv(ccio->icd, NULL, NULL, NULL, NULL);
  if (r == (size_t)-1) return -1;

  ccio->uc_pos = ccio->uc_used = 0;
  ccio->uc_incomplete_p = false;
  ccio->co_pos = ccio->co_used = 0;
  ccio->eof_received_p  = false;

  return 0;
}

scm_local_func int
scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_T buf_mode)
{
  int rslt;
  SCM_IO_MODE_T im;
  ScmIO *bufio;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT) {
    im = (BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT) ?
          SCM_IO_MODE_READ : SCM_IO_MODE_WRITE);
    rslt = scm_io_buffer_mode(SCM_PORT(port)->io, im,
                              &SCM_PORT(port)->buf_mode);
    if (rslt < 0) return -1;                /* [ERR]: [through] */
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

scm_local_func int
scm_port_init_encode(ScmObj port)
{
  ScmEncoding *sys_enc;
  const char *fcode, *tcode;
  ScmIO *io;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  sys_enc = scm_capi_system_encoding();

  if (sys_enc == SCM_PORT(port)->encoding)
    return 0;

  if (scm_port_input_port_p(port)) {
    fcode = SCM_PORT(port)->encoding->iconv_name;
    tcode = sys_enc->iconv_name;
  }
  else {
    fcode = sys_enc->iconv_name;
    tcode = SCM_PORT(port)->encoding->iconv_name;
  }

  if (fcode == NULL || tcode == NULL)
    return -1;

  io = (ScmIO *)scm_charconvio_new(SCM_PORT(port)->io, fcode, tcode);
  if (io == NULL) return -1;

  SCM_PORT(port)->io = io;
  SCM_PORT(port)->attr |= SCM_PORT_ATTR_CHARCONV;

  return 0;
}

scm_local_func ssize_t
scm_port_size_up_to_rearmost_lf(ScmObj port, const void *buf, size_t size)
{
  ScmEncoding *enc;
  ScmStrItr iter;
  ssize_t len, found;
  scm_char_t lf, cr;
  size_t lf_w, cr_w;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  enc = scm_capi_system_encoding();
  scm_str_itr_begin((void *)buf, size, enc, &iter);

  scm_enc_chr_lf(enc, &lf, &lf_w);
  scm_enc_chr_cr(enc, &cr, &cr_w);

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
    if ((lf_w == (size_t)w
         && memcmp(lf.bytes, scm_str_itr_ptr(&iter), (size_t)w) == 0)
        || (cr_w == (size_t)w
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
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  n = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;
  head = scm_port_pushback_buff_head(port);
  memcpy(buf, head, n);
  SCM_PORT(port)->pb_used -= n;

  return (ssize_t)n;
}

scm_local_func ssize_t
scm_port_read_char_from_pushback_buf(ScmObj port, scm_char_t *chr)
{
  ScmEncoding *enc;
  ssize_t width;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(scm_port_textual_port_p(port));
  scm_assert(!scm_port_closed_p(port));

  if (SCM_PORT(port)->pb_used == 0) return 0;

  enc = scm_capi_system_encoding();
  width = scm_enc_char_width(enc,
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

scm_local_func ssize_t
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

scm_local_func ssize_t
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

scm_local_func ssize_t
scm_port_read(ScmObj port, void *buf, size_t size)
{
  ssize_t ret, pb_nr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(scm_port_input_port_p(port));
  scm_assert(!scm_port_closed_p(port));
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  pb_nr = scm_port_read_from_pushback_buf(port, buf, size);
  if (pb_nr < 0) return -1;     /* [ERR]: [through] */
  if ((size_t)pb_nr >= size) return pb_nr;

  ret = scm_port_read_from_io(port, (char *)buf + pb_nr, size - (size_t)pb_nr);
  if (ret < 0)
    return -1;

  if (ret == 0 && pb_nr == 0)
    SCM_PORT(port)->eof_received_p = false;

  return ret + pb_nr;
}

scm_local_func ssize_t
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

int
scm_port_initialize(ScmObj port, ScmIO *io,
                    SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
                    ScmEncoding *enc)
{
  int rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);
  scm_assert(enc != NULL);

  SCM_PORT(port)->attr = attr;
  SCM_PORT(port)->io = io;
  SCM_PORT(port)->buf_mode = SCM_PORT_BUF_NONE;
  SCM_PORT(port)->closed_p = false;
  SCM_PORT(port)->eof_received_p = false;
  SCM_PORT(port)->pb_used = 0;
  SCM_PORT(port)->encoding = enc;

  rslt = scm_port_init_buffer(port, buf_mode);
  if (rslt < 0) return -1;

  return scm_port_init_encode(port);
}

void
scm_port_finalize(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  scm_port_close(port);
  scm_io_end(SCM_PORT(port)->io);
}

ScmObj
scm_port_new(SCM_MEM_TYPE_T mtype,
             ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_T buf_mode,
             ScmEncoding *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  int rslt;

  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);
  scm_assert(enc != NULL);

  port = scm_capi_mem_alloc(&SCM_PORT_TYPE_INFO, 0, mtype);

  rslt = scm_port_initialize(port, io, attr, buf_mode, enc);
  if (rslt < 0) return SCM_OBJ_NULL;      /* [ERR]: [through] */

  return port;
}

ScmObj
scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr,
                    SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  return scm_port_new(SCM_MEM_HEAP,
                      io, attr | SCM_PORT_ATTR_INPUT, buf_mode, enc);
}

ScmObj
scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr,
                     SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  return scm_port_new(SCM_MEM_HEAP,
                      io, attr | SCM_PORT_ATTR_OUTPUT, buf_mode, enc);
}

ScmObj
scm_port_open_input_fd(int fd, SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  ScmIO *io;

  scm_assert(fd >= 0);

  io = (ScmIO *)scm_fileio_new(fd);
  if (io == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_port_open_input(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_FILE,
                             buf_mode, enc);
}

ScmObj
scm_port_open_output_fd(int fd, SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  ScmIO *io;

  scm_assert(fd >= 0);

  io = (ScmIO *)scm_fileio_new(fd);
  if (io == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_port_open_output(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_FILE,
                              buf_mode, enc);
}


ScmObj
scm_port_open_input_file(const char *path,
                         SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  ScmIO *io;

  scm_assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_RDONLY, 0);
  if (io == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_port_open_input(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_FILE,
                             buf_mode, enc);
}

ScmObj
scm_port_open_output_file(const char *path,
                          SCM_PORT_BUF_T buf_mode, ScmEncoding *enc)
{
  ScmIO *io;

  scm_assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_WRONLY | O_CREAT, 00644);
  if (io == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_port_open_output(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_FILE,
                              buf_mode, enc);
}

ScmObj
scm_port_open_input_string(const void *string, size_t size, ScmEncoding *enc)
{
  ScmIO *io;

  scm_assert(string != NULL);

  io = (ScmIO *)scm_stringio_new(string, size);
  if (io == NULL) return SCM_OBJ_NULL; /* [ERR]: [through] */

  return scm_port_open_input(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_STRING,
                             SCM_PORT_BUF_DEFAULT, enc);
}

ScmObj
scm_port_open_output_string(ScmEncoding *enc)
{
  ScmIO *io;

  io = (ScmIO *)scm_stringio_new(NULL, 0);
  if (io == NULL) return SCM_OBJ_NULL;

  return scm_port_open_output(io, SCM_PORT_ATTR_TEXTUAL | SCM_PORT_ATTR_STRING,
                              SCM_PORT_BUF_DEFAULT, enc);
}

bool
scm_port_input_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT);
}

bool
scm_port_output_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_OUTPUT);
}

bool
scm_port_textual_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_TEXTUAL);
}

bool
scm_port_binary_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_BINARY);
}

bool
scm_port_buffered_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_BUFFERED);
}

bool
scm_port_code_converted_port_p(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT(port)->attr, SCM_PORT_ATTR_CHARCONV);
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
  scm_assert(scm_port_input_port_p(port));

  if (SCM_PORT(port)->pb_used > 0) {
    return true;
  }
  else if (SCM_PORT(port)->eof_received_p) {
    return true;
  }
  else {
    int ret = scm_io_ready_p(SCM_PORT(port)->io);
    if (ret < 0) return false; /* [ERR]: [through] */
    return (ret == 0) ? false : true;
  }
}

ScmEncoding *
scm_port_encoding(ScmObj port)
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
  int ret;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_closed_p(port)) return 0;

  if (scm_port_output_port_p(port))
    scm_port_flush(port);

  ret = scm_io_close(SCM_PORT(port)->io);
  if (ret < 0) return ret;      /* [ERR]: [through] */

  SCM_PORT(port)->closed_p = true;
  return 0;
}

ssize_t
scm_port_read_bytes(ScmObj port, void *buf, size_t size)
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

  return scm_port_read(port, buf, size);
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

  while ((rslt = scm_port_read_char_from_pushback_buf(port, chr)) == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;                /* [ERR]: [through] */
    else if (r == 0) {
      if (SCM_PORT(port)->pb_used > 0) {
        return -1;                /* TODO: error handling (illegal sequence) */
      }
      else {
        SCM_PORT(port)->eof_received_p = false;
        return 0;
      }
    }
  }

  return rslt;
}

ssize_t
scm_port_read_line(ScmObj port, void *buf, size_t size)
{
  char *bp;
  size_t sz, lf_w, cr_w;
  ssize_t ret;
  scm_char_t chr;
  scm_char_t lf, cr;
  bool lf_exists_p, cr_exists_p;
  ScmEncoding *enc;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

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

  enc = scm_capi_system_encoding();
  scm_enc_chr_lf(enc, &lf, &lf_w);
  scm_enc_chr_cr(enc, &cr, &cr_w);

  bp = buf;
  sz = 0;
  lf_exists_p = false;
  cr_exists_p = false;

  while (sz < size) {
    ret = scm_port_peek_char(port, &chr);
    if (ret < 0) return -1;
    else if (ret == 0) break;

    if ((size_t)ret == lf_w && memcmp(lf.bytes, chr.bytes, lf_w) == 0) {
      lf_exists_p = true;
      break;
    }
    else if ((size_t)ret == cr_w && memcmp(cr.bytes, chr.bytes, cr_w) == 0) {
      cr_exists_p = true;
      break;
    }

    if (size - sz < (size_t)ret) break;

    memcpy(bp, chr.bytes, (size_t)ret);

    bp += (size_t)ret;
    sz += (size_t)ret;

    ret = scm_port_read(port, &chr, (size_t)ret);
    if (ret < 0) return -1;
  }

  if (lf_exists_p || cr_exists_p) {
    ret = scm_port_read(port, &chr, (size_t)ret);
    if (ret < 0) return -1;

    if (cr_exists_p) {
      ret = scm_port_peek_char(port, &chr);
      if (ret < 0) return -1;

      if ((size_t)ret == lf_w && memcmp(lf.bytes, chr.bytes, lf_w) == 0) {
        ret = scm_port_read(port, &chr, (size_t)ret);
        if (ret < 0) return -1;
      }
    }
  }

  if (sz == 0)
    SCM_PORT(port)->eof_received_p = false;

  return (ssize_t)sz;
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
  ScmEncoding *enc;
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

  enc = scm_capi_system_encoding();
  w = scm_enc_char_width(enc, chr->bytes, sizeof(scm_char_t));
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
    if (ret < 0) return -1;     /* [ERR]: [through] */
  }

  npeek = (size < SCM_PORT(port)->pb_used) ? size : SCM_PORT(port)->pb_used;

  memcpy(buf, scm_port_pushback_buff_head(port), npeek);

  return (ssize_t)npeek;
}

ssize_t
scm_port_peek_char(ScmObj port, scm_char_t *chr)
{
  ScmEncoding *enc;
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

  enc = scm_capi_system_encoding();

  while ((rslt = scm_enc_char_width(enc,
                                    scm_port_pushback_buff_head(port),
                                    SCM_PORT(port)->pb_used))
         == 0) {
    ssize_t r = scm_port_read_into_pushback_buf(port, 1);
    if (r < 0)
      return -1;                /* [ERR]: [through] */
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
  ScmEncoding *enc;
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

  enc = scm_capi_system_encoding();
  s = scm_enc_char_width(enc, chr.bytes, sizeof(chr));
  if (s < 0) {
    /* TODO; change error message */
    scm_capi_error("illegal byte sequence", 0);
    return -1;
  }

  return scm_port_write(port, chr.bytes, (size_t)s);
}

int
scm_port_seek(ScmObj port, off_t offset, int whence)
{
  off_t rslt;

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

  rslt = scm_io_seek(SCM_PORT(port)->io, offset, whence);
  if (rslt < 0) return (int)rslt; /* [ERR]: [through] */

  if (scm_port_input_port_p(port)) {
    int r = scm_io_clear(SCM_PORT(port)->io);
    if (r < 0) return -1;
  }

  SCM_PORT(port)->eof_received_p = false;

  return 0;
}

const void *
scm_port_string_buffer(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) {
    scm_capi_error("port is not string port", 0);
    return NULL;
  }

  return (void *)scm_stringio_buffer((ScmStringIO *)SCM_PORT(port)->io);
}

ssize_t
scm_port_string_buffer_length(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_string_port_p(port)) {
    scm_capi_error("port is not string port", 0);
    return -1;
  }

  return (ssize_t)scm_stringio_length((ScmStringIO *)SCM_PORT(port)->io);
}

void
scm_port_gc_initialize(ScmObj obj, ScmObj mem)
{
  /* nothing to do */
}

void
scm_port_gc_finalize(ScmObj obj)
{
  scm_port_finalize(obj);
}
