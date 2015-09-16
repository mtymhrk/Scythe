#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>
#include <iconv.h>
#include <assert.h>

#include "scythe/object.h"
#include "scythe/parser.h"
#include "scythe/encoding.h"
#include "scythe/impl_utils.h"
#include "scythe/bedrock.h"
#include "scythe/vm.h"
#include "scythe/memory.h"
#include "scythe/refstk.h"
#include "scythe/number.h"
#include "scythe/fixnum.h"
#include "scythe/char.h"
#include "scythe/exception.h"
#include "scythe/miscobjects.h"
#include "scythe/pair.h"
#include "scythe/procedure.h"
#include "scythe/string.h"
#include "scythe/vector.h"
#include "scythe/port.h"


/* Note: size_t 型の引数が SSIZE_MAX 以下であることを assert でチェックしてい
 *       るのは、read/write の戻り値が ssize_t 型であるため。 */
#define BIT_SET_P SCM_PORT_BIT_SET_P

#define SCM_STRINGIO_INIT_BUF_SIZE 64
#define SCM_CHARCONVIO_UC_BUF_SIZE 64
#define SCM_CHARCONVIO_SV_BUF_SIZE 8
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
                  ScmIOReadyFunc ready,
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
  io->ready_func = ready;
  io->default_buf_mode_func = buf_mode;
  io->blk_size_func = blk_size;
  io->flush_func = flush;
  io->clear_func = clear;
  io->lower_func = lower;
}

ScmFileIO *
scm_fileio_new(int fd)
{
  ScmFileIO *fileio;

  fileio = scm_malloc(sizeof(ScmFileIO));
  if (fileio == NULL) return NULL;

  scm_io_initialize((ScmIO *)fileio,
                    (ScmIOFinFunc)scm_fileio_end,
                    (ScmIOReadFunc)scm_fileio_read,
                    (ScmIOWriteFunc)scm_fileio_write,
                    (ScmIOSeekFunc)scm_fileio_seek,
                    (ScmIOPosFunc)scm_fileio_pos,
                    (ScmIOCloseFunc)scm_fileio_close,
                    (ScmIOReadyFunc)scm_fileio_ready,
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

  scm_free(fileio);
}

ScmFileIO *
scm_fileio_open(const char *pathname, int flags, mode_t mode)
{
  int fd;

  scm_assert(pathname != NULL);

  SCM_SYSCALL(fd, open(pathname, flags, mode));
  if (fd < 0) {
    /* TODO; change error message */
    scm_file_error("system call error: open", 0);
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
    scm_error("system call error: read", 0);
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
      scm_error("system call error: write", 0);
      return n;
    }

    rest -= (size_t)n;
    p += n;
  }

  return (ssize_t)(size - rest);
}

int
scm_fileio_ready(ScmFileIO *fileio)
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
    scm_error("system call error: select", 0);
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
    scm_error("system call error: lseek", 0);
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
    scm_error("system call error: lseek", 0);
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
    scm_error("system call error: close", 0);
    return n;
  }

  return n;
}

int
scm_fileio_buffer_mode(ScmFileIO *fileio, scm_io_mode_t im,
                       scm_port_buf_t *mode)
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
    scm_error("system call error: fstat", 0);
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
    scm_error("system call error: fstat", 0);
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
    strio->string = scm_malloc(new_size);
    strio->capacity = new_size;
  }
  else if (new_size > strio->capacity) {
    char *new_buffer = scm_realloc(strio->string, new_size);
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

  strio = scm_malloc(sizeof(ScmStringIO));
  if (strio == NULL) return NULL;

  scm_io_initialize((ScmIO *)strio,
                    (ScmIOFinFunc)scm_stringio_end,
                    (ScmIOReadFunc)scm_stringio_read,
                    (ScmIOWriteFunc)scm_stringio_write,
                    (ScmIOSeekFunc)scm_stringio_seek,
                    (ScmIOPosFunc)scm_stringio_pos,
                    (ScmIOCloseFunc)scm_stringio_close,
                    (ScmIOReadyFunc)scm_stringio_ready,
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
  scm_free(strio);
  return NULL;
}

void
scm_stringio_end(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  scm_free(strio->string);
  scm_free(strio);
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
scm_stringio_ready(ScmStringIO *strio)
{
  scm_assert(strio != NULL);

  /* eof 相当のケースでも読取可能と返す */
  return (strio->pos <= strio->length) ? 1 : 0;
}

off_t
scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence)
{
  ssize_t pos;

  scm_assert(strio != NULL);

  switch (whence) {
  case SEEK_SET:
    pos = offset;
    break;
  case SEEK_CUR:
    if (SSIZE_MAX - (ssize_t)strio->pos < offset) {
      scm_error("failed to seek: offset is out of range", 0);
      return -1;
    }
    pos = (ssize_t)strio->pos + offset;
    break;
  case SEEK_END:
    if (SSIZE_MAX - (ssize_t)strio->length < offset) {
      scm_error("failed to seek: offset is out of range", 0);
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
    scm_error("offset is out of range", 0);
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
scm_stringio_buffer_mode(ScmStringIO *strio, scm_io_mode_t im,
                         scm_port_buf_t *mode)
{
  scm_assert(strio != NULL);
  scm_assert(mode != NULL);

  *mode = SCM_PORT_BUF_NONE;

  return 0;
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

  bufio->buffer = scm_malloc(bufio->capacity);
  if (bufio->buffer == NULL) return -1;

  return 0;
}


ScmBufferedIO *
scm_bufferedio_new(ScmIO *io)
{
  ScmBufferedIO *bufio;
  int r;

  bufio = scm_malloc(sizeof(ScmBufferedIO));
  if (bufio == NULL) return NULL;

  scm_io_initialize((ScmIO *)bufio,
                    (ScmIOFinFunc)scm_bufferedio_end,
                    (ScmIOReadFunc)scm_bufferedio_read,
                    (ScmIOWriteFunc)scm_bufferedio_write,
                    (ScmIOSeekFunc)scm_bufferedio_seek,
                    (ScmIOPosFunc)scm_bufferedio_pos,
                    (ScmIOCloseFunc)scm_bufferedio_close,
                    (ScmIOReadyFunc)scm_bufferedio_ready,
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
    scm_free(bufio);
    return NULL;
  }

  return bufio;
}

void
scm_bufferedio_end(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  scm_io_end(bufio->io);

  scm_free(bufio->buffer);
  scm_free(bufio);
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
scm_bufferedio_ready(ScmBufferedIO *bufio)
{
  scm_assert(bufio != NULL);

  if (bufio->pos > 0)
    return 1;
  else if (bufio->eof_received_p)
    return 1;
  else
    return scm_io_ready(bufio->io);
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
                           scm_io_mode_t im, scm_port_buf_t *mode)
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

/* XXX :
 *  CharConvIO は Read/Write の同時利用不可。Seek 不可
 */

static int
scm_charconvio_init(ScmCharConvIO *ccio, scm_io_mode_t mode,
                    const char *incode, const char *extcode)
{
  scm_assert(ccio != NULL);
  scm_assert(incode != NULL);
  scm_assert(extcode != NULL);

  ccio->unconverted = scm_malloc(SCM_CHARCONVIO_UC_BUF_SIZE);
  if (ccio->unconverted == NULL)
    return -1;

  ccio->save = scm_malloc(SCM_CHARCONVIO_SV_BUF_SIZE);
  if (ccio->save == NULL) {
    scm_free(ccio->unconverted);
    return -1;
  }

  if (mode == SCM_IO_MODE_READ)
    scm_enc_cnv_init(&ccio->cnv, extcode, incode, ccio->unconverted, 0);
  else
    scm_enc_cnv_init(&ccio->cnv, incode, extcode, NULL, 0);

  if (scm_enc_cnv_err_p(&ccio->cnv)) {
    scm_free(ccio->unconverted);
    scm_free(ccio->save);
    /* TODO: write the error message */
    scm_error("", 0);
    return -1;
  }

  ccio->im = mode;
  ccio->uc_incomplete_p = false;
  ccio->sv_pos = 0;
  ccio->sv_size = 0;
  ccio->eof_received_p = false;

  return 0;
}

static ssize_t
scm_charconvio_read_from_io(ScmCharConvIO *ccio, size_t limit)
{
  ssize_t rslt;
  size_t used, rsize;

  scm_assert(ccio != NULL);

  if (ccio->eof_received_p)
    return 0;

  used = ((size_t)(scm_enc_cnv_ptr(&ccio->cnv) - ccio->unconverted)
          + scm_enc_cnv_rest(&ccio->cnv));
  rsize = SCM_CHARCONVIO_UC_BUF_SIZE - used;
  if (limit > 0 && rsize > limit)
    rsize = limit;

  rslt = scm_io_read(ccio->io, ccio->unconverted + used, rsize);
  if (rslt < 0) return -1;

  ccio->eof_received_p = (rslt == 0) ? true : false;
  if (scm_enc_cnv_incomplete_p(&ccio->cnv)) {
    if (rslt == 0) {
      /* 文字コードシーケンスが不完全なまま EOF を read */
      /* TODO: write a error message */
      scm_error("character encoding conversion error: unexpected EOF", 0);
      return -1;
    }
    scm_enc_cnv_clear_cnv_err(&ccio->cnv);
  }

  scm_enc_cnv_appended(&ccio->cnv, (size_t)rslt);
  return rslt;
}

static ssize_t
scm_charconvio_conv_read(ScmCharConvIO *ccio, void *buf, size_t size)
{
  size_t rslt;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  rslt = scm_enc_cnv_convert(&ccio->cnv, buf, size, ccio->eof_received_p);
  if (scm_enc_cnv_err_p(&ccio->cnv)) {
    if (scm_enc_cnv_incomplete_p(&ccio->cnv)
        && ccio->unconverted != scm_enc_cnv_ptr(&ccio->cnv)) {
      size_t s = scm_enc_cnv_rest(&ccio->cnv);
      memmove(ccio->unconverted, scm_enc_cnv_ptr(&ccio->cnv), s);
      scm_enc_cnv_next(&ccio->cnv, ccio->unconverted, s);
    }
    else {
      /* TODO: write a error message */
      scm_error("character encoding conversion error", 0);
      return -1;
    }
  }

  if (scm_enc_cnv_end_p(&ccio->cnv))
    scm_enc_cnv_next(&ccio->cnv, ccio->unconverted, 0);

  return (ssize_t)rslt;
}

static ssize_t
scm_charconvio_read_into_converted(ScmCharConvIO *ccio)
{
  ssize_t rslt;

  scm_assert(ccio != NULL);

  scm_assert(ccio->sv_pos == 0);
  scm_assert(ccio->sv_size == 0);

  rslt = scm_charconvio_conv_read(ccio, ccio->save, SCM_CHARCONVIO_SV_BUF_SIZE);
  if (rslt == 0) return 0;
  if (rslt < 0) return -1;

  ccio->sv_size = (size_t)rslt;
  return rslt;
}

static ssize_t
scm_charconvio_read_from_converted(ScmCharConvIO *ccio, void *buf, size_t size)
{
  size_t s;

  scm_assert(ccio != NULL);
  scm_assert(size <= SSIZE_MAX);

  s = (size < ccio->sv_size) ? size : ccio->sv_size;
  memcpy(buf, ccio->save + ccio->sv_pos, s);
  ccio->sv_size -= s;
  if (ccio->sv_size == 0)
    ccio->sv_pos = 0;
  else
    ccio->sv_pos += s;

  return (ssize_t)s;
}

static int
scm_charconvio_try_to_peek_1char(ScmCharConvIO *ccio)
{
  ssize_t rslt;

  scm_assert(ccio != NULL);

  scm_assert(ccio->sv_pos == 0);
  scm_assert(ccio->sv_size == 0);

  if (!scm_enc_cnv_insufficient_buf_p(&ccio->cnv)) {
    rslt = scm_charconvio_read_from_io(ccio, 1);
    if (rslt == 0) return 0;
    else if (rslt < 0) return -1;
  }

  rslt = scm_charconvio_read_into_converted(ccio);
  if (rslt == 0) return 0;
  if (rslt < 0) return -1;

  return 1;
}

static ssize_t
scm_charconvio_conv_write(ScmCharConvIO *ccio, const void *buf, size_t size)
{
  char outbuf[256];
  ssize_t nw;
  size_t nc;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  scm_enc_cnv_next(&ccio->cnv, buf, size);
  while (!scm_enc_cnv_end_p(&ccio->cnv)
         && !scm_enc_cnv_incomplete_p(&ccio->cnv)) {
    nc = scm_enc_cnv_convert(&ccio->cnv, outbuf, sizeof(outbuf), false);
    if (scm_enc_cnv_err_p(&ccio->cnv)
        && !scm_enc_cnv_incomplete_p(&ccio->cnv)) {
      /* TODO: write a error message */
      scm_error("character encoding conversion error", 0);
      return -1;
    }

    nw = scm_io_write(ccio->io, outbuf, nc);
    if (nw < 0) return -1;
  }

  return (ssize_t)(size - scm_enc_cnv_rest(&ccio->cnv));
}

static ssize_t
scm_charconvio_write_incomplete(ScmCharConvIO *ccio, const void *buf, size_t size)
{
  ssize_t r;
  size_t cnt;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (ccio->sv_size == 0)
    return 0;

  for (cnt = 0; cnt < size; cnt++) {
    scm_assert(ccio->sv_pos + ccio->sv_size < SCM_CHARCONVIO_SV_BUF_SIZE);

    if (!scm_enc_cnv_incomplete_p(&ccio->cnv)) {
      ccio->sv_pos = ccio->sv_size = 0;
      break;
    }

    scm_enc_cnv_clear_cnv_err(&ccio->cnv);
    ccio->save[ccio->sv_pos + ccio->sv_size++] = ((const char *)buf)[cnt];
    r = scm_charconvio_conv_write(ccio,
                                  ccio->save + ccio->sv_pos, ccio->sv_size);
    if (r < 0) return -1;
  }

  return (ssize_t)cnt;
}

static int
scm_charconvio_write_terminate(ScmCharConvIO *ccio)
{
  char outbuf[256];
  ssize_t nw;
  size_t nc;

  scm_assert(ccio != NULL);

  if (scm_enc_cnv_err_p(&ccio->cnv)) {
    /* TODO: write a error message */
    scm_error("character encoding conversion error", 0);
    return -1;
  }

  nc = scm_enc_cnv_convert(&ccio->cnv, outbuf, sizeof(outbuf), true);
  if (scm_enc_cnv_err_p(&ccio->cnv)) {
    /* TODO: write a error message */
    scm_error("character encoding conversion error", 0);
    return -1;
  }

  if (nc > 0) {
    nw = scm_io_write(ccio->io, outbuf, nc);
    if (nw < 0) return -1;
  }

  return 0;
}

ScmCharConvIO *
scm_charconvio_new(ScmIO *io, scm_io_mode_t mode,
                   const char *incode, const char *extcode)
{
  ScmCharConvIO *ccio;
  int rslt;

  ccio = scm_malloc(sizeof(ScmCharConvIO));
  if (ccio == NULL) return NULL;

  scm_io_initialize((ScmIO *)ccio,
                    (ScmIOFinFunc)scm_charconvio_end,
                    (ScmIOReadFunc)scm_charconvio_read,
                    (ScmIOWriteFunc)scm_charconvio_write,
                    (ScmIOSeekFunc)NULL,
                    (ScmIOPosFunc)NULL,
                    (ScmIOCloseFunc)scm_charconvio_close,
                    (ScmIOReadyFunc)scm_charconvio_ready,
                    (ScmIOBuffModeFunc)scm_charconvio_buffer_mode,
                    (ScmIOBlkSizeFunc)NULL,
                    (ScmIOFlushFunc)scm_charconvio_flush,
                    (ScmIOClearFunc)scm_charconvio_clear,
                    (ScmIOLowerFunc)scm_charconvio_lower);

  ccio->io = io;

  rslt = scm_charconvio_init(ccio, mode, incode, extcode);
  if (rslt < 0) {
    scm_free(ccio);
    return NULL;
  }

  return ccio;
}

void
scm_charconvio_end(ScmCharConvIO *ccio)
{
  scm_assert(ccio != NULL);

  scm_io_end(ccio->io);

  scm_enc_cnv_fin(&ccio->cnv);
  scm_free(ccio->unconverted);
  scm_free(ccio->save);
  scm_free(ccio);
}

ssize_t
scm_charconvio_read(ScmCharConvIO *ccio, void *buf, size_t size)
{
  size_t nread;
  ssize_t rslt;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);

  if (size == 0)
    return 0;

  rslt = scm_charconvio_read_from_converted(ccio, buf, size);
  if (rslt < 0) return -1;

  nread = (size_t)rslt;

  while (nread < size) {
    if (scm_enc_cnv_end_p(&ccio->cnv)
        || scm_enc_cnv_incomplete_p(&ccio->cnv)) {
      rslt = scm_charconvio_read_from_io(ccio, 0);
      if (rslt < 0) return -1;
      if (rslt == 0) break;
    }

    rslt = scm_charconvio_conv_read(ccio, (char *)buf + nread, size - nread);
    if (rslt < 0) return -1;
    nread += (size_t)rslt;

    if (scm_enc_cnv_insufficient_buf_p(&ccio->cnv))
      break;
  }

  if (nread < size && scm_enc_cnv_insufficient_buf_p(&ccio->cnv)) {
    rslt = scm_charconvio_read_into_converted(ccio);
    if (rslt < 0) return -1;
    scm_assert(rslt > 0);
    rslt = scm_charconvio_read_from_converted(ccio,
                                              (char *)buf + nread,
                                              size - nread);
    if (rslt < 0) return -1;
    nread += (size_t)rslt;
  }

  if (nread == 0)
    ccio->eof_received_p = false;

  return (ssize_t)nread;
}

ssize_t
scm_charconvio_write(ScmCharConvIO *ccio, const void *buf, size_t size)
{
  ssize_t nw;

  scm_assert(ccio != NULL);
  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  nw = scm_charconvio_write_incomplete(ccio, buf, size);
  if (nw < 0) return -1;

  if ((size_t)nw >= size)
    return nw;

  nw = scm_charconvio_conv_write(ccio,
                                 (const char *)buf + nw, size - (size_t)nw);
  if (nw < 0) return -1;

  scm_assert(scm_enc_cnv_rest(&ccio->cnv) == 0
             || scm_enc_cnv_incomplete_p(&ccio->cnv));

  if (scm_enc_cnv_incomplete_p(&ccio->cnv)) {
    scm_assert(SCM_CHARCONVIO_SV_BUF_SIZE <= scm_enc_cnv_rest(&ccio->cnv));
    memcpy(ccio->save,
           scm_enc_cnv_ptr(&ccio->cnv), scm_enc_cnv_rest(&ccio->cnv));
    ccio->sv_pos = 0;
    ccio->sv_size = scm_enc_cnv_rest(&ccio->cnv);
  }

  return (ssize_t)size;
}

int
scm_charconvio_ready(ScmCharConvIO *ccio)
{
  int r;

  scm_assert(ccio != NULL);

  if (ccio->sv_size > 0)
    return 1;
  else if (scm_enc_cnv_insufficient_buf_p(&ccio->cnv))
    return 1;

  while ((r = scm_io_ready(ccio->io)) > 0) {
    r = scm_charconvio_try_to_peek_1char(ccio);
    if (r < 0)
      return -1;
    else if (r > 0)
      return 1;
    else if (ccio->eof_received_p)
      return 1;
  }

  return r;
}

int
scm_charconvio_close(ScmCharConvIO *ccio)
{
  int r, ret;

  scm_assert(ccio != NULL);

  ret = 0;

  if (ccio->im == SCM_IO_MODE_WRITE) {
    r = scm_charconvio_write_terminate(ccio);
    if (r < 0) ret = -1;;
  }

  r = scm_io_close(ccio->io);
  if (r < 0) ret = -1;

  return ret;
}

int
scm_charconvio_buffer_mode(ScmCharConvIO *ccio,
                           scm_io_mode_t im, scm_port_buf_t *mode)
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
  scm_assert(ccio != NULL);

  scm_enc_cnv_clear_cnv_stat(&ccio->cnv);
  scm_enc_cnv_next(&ccio->cnv, ccio->unconverted, 0);

  ccio->uc_incomplete_p = false;
  ccio->sv_pos = 0;
  ccio->sv_size = 0;
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
scm_port_init_buffer(ScmObj port, scm_port_buf_t buf_mode)
{
  int rslt;
  scm_io_mode_t im;
  ScmIO *bufio;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT) {
    im = (BIT_SET_P(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT) ?
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
  scm_io_mode_t im;
  ScmEncoding *enc;
  const char *icode, *ecode;
  ScmIO *io;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (SCM_PORT(port)->encoding[0] == '\0'
      && BIT_SET_P(SCM_PORT(port)->attr, SCM_PORT_ATTR_BINARY))
    return 0;

  enc = scm_enc_find_enc(SCM_PORT(port)->encoding);
  if (SCM_PORT(port)->inn_enc == enc)
    return 0;

  icode = scm_enc_name(SCM_PORT(port)->inn_enc);
  ecode = SCM_PORT(port)->encoding;
  if (icode == NULL || ecode == NULL)
    return -1;

  im = (BIT_SET_P(SCM_PORT(port)->attr, SCM_PORT_ATTR_INPUT) ?
        SCM_IO_MODE_READ : SCM_IO_MODE_WRITE);
  io = (ScmIO *)scm_charconvio_new(SCM_PORT(port)->io, im, icode, ecode);
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

  scm_str_itr_begin(buf, size, SCM_PORT(port)->inn_enc, &iter);

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
      scm_error("illegal byte sequence", 0);
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
      scm_error("illegal byte sequence", 0);
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
    scm_error("illegal byte sequence", 0);
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
    scm_error("illegal byte sequence", 0);
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
                       unsigned int *attr, unsigned int *oflg)
{
  unsigned int a, o;
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
    scm_error("unknown mode", 0);
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
      scm_error("unknown mode", 0);
      return -1;
    }
  }

  if (!BIT_SET_P(a, SCM_PORT_ATTR_BINARY))
    a |= SCM_PORT_ATTR_TEXTUAL;

  if (attr != NULL) *attr |= a;
  if (oflg != NULL) *oflg |= o;

  return 0;
}

ScmObj
scm_port_P(ScmObj obj)
{
  return scm_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

int
scm_port_initialize(ScmObj port, ScmIO *io,
                    unsigned int attr, scm_port_buf_t buf_mode,
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
scm_port_new(scm_mem_type_t mtype,
             ScmIO *io, unsigned int attr, scm_port_buf_t buf_mode,
             ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  int rslt;

  scm_assert(io != NULL);
  scm_assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);
  scm_assert(inn_enc != NULL);

  port = scm_alloc_mem(&SCM_PORT_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(port)) return SCM_OBJ_NULL;

  rslt = scm_port_initialize(port, io, attr, buf_mode, inn_enc, enc);
  if (rslt < 0) return SCM_OBJ_NULL;

  return port;
}

ScmObj
scm_port_open_fd_inter(int fd, unsigned int attr, scm_port_buf_t buf_mode,
                       ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmIO *io;

  scm_assert(fd >= 0);
  scm_assert((attr & (SCM_PORT_ATTR_BINARY | SCM_PORT_ATTR_TEXTUAL)) != 0);
  scm_assert(!(BIT_SET_P(attr, SCM_PORT_ATTR_BINARY)
               && BIT_SET_P(attr, SCM_PORT_ATTR_TEXTUAL)));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_STRING));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_BUFFERED));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_CHARCONV));
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
scm_port_open_fd(int fd, const char *mode, scm_port_buf_t buf_mode,
                 ScmEncoding *inn_enc, const char *enc)
{
  unsigned int attr;
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
                         unsigned int attr, unsigned int oflg,
                         scm_port_buf_t buf_mode, mode_t perm,
                         ScmEncoding *inn_enc, const char *enc)
{
  ScmObj port = SCM_OBJ_INIT;
  ScmIO *io;
  unsigned int a;
  int flags;

  scm_assert(path != NULL);
  scm_assert((attr & (SCM_PORT_ATTR_BINARY | SCM_PORT_ATTR_TEXTUAL)) != 0);
  scm_assert(!(BIT_SET_P(attr, SCM_PORT_ATTR_BINARY)
               && BIT_SET_P(attr, SCM_PORT_ATTR_TEXTUAL)));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_STRING));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_BUFFERED));
  scm_assert(!BIT_SET_P(attr, SCM_PORT_ATTR_CHARCONV));
  scm_assert(inn_enc != NULL);

  a = attr & (SCM_PORT_ATTR_INPUT | SCM_PORT_ATTR_OUTPUT);
  if (a == SCM_PORT_ATTR_INPUT)
    flags = O_RDONLY;
  else if (a ==  SCM_PORT_ATTR_OUTPUT)
    flags = O_WRONLY;
  else
    flags = O_RDWR;

  if (BIT_SET_P(oflg, SCM_PORT_OFLG_TRUNC))  flags |= O_TRUNC;
  if (BIT_SET_P(oflg, SCM_PORT_OFLG_APPEND)) flags |= O_APPEND;
  if (BIT_SET_P(oflg, SCM_PORT_OFLG_CREATE)) flags |= O_CREAT;
  if (BIT_SET_P(oflg, SCM_PORT_OFLG_EXCL))  flags |= O_EXCL;

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
                   scm_port_buf_t buf_mode, mode_t perm,
                   ScmEncoding *inn_enc, const char *enc)
{
  unsigned int attr, oflg;
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
  unsigned int attr;
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
    int ret = scm_io_ready(SCM_PORT(port)->io);
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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
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
        scm_error("failed to peek a character: illegal byte sequence", 0);
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
  if (*SCM_PORT(port)->encoding == '\0')
    return scm_enc_name(scm_port_internal_enc(port));
  else
    return SCM_PORT(port)->encoding;
}

int
scm_port_flush(ScmObj port)
{
  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_output_port_p(port)) {
    scm_error("port is not output port", 0);
    return -1;
  }

  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
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
  ssize_t n;
  size_t nr;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(buf != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }

  if (size == 0)
    return 0;

  for (nr = 0; nr < size; nr += (size_t)n) {
    n = scm_port_read(port, buf, size);
    if (n < 0)
      return -1;
    else if (n == 0)
      break;
    else if (SCM_PORT(port)->buf_mode == SCM_PORT_BUF_MODEST)
      break;
  }

  if (nr == 0)
    SCM_PORT(port)->eof_received_p = false;

  return (ssize_t)nr;
}

ssize_t
scm_port_read_char(ScmObj port, scm_char_t *chr)
{
  ssize_t rslt;

  scm_assert_obj_type(port, &SCM_PORT_TYPE_INFO);
  scm_assert(chr != NULL);

  if (!scm_port_input_port_p(port)) {
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }

  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\n', &lf);
  scm_enc_cnv_from_ascii(SCM_PORT(port)->inn_enc, '\r', &cr);

  lf_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, lf.bytes, sizeof(lf));
  cr_w = scm_enc_char_width(SCM_PORT(port)->inn_enc, cr.bytes, sizeof(cr));

  cr_exists_p = false;
  nr = 0;

  while (true) {
    if (nr > 0 && SCM_PORT(port)->buf_mode == SCM_PORT_BUF_MODEST) {
      bool ready_p;
      int r = scm_port_char_ready(port, &ready_p);
      if (r < 0)
        return -1;
      else if (!ready_p)
        break;
    }

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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }

  if (n == 0)
    return 0;

  for (i = 0; i < n; i++) {
    if (i > 0 && SCM_PORT(port)->buf_mode == SCM_PORT_BUF_MODEST) {
      bool ready_p;
      int r = scm_port_char_ready(port, &ready_p);
      if (r < 0)
        return -1;
      else if (!ready_p)
        break;
    }

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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }
  if (size > scm_port_pushback_buff_unused(port)) {
    /* TODO; change error message */
    scm_error("pushback buffer overflow", 0);
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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }

  w = scm_enc_char_width(SCM_PORT(port)->inn_enc,
                         chr->bytes, sizeof(scm_char_t));
  if (w <= 0) {
    /* TODO; change error message */
    scm_error("illegal byte sequence", 0);
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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }
  if (size > SCM_PORT_PUSHBACK_BUFF_SIZE) {
    scm_error("pushback buffer overflow", 0);
    return -1;
  }

  while (size > SCM_PORT(port)->pb_used) {
    ssize_t ret =
      scm_port_read_into_pushback_buf(port, size - SCM_PORT(port)->pb_used);
    if (ret < 0)
      return -1;
    else if (ret == 0)
      break;
    else if (SCM_PORT(port)->buf_mode == SCM_PORT_BUF_MODEST)
      break;
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
    scm_error("port is not input port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
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
        scm_error("illegal byte sequence", 0);
        return -1;
      }
      else
        return 0;
    }
  }

  if (rslt < 0) {
    /* TODO; change error message */
    scm_error("illegal byte sequence", 0);
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
    scm_error("port is not output port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
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
    scm_error("port is not output port", 0);
    return -1;
  }
  if (!scm_port_textual_port_p(port)) {
    scm_error("port is not textual port", 0);
    return -1;
  }
  if (scm_port_closed_p(port)) {
    scm_error("port is already closed", 0);
    return -1;
  }

  s = scm_enc_char_width(SCM_PORT(port)->inn_enc, chr.bytes, sizeof(chr));
  if (s < 0) {
    /* TODO; change error message */
    scm_error("illegal byte sequence", 0);
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
    scm_error("port is already closed", 0);
    return -1;
  }
  else if (scm_port_code_converted_port_p(port)) {
    scm_error("seek operation of the port is not supported", 0);
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
    scm_error("port is already closed", 1, port);
    return -1;
  }
  else if (scm_port_code_converted_port_p(port)) {
    scm_error("pos operation to code-converted port is not supported",
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
    scm_error("port is not string port", 0);
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
    scm_error("port is not string port", 0);
    return -1;
  }

  for (io = SCM_PORT(port)->io; (l = scm_io_lower(io)) != NULL; io = l);

  return (ssize_t)scm_stringio_length((ScmStringIO *)io);
}

void
scm_port_gc_initialize(ScmObj obj)
{
  SCM_PORT(obj)->io = NULL;
}

void
scm_port_gc_finalize(ScmObj obj)
{
  scm_port_finalize(obj);
}


/*******************************************************************/
/*  Predicate/Open/Close                                           */
/*******************************************************************/

ScmObj
scm_input_port_P(ScmObj obj)
{
  return scm_input_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_output_port_P(ScmObj obj)
{
  return scm_output_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_textual_port_P(ScmObj obj)
{
  return scm_textual_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_binary_port_P(ScmObj obj)
{
  return scm_binary_port_p(obj) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_input_port_open_P(ScmObj port)
{
  return scm_input_port_open_p(port) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_output_port_open_P(ScmObj port)
{
  return scm_output_port_open_p(port) ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_open_input_fd(int fd, const char *enc)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "r", SCM_PORT_BUF_DEFAULT,
                          scm_system_encoding(), scm_external_encoding());
}

ScmObj
scm_open_binary_input_fd(int fd)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "rb", SCM_PORT_BUF_DEFAULT,
                          scm_system_encoding(), NULL);
}

ScmObj
scm_open_output_fd(int fd, const char *enc)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "w", SCM_PORT_BUF_DEFAULT,
                          scm_system_encoding(), scm_external_encoding());
}

ScmObj
scm_open_binary_output_fd(int fd)
{
  scm_assert(fd >= 0);
  return scm_port_open_fd(fd, "wb", SCM_PORT_BUF_DEFAULT,
                          scm_system_encoding(), NULL);
}

ScmObj
scm_open_input_file(const char *path, const char *enc)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "r", SCM_PORT_BUF_DEFAULT,
                            0, scm_system_encoding(), scm_external_encoding());
}

ScmObj
scm_open_binary_input_file(const char *path)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "rb", SCM_PORT_BUF_DEFAULT,
                            0, scm_system_encoding(), NULL);
}

ScmObj
scm_open_output_file(const char *path, const char *enc)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "w", SCM_PORT_BUF_DEFAULT, 0644,
                            scm_system_encoding(), scm_external_encoding());
}

ScmObj
scm_open_binary_output_file(const char *path)
{
  scm_assert(path != NULL);
  return scm_port_open_file(path, "wb", SCM_PORT_BUF_DEFAULT,
                            0644, scm_system_encoding(), NULL);
}

ScmObj
scm_open_input_string_cstr(const char *str, const char *enc)
{
  return scm_port_open_string(str, (str == NULL)? 0 : strlen(str), "r",
                              scm_system_encoding(), scm_external_encoding());
}

ScmObj
scm_open_input_string(ScmObj str)
{
  scm_assert(scm_string_p(str));
  return scm_port_open_string(scm_string_content(str),
                              scm_string_bytesize(str),
                              "r",
                              scm_system_encoding(),
                              scm_enc_name(scm_string_encoding(str)));
}

ScmObj
scm_open_output_string(void)
{
  return scm_port_open_string(NULL, 0, "w", scm_system_encoding(), NULL);
}

ScmObj
scm_get_output_string(ScmObj port)
{
  const void *p;
  ssize_t s;
  const char *enc_name;
  ScmEncoding *e;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_port_string_port_p(port));
  scm_assert(scm_textual_port_P(port));

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
      scm_error("failed to get output string: unsupported encoding", 0);
      return SCM_OBJ_NULL;
    }
  }

  return scm_make_string_from_bin(p, (size_t)s, e);
}

ScmObj
scm_open_input_bytevector_cbytes(const void *bytes, size_t size)
{
  return scm_port_open_string(bytes, (bytes == NULL) ? 0 : size,
                              "rb", scm_system_encoding(), NULL);
}

ScmObj
scm_open_input_bytevector(ScmObj vec)
{
  scm_assert(scm_bytevector_p(vec));
  return scm_port_open_string(scm_bytevector_content(vec),
                              scm_bytevector_length(vec),
                              "rb",
                              scm_system_encoding(),
                              NULL);
}

ScmObj
scm_open_output_bytevector(void)
{
  return scm_port_open_string(NULL, 0, "wb", scm_system_encoding(), NULL);
}

ScmObj
scm_get_output_bytevector(ScmObj port)
{
  const void *p;
  ssize_t s;

  scm_assert(scm_output_port_p(port));
  scm_assert(scm_port_string_port_p(port));
  scm_assert(scm_binary_port_p(port));

  p = scm_port_string_buffer(port);
  if (p == NULL) return SCM_OBJ_NULL;

  s = scm_port_string_buffer_length(port);
  if (s < 0) return SCM_OBJ_NULL;

  return scm_make_bytevector_from_cv(p, (size_t)s);
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

  r = scm_cached_global_var_ref(SCM_CACHED_GV_CURRENT_INPUT_PORT,
                                SCM_CSETTER_L(val));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_error("unbound variable: current-input-port", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_parameter_p(val)) {
    val = scm_parameter_value(val);
    if (scm_obj_null_p(val)) return SCM_OBJ_NULL;
  }

  if (!scm_input_port_p(val)) {
    scm_error("failed to get default input-port: "
                  "input-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  if (textual && !scm_textual_port_p(val)) {
    scm_error("failed to get default input-port: "
                  "textual-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }


  if (binary && !scm_binary_port_p(val)) {
    scm_error("failed to get default input-port: "
                  "binary-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  return val;
}

ScmObj
scm_read(ScmObj port)
{
  ScmObj parser = SCM_OBJ_INIT, obj = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port,
                      &parser, &obj);

  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  scm_assert(scm_input_port_p(port));
  scm_assert(scm_textual_port_p(port));

  if (scm_port_closed_p(port)) {
    scm_error("failed to read a S-exp: input-port closed", 1, port);
    return SCM_OBJ_NULL;
  }

  parser = scm_parser_new(SCM_MEM_HEAP);
  if (scm_obj_null_p(parser)) return SCM_OBJ_NULL;

  return scm_parser_parse(parser, port);
}

ssize_t
scm_read_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  return scm_port_read_char(port, chr);
}

ScmObj
scm_read_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  SCM_REFSTK_INIT_REG(&port);

  s = scm_read_cchr(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return SCM_EOF_OBJ;
  else
    return scm_make_char(&chr, scm_port_internal_enc(port));
}

ssize_t
scm_peek_cchr(scm_char_t *chr, ScmObj port)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  return scm_port_peek_char(port, chr);
}

ScmObj
scm_peek_char(ScmObj port)
{
  scm_char_t chr;
  ssize_t s;

  s = scm_peek_cchr(&chr, port);
  if (s < 0) return SCM_OBJ_NULL;

  if (s == 0)
    return SCM_EOF_OBJ;
  else
    return scm_make_char(&chr, scm_port_internal_enc(port));
}

ScmObj
scm_read_line(ScmObj port)
{
  ScmObj line = SCM_OBJ_INIT;
  ScmStringIO *sio;
  ssize_t ret;

  SCM_REFSTK_INIT_REG(&port, &line);

  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return SCM_OBJ_NULL;
  }

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  ret = scm_port_read_line(port, (ScmIO *)sio);

  if (ret < 0)
    line = SCM_OBJ_NULL;
  else if (ret == 0)
    line = SCM_EOF_OBJ;
  else
    line = scm_make_string_from_bin(scm_stringio_buffer(sio),
                                    scm_stringio_length(sio),
                                    scm_port_internal_enc(port));

  scm_stringio_end(sio);

  return line;
}

int
scm_char_ready(ScmObj port, bool *rslt)
{
  if (scm_obj_null_p(port)) {
    port = default_input_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  return scm_port_char_ready(port, rslt);
}

ScmObj
scm_char_ready_P(ScmObj port)
{
  bool rslt;
  int ret;

  ret = scm_char_ready(port, &rslt);
  if (ret < 0) return SCM_OBJ_NULL;

  return rslt ? SCM_TRUE_OBJ : SCM_FALSE_OBJ;
}

ScmObj
scm_read_string(size_t n, ScmObj port)
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

  if (n == 0)
    return scm_make_string_from_bin(NULL, 0, scm_port_internal_enc(port));

  sio = scm_stringio_new(NULL, 0);
  if (sio == NULL) return SCM_OBJ_NULL;

  nr = scm_port_read_string(n, port, (ScmIO *)sio);
  if (nr < 0) goto end;

  if (scm_stringio_length(sio) == 0)
    str = SCM_EOF_OBJ;
  else
    str = scm_make_string_from_bin(scm_stringio_buffer(sio),
                                   scm_stringio_length(sio),
                                   scm_port_internal_enc(port));

 end:
  scm_stringio_end(sio);
  return str;
}

ssize_t
scm_read_cbytes(void *buf, size_t size, ScmObj port)
{
  SCM_REFSTK_INIT_REG(&port);

  if (scm_obj_null_p(port)) {
    port = default_input_port(false, true);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(buf != NULL);
  scm_assert(size <= SSIZE_MAX);

  if (!scm_binary_port_p(port)) {
    scm_error("port is not binary port", 0);
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

  r = scm_cached_global_var_ref(SCM_CACHED_GV_CURRENT_OUTPUT_PORT,
                                    SCM_CSETTER_L(val));
  if (r < 0) return SCM_OBJ_NULL;

  if (scm_obj_null_p(val)) {
    scm_error("unbound variable: current-output-port", 0);
    return SCM_OBJ_NULL;
  }

  if (scm_parameter_p(val)) {
    val = scm_parameter_value(val);
    if (scm_obj_null_p(val)) return SCM_OBJ_NULL;
  }

  if (!scm_output_port_p(val)) {
    scm_error("failed to get default output-port: "
                  "output-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  if (binary && !scm_binary_port_p(val)) {
    scm_error("failed to get default output-port: "
                  "binary-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  if (textual && !scm_textual_port_p(val)) {
    scm_error("failed to get default output-port: "
                  "textual-port required, but got", 1, val);
    return SCM_OBJ_NULL;
  }

  return val;
}

static ScmObj
ws_acons(ScmObj car, ScmObj cdr, ScmObj alist)
{
  ScmObj pair = SCM_OBJ_INIT;

  scm_assert(scm_obj_not_null_p(car));
  scm_assert(scm_obj_not_null_p(cdr));
  scm_assert(scm_pair_p(alist) || scm_nil_p(alist));

  SCM_REFSTK_INIT_REG(&car, &cdr, &alist,
                      &pair);

  pair = scm_cons(car, cdr);
  if (scm_obj_null_p(pair)) return SCM_OBJ_NULL;

  return scm_cons(pair, alist);
}

static ScmObj
ws_scan_internal(ScmObj obj, bool (*interesting_p)(ScmObj), ScmObj alist)
{
  ScmObj pair = SCM_OBJ_INIT, elm = SCM_OBJ_INIT;

  scm_assert(interesting_p != NULL);
  scm_assert(scm_pair_p(alist) || scm_nil_p(alist));

  SCM_REFSTK_INIT_REG(&obj, &alist,
                      &pair, &elm);

  if (!interesting_p(obj))
    return alist;

  pair = scm_assq(obj, alist);
  if (scm_true_p(pair)) {
    if (scm_true_p(scm_cdr(pair)))
      return alist;
    else
      return ws_acons(obj, SCM_TRUE_OBJ, alist);
  }

  alist = ws_acons(obj, SCM_FALSE_OBJ, alist);
  if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

  if (scm_pair_p(obj)) {
    elm = scm_cdr(obj);
    alist = ws_scan_internal(elm, interesting_p, alist);
    if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

    elm = scm_car(obj);
    return ws_scan_internal(elm, interesting_p, alist);
  }
  else if (scm_vector_p(obj)) {
    size_t len = scm_vector_length(obj);
    for (size_t i = 0; i < len; i++) {
      elm = scm_vector_ref(obj, i);
      alist = ws_scan_internal(elm, interesting_p, alist);
      if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;
    }
    return alist;
  }
  else {
    return alist;
  }
}

static ScmObj
ws_scan(ScmObj obj, bool (*interesting_p)(ScmObj))
{
  ScmObj alist = SCM_OBJ_INIT;

  alist = ws_scan_internal(obj, interesting_p, SCM_NIL_OBJ);
  if (scm_obj_null_p(alist)) return SCM_OBJ_NULL;

  return ws_acons(SCM_EOF_OBJ, SCM_FIXNUM_NN_1, alist);
}

static int
ws_extract_counter(ScmObj alist, scm_sword_t *n)
{
  ScmObj num = SCM_OBJ_INIT;

  scm_assert(scm_pair_p(alist) || scm_nil_p(alist));
  scm_assert(n != NULL);

  num = scm_cxr(alist, "da");
  return scm_integer_to_sword(num, n);
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
  scm_assert(scm_pair_p(alist) || scm_nil_p(alist));

  r = ws_extract_counter(alist, &n);
  if (r < 0) return -1;

  if (n >= SCM_SWORD_MAX) {
    scm_error("failed to print shared structure object: "
                  "too many shared object", 0);
    return -1;
  }

  snprintf(str, sizeof(str), "#%lu=", ++n);
  r = scm_write_cstr(str, SCM_ENC_SRC, port);
  if (r < 0) return -1;

  num = scm_make_number_from_sword(n);
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

  scm_assert(scm_num_integer_p(num));

  r = scm_integer_to_size_t(num, &n);
  if (r < 0) return -1;

  snprintf(str, sizeof(str), "#%lu#", n);
  r = scm_write_cstr(str, SCM_ENC_SRC, port);
  if (r < 0) return -1;

  return 0;
}

static bool
obj_print_handler_interesting_p_shared(ScmObj obj)
{
  return (scm_pair_p(obj)
          || scm_string_p(obj)
          || scm_vector_p(obj)
          || scm_bytevector_p(obj));
}

static bool
obj_print_handler_interesting_p_display(ScmObj obj)
{
  return (scm_pair_p(obj)
          || scm_vector_p(obj));
}

static int
obj_print_handler_print_obj_shared(ScmObjPrintHandler handler,
                                   ScmObj obj, ScmObj port, int kind)
{
  ScmObj alist = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  bool (*interesting_p)(ScmObj);

  alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
  interesting_p = SCM_OBJ_PRINT_HANDLER_BODY(handler)->interesting_p;
  if (interesting_p(obj)) {
    val = scm_cdr(scm_assq(obj, alist));
    if (scm_number_p(val))
      return ws_print_shared_use(val, port);
    else if (scm_true_p(val))
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

#define obj_print_handler_print_obj_display obj_print_handler_print_obj_shared

static int
obj_print_handler_print_list(ScmObjPrintHandler handler,
                             ScmObj obj, ScmObj port, int kind)
{
  ScmObj lst = SCM_OBJ_INIT, car = SCM_OBJ_INIT, cdr = SCM_OBJ_INIT;
  ScmObj alist = SCM_OBJ_INIT, val = SCM_OBJ_INIT;
  bool (*interesting_p)(ScmObj);
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port,
                      &lst, &car, &cdr,
                      &alist, &val);

  scm_assert(scm_pair_p(obj));

  alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
  interesting_p = SCM_OBJ_PRINT_HANDLER_BODY(handler)->interesting_p;

  rslt = scm_write_cstr("(", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  lst = obj;
  while (1) {
    car = scm_car(lst);
    cdr = scm_cdr(lst);

    rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler, car, port, kind);
    if (rslt < 0) return -1;

    if (scm_nil_p(cdr)) {
      break;
    }
    else if (!scm_pair_p(cdr)) {
      rslt = scm_write_cstr(" . ", SCM_ENC_SRC, port);
      if (rslt < 0) return -1;

      rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(handler, cdr, port, kind);
      if (rslt < 0) return -1;
      break;
    }
    else if (kind != SCM_OBJ_PRINT_SIMPLE && interesting_p(cdr)) {
      alist = SCM_OBJ_PRINT_HANDLER_BODY(handler)->val;
      val = scm_cdr(scm_assq(cdr, alist));
      if (scm_true_p(val)) {
        rslt = scm_write_cstr(" . ", SCM_ENC_SRC, port);
        if (rslt < 0) return -1;

        if (scm_number_p(val))
          rslt = ws_print_shared_use(val, port);
        else
          rslt = ws_print_shared_dec(cdr, port, kind, handler);

        if (rslt < 0) return -1;
        break;
      }
    }

    rslt = scm_write_cstr(" ", SCM_ENC_SRC, port);
    if (rslt < 0) return -1;

    lst = cdr;
  }

  rslt = scm_write_cstr(")", SCM_ENC_SRC, port);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_write_shared(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .interesting_p = obj_print_handler_interesting_p_shared,
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

  handler.val = ws_scan(obj, handler.interesting_p);
  if (scm_obj_null_p(handler.val)) return -1;

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_SHARED);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_write_simple(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .interesting_p = NULL,
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

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_SIMPLE);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_display(ScmObj obj, ScmObj port)
{
  ScmObjPrintHandlerBody handler = {
    .interesting_p = obj_print_handler_interesting_p_display,
    .print_obj = obj_print_handler_print_obj_display,
    .print_list = obj_print_handler_print_list,
    .val = SCM_OBJ_NULL
  };
  int rslt;

  SCM_REFSTK_INIT_REG(&obj, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  handler.val = ws_scan(obj, handler.interesting_p);
  if (scm_obj_null_p(handler.val)) return -1;

  rslt = SCM_OBJ_PRINT_HANDLER_PRINT_OBJ(SCM_OBJ_PRINT_MAKE_HANDLER(handler),
                                         obj, port, SCM_OBJ_PRINT_DISPLAY);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_newline(ScmObj port)
{
  ScmEncoding *enc;
  scm_char_t nl;
  ssize_t rslt;

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  enc = scm_port_internal_enc(port);
  scm_enc_cnv_from_ascii(enc, '\n', &nl);
  rslt = scm_port_write_char(port, nl);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_write_cchr(scm_char_t chr, ScmEncoding *enc, ScmObj port)
{
  ScmObj c = SCM_OBJ_INIT;
  int r;

  SCM_REFSTK_INIT_REG(&port,
                      &c);

  scm_assert(enc != NULL);

  /* TODO: scm_write_char を使わない (char オブジェクトを生成しない) */

  c = scm_make_char(&chr, enc);
  if (scm_obj_null_p(c)) return -1;

  r = scm_write_char(c, port);
  if (r < 0) return -1;

  return 0;
}

int
scm_write_char(ScmObj chr, ScmObj port)
{
  ScmEncoding *p_enc, *c_enc;
  scm_char_t c;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&chr, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_char_p(chr));

  p_enc = scm_port_internal_enc(port);
  c_enc = scm_char_encoding(chr);

  if (p_enc != c_enc) {
    chr = scm_char_encode(chr, p_enc);
    if (scm_obj_null_p(chr)) return -1;
  }

  c = scm_char_value(chr);
  rslt = scm_port_write_char(port, c);
  if (rslt < 0) return -1;

  return 0;
}

int
scm_write_cstr(const char *str, ScmEncoding *enc, ScmObj port)
{
  ScmObj s = SCM_OBJ_INIT;

  SCM_REFSTK_INIT_REG(&port, &s);

  scm_assert(enc != NULL);

  /* TODO: scm_write_string を使わない (string オブジェクトを生成しない) */

  s = scm_make_string_from_cstr(str, enc);
  if (scm_obj_null_p(s)) return -1;

  return scm_write_string(s, port, -1, -1);
}

int
scm_write_string(ScmObj str, ScmObj port, ssize_t start, ssize_t end)
{
  ScmEncoding *p_enc;
  ssize_t rslt;

  SCM_REFSTK_INIT_REG(&str, &port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(true, false);
    if (scm_obj_null_p(port)) return -1;
  }

  scm_assert(scm_string_p(str));
  scm_assert(start < 0 || (size_t)start < scm_string_length(str));
  scm_assert(end < 0 || (size_t)end <= scm_string_length(str));
  scm_assert(start < 0 || end < 0 || start <= end);

  if (!scm_textual_port_p(port)) {
    scm_error("failed to write string: output-port is not textual-port",
              1, port);
    return -1;
  }

  if (start != 0 || end != (ssize_t)scm_string_length(str)) {
    str = scm_string_copy(str, start, end);
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
scm_write_cbytes(const void *bytes, size_t size, ScmObj port)
{
  ssize_t rslt;

  if (scm_obj_null_p(port)) {
    port = default_output_port(false, true);
    if (scm_obj_null_p(port)) return -1;
  }

  if (!scm_binary_port_p(port)) {
    scm_error("failed to write byte sequences: output-port is not binary-port",
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
scm_flush_output_port(ScmObj port)
{
  int rslt;

  SCM_REFSTK_INIT_REG(&port);

  if (scm_obj_null_p(port)) {
    port = default_output_port(false, false);
    if (scm_obj_null_p(port)) return -1;
  }

  rslt = scm_port_flush(port);
  if (rslt < 0) return -1;

  return 0;
}
