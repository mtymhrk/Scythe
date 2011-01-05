#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

#include "memory.h"
#include "object.h"
#include "reference.h"
#include "obuffer.h"
#include "charconv.h"
#include "port.h"

/* XXX: implementation of encoding translation port has issue of GC */


#define SCM_SYSCALL(result, expr)                 \
  do {                                            \
    (result) = (expr);                            \
  } while (result < 0 && errno == EINTR)

#define BIT_IS_SETED(val, bit) (((val) & (bit)) ? true : false)

#define SCM_STRINGIO_INIT_BUF_SIZE 64
#define SCM_PORT_DEFAULT_BUF_SIZE 256
#define SCM_CHARCONVIO_DEFAULT_BLOCK_SIZE 256

typedef void (*DestructFunc)(ScmIO *io);
typedef ssize_t (*ReadFunc)(ScmIO *io, void *buf, size_t size);
typedef ssize_t (*WriteFunc)(ScmIO *io, const void *buf, size_t size);
typedef off_t (*SeekFunc)(ScmIO *io, off_t offset, int whence);
typedef int (*CloseFunc)(ScmIO *io);
typedef void (*ClearErrorFunc)(ScmIO *io);
typedef bool (*IsReadyFunc)(ScmIO *io);
typedef bool (*IsClosedFunc)(ScmIO *io);
typedef bool (*IsEOFFunc)(ScmIO *io);
typedef SCM_PORT_BUF_MODE (*BufferModeFunc)(ScmIO *io);
typedef ssize_t (*BlockSizeFunc)(ScmIO *io);
typedef bool (*HasErrorFunc)(ScmIO *io);
typedef int (*ErrnoFunc)(ScmIO *io);

struct ScmIORec {
  DestructFunc destruct;
  ReadFunc read;
  WriteFunc write;
  SeekFunc seek;
  CloseFunc close;
  ClearErrorFunc clear_error;
  IsReadyFunc is_ready;
  IsClosedFunc is_closed;
  IsEOFFunc is_eof;
  BufferModeFunc default_buffer_mode;
  BlockSizeFunc block_size;
  HasErrorFunc has_error;
  ErrnoFunc error_no;
  int ref_cnt;
};

struct ScmFileIORec {
  ScmIO io_base;
  int fd;
  int error_no;
  bool is_eof;
  bool is_closed;
};

struct ScmStringIORec {
  ScmIO io_base;
  char *string;
  size_t capacity;
  size_t length;
  size_t pos;
  bool is_closed;
};

struct ScmCharConvIORec {
  ScmIO io_base;
  ScmIO *io;
  ScmCharConv *in_conv;
  ScmCharConv *out_conv;
  size_t block_size;
  bool is_closed;
  bool is_owner;
};


ScmTypeInfo SCM_PORT_TYPE_INFO = {
  NULL,                       /* pp_func              */
  sizeof(ScmPort),            /* obj_size             */
  NULL,                       /* gc_ini_func          */
  scm_port_gc_finalize,       /* gc_fin_func          */
  NULL,                       /* gc_accept_func       */
  NULL,                       /* gc_accpet_func_weak  */
};

static void
scm_io_initialize(ScmIO *io,
                  DestructFunc destruct,
                  ReadFunc read,
                  WriteFunc write,
                  SeekFunc seek,
                  CloseFunc close,
                  ClearErrorFunc clear_error,
                  IsReadyFunc is_ready,
                  IsClosedFunc is_closed,
                  IsEOFFunc is_eof,
                  BufferModeFunc default_buffer_mode,
                  BlockSizeFunc block_size,
                  HasErrorFunc has_error,
                  ErrnoFunc error_no)
{
  assert(io != NULL);

  io->destruct = destruct;
  io->read = read;
  io->write = write;
  io->seek = seek;
  io->close = close;
  io->clear_error = clear_error;
  io->is_ready = is_ready;
  io->is_closed = is_closed;
  io->is_eof = is_eof;
  io->default_buffer_mode = default_buffer_mode;
  io->block_size = block_size;
  io->has_error = has_error;
  io->error_no = error_no;

  io->ref_cnt = 1;
}

void
scm_io_referred(ScmIO *io)
{
  assert(io != NULL);
  io->ref_cnt++;
}

void
scm_io_end(ScmIO *io)
{
  assert(io != NULL);
  io->ref_cnt--;
  if (io->ref_cnt <= 0 && io->destruct != NULL)
    io->destruct(io);
}

ssize_t
scm_io_read(ScmIO *io, void *buf, size_t size)
{
  assert(io != NULL);
  return (io->read != NULL) ? io->read(io, buf, size) : -1;
}

ssize_t
scm_io_write(ScmIO *io, const void *buf, size_t size)
{
  assert(io != NULL);
  return (io->write != NULL) ? io->write(io, buf, size) : -1;
}

bool
scm_io_seek(ScmIO *io, off_t offset, int whence)
{
  assert(io != NULL);
  return (io->seek != NULL) ? io->seek(io, offset, whence) : -1;
}

int
scm_io_close(ScmIO *io)
{
  assert(io != NULL);
  return (io->close != NULL) ? io->close(io) : -1;
}

void
scm_io_clear_error(ScmIO *io)
{
  assert(io != NULL);
  if (io->clear_error != NULL)
    io->clear_error(io);
}

bool
scm_io_is_ready(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_ready != NULL) ? io->is_ready(io) : true;
}

bool
scm_io_is_closed(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_closed != NULL) ? io->is_closed(io) : false;
}

bool
scm_io_is_eof(ScmIO *io)
{
  assert(io != NULL);
  return (io->is_eof != NULL) ? io->is_eof(io) : false;
}

SCM_PORT_BUF_MODE
scm_io_default_buffer_mode(ScmIO *io)
{
  assert(io != NULL);
  if (io->default_buffer_mode != NULL)
    return io->default_buffer_mode(io);
  else
    return SCM_PORT_BUF_FULL;
}

ssize_t
scm_io_block_size(ScmIO *io)
{
  assert(io != NULL);
  return (io->block_size != NULL) ? io->block_size(io) : -1;
}

bool
scm_io_has_error(ScmIO *io)
{
  assert(io != NULL);
  return (io->has_error != NULL) ? io->has_error(io) : false;
}

int
scm_io_errno(ScmIO *io)
{
  assert(io != NULL);
  return (io->error_no != NULL) ? io->error_no(io) : 0;
}

ScmFileIO *
scm_fileio_new(int fd)
{
  ScmFileIO *fileio;

  fileio = scm_memory_allocate(sizeof(ScmFileIO));
  scm_io_initialize((ScmIO *)fileio,
                    (DestructFunc)scm_fileio_end,
                    (ReadFunc)scm_fileio_read,
                    (WriteFunc)scm_fileio_write,
                    (SeekFunc)scm_fileio_seek,
                    (CloseFunc)scm_fileio_close,
                    (ClearErrorFunc)scm_fileio_clear_error,
                    (IsReadyFunc)scm_fileio_is_ready,
                    (IsClosedFunc)scm_fileio_is_closed,
                    (IsEOFFunc)scm_fileio_is_eof,
                    (BufferModeFunc)scm_fileio_default_buffer_mode,
                    (BlockSizeFunc)scm_fileio_block_size,
                    (HasErrorFunc)scm_fileio_has_error,
                    (ErrnoFunc)scm_fileio_errno);
  fileio->fd = fd;
  fileio->error_no = 0;
  fileio->is_eof = false;
  fileio->is_closed = false;

  return fileio;
}

void
scm_fileio_end(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  
  scm_fileio_close(fileio);
  scm_memory_release(fileio);
}

ScmFileIO *
scm_fileio_open(const char *pathname, int flags, mode_t mode)
{
  int fd;

  assert(pathname != NULL);

  SCM_SYSCALL(fd, open(pathname, flags, mode));
  if (fd < 0) return NULL;

  return scm_fileio_new(fd);
}

ssize_t
scm_fileio_read(ScmFileIO *fileio, void *buf, size_t size)
{
  ssize_t n;

  assert(fileio != NULL);
  assert(buf != NULL);

  if (fileio->is_eof) return 0;
  else if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, read(fileio->fd, buf, size));
  if (n == 0)
    fileio->is_eof = true;
  if (n < 0)
    fileio->error_no = errno;

  return n;
}

ssize_t
scm_fileio_write(ScmFileIO *fileio, const void *buf, size_t size)
{
  ssize_t n;

  assert(fileio != NULL);
  assert(buf != NULL);

  if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, write(fileio->fd, buf, size));
  if (n < 0)
    fileio->error_no = errno;
  
  return n;
}

bool
scm_fileio_is_ready(ScmFileIO *fileio)
{
  int n;
  fd_set readfds;
  struct timeval tout;

  assert(fileio != NULL);

  if (fileio->is_eof) return false;
  else if (fileio->is_closed) return false;
  else if (fileio->error_no != 0) return false;

  FD_ZERO(&readfds);
  FD_SET(fileio->fd, &readfds);
  tout.tv_sec = 0;
  tout.tv_usec = 0;

  SCM_SYSCALL(n, select(fileio->fd + 1, &readfds, NULL, NULL, &tout));
  if (n < 0)
    fileio->error_no = errno;
  
  if (n > 0)
    return true;
  else
    return false;
}

off_t
scm_fileio_seek(ScmFileIO *fileio, off_t offset, int whence)
{
  off_t n;

  assert(fileio != NULL);

  if (fileio->is_closed) return -1;
  else if (fileio->error_no != 0) return -1;

  SCM_SYSCALL(n, lseek(fileio->fd, offset, whence));
  if (n < 0)
    fileio->error_no = errno;

  fileio->is_eof = false;

  return n;
}

int
scm_fileio_close(ScmFileIO *fileio)
{
  int n;

  assert(fileio != NULL);

  if (!fileio->is_closed) {
    SCM_SYSCALL(n, close(fileio->fd));
    if (n < 0)
      fileio->error_no = errno;
    else
      fileio->is_closed = true;
    return n;
  }
  else
    return 0;
}

bool
scm_fileio_is_closed(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->is_closed;
}

bool
scm_fileio_is_eof(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->is_eof;
}

void
scm_fileio_clear_error(ScmFileIO *fileio)
{
  assert(fileio != NULL);

  fileio->error_no = 0;
}

SCM_PORT_BUF_MODE
scm_fileio_default_buffer_mode(ScmFileIO *fileio)
{
  struct stat st;
  int ret;

  assert(fileio != NULL);

  if (isatty(fileio->fd))
    return SCM_PORT_BUF_MODEST;

  SCM_SYSCALL(ret, fstat(fileio->fd, &st));
  if (ret < 0) {
    fileio->error_no = errno;
    return SCM_PORT_BUF_FULL;
  }

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

  assert(fileio != NULL);

  SCM_SYSCALL(rslt, fstat(fileio->fd, &stat));
  if (rslt < 0)
    return -1;
  else
    return stat.st_blksize;
}

bool
scm_fileio_has_error(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->error_no != 0;
}

int
scm_fileio_errno(ScmFileIO *fileio)
{
  assert(fileio != NULL);
  return fileio->error_no;
}

void
scm_stringio_expand_buffer(ScmStringIO *strio, size_t needed_size)
{
  size_t new_size;

  assert(strio != NULL);

  for (new_size = SCM_STRINGIO_INIT_BUF_SIZE;
       new_size < needed_size;
       new_size *= 2)
    ;

  if (strio->string == NULL) {
    strio->string = scm_memory_allocate(new_size);
    strio->capacity = new_size;
  }
  else if (new_size > strio->capacity) {
    char *new_buffer = scm_memory_allocate(new_size);
    memset(new_buffer, 0, new_size);
    memcpy(new_buffer, strio->string, strio->length);
    scm_memory_release(strio->string);
    strio->string = new_buffer;
    strio->capacity = new_size;
  }
}


ScmStringIO *
scm_stringio_new(const char *str, size_t len)
{
  ScmStringIO *strio;

  strio = scm_memory_allocate(sizeof(ScmStringIO));
  scm_io_initialize((ScmIO *)strio,
                    (DestructFunc)scm_stringio_end,
                    (ReadFunc)scm_stringio_read,
                    (WriteFunc)scm_stringio_write,
                    (SeekFunc)scm_stringio_seek,
                    (CloseFunc)scm_stringio_close,
                    (ClearErrorFunc)scm_stringio_clear_error,
                    (IsReadyFunc)scm_stringio_is_ready,
                    (IsClosedFunc)scm_stringio_is_closed,
                    (IsEOFFunc)scm_stringio_is_eof,
                    (BufferModeFunc)scm_stringio_default_buffer_mode,
                    (BlockSizeFunc)NULL,
                    (HasErrorFunc)scm_stringio_has_error,
                    (ErrnoFunc)scm_stringio_errno);
  strio->string = NULL;
  strio->capacity = 0;
  strio->length = 0;
  strio->pos = 0;
  strio->is_closed = false;

  if (str != NULL) {
    scm_stringio_expand_buffer(strio, len);
    memcpy(strio->string, str, len);
    strio->length = len;
  }
  else
    scm_stringio_expand_buffer(strio, 0);

  return strio;
}

void
scm_stringio_end(ScmStringIO *strio)
{
  assert(strio != NULL);
  scm_memory_release(strio->string);
  scm_memory_release(strio);
}

size_t
scm_stringio_read(ScmStringIO *strio, void *buf, size_t size)
{
  size_t n;
  size_t rest;

  assert(strio != NULL);
  assert(buf != NULL);

  if (strio->is_closed) return -1;

  rest = strio->length - strio->pos;
  if (rest <= 0) return 0;

  n = (size <= rest) ? size : rest;
  memcpy(buf, strio->string + strio->pos, n);
  strio->pos += n;

  return n;
}

size_t
scm_stringio_write(ScmStringIO *strio, const void *buf, size_t size)
{
  assert(strio != NULL);
  assert(buf != NULL);

  if (strio->is_closed) return -1;

  scm_stringio_expand_buffer(strio, strio->pos + size);
  memcpy(strio->string + strio->pos, buf, size);
  strio->pos += size;
  if (strio->pos > strio->length)
    strio->length = strio->pos;

  return size;
}


bool
scm_stringio_is_ready(ScmStringIO *strio)
{
  assert(strio != NULL);

  if (strio->is_closed) return false;

  return (strio->pos < strio->length);
}

off_t
scm_stringio_seek(ScmStringIO *strio, off_t offset, int whence)
{
  off_t pos;

  assert(strio != NULL);

  if (strio->is_closed) return -1;

  switch (whence) {
  case SEEK_SET:
    pos = offset;
    break;
  case SEEK_CUR:
    pos = strio->pos + offset;
    break;
  case SEEK_END:
    pos = strio->length + offset;
    break;
  default:
    return -1;
  }

  scm_stringio_expand_buffer(strio, pos);
  strio->pos = (size_t)pos;

  return pos;
}

int
scm_stringio_close(ScmStringIO *strio)
{
  assert(strio != NULL);
  strio->is_closed = true;
  return 0;
}

bool
scm_stringio_is_closed(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->is_closed;
}

bool
scm_stringio_is_eof(ScmStringIO *strio)
{
  assert(strio != NULL);  
  return (strio->pos >= strio->length);
}

void
scm_stringio_clear_error(ScmStringIO *strio)
{
  assert(strio != NULL);
}

SCM_PORT_BUF_MODE
scm_stringio_default_buffer_mode(ScmStringIO *strio)
{
  assert(strio != NULL);
  return SCM_PORT_BUF_NONE;
}

bool
scm_stringio_has_error(ScmStringIO *strio)
{
  assert(strio != NULL);
  return false;
}

int
scm_stringio_errno(ScmStringIO *strio)
{
  assert(strio != NULL);
  return 0;
}

char *
scm_stringio_buffer(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->string;
}

size_t
scm_stringio_length(ScmStringIO *strio)
{
  assert(strio != NULL);
  return strio->length;
}

ScmCharConvIO *
scm_charconvio_new(ScmIO *io,
                         const char *internal_encode,
                         const char *external_encode,
                         bool owner)
{
  ScmCharConvIO *convio;

  assert(io != NULL);

  convio = scm_memory_allocate(sizeof(ScmCharConvIO));
  scm_io_initialize((ScmIO *)convio,
                    (DestructFunc)scm_charconvio_end,
                    (ReadFunc)scm_charconvio_read,
                    (WriteFunc)scm_charconvio_write,
                    (SeekFunc)scm_charconvio_seek,
                    (CloseFunc)scm_charconvio_close,
                    (ClearErrorFunc)scm_charconvio_clear_error,
                    (IsReadyFunc)scm_charconvio_is_ready,
                    (IsClosedFunc)scm_charconvio_is_closed,
                    (IsEOFFunc)scm_charconvio_is_eof,
                    (BufferModeFunc)scm_charconvio_default_buffer_mode,
                    (BlockSizeFunc)scm_charconvio_block_size,
                    (HasErrorFunc)scm_charconvio_has_error,
                    (ErrnoFunc)scm_charconvio_errno);

  convio->io = io;
  convio->is_closed = false;
  convio->is_owner = owner;

  convio->in_conv = scm_charconv_new(external_encode, internal_encode,
                                           SCM_CHARCONV_ERROR);
  if (convio->in_conv == NULL) {
    scm_memory_release(convio);
    return NULL;
  }

  convio->out_conv = scm_charconv_new(internal_encode, external_encode,
                                            SCM_CHARCONV_ERROR);
  if (convio->out_conv == NULL) {
    scm_charconv_end(convio->in_conv);
    scm_memory_release(convio);
    return NULL;
  }

  convio->block_size = scm_io_block_size(convio->io);
  if (convio->block_size <= 0)
    convio->block_size = SCM_CHARCONVIO_DEFAULT_BLOCK_SIZE;

  scm_io_referred(convio->io);

  return convio;
}

void
scm_charconvio_end(ScmCharConvIO *convio)
{
  assert(convio != NULL);

  scm_io_end(convio->io);
  scm_charconv_end(convio->in_conv);
  scm_charconv_end(convio->out_conv);
  scm_memory_release(convio);
}

ssize_t
scm_charconvio_read(ScmCharConvIO *convio, void *buf, size_t size)
{
  ssize_t rest, nread, nconv;
  void *ptr;

  assert(convio != NULL);
  assert(buf != NULL);

  if (scm_io_is_closed(convio->io)) return -1;
  if (scm_io_has_error(convio->io)) return -1;
  if (convio->is_closed) return -1;

  rest = size;
  ptr = buf;

  nconv = scm_charconv_get(convio->in_conv, ptr, rest);
  if (nconv < 0)
    return -1;

  ptr += nconv;
  rest -= nconv;

  while (rest > 0 && !scm_io_is_eof(convio->io)) {
    char block[convio->block_size];

    nread = scm_io_read(convio->io, block, convio->block_size);
    if (nread < 0)
      return -1;

    if (nread == 0)
      scm_charconv_terminate(convio->in_conv, NULL, 0);
    else
      scm_charconv_put(convio->in_conv, block, nread);

    nconv = scm_charconv_get(convio->in_conv, ptr, rest);
    ptr += nconv;
    rest -= nconv;

    if (scm_charconv_has_error(convio->in_conv))
      return -1;

    if (nread < convio->block_size)
      break;
  }

  return size - rest;
}

ssize_t
scm_charconvio_write(ScmCharConvIO *convio, const void *buf, size_t size)
{
  ssize_t nconv, nwrite;
  
  assert(convio != NULL);
  assert(buf != NULL);

  if (scm_io_is_closed(convio->io)) return -1;
  if (scm_io_has_error(convio->io)) return -1;
  if (convio->is_closed) return -1;

  scm_charconv_put(convio->out_conv, buf, size);
  if (scm_charconv_has_error(convio->out_conv))
    return -1;

  do {
    char block[convio->block_size];
    
    nconv = scm_charconv_get(convio->out_conv, block, convio->block_size);
    nwrite = scm_io_write(convio->io, block, nconv);
    if (nwrite < 0)
      return -1;

  } while (nconv > 0);

  return size;
}

bool
scm_charconvio_is_ready(ScmCharConvIO *convio)
{
  assert(convio != NULL);

  if (convio->io == NULL) return false;
  if (convio->is_closed) return false;
  if (scm_charconv_has_error(convio->in_conv)) return false;

  if (scm_charconv_is_ready(convio->in_conv)) return true;
  if (scm_io_is_ready(convio->io)) return true;

  return false;
}

off_t
scm_charconvio_seek(ScmCharConvIO *convio, off_t offset, int whence)
{
  off_t n;
  assert(convio != NULL);

  n = scm_io_seek(convio->io, offset, whence);
  if (n < 0) return n;

  scm_charconv_clear(convio->in_conv);
  scm_charconv_clear(convio->out_conv);

  return n;
}

int
scm_charconvio_close(ScmCharConvIO *convio)
{
  assert(convio != NULL);

  if (convio->is_closed) return 0;
  if (!scm_charconv_has_error(convio->out_conv)
      && !scm_io_has_error(convio->io)) {
    char block[convio->block_size];
    ssize_t nconv, nwrite;

    scm_charconv_terminate(convio->out_conv, NULL, 0);
    while ((nconv = scm_charconv_get(convio->out_conv,
                                     block, convio->block_size))
           > 0) {
      nwrite = scm_io_write(convio->io, block, nconv);
      if (nwrite < nconv) break;
    }
  }

  convio->is_closed = true;
  if (convio->is_owner) scm_io_close(convio->io);

  return 0;
}

bool
scm_charconvio_is_closed(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  return convio->is_closed;
}

bool
scm_charconvio_is_eof(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  return (scm_io_is_eof(convio->io) && !scm_charconv_is_ready(convio->in_conv));
}

void
scm_charconvio_clear_error(ScmCharConvIO *convio)
{
  assert(convio != NULL);

  if (scm_io_has_error(convio->io))
    scm_io_clear_error(convio->io);
}

SCM_PORT_BUF_MODE
scm_charconvio_default_buffer_mode(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  return scm_io_default_buffer_mode(convio->io);
}

ssize_t
scm_charconvio_block_size(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  return scm_io_block_size(convio->io);
}

bool
scm_charconvio_has_error(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  return (scm_io_has_error(convio->io)
          || scm_charconv_has_error(convio->in_conv)
          || scm_charconv_has_error(convio->out_conv));
}

int
scm_charconvio_errno(ScmCharConvIO *convio)
{
  assert(convio != NULL);
  if (scm_charconv_has_error(convio->in_conv))
    return scm_charconv_errorno(convio->in_conv);
  else if (scm_charconv_has_error(convio->out_conv))
    return scm_charconv_errorno(convio->out_conv);
  else if (scm_io_has_error(convio->io))
    return scm_io_errno(convio->io);
  else
    return 0;
}

static void
scm_port_init_buffer(ScmObj port, SCM_PORT_BUF_MODE buf_mode)
{
  ssize_t s;

  SCM_STACK_FRAME_PUSH(&port);

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  if (buf_mode == SCM_PORT_BUF_DEFAULT)
    SCM_PORT_BUFFER_MODE(port) = scm_io_default_buffer_mode(SCM_PORT_IO(port));
  else
    SCM_PORT_BUFFER_MODE(port) = buf_mode;

  SCM_PORT_POS(port) = 0;
  SCM_PORT_USED(port) = 0;
  switch (SCM_PORT_BUFFER_MODE(port)) {
  case SCM_PORT_BUF_FULL:   /* fall through */
  case SCM_PORT_BUF_LINE:   /* fall through */
  case SCM_PORT_BUF_MODEST: /* fall through */
    s = scm_io_block_size(SCM_PORT_IO(port));
    if (s >= 0)
      SCM_PORT_CAPACITY(port) = s;
    else
      SCM_PORT_CAPACITY(port) = SCM_PORT_DEFAULT_BUF_SIZE;
    break;
  case SCM_PORT_BUF_NONE:
    SCM_PORT_CAPACITY(port) = 0;
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    SCM_PORT_CAPACITY(port) = 0; /* must not happen */
    break;
  }

  if (SCM_PORT_CAPACITY(port) == 0) {
    SCM_PORT_BUFFER(port) = NULL;
    return;
  }

  SCM_PORT_BUFFER(port) = scm_memory_allocate(SCM_PORT_CAPACITY(port));
}

void
scm_port_initialize(ScmObj port, ScmIO *io,
                    SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(io != NULL);
  assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  SCM_PORT_ATTR(port) = attr;
  SCM_PORT_IO(port) = io;
  SCM_PORT_BUFFER_MODE(port) = SCM_PORT_BUF_NONE;
  SCM_PORT_BUFFER(port) = NULL;
  SCM_PORT_CAPACITY(port) = 0;
  SCM_PORT_POS(port) = 0;
  SCM_PORT_USED(port) = 0;

  scm_port_init_buffer(port, buf_mode);
}

void
scm_port_finalize(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  scm_port_flush(port);
  if (BIT_IS_SETED(SCM_PORT_ATTR(port), SCM_PORT_ATTR_DESTRUCT_IO)) {
    scm_port_close(port);
    scm_io_end(SCM_PORT_IO(port));
  }
  scm_memory_release(SCM_PORT_BUFFER(port));
}

ScmObj
scm_port_new(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  ScmObj port = SCM_OBJ_INIT;

  assert(io != NULL);
  assert(/* buf_mode >= 0 && */ buf_mode < SCM_PORT_NR_BUF_MODE);

  scm_mem_alloc_root(scm_vm_current_mm(),
                     &SCM_PORT_TYPE_INFO, SCM_REF_MAKE(port));
  /* TODO: replace above by below */
  /* scm_mem_alloc_heap(scm_vm_current_mm(), */
  /*                    &SCM_PORT_TYPE_INFO, SCM_REF_MAKE(port)); */
  scm_port_initialize(port, io, attr, buf_mode);

  return port;
}

ScmObj
scm_port_open_input(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_new(io, attr | SCM_PORT_ATTR_READABLE, buf_mode);
}

ScmObj
scm_port_open_output(ScmIO *io, SCM_PORT_ATTR attr, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_new(io, attr | SCM_PORT_ATTR_WRITABLE, buf_mode);
}

ScmObj
scm_port_open_input_file_with_charconv(const char *path,
                                       SCM_PORT_BUF_MODE buf_mode,
                                       const char *internal_encode,
                                       const char *external_encode)
{
  ScmIO *io;

  assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_RDONLY, 0);
  if (io == NULL) return NULL;

  if (internal_encode != NULL && external_encode != NULL
      && strcmp(internal_encode, external_encode) != 0) {
    const bool OWNER = true;
    ScmIO *convio;

    convio = (ScmIO *)scm_charconvio_new(io,
                                               internal_encode, external_encode,
                                               OWNER);
    if (convio == NULL) {
      scm_io_end(io);
      return NULL;
    }

    io = convio;
  }

  return scm_port_open_input(io,
                             SCM_PORT_ATTR_FILE | SCM_PORT_ATTR_DESTRUCT_IO,
                             buf_mode);
}

ScmObj
scm_port_open_output_file_with_charconv(const char *path,
                                        SCM_PORT_BUF_MODE buf_mode,
                                        const char *internal_encode,
                                        const char *external_encode)

{
  ScmIO *io;

  assert(path != NULL);

  io = (ScmIO *)scm_fileio_open(path, O_WRONLY | O_CREAT, 00644);
  if (io == NULL) return NULL;

  if (internal_encode != NULL && external_encode != NULL
      && strcmp(internal_encode, external_encode) != 0) {
    const bool OWNER = true;
    ScmIO *convio;

    convio = (ScmIO *)scm_charconvio_new(io,
                                               internal_encode, external_encode,
                                               OWNER);
    if (convio == NULL) {
      scm_io_end(io);
      return NULL;
    }

    io = convio;
  }

  return scm_port_open_output(io,
                              SCM_PORT_ATTR_FILE | SCM_PORT_ATTR_DESTRUCT_IO,
                              buf_mode);
}

ScmObj
scm_port_open_input_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_open_input_file_with_charconv(path, buf_mode, NULL, NULL);
}

ScmObj
scm_port_open_output_file(const char *path, SCM_PORT_BUF_MODE buf_mode)
{
  return scm_port_open_output_file_with_charconv(path, buf_mode, NULL, NULL);
}

ScmObj
scm_port_open_input_string(const void *string, size_t size)
{
  ScmIO *io;

  assert(string != NULL);

  io = (ScmIO *)scm_stringio_new(string, size);
  if (io == NULL) return NULL;

  return scm_port_open_input(io,
                             SCM_PORT_ATTR_STRING | SCM_PORT_ATTR_DESTRUCT_IO,
                             SCM_PORT_BUF_DEFAULT);
}

ScmObj
scm_port_open_output_string(void)
{
  ScmIO *io;

  io = (ScmIO *)scm_stringio_new(NULL, 0);
  if (io == NULL) return NULL;

  return scm_port_open_output(io,
                              SCM_PORT_ATTR_STRING | SCM_PORT_ATTR_DESTRUCT_IO,
                              SCM_PORT_BUF_DEFAULT);
}

bool
scm_port_is_readable(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT_ATTR(port), SCM_PORT_ATTR_READABLE);
}

bool
scm_port_is_writable(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT_ATTR(port), SCM_PORT_ATTR_WRITABLE);
}

bool
scm_port_is_file_port(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT_ATTR(port), SCM_PORT_ATTR_FILE);
}

bool
scm_port_is_string_port(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return BIT_IS_SETED(SCM_PORT_ATTR(port), SCM_PORT_ATTR_STRING);
}

bool
scm_port_is_port(ScmObj obj)
{
  assert(SCM_OBJ_IS_NOT_NULL(obj));

  return SCM_OBJ_IS_TYPE(obj, &SCM_PORT_TYPE_INFO);
}

bool
scm_port_is_closed(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return scm_io_is_closed(SCM_PORT_IO(port));
}

static bool
scm_port_is_buffer_empty(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return (SCM_PORT_POS(port) == SCM_PORT_USED(port));
}

bool
scm_port_is_ready(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_is_buffer_empty(port))
    return scm_io_is_ready(SCM_PORT_IO(port));
  else
    return true;
}

bool
scm_port_is_eof(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return (scm_port_is_buffer_empty(port) && scm_io_is_eof(SCM_PORT_IO(port)));
}

int
scm_port_flush(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_is_writable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  if (SCM_PORT_USED(port) > 0) {
    scm_io_write(SCM_PORT_IO(port),
                 SCM_PORT_BUFFER(port), SCM_PORT_USED(port));
    SCM_PORT_USED(port) = 0;
    SCM_PORT_POS(port) = 0;
  }

  return 0;
}

int
scm_port_close(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_is_closed(port)) return 0;

  if (scm_port_is_writable(port))
    scm_port_flush(port);

  return scm_io_close(SCM_PORT_IO(port));
}

static void
scm_port_clear_buffer(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  SCM_PORT_USED(port) = 0;
  SCM_PORT_POS(port) = 0;
}

static ssize_t
scm_port_read_from_buffer(ScmObj port, void *buf, size_t size)
{
  ssize_t n;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(buf != NULL);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(SCM_PORT_BUFFER_MODE(port) != SCM_PORT_BUF_NONE);

  n = SCM_PORT_USED(port) - SCM_PORT_POS(port);
  n = (n < size) ? n : size;

  memcpy(buf, SCM_PORT_BUFFER(port) + SCM_PORT_POS(port), n);
  SCM_PORT_POS(port) += n;

  return n;
}

static ssize_t
scm_port_read_into_buffer(ScmObj port)
{
  ssize_t n;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(SCM_PORT_BUFFER_MODE(port) != SCM_PORT_BUF_NONE);

  n = scm_io_read(SCM_PORT_IO(port),
                  SCM_PORT_BUFFER(port), SCM_PORT_CAPACITY(port));
  if (n < 0) return -1; // error

  SCM_PORT_POS(port) = 0;
  SCM_PORT_USED(port) = n;

  return n;
}

static ssize_t
scm_port_read_prim_buf(ScmObj port, void *buf, size_t size, bool wait_all)
{
  ssize_t p;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(buf != NULL);
  assert(scm_port_is_readable(port));
  assert(!scm_port_is_closed(port));
  assert(SCM_PORT_BUFFER_MODE(port) != SCM_PORT_BUF_NONE);

  p = 0;
  do {
    ssize_t n;

    if (scm_port_is_buffer_empty(port)) {
      n = scm_port_read_into_buffer(port);
      if (n < 0) return -1; // error
      if (n == 0) break;
    }

    n = scm_port_read_from_buffer(port, (uint8_t *)buf + p, size - p);
    if (n < 0) return -1; // error
    p += n;

    if (p >= size) break;

  } while (wait_all || (!wait_all && scm_port_is_ready(port)));

  return p;
}

ssize_t
scm_port_read_prim(ScmObj port, void *buf, size_t size)
{
  const bool wait_all = true;
  const bool dont_wait = false;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(buf != NULL);

  if (!scm_port_is_readable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  switch (SCM_PORT_BUFFER_MODE(port)) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE:
    return scm_port_read_prim_buf(port, buf, size, wait_all);
    break;
  case SCM_PORT_BUF_MODEST:
    return scm_port_read_prim_buf(port, buf, size, dont_wait);
    break;
  case SCM_PORT_BUF_NONE:
    return scm_io_read(SCM_PORT_IO(port), buf, size);
    break;
  case SCM_PORT_BUF_DEFAULT:    /* fall through */
  default:
    return -1; /* must not happen */
    break;
  }
}

static ssize_t
scm_port_write_prim_buf_full(ScmObj port, const void *buf, size_t size)
{
  size_t i;
  int n;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(buf != NULL);
  assert(scm_port_is_writable(port));
  assert(!scm_port_is_closed(port));

  for (i = 0; i < size; i += n) {
    n = SCM_PORT_CAPACITY(port) - SCM_PORT_POS(port);
    n = (n > size) ? size : n;

    memcpy(SCM_PORT_BUFFER(port) + SCM_PORT_POS(port), (uint8_t *)buf + i, n);
    SCM_PORT_POS(port) += n;
    if (SCM_PORT_POS(port) > SCM_PORT_USED(port))
      SCM_PORT_USED(port) = SCM_PORT_POS(port);

    if (SCM_PORT_USED(port) >= SCM_PORT_CAPACITY(port))
      scm_port_flush(port);
  }

  return i;
}

/* static ssize_t */
/* scm_port_write_prim_buf_line(ScmPort *port, const void *buf, size_t size) */
/* { */
/*   int i, n; */
  
/*   assert(port != NULL); */
/*   assert(buf != NULL); */
/*   assert(scm_port_is_writable(port)); */
/*   assert(!scm_port_is_closed(port)); */

/*   for (i = 0; i < size; i++) { */
/*     n = scm_port_write_prim_buf_full(port, buf + i, 1); */
/*     if (n < 0) return -1; */
/*     if (((const char *)buf)[i] == '\n') scm_port_flush(port); */
/*   } */

/*   return i; */
/* } */

ssize_t
scm_port_write_prim(ScmObj port, const void *buf, size_t size)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);
  assert(buf != NULL);

  if (!scm_port_is_writable(port)) return -1;
  if (scm_port_is_closed(port)) return -1;

  switch (SCM_PORT_BUFFER_MODE(port)) {
  case SCM_PORT_BUF_FULL: /* fall through */
  case SCM_PORT_BUF_LINE: /* fall through */
  case SCM_PORT_BUF_MODEST:
    return scm_port_write_prim_buf_full(port, buf, size);
    break;
  case SCM_PORT_BUF_NONE:
    return scm_io_write(SCM_PORT_IO(port), buf, size);
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
  off_t off, result;

  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (scm_port_is_closed(port)) return -1;

  if (scm_port_is_writable(port))
    scm_port_flush(port);

  switch (whence) {
  case SEEK_SET:
    result = scm_io_seek(SCM_PORT_IO(port), offset, whence);
    break;
  case SEEK_CUR:
    off = offset - (SCM_PORT_USED(port) - SCM_PORT_POS(port));
    result = scm_io_seek(SCM_PORT_IO(port), off, whence);
    break;
  case SEEK_END:
    result = scm_io_seek(SCM_PORT_IO(port), offset, whence);
    break;
  default:
    return -1;
    break;
  }

  if (!scm_port_is_writable(port))
    scm_port_clear_buffer(port);

  return (result < 0) ? result : 0;
}

bool
scm_port_has_error(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return scm_io_has_error(SCM_PORT_IO(port));
}

int
scm_port_errno(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  return scm_io_errno(SCM_PORT_IO(port));
}

void *
scm_port_string_buffer(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_is_string_port(port)) return NULL;

  return (void *)scm_stringio_buffer((ScmStringIO *)SCM_PORT_IO(port));
}

ssize_t
scm_port_string_buffer_length(ScmObj port)
{
  SCM_OBJ_ASSERT_TYPE(port, &SCM_PORT_TYPE_INFO);

  if (!scm_port_is_string_port(port)) return -1;

  return scm_stringio_length((ScmStringIO *)SCM_PORT_IO(port));
}

void
scm_port_gc_finalize(ScmObj obj)
{
  scm_port_finalize(obj);
}
