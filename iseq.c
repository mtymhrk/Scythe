#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "chashtbl.h"
#include "api.h"
#include "earray.h"
#include "impl_utils.h"
#include "iseq.h"

ScmTypeInfo SCM_ISEQ_TYPE_INFO = {
  .name                = "iseq",
  .flags               = SCM_TYPE_FLG_MMO,
  .pp_func             = scm_iseq_pretty_print,
  .obj_size            = sizeof(ScmISeq),
  .gc_ini_func         = scm_iseq_gc_initialize,
  .gc_fin_func         = scm_iseq_gc_finalize,
  .gc_accept_func      = scm_iseq_gc_accept,
  .gc_accept_func_weak = NULL,
  .extra               = NULL,
};

static inline void
scm_iseq_put_ushort(scm_byte_t **ip, unsigned short val)
{
  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  *(unsigned short *)*ip = val;
  *ip = *ip + sizeof(unsigned short);
}

static inline void
scm_iseq_put_uint(scm_byte_t **ip, unsigned int val)
{
  scm_iseq_put_ushort(ip, val & USHRT_MAX);

#if UINT_MAX > USHORT_MAX
  scm_iseq_put_ushort(ip, (unsigned short)((val >> SCM_SHRT_BIT) & USHRT_MAX));
#endif
}

static inline void
scm_iseq_put_ullong(scm_byte_t **ip, unsigned long long val)
{
  scm_iseq_put_uint(ip, val & UINT_MAX);

#if ULLONG_MAX > UINT_MAX
  scm_iseq_put_uint(ip, (unsigned int)((val >> SCM_INT_BIT) & UINT_MAX));
#endif
}

int
scm_iseq_initialize(ScmObj iseq) /* GC OK */
{
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  rslt = eary_init(SCM_ISEQ_EARY_SEQ(iseq),
                   sizeof(scm_byte_t), SCM_ISEQ_DEFAULT_SEQ_SIZE);
  if (rslt != 0) return -1;  /* [ERR]: [through] */

  rslt = eary_init(SCM_ISEQ_EARY_INDEX(iseq),
                   sizeof(size_t), SCM_ISEQ_DEFAULT_INDEX_SIZE);
  if (rslt != 0) return -1;  /* [ERR]: [through] */

  return 0;
}

ScmObj
scm_iseq_new(SCM_MEM_TYPE_T mtype) /* GC OK */
{
  ScmObj iseq = SCM_OBJ_INIT;

  SCM_STACK_FRAME_PUSH(&iseq);

  iseq = scm_capi_mem_alloc(&SCM_ISEQ_TYPE_INFO, 0, mtype);
  if (scm_obj_null_p(iseq)) return SCM_OBJ_NULL; /* [ERR]: [through] */

  if (scm_iseq_initialize(iseq) < 0)
    return SCM_OBJ_NULL;        /* [ERR]: [through] */

  return iseq;
}

void
scm_iseq_finalize(ScmObj obj) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_fin(SCM_ISEQ_EARY_SEQ(obj));
  eary_fin(SCM_ISEQ_EARY_INDEX(obj));
}

ssize_t
scm_iseq_push_ushort(ScmObj iseq, unsigned short val)
{
  int err;
  scm_byte_t *ip;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq))
             <= SSIZE_MAX - sizeof(unsigned short));

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq),
           scm_byte_t, idx + sizeof(unsigned short) - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_ushort(&ip, val);

  return (ssize_t)idx + (ssize_t)sizeof(unsigned short);
}

unsigned short
scm_iseq_get_ushort(ScmObj iseq, size_t idx)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) - sizeof(unsigned short));

  ip = scm_iseq_to_ip(iseq) + idx;
  return scm_iseq_fetch_ushort(&ip);
}

ssize_t
scm_iseq_push_uint(ScmObj iseq, unsigned int val)
{
  int err;
  scm_byte_t *ip;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq))
             <= SSIZE_MAX - sizeof(unsigned int));

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq),
           scm_byte_t, idx + sizeof(unsigned int) - 1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint(&ip, val);

  return (ssize_t)idx + (ssize_t)sizeof(unsigned int);
}

unsigned int
scm_iseq_get_uint(ScmObj iseq, size_t idx)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) - sizeof(unsigned int));

  ip = scm_iseq_to_ip(iseq) + idx;
  return scm_iseq_fetch_uint(&ip);
}

ssize_t
scm_iseq_set_uint(ScmObj iseq, size_t idx, unsigned int val)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - sizeof(unsigned int));

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint(&ip, val);

  return (ssize_t)idx;
}

ssize_t
scm_iseq_push_ullong(ScmObj iseq, unsigned long long val)
{
  int err;
  scm_byte_t *ip;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq))
             <= SSIZE_MAX - sizeof(unsigned long long));

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq),
           scm_byte_t, idx + sizeof(unsigned long long) -1, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_ullong(&ip, val);

  return (ssize_t)idx + (ssize_t)sizeof(unsigned long long);
}

unsigned long long
scm_iseq_get_ullong(ScmObj iseq, size_t idx)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) - sizeof(unsigned long long));

  ip = scm_iseq_to_ip(iseq) + idx;
  return scm_iseq_fetch_ullong(&ip);
}

ssize_t
scm_iseq_set_ullong(ScmObj iseq, size_t idx, unsigned long long val)
{
  scm_byte_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - sizeof(unsigned long long));

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_ullong(&ip, val);

  return (ssize_t)idx;
}

ssize_t
scm_iseq_push_obj(ScmObj iseq, ScmObj obj)
{
  size_t idx;
  ssize_t rslt;
  int err;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(obj));

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_PUSH(SCM_ISEQ_EARY_INDEX(iseq), size_t, idx, err);
  if(err != 0) return -1;

#if SCM_UWORD_MAX > UINT_MAX
  rslt = scm_iseq_push_ullong(iseq, (unsigned long long)obj);
#else
  rslt = scm_iseq_push_uint(iseq, (unsigned int)obj);
#endif

  if (rslt < 0) {
    EARY_POP(SCM_ISEQ_EARY_INDEX(iseq), size_t, idx);
    return -1;
  }

  SCM_WB_EXP(iseq, /* nothing to do */);

  return rslt;
}

ScmObj
scm_iseq_get_obj(ScmObj iseq, size_t idx)
{
#if SCM_UWORD_MAX > UINT_MAX
  return SCM_OBJ(scm_iseq_get_ullong(iseq, idx));
#else
  return SCM_OBJ(scm_iseq_get_uint(iseq, idx));
#endif
}

ssize_t
scm_iseq_set_obj(ScmObj iseq, size_t idx, ScmObj val)
{
#if SCM_UWORD_MAX > UINT32_MAX
  return scm_iseq_set_ullong(iseq, idx, (unsigned long long)val);
#else
  return scm_iseq_set_uint(iseq, idx, (unsigned int)val);
#endif

  SCM_WB_EXP(iseq, /* nothing to do */);
}

int
scm_iseq_pretty_print(ScmObj obj, ScmObj port, bool write_p)
{
  char cstr[64];
  int rslt;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  snprintf(cstr, sizeof(cstr), "#<iseq %llx>", (unsigned long long)obj);

  rslt = scm_capi_write_cstr(cstr, SCM_ENC_ASCII, port);
  if (rslt < 0) return -1;

  return 0;
}

void
scm_iseq_gc_initialize(ScmObj obj, ScmObj mem) /* GC OK */
{
  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);

  eary_init(SCM_ISEQ_EARY_SEQ(obj), 0, 0);
  eary_init(SCM_ISEQ_EARY_INDEX(obj), 0, 0);
}

void
scm_iseq_gc_finalize(ScmObj obj) /* GC OK */
{
  scm_iseq_finalize(obj);
}

int
scm_iseq_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler) /* GC OK */
{
  int rslt = SCM_GC_REF_HANDLER_VAL_INIT;

  scm_assert_obj_type(obj, &SCM_ISEQ_TYPE_INFO);
  scm_assert(scm_obj_not_null_p(mem));
  scm_assert(handler != NULL);

  for (size_t i = 0; i < SCM_ISEQ_IDX_LENGTH(obj); i++) {
    size_t idx = SCM_ISEQ_IDX_VEC(obj)[i];
    ScmObj chld = scm_iseq_get_obj(obj, idx);

    rslt = SCM_GC_CALL_REF_HANDLER(handler, obj, chld, mem);
    if (scm_gc_ref_handler_failure_p(rslt))
      return rslt;

#if SCM_UWORD_MAX > UINT32_MAX
    scm_iseq_set_ullong(obj, idx, (unsigned long long)chld);
#else
    scm_iseq_set_uint(obj, idx, (unsigned int)chld);
#endif
  }

  return rslt;
}
