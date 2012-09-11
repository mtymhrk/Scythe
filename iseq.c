#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <assert.h>

#include "object.h"
#include "reference.h"
#include "chashtbl.h"
#include "api.h"
#include "earray.h"
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
scm_iseq_put_uint32(uint8_t **ip, uint32_t val)
{
  uint8_t v1, v2, v3, v4;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v1 = (uint8_t)(val & 0xff);
  v2 = (uint8_t)((val >> 8) & 0xff);
  v3 = (uint8_t)((val >> 16) & 0xff);
  v4 = (uint8_t)((val >> 24) & 0xff);

  (*ip)[0] = v1;
  (*ip)[1] = v2;
  (*ip)[2] = v3;
  (*ip)[3] = v4;

  *ip = *ip + 4;
}

static inline void
scm_iseq_put_uint64(uint8_t **ip, uint64_t val)
{
  uint8_t v1, v2, v3, v4, v5, v6, v7, v8;

  scm_assert(ip != NULL);
  scm_assert(*ip != NULL);

  v1 = (uint8_t)(val & 0xff);
  v2 = (uint8_t)((val >> 8) & 0xff);
  v3 = (uint8_t)((val >> 16) & 0xff);
  v4 = (uint8_t)((val >> 24) & 0xff);
  v5 = (uint8_t)((val >> 32) & 0xff);
  v6 = (uint8_t)((val >> 40) & 0xff);
  v7 = (uint8_t)((val >> 48) & 0xff);
  v8 = (uint8_t)((val >> 56) & 0xff);

  (*ip)[0] = v1;
  (*ip)[1] = v2;
  (*ip)[2] = v3;
  (*ip)[3] = v4;
  (*ip)[4] = v5;
  (*ip)[5] = v6;
  (*ip)[6] = v7;
  (*ip)[7] = v8;

  *ip = *ip + 8;
}

int
scm_iseq_initialize(ScmObj iseq) /* GC OK */
{
  int rslt;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);

  rslt = eary_init(SCM_ISEQ_EARY_SEQ(iseq),
                   sizeof(uint8_t), SCM_ISEQ_DEFAULT_SEQ_SIZE);
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
scm_iseq_push_uint8(ScmObj iseq, uint8_t val)
{
  int err;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - 1);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));
  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx, val, err);
  if (err != 0) return -1;

  return (ssize_t)idx + 1;
}

ssize_t
scm_iseq_push_uint32(ScmObj iseq, uint32_t val)
{
  int err;
  uint8_t *ip;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - 4);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 3, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint32(&ip, val);

  return (ssize_t)idx + 4;
}

uint32_t
scm_iseq_get_uint32(ScmObj iseq, size_t idx)
{
  uint8_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) - 4);

  ip = scm_iseq_to_ip(iseq) + idx;
  return scm_iseq_fetch_uint32(&ip);
}

ssize_t
scm_iseq_set_uint32(ScmObj iseq, size_t idx, uint32_t val)
{
  uint8_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - 4);

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint32(&ip, val);

  return (ssize_t)idx;
}

ssize_t
scm_iseq_push_uint64(ScmObj iseq, uint64_t val)
{
  int err;
  uint8_t *ip;
  size_t idx;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) <= SSIZE_MAX - 8);

  idx = EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq));

  EARY_SET(SCM_ISEQ_EARY_SEQ(iseq), uint8_t, idx + 7, 0, err);
  if (err != 0) return -1;

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint64(&ip, val);

  return (ssize_t)idx + 8;
}

uint64_t
scm_iseq_get_uint64(ScmObj iseq, size_t idx)
{
  uint8_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= EARY_SIZE(SCM_ISEQ_EARY_SEQ(iseq)) - 8);

  ip = scm_iseq_to_ip(iseq) + idx;
  return scm_iseq_fetch_uint64(&ip);
}

ssize_t
scm_iseq_set_uint64(ScmObj iseq, size_t idx, uint64_t val)
{
  uint8_t *ip;

  scm_assert_obj_type(iseq, &SCM_ISEQ_TYPE_INFO);
  scm_assert(idx <= SCM_ISEQ_SEQ_LENGTH(iseq) - 8);

  ip = scm_iseq_to_ip(iseq) + idx;
  scm_iseq_put_uint64(&ip, val);

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

#if SCM_UWORD_MAX > UINT32_MAX
  rslt = scm_iseq_push_uint64(iseq, (uint64_t)obj);
#else
  rslt = scm_iseq_push_uint32(iseq, (uint32_t)obj);
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
#if SCM_UWORD_MAX > UINT32_MAX
  return SCM_OBJ(scm_iseq_get_uint64(iseq, idx));
#else
  return SCM_OBJ(scm_iseq_get_uint32(iseq, idx));
#endif
}

ssize_t
scm_iseq_set_obj(ScmObj iseq, size_t idx, ScmObj val)
{
#if SCM_UWORD_MAX > UINT32_MAX
  return scm_iseq_set_uint64(iseq, idx, (uint64_t)val);
#else
  return scm_iseq_set_uint32(iseq, idx, (uint32_t)val);
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
    scm_iseq_set_uint64(obj, idx, (uint64_t)chld);
#else
    scm_iseq_set_uint32(obj, idx, (uint32_t)chld);
#endif
  }

  return rslt;
}
