#ifndef INCLUDE_VECTOR_H__
#define INCLUDE_VECTOR_H__

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#include "scythe/object.h"
#include "scythe/earray.h"
#include "scythe/memory.h"


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

typedef struct ScmVectorRec ScmVector;

struct ScmVectorRec {
  ScmObjHeader header;
  EArray array;
};

#define SCM_VECTOR(obj) ((ScmVector *)(obj))

extern ScmTypeInfo SCM_VECTOR_TYPE_INFO;

#define SCM_VECTOR_EARRAY(obj) (&SCM_VECTOR(obj)->array)
#define SCM_VECTOR_ARRAY(obj) ((ScmObj *)EARY_HEAD(SCM_VECTOR_EARRAY(obj)))
#define SCM_VECTOR_LENGTH(obj) (EARY_SIZE(SCM_VECTOR_EARRAY(obj)))

ScmObj scm_vector_P(ScmObj obj);
int scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill);
int scm_vector_initialize_ary(ScmObj vector, const ScmObj *elms, size_t length);
int scm_vector_initialize_lst(ScmObj vector, size_t length, ScmObj lst);
void scm_vector_finalize(ScmObj vector);
ScmObj scm_vector_new(scm_mem_type_t mtype, size_t length, ScmObj fill);
ScmObj scm_vector_new_cv(scm_mem_type_t mtype,
                         const ScmObj *elms, size_t length);
ScmObj scm_vector_new_lst(scm_mem_type_t mtype, ScmObj lst);
ScmObj scm_vector(size_t n, ...);
int scm_vector_push(ScmObj vector, ScmObj obj);
ScmObj scm_vector_to_list(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_vector_to_string(ScmObj vec, ssize_t start, ssize_t end);
ScmObj scm_string_to_vector(ScmObj str, ssize_t start, ssize_t end);
ScmObj scm_vector_copy(ScmObj vec, ssize_t start, ssize_t end);
int scm_vector_copy_i(ScmObj to, size_t at,
                      ScmObj from, ssize_t start, ssize_t end);
ScmObj scm_vector_append_lst(ScmObj lst);
ScmObj scm_vector_append_cv(ScmObj *ary, size_t n);
ScmObj scm_vector_append(size_t n, ...);
void scm_vector_fill_i(ScmObj vec, ScmObj fill, ssize_t start, ssize_t end);
int scm_vector_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_vector_gc_initialize(ScmObj obj);
void scm_vector_gc_finalize(ScmObj obj);
int scm_vector_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_vector_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_VECTOR_TYPE_INFO);
}

static inline ScmObj
scm_make_vector(size_t len, ScmObj fill)
{
  scm_assert(len <= SSIZE_MAX);
  return scm_vector_new(SCM_MEM_HEAP, len, fill);
}

static inline ScmObj
scm_vector_lst(ScmObj lst)
{
  return scm_vector_new_lst(SCM_MEM_HEAP, lst);
}

static inline ScmObj
scm_vector_cv(const ScmObj *elm, size_t n)
{
  scm_assert(n <= SIZE_MAX);
  return scm_vector_new_cv(SCM_MEM_HEAP, elm, n);
}

static inline size_t
scm_vector_length(ScmObj vec)
{
  scm_assert(scm_vector_p(vec));
  return SCM_VECTOR_LENGTH(vec);
}

static inline ScmObj
scm_vector_ref(ScmObj vec, size_t index)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(index < scm_vector_length(vec));
  return SCM_VECTOR_ARRAY(vec)[index];
}

static inline int
scm_vector_set(ScmObj vec, size_t index, ScmObj obj)
{
  int err __attribute((unused));
  scm_assert(scm_vector_p(vec));
  scm_assert(index < scm_vector_length(vec));
  scm_assert(scm_obj_not_null_p(obj));
  EARY_SET_SCMOBJ(SCM_VECTOR_EARRAY(vec), index, obj, vec, err);
  return 0;
}

static inline void
scm_vector_fill(ScmObj vec, ScmObj fill)
{
  scm_assert(scm_vector_p(vec));
  scm_assert(scm_obj_not_null_p(fill));
  for (size_t i = 0; i < scm_vector_length(vec); i++)
    scm_vector_set(vec, i, fill);
}

static inline int
scm_vector_contract_redundant_space(ScmObj vec)
{
  int r;
  scm_assert(scm_vector_p(vec));
  r = eary_contract(SCM_VECTOR_EARRAY(vec));
  if (r < 0) return -1;
  return 0;
}

static inline const ScmObj *
scm_vector_content(ScmObj vec)
{
  scm_assert(scm_vector_p(vec));
  return SCM_VECTOR_ARRAY(vec);
}

static inline ScmObj
scm_list_to_vector(ScmObj lst)
{
  return scm_vector_lst(lst);
}


/*******************************************************************/
/*  ByteVector                                                     */
/*******************************************************************/

typedef struct ScmByteVectorRec ScmByteVector;

struct ScmByteVectorRec {
  ScmObjHeader header;
  EArray array;
};

#define SCM_BYTEVECTOR(obj) ((ScmByteVector *)(obj))
#define SCM_BYTEVECTOR_EARRAY(obj) (&SCM_BYTEVECTOR(obj)->array)
#define SCM_BYTEVECTOR_ARRAY(obj) \
  ((uint8_t *)EARY_HEAD(SCM_BYTEVECTOR_EARRAY(obj)))
#define SCM_BYTEVECTOR_LENGTH(obj) (EARY_SIZE(SCM_BYTEVECTOR_EARRAY(obj)))

extern ScmTypeInfo SCM_BYTEVECTOR_TYPE_INFO;

ScmObj scm_bytevector_P(ScmObj obj);
int scm_bytevector_initialize(ScmObj vec, size_t length, int fill);
int scm_bytevector_initialize_cbytes(ScmObj vec,
                                     const void *bytes, size_t length);
void scm_bytevector_finalize(ScmObj vec);
ScmObj scm_bytevector_new(scm_mem_type_t mtype, size_t length, int fill);
ScmObj scm_bytevector_new_cbyte(scm_mem_type_t mtype,
                                const void *bytes, size_t length);
int scm_bytevector_push(ScmObj vec, int val);
int scm_bytevector_cmp(ScmObj v1, ScmObj v2);
void *scm_bytevector_to_cv(ScmObj vec, void *buf, size_t size);
int scm_bytevector_obj_print(ScmObj obj, ScmObj port, int kind,
                             ScmObjPrintHandler handler);
void scm_bytevector_gc_initialize(ScmObj obj);
void scm_bytevector_gc_finalize(ScmObj obj);

static inline bool
scm_bytevector_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_BYTEVECTOR_TYPE_INFO) ? true : false;
}

static inline ScmObj
scm_make_bytevector(size_t len, int fill)
{
  return scm_bytevector_new(SCM_MEM_HEAP, len, fill);
}

static inline ScmObj
scm_make_bytevector_from_cv(const void *bytes, size_t length)
{
  return scm_bytevector_new_cbyte(SCM_MEM_HEAP, bytes, length);
}

static inline const void *
scm_bytevector_content(ScmObj vec)
{
  scm_assert(scm_bytevector_p(vec));
  return SCM_BYTEVECTOR_ARRAY(vec);
}

static inline size_t
scm_bytevector_length(ScmObj vec)
{
  scm_assert(scm_bytevector_p(vec));
  return SCM_BYTEVECTOR_LENGTH(vec);
}

static inline int
scm_bytevector_u8_set(ScmObj vec, size_t idx, int val)
{
  scm_assert(scm_bytevector_p(vec));
  scm_assert(idx < scm_bytevector_length(vec));
  scm_assert(0 <= val && val <= 255);

  SCM_BYTEVECTOR_ARRAY(vec)[idx] = (uint8_t)val;
  return 0;
}

static inline int
scm_bytevector_contract_redundant_space(ScmObj vec)
{
  int r;
  scm_assert(scm_bytevector_p(vec));
  r = eary_contract(SCM_VECTOR_EARRAY(vec));
  if (r < 0) return -1;
  return 0;
}

#endif /* INCLUDE_VECTOR_H__ */
