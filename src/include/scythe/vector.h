#ifndef INCLUDE_VECTOR_H__
#define INCLUDE_VECTOR_H__

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef struct ScmVectorRec ScmVector;
typedef struct ScmByteVectorRec ScmByteVector;

#define SCM_VECTOR(obj) ((ScmVector *)(obj))
#define SCM_BYTEVECTOR(obj) ((ScmByteVector *)(obj))

#include "scythe/object.h"
#include "scythe/earray.h"


/*******************************************************************/
/*  Vector                                                         */
/*******************************************************************/

extern ScmTypeInfo SCM_VECTOR_TYPE_INFO;

struct ScmVectorRec {
  ScmObjHeader header;
  EArray array;
};

#define SCM_VECTOR_EARRAY(obj) (&SCM_VECTOR(obj)->array)
#define SCM_VECTOR_ARRAY(obj) ((ScmObj *)EARY_HEAD(SCM_VECTOR_EARRAY(obj)))
#define SCM_VECTOR_LENGTH(obj) (EARY_SIZE(SCM_VECTOR_EARRAY(obj)))

int scm_vector_initialize(ScmObj vector, size_t length, ScmObj fill);
int scm_vector_initialize_ary(ScmObj vector, const ScmObj *elms, size_t length);
int scm_vector_initialize_lst(ScmObj vector, size_t length, ScmObj lst);
void scm_vector_finalize(ScmObj vector);
size_t scm_vector_length(ScmObj vector);
ScmObj scm_vector_ref(ScmObj vector, size_t index);
int scm_vector_set(ScmObj vector, size_t index, ScmObj obj);
void scm_vector_fill(ScmObj vector, ScmObj fill);
int scm_vector_push(ScmObj vector, ScmObj obj);
int scm_vector_contract_redundant_space(ScmObj vector);
const ScmObj *scm_vector_content(ScmObj vector);
int scm_vector_obj_print(ScmObj obj, ScmObj port, int kind,
                         ScmObjPrintHandler handler);
void scm_vector_gc_initialize(ScmObj obj, ScmObj mem);
void scm_vector_gc_finalize(ScmObj obj);
int scm_vector_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);


/*******************************************************************/
/*  ByteVector                                                     */
/*******************************************************************/

extern ScmTypeInfo SCM_BYTEVECTOR_TYPE_INFO;

struct ScmByteVectorRec {
  ScmObjHeader header;
  EArray array;
};

#define SCM_BYTEVECTOR_EARRAY(obj) (&SCM_BYTEVECTOR(obj)->array)
#define SCM_BYTEVECTOR_ARRAY(obj) \
  ((uint8_t *)EARY_HEAD(SCM_BYTEVECTOR_EARRAY(obj)))
#define SCM_BYTEVECTOR_LENGTH(obj) (EARY_SIZE(SCM_BYTEVECTOR_EARRAY(obj)))

int scm_bytevector_initialize(ScmObj vec, size_t length, int fill);
int scm_bytevector_initialize_cbytes(ScmObj vec,
                                     const void *bytes, size_t length);
void scm_bytevector_finalize(ScmObj vec);
ScmObj scm_bytevector_new(scm_mem_type_t mtype, size_t length, int fill);
ScmObj scm_bytevector_new_cbyte(scm_mem_type_t mtype,
                                const void *bytes, size_t length);
int scm_bytevector_u8_set(ScmObj vec, size_t idx, int val);
int scm_bytevector_push(ScmObj vec, int val);
int scm_bytevector_contract_redundant_space(ScmObj vec);
int scm_bytevector_cmp(ScmObj v1, ScmObj v2);
int scm_bytevector_obj_print(ScmObj obj, ScmObj port, int kind,
                             ScmObjPrintHandler handler);
void scm_bytevector_gc_initialize(ScmObj obj, ScmObj mem);
void scm_bytevector_gc_finalize(ScmObj obj);

static inline const void *
scm_bytevector_content(ScmObj vec)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);

  return SCM_BYTEVECTOR_ARRAY(vec);
}

static inline size_t
scm_bytevector_length(ScmObj vec)
{
  scm_assert_obj_type(vec, &SCM_BYTEVECTOR_TYPE_INFO);

  return SCM_BYTEVECTOR_LENGTH(vec);
}

#endif /* INCLUDE_VECTOR_H__ */
