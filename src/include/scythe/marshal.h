#ifndef INCLUDE_MARSHAL_H__
#define INCLUDE_MARSHAL_H__

#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/chashtbl.h"
#include "scythe/memory.h"
#include "scythe/earray.h"

typedef struct ScmMarshalHeaderRec ScmMarshalHeader;
typedef struct ScmMarshalBufferRec ScmMarshalBuffer;
typedef struct ScmMarshalRec ScmMarshal;
typedef struct ScmUnmarshalRec ScmUnmarshal;

struct ScmMarshalHeaderRec {
  size_t size;
  size_t nr_obj;
  size_t nr_shared;
  size_t obj_pos;
  size_t shared_pos;
  ScmEncoding *enc;
};

struct ScmMarshalBufferRec {
  union {
    struct {
      EArray data;
    } m;
    struct {
      const void *data;
      size_t size;
    } u;
  } buf;
  char type;
  size_t cur;
};

struct ScmMarshalRec {
  ScmObjHeader header;
  ScmMarshalBuffer *output;
  ScmCHashTbl *obj2pos;
  EArray shared;
  EArray top;
};

struct ScmUnmarshalRec {
  ScmObjHeader header;
  ScmMarshalBuffer *input;
  ScmMarshalHeader mh;
  size_t *obj_pos;
  size_t *shared_pos;
  ScmObj *shared_obj;
  ScmObj *unmarshaled;
};

#define SCM_MARSHAL(obj) ((ScmMarshal *)(obj))
#define SCM_UNMARSHAL(obj) ((ScmUnmarshal *)(obj))

extern ScmTypeInfo SCM_MARSHAL_TYPE_INFO;
extern ScmTypeInfo SCM_UNMARSHAL_TYPE_INFO;

int scm_marshal_initialize(ScmObj marshal);
void scm_marshal_finalize(ScmObj marshal);
ScmObj scm_marshal_new(scm_mem_type_t mtype);
int scm_marshal_push(ScmObj marshal, ScmObj obj);
void *scm_marshal_terminate(ScmObj marshal, size_t *size);
void scm_marshal_gc_initialize(ScmObj obj);
void scm_marshal_gc_finalize(ScmObj obj);
int scm_marshal_gc_accept(ScmObj obj, ScmGCRefHandler handler);

int scm_unmarshal_initialize(ScmObj unmarshal, const void *data);
void scm_unmarshal_finalize(ScmObj unmarshal);
ScmObj scm_unmarshal_new(scm_mem_type_t mtype, const void *data);
ScmObj scm_unmarshal_ref(ScmObj unmarshal, size_t idx);
void scm_unmarshal_gc_initialize(ScmObj obj);
void scm_unmarshal_gc_finalize(ScmObj obj);
int scm_unmarshal_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_marshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_MARSHAL_TYPE_INFO);
}

static inline ScmObj
scm_make_marshal(void)
{
  return scm_marshal_new(SCM_MEM_HEAP);
}

static inline bool
scm_marshal_terminated_p(ScmObj marshal)
{
  scm_assert(scm_marshal_p(marshal));
  return ((SCM_MARSHAL(marshal)->output == NULL) ? true : false);
}

static inline bool
scm_unmarshal_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_UNMARSHAL_TYPE_INFO);
}

static inline ScmObj
scm_make_unmarshal(const void *data)
{
  return scm_unmarshal_new(SCM_MEM_HEAP, data);
}

static inline size_t
scm_unmarshal_num(ScmObj unmarshal)
{
  scm_assert(scm_unmarshal_p(unmarshal));
  return SCM_UNMARSHAL(unmarshal)->mh.nr_obj;
}


/****************************************************************************/
/* Facade                                                                   */
/****************************************************************************/

void *scm_marshal_va(size_t *size, va_list args);
void *scm_marshal(size_t *size, ...);


#endif /* INCLUDE_MARSHAL_H__ */
