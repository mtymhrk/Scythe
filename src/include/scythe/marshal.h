#ifndef INCLUDE_MARSHAL_H__
#define INCLUDE_MARSHAL_H__

#include <stddef.h>
#include <stdbool.h>

typedef struct ScmMarshalHeaderRec ScmMarshalHeader;
typedef struct ScmMarshalBufferRec ScmMarshalBuffer;
typedef struct ScmMarshalRec ScmMarshal;
typedef struct ScmUnmarshalRec ScmUnmarshal;

#define SCM_MARSHAL(obj) ((ScmMarshal *)(obj))
#define SCM_UNMARSHAL(obj) ((ScmUnmarshal *)(obj))

#include "scythe/object.h"
#include "scythe/encoding.h"
#include "scythe/chashtbl.h"
#include "earray.h"

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

extern ScmTypeInfo SCM_MARSHAL_TYPE_INFO;
extern ScmTypeInfo SCM_UNMARSHAL_TYPE_INFO;

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

int scm_marshal_initialize(ScmObj marshal);
void scm_marshal_finalize(ScmObj marshal);
int scm_marshal_push_obj(ScmObj marshal, ScmObj obj);
void *scm_marshal_terminate(ScmObj marshal, size_t *size);
void scm_marshal_gc_initialize(ScmObj obj, ScmObj mem);
void scm_marshal_gc_finalize(ScmObj obj);
int scm_marshal_gc_accept(ScmObj obj, ScmObj mem, ScmGCRefHandlerFunc handler);

int scm_unmarshal_initialize(ScmObj unmarshal, const void *data);
void scm_unmarshal_finalize(ScmObj unmarshal);
ScmObj scm_unmarshal_ref(ScmObj unmarshal, size_t idx);
void scm_unmarshal_gc_initialize(ScmObj obj, ScmObj mem);
void scm_unmarshal_gc_finalize(ScmObj obj);
int scm_unmarshal_gc_accept(ScmObj obj, ScmObj mem,
                            ScmGCRefHandlerFunc handler);

static inline bool
scm_marshal_terminated_p(ScmObj marshal)
{
  scm_assert_obj_type(marshal, &SCM_MARSHAL_TYPE_INFO);
  return ((SCM_MARSHAL(marshal)->output == NULL) ? true : false);
}

static inline size_t
scm_unmarshal_num_of_objs(ScmObj unmarshal)
{
  scm_assert_obj_type(unmarshal, &SCM_UNMARSHAL_TYPE_INFO);
  return SCM_UNMARSHAL(unmarshal)->mh.nr_obj;
}

#endif /* INCLUDE_MARSHAL_H__ */
