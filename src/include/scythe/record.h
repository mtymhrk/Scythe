#ifndef INCLUDE_RECORD_H__
#define INCLUDE_RECORD_H__

#include <stddef.h>
#include <stdbool.h>

#include "scythe/object.h"
#include "scythe/memory.h"

typedef struct ScmRecordTypeRec ScmRecordType;
typedef struct ScmRecordRec ScmRecord;

struct ScmRecordTypeRec {
  ScmObjHeader header;
  ScmObj name;
};

struct ScmRecordRec {
  ScmObjHeader header;
  ScmObj type;
  size_t nr_slots;
  ScmObj slots[];
};

#define SCM_RECORDTYPE(obj) ((ScmRecordType *)(obj))
#define SCM_RECORDTYPE_NAME(obj) (SCM_RECORDTYPE(obj)->name)
#define SCM_RECORDTYPE_SET_NAME(obj, n) \
  SCM_SLOT_SETQ(ScmRecordType, obj, name, n)

#define SCM_RECORD(obj) ((ScmRecord *)(obj))
#define SCM_RECORD_TYPE(obj) (SCM_RECORD(obj)->type)
#define SCM_RECORD_NR_SLOTS(obj) (SCM_RECORD(obj)->nr_slots)
#define SCM_RECORD_SLOT(obj, i) (SCM_RECORD(obj)->slots[i])
#define SCM_RECORD_SET_TYPE(obj, t) \
  SCM_SLOT_SETQ(ScmRecord, obj, type, t)
#define SCM_RECORD_SET_NR_SLOTS(obj, n) \
  SCM_SLOT_SETQ(ScmRecord, obj, nr_slots, n)
#define SCM_RECORD_SET_SLOT(obj, i, v) \
  SCM_SLOT_SETQ(ScmRecord, obj, slots[i], v)

extern ScmTypeInfo SCM_RECORDTYPE_TYPE_INFO;
extern ScmTypeInfo SCM_RECORD_TYPE_INFO;

ScmObj scm_record_P(ScmObj obj);
int scm_recordtype_initialize(ScmObj type, ScmObj name);
int scm_record_initialize(ScmObj ins, ScmObj type, size_t n, ScmObj slots);
ScmObj scm_recordtype_new(scm_mem_type_t mtype, ScmObj name);
ScmObj scm_record_new(scm_mem_type_t mtype, ScmObj type, size_t n, ScmObj slots);
int scm_recordtype_obj_print(ScmObj obj,
                             ScmObj port, int kind, ScmObjPrintHandler handler);
void scm_recordtype_gc_initialize(ScmObj obj);
int scm_recordtype_gc_accept(ScmObj obj, ScmGCRefHandler handler);
int scm_record_obj_print(ScmObj obj,
                            ScmObj port, int kind, ScmObjPrintHandler handler);
void scm_record_gc_initialize(ScmObj obj);
int scm_record_gc_accept(ScmObj obj, ScmGCRefHandler handler);

static inline bool
scm_recordtype_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_RECORDTYPE_TYPE_INFO);
}

static inline bool
scm_record_p(ScmObj obj)
{
  return scm_obj_type_p(obj, &SCM_RECORD_TYPE_INFO);
}

static inline ScmObj
scm_make_recordtype(ScmObj name)
{
  return scm_recordtype_new(SCM_MEM_HEAP, name);
}

static inline ScmObj
scm_make_record(ScmObj type, size_t n, ScmObj slots)
{
  return scm_record_new(SCM_MEM_HEAP, type, n, slots);
}

static inline ScmObj
scm_recordtype_name(ScmObj type)
{
  scm_assert(scm_recordtype_p(type));
  return SCM_RECORDTYPE_NAME(type);
}

static inline ScmObj
scm_record_type(ScmObj rec)
{
  scm_assert(scm_record_p(rec));
  return SCM_RECORD_TYPE(rec);
}

static inline size_t
scm_record_nr_slots(ScmObj rec)
{
  scm_assert(scm_record_p(rec));
  return SCM_RECORD_NR_SLOTS(rec);
}

static inline ScmObj
scm_record_slot_ref(ScmObj rec, size_t i)
{
  scm_assert(scm_record_p(rec));
  scm_assert(i < SCM_RECORD_NR_SLOTS(rec));
  return SCM_RECORD_SLOT(rec, i);
}

static inline void
scm_record_slot_set(ScmObj rec, size_t i, ScmObj obj)
{
  scm_assert(scm_record_p(rec));
  scm_assert(i < SCM_RECORD_NR_SLOTS(rec));
  SCM_RECORD_SET_SLOT(rec, i, obj);
}

#endif  /* INCLUDE_RECORD_H__ */
