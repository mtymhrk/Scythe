#ifndef INCLUDE_FCD_MEMORY_H__
#define INCLUDE_FCD_MEMORY_H__

#include <stddef.h>

#include "scythe/object.h"
#include "scythe/fcd_type.h"
#include "scythe/fcd_vm.h"

ScmObj scm_fcd_mem_alloc_heap(ScmTypeInfo *type, size_t add_size);
ScmObj scm_fcd_mem_alloc_root(ScmTypeInfo *type, size_t add_size);
ScmObj scm_fcd_mem_alloc(ScmTypeInfo *otype, size_t add_size,
                         SCM_MEM_TYPE_T mtype);
ScmObj scm_fcd_mem_free_root(ScmObj obj);
ScmRef scm_fcd_mem_register_extra_rfrn(ScmRef ref);
void scm_fcd_gc_start(void);
void scm_fcd_gc_enable(void);
void scm_fcd_gc_disable(void);

static inline void *
scm_fcd_malloc(size_t size)
{
  void *p = malloc(size);
  if (p == NULL) scm_fcd_fatal("memory allocation error");
  return p;
}

static inline void *
scm_fcd_free(void *ptr)
{
  free(ptr);
  return NULL;
}

static inline void *
scm_fcd_realloc(void *ptr, size_t size)
{
  void *p = realloc(ptr, size);
  if (p == NULL) scm_fcd_fatal("memory allocation error");
  return p;
}

#endif /* INCLUDE_FCD_MEMORY_H__ */
