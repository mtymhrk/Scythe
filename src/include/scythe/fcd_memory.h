#ifndef INCLUDE_FCD_MEMORY_H__
#define INCLUDE_FCD_MEMORY_H__

#include <stddef.h>

typedef struct ScmMemRec ScmMem;
typedef enum scm_mem_type scm_mem_type_t;

#include "scythe/object.h"

enum scm_mem_type {
  SCM_MEM_HEAP,
  SCM_MEM_ROOT,
};

ScmMem *scm_fcd_mem_new(void);
ScmMem *scm_fcd_mem_end(ScmMem *mem);
ScmObj scm_fcd_mem_alloc_heap(ScmTypeInfo *type, size_t add_size);
ScmObj scm_fcd_mem_alloc_root(ScmTypeInfo *type, size_t add_size);
ScmObj scm_fcd_mem_alloc(ScmTypeInfo *otype, size_t add_size,
                         scm_mem_type_t mtype);
ScmObj scm_fcd_mem_free_root(ScmObj obj);
ScmRef scm_fcd_mem_register_extra_rfrn(ScmRef ref);
void scm_fcd_gc_start(void);
void scm_fcd_gc_enable(void);
void scm_fcd_gc_disable(void);

void *scm_fcd_malloc(size_t size);
void *scm_fcd_free(void *ptr);
void *scm_fcd_realloc(void *ptr, size_t size);

#endif /* INCLUDE_FCD_MEMORY_H__ */
