#include <stddef.h>

#include "scythe/object.h"
#include "scythe/fcd.h"
#include "scythe/memory.h"
#include "scythe/vm.h"

ScmMem *
scm_fcd_mem_new(void)
{
  ScmMem *mem = NULL;

  mem = scm_fcd_malloc(sizeof(*mem));
  if (mem == NULL) return NULL;

  return scm_mem_initialize(mem);
}

ScmMem *
scm_fcd_mem_end(ScmMem *mem)
{
  if (mem == NULL) return NULL;

  scm_mem_finalize(mem);
  scm_fcd_free(mem);

  return NULL;
}

ScmObj
scm_fcd_mem_alloc_heap(ScmTypeInfo *type, size_t add_size)
{
  scm_assert(type != NULL);
  return scm_mem_alloc_heap(scm_bedrock_mem(scm_fcd_current_br()),
                            type, add_size);
}

ScmObj
scm_fcd_mem_alloc_root(ScmTypeInfo *type, size_t add_size)
{
  scm_assert(type != NULL);
  return scm_mem_alloc_root(scm_bedrock_mem(scm_fcd_current_br()),
                            type, add_size);
}

ScmObj
scm_fcd_mem_alloc(ScmTypeInfo *otype, size_t add_size, SCM_MEM_TYPE_T mtype)
{
  switch(mtype) {
  case SCM_MEM_HEAP:
    return scm_fcd_mem_alloc_heap(otype, add_size);
    break;
  case SCM_MEM_ROOT:
    return scm_fcd_mem_alloc_root(otype, add_size);
    break;
  default:
    scm_assert(false);
    return SCM_OBJ_NULL;
    break;
  };
}

ScmObj
scm_fcd_mem_free_root(ScmObj obj)
{
  if (obj == SCM_OBJ_NULL) return SCM_OBJ_NULL;
  return scm_mem_free_root(scm_bedrock_mem(scm_fcd_current_br()), obj);
}

ScmRef
scm_fcd_mem_register_extra_rfrn(ScmRef ref)
{
  if (ref == SCM_REF_NULL) return ref;
  return scm_mem_register_extra_rfrn(scm_bedrock_mem(scm_fcd_current_br()), ref);
}

void
scm_fcd_gc_start(void)
{
  scm_mem_gc_start(scm_bedrock_mem(scm_fcd_current_br()));
}

void
scm_fcd_gc_enable(void)
{
  scm_mem_enable_gc(scm_bedrock_mem(scm_fcd_current_br()));
}

void
scm_fcd_gc_disable(void)
{
  scm_mem_disable_gc(scm_bedrock_mem(scm_fcd_current_br()));
}
