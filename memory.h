#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>

typedef struct ScmMemHeapBlockRec ScmMemHeapBlock;
typedef struct ScmMemHeapRec ScmMemHeap;
typedef struct ScmMemRec ScmMem;
typedef struct ScmForwardRec ScmForward;

#define SCM_FORWORD(obj) ((ScmFoward *)(obj))

#include "basichash.h"
#include "object.h"

typedef void (*SCM_MEM_FINALIZER)(ScmObj obj);

extern const ScmTypeInfo SCM_FORWARD_TYPE_INFO;

void *scm_memory_allocate(size_t size);
void *scm_memory_release(void *block);

ScmMem *scm_mem_construct(void);
ScmMem *scm_mem_destruct(ScmMem *mem);
ScmMem *scm_mem_alloc(ScmMem *mem, SCM_OBJ_TYPE_T type, ScmObj *box);
void scm_mem_gc_start(ScmMem *mem);

#endif /* INCLUDED_MEMORY_H__ */
