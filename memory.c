#include <unistd.h>
#include <stdlib.h>
#include <assert.h>

#include "memory.h"

/* struct ScmMemoryRec { */
/*   int dummy; */
/* }; */

/* typedef struct ScmMemoryRec ScmMemory; */

/* static ScmMemory *memory_instance = NULL; */

/* static ScmMemory * */
/* scm_memory_instance(void) */
/* { */
/*   if (memory_instance == NULL) { */
/*     memory_instance = malloc(sizeof(ScmMemory)); */
/*   } */

/*   return memory_instance; */
/* } */

void *
scm_memory_allocate(size_t size)
{
  
  return malloc(size);
}

void *
scm_memory_release(void *block)
{
  free(block);
  return NULL;
}
