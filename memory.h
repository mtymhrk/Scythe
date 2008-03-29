#ifndef INCLUDED_MEMORY_H__
#define INCLUDED_MEMORY_H__

#include <unistd.h>

void *scm_memory_allocate(size_t size);
void *scm_memory_release(void *block);

#endif /* INCLUDED_MEMORY_H__ */
