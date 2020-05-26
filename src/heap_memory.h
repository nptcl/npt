#ifndef __HEAP_MEMORY_HEADER__
#define __HEAP_MEMORY_HEADER__

#include "memory.h"
#include "typedef.h"
#include "thread.h"

struct heap_addr {
	addr pos;
};

/* variable */
__extern void *heap_alloc;
__extern addr heap_root;
__extern addr heap_front;
__extern addr heap_pos;
__extern addr heap_tail;
__extern addr heap_range;
__extern size_t heap_object;
__extern size_t heap_count;
__extern size_t heap_gc_count;
__extern size_t heap_gc_partial;
__extern size_t heap_gc_full;

_g struct heap_addr *alloctail(void);
_g void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2);

/* function */
_g int alloc_heap(size_t);
_g void free_heap(void);

/* gc */
_g void makespace_heap(addr pos, size_t size);
_g int valid_heap(const void *);
_g size_t get_heap_object(void);
_g size_t get_heap_count(void);
_g size_t get_heap_gc_count(void);
_g size_t get_heap_size(void);

#endif

