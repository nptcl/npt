#ifndef __HEAP_MEMORY_HEADER__
#define __HEAP_MEMORY_HEADER__

#include "memory.h"
#include "typedef.h"
#include "thread.h"

#define HeapCount               128
struct heapcell {
	struct heapcell *next, *chain;
	size_t count, search;
	addr point[HeapCount];
};
struct heapinfo {
	unsigned direct : 1;
	struct heapcell *root, *front;
	enum LISPCLASS type;
	mutexlite *mutex;
};

/* variable */
__extern mutexlite heap_cons_mutex;
__extern mutexlite heap_symbol_mutex;
__extern int heap_cons_count;
__extern int heap_symbol_count;
__extern struct heapinfo  *heap_info;

__extern void *heap_alloc;
__extern addr heap_root;
__extern addr heap_front;
__extern addr heap_pos;
__extern size_t heap_object;
__extern size_t heap_count;
__extern size_t heap_gc_count;
__extern size_t heap_gc_partial;
__extern size_t heap_gc_full;

_g struct heapcell *cellexpand(struct heapinfo *root, struct heapcell *cell);
_g void allocheap_small(size_t size, int index, addr *ret);
_g void allocheap_large(size_t size, int index, addr *ret);
_g void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2);

/* function */
_g int alloc_heap(size_t);
_g void free_heap(void);

/* gc */
_g void makespace(addr pos, size_t size);
_g void makereserved(addr pos, size_t size);
_g void foreach_heap(void (*call)(struct heapinfo *));
_g int foreach_check_heap(int (*call)(struct heapinfo *));
_g void cellupdate_heap(void);
_g int valid_heap(const void *);
_g size_t get_heap_object(void);
_g size_t get_heap_count(void);
_g size_t get_heap_gc_count(void);
_g size_t get_heap_size(void);

#endif

