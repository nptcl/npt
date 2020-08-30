#ifndef __HEAP_MEMORY_HEADER__
#define __HEAP_MEMORY_HEADER__

#include "memory.h"
#include "typedef.h"
#include "thread.h"

#define heap_alloc _n(heap_alloc)
#define heap_root _n(heap_root)
#define heap_front _n(heap_front)
#define heap_pos _n(heap_pos)
#define heap_tail _n(heap_tail)
#define heap_range _n(heap_range)
#define heap_object _n(heap_object)
#define heap_count _n(heap_count)
#define heap_gc_count _n(heap_gc_count)
#define heap_gc_partial _n(heap_gc_partial)
#define heap_gc_full _n(heap_gc_full)
#define heap_cons_count _n(heap_cons_count)
#define heap_symbol_count _n(heap_symbol_count)

#define alloctail _n(alloctail)
#define allocheap _n(allocheap)
#define alloc_heap _n(alloc_heap)
#define free_heap _n(free_heap)
#define makespace_heap _n(makespace_heap)
#define valid_heap _n(valid_heap)
#define get_heap_object _n(get_heap_object)
#define get_heap_count _n(get_heap_count)
#define get_heap_gc_count _n(get_heap_gc_count)
#define get_heap_size _n(get_heap_size)

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
__extern size_t heap_cons_count;
__extern size_t heap_symbol_count;

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

