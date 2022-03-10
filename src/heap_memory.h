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
#define reload_heap _n(reload_heap)
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
extern void *heap_alloc;
extern addr heap_root;
extern addr heap_front;
extern addr heap_pos;
extern addr heap_tail;
extern addr heap_range;
extern size_t heap_object;
extern size_t heap_count;
extern size_t heap_gc_count;
extern size_t heap_gc_partial;
extern size_t heap_gc_full;
extern size_t heap_cons_count;
extern size_t heap_symbol_count;

struct heap_addr *alloctail(void);
void allocheap(size_t size, enum LISPTYPE type, addr *root, int size2);

/* function */
int alloc_heap(size_t);
void free_heap(void);
void reload_heap(void);

/* gc */
void makespace_heap(addr pos, size_t size);
int valid_heap(const void *);
size_t get_heap_object(void);
size_t get_heap_count(void);
size_t get_heap_gc_count(void);
size_t get_heap_size(void);

#endif

