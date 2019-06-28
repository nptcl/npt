#ifndef __HEAP_HEADER__
#define __HEAP_HEADER__

#include <stddef.h>
#include "file_type.h"
#include "memory.h"
#include "typedef.h"

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
extern void *heap_alloc;
extern addr heap_root;
extern addr heap_front;
extern addr heap_pos;

/* function */
int alloc_heap(size_t);
void free_heap(void);

/* gc */
void makespace(addr pos, size_t size);
void makereserved(addr pos, size_t size);
void foreach_heap(void (*call)(struct heapinfo *));
int foreach_check_heap(int (*call)(struct heapinfo *));
void cellupdate_heap(void);
int valid_heap(const void *);

addr heapr_cons(void);
addr heapr_symbol(void);
addr heapr_array2_memory(enum LISPTYPE type, byte16 array);
addr heapr_array4_memory(enum LISPTYPE type, byte32 array);
addr heapr_body2_memory(enum LISPTYPE type, byte16 body);
addr heapr_body4_memory(enum LISPTYPE type, byte32 body);
addr heapr_smallsize_memory(enum LISPTYPE type, byte array, byte body);
addr heapr_arraybody_memory(enum LISPTYPE type, byte16 array, byte16 body);
addr heapr_array(enum LISPTYPE type, size_t array);
addr heapr_body(enum LISPTYPE type, size_t body);
#ifdef LISP_ARCH_64BIT
addr heapr_array8(enum LISPTYPE type, size_t array);
addr heapr_body8(enum LISPTYPE type, size_t body);
#endif

#ifdef LISP_DEBUG
addr heapr_array2_debug(enum LISPTYPE type, size_t array);
addr heapr_array4_debug(enum LISPTYPE type, size_t array);
addr heapr_body2_debug(enum LISPTYPE type, size_t body);
addr heapr_body4_debug(enum LISPTYPE type, size_t body);
addr heapr_smallsize_debug(enum LISPTYPE type, size_t array, size_t body);
addr heapr_arraybody_debug(enum LISPTYPE type, size_t array, size_t body);
#define heapr_array2 heapr_array2_debug
#define heapr_array4 heapr_array4_debug
#define heapr_body2 heapr_body2_debug
#define heapr_body4 heapr_body4_debug
#define heapr_smallsize heapr_smallsize_debug
#define heapr_arraybody heapr_arraybody_debug
#else
#define heapr_array2(t,a) heapr_array2_memory((t), (byte16)(a))
#define heapr_array4(t,a) heapr_array4_memory((t), (byte32)(a))
#define heapr_body2(t,a) heapr_body2_memory((t), (byte16)(a))
#define heapr_body4(t,a) heapr_body4_memory((t), (byte32)(a))
#define heapr_smallsize(t,a,b) heapr_smallsize_memory((t),(byte)(a),(byte)(b))
#define heapr_arraybody(t,a,b) heapr_arraybody_memory((t),(byte16)(a),(byte16)(b))
#endif

void heap_cons(addr *root);
void heap_symbol(addr *root);
void heap_array2_memory(addr *root, enum LISPTYPE type, byte16 array);
void heap_array4_memory(addr *root, enum LISPTYPE type, byte32 array);
void heap_body2_memory(addr *root, enum LISPTYPE type, byte16 body);
void heap_body4_memory(addr *root, enum LISPTYPE type, byte32 body);
void heap_smallsize_memory(addr *root, enum LISPTYPE type, byte array, byte body);
void heap_arraybody_memory(addr *root, enum LISPTYPE type, byte16 array, byte16 body);
void heap_array(addr *root, enum LISPTYPE type, size_t array);
void heap_body(addr *root, enum LISPTYPE type, size_t body);
#ifdef LISP_ARCH_64BIT
void heap_array8(addr *root, enum LISPTYPE type, size_t array);
void heap_body8(addr *root, enum LISPTYPE type, size_t body);
#endif

#ifdef LISP_DEBUG
void heap_array2_debug(addr *root, enum LISPTYPE type, size_t array);
void heap_array4_debug(addr *root, enum LISPTYPE type, size_t array);
void heap_body2_debug(addr *root, enum LISPTYPE type, size_t body);
void heap_body4_debug(addr *root, enum LISPTYPE type, size_t body);
void heap_smallsize_debug(addr *root, enum LISPTYPE type, size_t array, size_t body);
void heap_arraybody_debug(addr *root, enum LISPTYPE type, size_t array, size_t body);
#define heap_array2 heap_array2_debug
#define heap_array4 heap_array4_debug
#define heap_body2 heap_body2_debug
#define heap_body4 heap_body4_debug
#define heap_smallsize heap_smallsize_debug
#define heap_arraybody heap_arraybody_debug
#else
#define heap_array2(r,t,a) heap_array2_memory((r),(t),(byte16)(a))
#define heap_array4(r,t,a) heap_array4_memory((r),(t),(byte32)(a))
#define heap_body2(r,t,a) heap_body2_memory((r),(t),(byte16)(a))
#define heap_body4(r,t,a) heap_body4_memory((r),(t),(byte32)(a))
#define heap_smallsize(r,t,a,b) heap_smallsize_memory((r),(t),(byte)(a),(byte)(b))
#define heap_arraybody(r,t,a,b) heap_arraybody_memory((r),(t),(byte16)(a),(byte16)(b))
#endif

/* core */
int save_heap(struct filememory *fm);
int load_heap(struct filememory *fm);

/* initialize */
void init_heap(void);

#endif

