#ifndef __HEAP_HEADER__
#define __HEAP_HEADER__

#include "memory.h"
#include "typedef.h"

#define heap_cons _n(heap_cons)
#define heap_symbol _n(heap_symbol)
#define heap_array2_memory _n(heap_array2_memory)
#define heap_array4_memory _n(heap_array4_memory)
#define heap_body2_memory _n(heap_body2_memory)
#define heap_body4_memory _n(heap_body4_memory)
#define heap_smallsize_memory _n(heap_smallsize_memory)
#define heap_arraybody_memory _n(heap_arraybody_memory)
#define heap_array _n(heap_array)
#define heap_body _n(heap_body)
#define heap_array8 _n(heap_array8)
#define heap_body8 _n(heap_body8)
#define heap_array2_debug _n(heap_array2_debug)
#define heap_array4_debug _n(heap_array4_debug)
#define heap_body2_debug _n(heap_body2_debug)
#define heap_body4_debug _n(heap_body4_debug)
#define heap_smallsize_debug _n(heap_smallsize_debug)
#define heap_arraybody_debug _n(heap_arraybody_debug)
#define init_heap _n(init_heap)

void heap_cons(addr *ret);
void heap_symbol(addr *ret);
void heap_array2_memory(addr *ret, enum LISPTYPE type, byte16 array);
void heap_array4_memory(addr *ret, enum LISPTYPE type, byte32 array);
void heap_body2_memory(addr *ret, enum LISPTYPE type, byte16 body);
void heap_body4_memory(addr *ret, enum LISPTYPE type, byte32 body);
void heap_smallsize_memory(addr *ret, enum LISPTYPE type, byte array, byte body);
void heap_arraybody_memory(addr *ret, enum LISPTYPE type, byte16 array, byte16 body);
void heap_array(addr *ret, enum LISPTYPE type, size_t array);
void heap_body(addr *ret, enum LISPTYPE type, size_t body);
#ifdef LISP_ARCH_64BIT
void heap_array8(addr *ret, enum LISPTYPE type, size_t array);
void heap_body8(addr *ret, enum LISPTYPE type, size_t body);
#endif

#ifdef LISP_DEBUG
void heap_array2_debug(addr *ret, enum LISPTYPE type, size_t array);
void heap_array4_debug(addr *ret, enum LISPTYPE type, size_t array);
void heap_body2_debug(addr *ret, enum LISPTYPE type, size_t body);
void heap_body4_debug(addr *ret, enum LISPTYPE type, size_t body);
void heap_smallsize_debug(addr *ret, enum LISPTYPE type, size_t array, size_t body);
void heap_arraybody_debug(addr *ret, enum LISPTYPE type, size_t array, size_t body);
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

void init_heap(void);

#endif

