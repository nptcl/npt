#ifndef __HEAP_HEADER__
#define __HEAP_HEADER__

#include "memory.h"
#include "typedef.h"

_g void heap_cons(addr *ret);
_g void heap_symbol(addr *ret);
_g void heap_array2_memory(addr *ret, enum LISPTYPE type, byte16 array);
_g void heap_array4_memory(addr *ret, enum LISPTYPE type, byte32 array);
_g void heap_body2_memory(addr *ret, enum LISPTYPE type, byte16 body);
_g void heap_body4_memory(addr *ret, enum LISPTYPE type, byte32 body);
_g void heap_smallsize_memory(addr *ret, enum LISPTYPE type, byte array, byte body);
_g void heap_arraybody_memory(addr *ret, enum LISPTYPE type, byte16 array, byte16 body);
_g void heap_array(addr *ret, enum LISPTYPE type, size_t array);
_g void heap_body(addr *ret, enum LISPTYPE type, size_t body);
#ifdef LISP_ARCH_64BIT
_g void heap_array8(addr *ret, enum LISPTYPE type, size_t array);
_g void heap_body8(addr *ret, enum LISPTYPE type, size_t body);
#endif

#ifdef LISP_DEBUG
_g void heap_array2_debug(addr *ret, enum LISPTYPE type, size_t array);
_g void heap_array4_debug(addr *ret, enum LISPTYPE type, size_t array);
_g void heap_body2_debug(addr *ret, enum LISPTYPE type, size_t body);
_g void heap_body4_debug(addr *ret, enum LISPTYPE type, size_t body);
_g void heap_smallsize_debug(addr *ret, enum LISPTYPE type, size_t array, size_t body);
_g void heap_arraybody_debug(addr *ret, enum LISPTYPE type, size_t array, size_t body);
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

_g void init_heap(void);

#endif

