#include "heap.h"
#include "heap_memory.h"
#include "heap_core.h"
#include "memory.h"

/*
 *  allocate
 */
_g void heap_cons(addr *ret)
{
	int index;
	addr pos;
	addr *array;

	/* rotate index */
	lock_mutexlite(&heap_cons_mutex);
	index = heap_cons_count++;
	if (LISPCLASS_ConsLength <= heap_cons_count)
		heap_cons_count = 0;
	unlock_mutexlite(&heap_cons_mutex);

	/* alloc memory */
	allocheap_small((size_t)ConsLength, index, &pos);
	*PtrValue2L(pos) = ConsLength;
	SetType(pos, LISPTYPE_CONS);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetChain(pos, 0);
	*PtrLenArrayA2(pos) = 2; /* left, right */
	array = (addr *)PtrByte2P(pos);
	array[0] = array[1] = Nil;
	*ret = pos;
}

_g void heap_symbol(addr *ret)
{
	int index;
	addr pos;

	/* rotate index */
	lock_mutexlite(&heap_symbol_mutex);
	index = heap_symbol_count++;
	if (LISPCLASS_SymbolLength <= heap_symbol_count)
		heap_symbol_count = 0;
	unlock_mutexlite(&heap_symbol_mutex);
	index += LISPCLASS_ConsLength;

	/* alloc memory */
	allocheap_small((size_t)SymbolLength, index, &pos);
	*PtrValue2L(pos) = SymbolLength;
	SetType(pos, LISPTYPE_SYMBOL);
	SetStatus(pos, LISPSIZE_ARRAY2);
	SetChain(pos, 0);
	*PtrLenArrayA2(pos) = SYMBOL_INDEX_SIZE;
	nilarray2(pos, SYMBOL_INDEX_SIZE);
	*ret = pos;
}

_g void heap_array2_memory(addr *ret, enum LISPTYPE type, byte16 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA2(array);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_ARRAY2);
	*PtrLenArrayA2(pos) = array;
	nilarray2(pos, array);
	*ret = pos;
}

_g void heap_array4_memory(addr *ret, enum LISPTYPE type, byte32 array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA4(array);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY4);
	*PtrLenArrayA4(pos) = array;
	nilarray4(pos, array);
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
_g void heap_array8(addr *ret, enum LISPTYPE type, size_t array)
{
	addr pos;
	size_t size;

	size = MemoryLengthA8(array);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAY8);
	*PtrLenArrayA8(pos) = array;
	nilarray8(pos, array);
	*ret = pos;
}
#endif

_g void heap_body2_memory(addr *ret, enum LISPTYPE type, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB2(body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_BODY2);
	*PtrLenBodyB2(pos) = body;
	*ret = pos;
}

_g void heap_body4_memory(addr *ret, enum LISPTYPE type, byte32 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB4(body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY4);
	*PtrLenBodyB4(pos) = body;
	*ret = pos;
}

#ifdef LISP_ARCH_64BIT
_g void heap_body8(addr *ret, enum LISPTYPE type, size_t body)
{
	addr pos;
	size_t size;

	size = MemoryLengthB8(body);
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_BODY8);
	*PtrLenBodyB8(pos) = body;
	*ret = pos;
}
#endif

_g void heap_smallsize_memory(addr *ret, enum LISPTYPE type, byte array, byte body)
{
	addr pos;
	size_t size;

	size = MemoryLengthSS(array, body);
	Check(0xFFFFUL < size, "size error");
	allocheap(size, type, &pos, 1);
	SetStatus(pos, LISPSIZE_SMALLSIZE);
	nilarray2(pos, array);
	*PtrLenArraySS(pos) = array;
	*PtrLenBodySS(pos) = body;
	*ret = pos;
}

_g void heap_arraybody_memory(addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
{
	addr pos;
	size_t size;

	size = MemoryLengthAB(array, body);
	Check(0xFFFFFFFFUL < size, "size error");
	allocheap(size, type, &pos, 0);
	SetStatus(pos, LISPSIZE_ARRAYBODY);
	nilarray4(pos, array);
	*PtrLenArrayAB(pos) = array;
	*PtrLenBodyAB(pos) = body;
	*ret = pos;
}

_g void heap_array(addr *ret, enum LISPTYPE type, size_t array)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		heap_array2_memory(ret, type, (byte16)array);
	else if (MemoryLengthA4(array) <= 0xFFFFFFFFUL)
		heap_array4_memory(ret, type, (byte32)array);
	else
		heap_array8(ret, type, array);
#else
	if (MemoryLengthA2(array) <= 0xFFFFUL)
		heap_array2_memory(ret, type, (byte16)array);
	else
		heap_array4_memory(ret, type, array);
#endif
}

_g void heap_body(addr *ret, enum LISPTYPE type, size_t body)
{
#ifdef LISP_ARCH_64BIT
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		heap_body2_memory(ret, type, (byte16)body);
	else if (MemoryLengthB4(body) <= 0xFFFFFFFFUL)
		heap_body4_memory(ret, type, (byte32)body);
	else
		heap_body8(ret, type, body);
#else
	if (MemoryLengthB2(body) <= 0xFFFFUL)
		heap_body2_memory(ret, type, (byte16)body);
	else
		heap_body4_memory(ret, type, body);
#endif
}

#ifdef LISP_DEBUG
_g void heap_array2_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	heap_array2_memory(root, type, (byte16)array);
}
_g void heap_array4_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	heap_array4_memory(root, type, (byte32)array);
}
_g void heap_body2_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	heap_body2_memory(root, type, (byte16)body);
}
_g void heap_body4_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	heap_body4_memory(root, type, (byte32)body);
}
_g void heap_smallsize_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	heap_smallsize_memory(root, type, (byte)array, (byte)body);
}
_g void heap_arraybody_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	heap_arraybody_memory(root, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  initialize
 */
_g void init_heap(void)
{
	init_heap_core();
}

