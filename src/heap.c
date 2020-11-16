#include "heap.h"
#include "heap_memory.h"
#include "heap_core.h"
#include "memory.h"

/*
 *  allocate
 */
void heap_cons(addr *ret)
{
	heap_array2_memory(ret, LISPTYPE_CONS, 2);
	heap_cons_count++;
}

void heap_symbol(addr *ret)
{
	heap_array2_memory(ret, LISPTYPE_SYMBOL, SYMBOL_INDEX_SIZE);
	heap_symbol_count++;
}

void heap_array2_memory(addr *ret, enum LISPTYPE type, byte16 array)
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

void heap_array4_memory(addr *ret, enum LISPTYPE type, byte32 array)
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
void heap_array8(addr *ret, enum LISPTYPE type, size_t array)
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

void heap_body2_memory(addr *ret, enum LISPTYPE type, byte16 body)
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

void heap_body4_memory(addr *ret, enum LISPTYPE type, byte32 body)
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
void heap_body8(addr *ret, enum LISPTYPE type, size_t body)
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

void heap_smallsize_memory(addr *ret, enum LISPTYPE type, byte array, byte body)
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

void heap_arraybody_memory(addr *ret, enum LISPTYPE type, byte16 array, byte16 body)
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

void heap_array(addr *ret, enum LISPTYPE type, size_t array)
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

void heap_body(addr *ret, enum LISPTYPE type, size_t body)
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
void heap_array2_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFUL < array, "size error");
	heap_array2_memory(root, type, (byte16)array);
}
void heap_array4_debug(addr *root, enum LISPTYPE type, size_t array)
{
	Check(0xFFFFFFFFUL < array, "size error");
	heap_array4_memory(root, type, (byte32)array);
}
void heap_body2_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFUL < body, "size error");
	heap_body2_memory(root, type, (byte16)body);
}
void heap_body4_debug(addr *root, enum LISPTYPE type, size_t body)
{
	Check(0xFFFFFFFFUL < body, "size error");
	heap_body4_memory(root, type, (byte32)body);
}
void heap_smallsize_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFUL < array, "array size error");
	Check(0xFFUL < body, "body size error");
	heap_smallsize_memory(root, type, (byte)array, (byte)body);
}
void heap_arraybody_debug(addr *root, enum LISPTYPE type, size_t array, size_t body)
{
	Check(0xFFFFUL < array, "array size error");
	Check(0xFFFFUL < body, "body size error");
	heap_arraybody_memory(root, type, (byte16)array, (byte16)body);
}
#endif


/*
 *  initialize
 */
void init_heap(void)
{
	init_heap_core();
}

