#include "heap_memory.c"
#include "heap.h"
#include "degrade.h"

static int test_GcCheck(void)
{
	test(GcCheck1 <= GcCheck2, "GcCheck.1");
	test(GcCheck2 <= GcCheck3, "GcCheck.2");
	test(GcCheck3 <= GcCheck4, "GcCheck.3");

	RETURN;
}

static int test_allocfront_size(void)
{
	byte mem[10000];
	size_t size;

	aatype(mem);
	heap_front = mem + 10000;
	SetType(mem, LISPTYPE_CONS);
	size = 0;
	test(allocfront_size(mem, &size) == 0, "allocfront_size.1");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 12);
	SetType(mem + 12, LISPTYPE_CONS);
	test(allocfront_size(mem, &size) != 0, "allocfront_size.2");
	test(size == 12, "allocfront_size.3");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE);
	SetSizeSpace(mem, 20);
	mem[20] = LISPTYPE_CONS;
	test(allocfront_size(mem, &size) != 0, "allocfront_size.4");
	test(size == 20, "allocfront_size.5");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 80);
	SetType(mem + 80, LISPSYSTEM_SPACE);
	SetSizeSpace(mem + 80, 800);
	mem[80 + 800] = LISPTYPE_CONS;
	test(allocfront_size(mem, &size) != 0, "allocfront_size.6");
	test(size == 80 + 800, "allocfront_size.7");

	aatype(mem);
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 12);
	heap_front = mem + 12;
	test(allocfront_size(mem, &size) != 0, "allocfront_size.8");
	test(size == 0, "allocfront_size.9");

	RETURN;
}

static int test_allocfront_search(void)
{
	byte mem[10000];
	addr pos;
	size_t size;

	/* space */
	aatype(mem);
	heap_front = mem + 10000;
	SetType(mem, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem, 80);
	SetType(mem + 80, LISPTYPE_CONS);
	size = 0;
	pos = allocfront_search(mem, &size);
	test(pos == mem, "allocfront_search.1");
	test(size == 80, "allocfront_search.2");

	/* object */
	aatype(mem);
	SetType(mem, LISPTYPE_CONS);
	SetStatus(mem, LISPSIZE_ARRAY4);
	*PtrValueL(mem) = 100;
	SetType(mem + 100, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem + 100, 80);
	SetType(mem + 180, LISPTYPE_CONS);
	size = 0;
	pos = allocfront_search(mem, &size);
	test (pos == mem + 100, "allocfront_search.3");
	test(size == 80, "allocfront_search.4");

	/* reserved */
	aatype(mem);
	heap_front = mem + 10;
	SetType(mem, LISPTYPE_CONS);
	SetStatus(mem, LISPSIZE_ARRAY4);
	*PtrValueL(mem) = 100;
	SetType(mem + 100, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem + 100, 80);
	size = 0;
	pos = allocfront_search(mem, &size);
	test(pos == NULL, "allocfront_search.5");
	test(heap_front == mem + 100, "allocfront_search.6");

	RETURN;
}

static int test_makespace_heap(void)
{
	byte mem[1000];
	size_t size, value;

	memset(mem, 0xBB, 1000);
	makespace_heap(mem, 2);
	test(mem[0] == LISPSYSTEM_SPACE1, "makespace_heap.1");
	test(mem[1] == 0, "makespace_heap.2");
	test(mem[2] == 0xBB, "makespace_heap.3");

	memset(mem, 0xAA, 1000);
	size = 8UL + IdxSize - 1;
	makespace_heap(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE1, "makespace_heap.4");
	test(mem[1] == size - 2, "makespace_heap.5");

	memset(mem, 0xAA, 1000);
	size = 8UL + IdxSize;
	makespace_heap(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE, "makespace_heap.6");
	memcpy(&value, mem + 8, IdxSize);
	test(value == 0, "makespace_heap.7");

	memset(mem, 0xAA, 1000);
	size = 100;
	makespace_heap(mem, size);
	test(mem[0] == LISPSYSTEM_SPACE, "makespace_heap.8");
	memcpy(&value, mem + 8, IdxSize);
	test(value == 100 - 8 - IdxSize, "makespace_heap.9");

	RETURN;
}

static int test_writereserved_heap(void)
{
	byte mem[0x1000];
	size_t size;

	heap_front = mem + 0x1000;
	heap_pos = mem;

	aatype(mem);
	writereserved_heap(mem, 80, 100);
	test(mem[80] == LISPSYSTEM_SPACE, "writereserved_heap.1");
	size = 0;
	GetSizeSpace(mem + 80, &size);
	test(size == 100 - 80, "writereserved_heap.2");

	aatype(mem);
	mem[99] = 0xAA;
	mem[100] = 0xBB;
	mem[101] = 0xCC;
	writereserved_heap(mem, 100, 100);
	test(mem[99] == 0xAA, "writereserved_heap.3");
	test(mem[100] == 0xBB, "writereserved_heap.4");
	test(mem[101] == 0xCC, "writereserved_heap.5");

	RETURN;
}

static int test_allocfront_expand(void)
{
	byte mem[0x1000];

	aatype(mem);
	heap_pos = NULL;
	heap_front = mem;
	FrontMax = mem;
	heap_tail = mem + 0x1000;
	test(allocfront_expand(80) == mem, "allocfront_expand.1");
	test(heap_pos == mem + 80, "allocfront_expand.2");
	test(heap_front == mem + 80, "allocfront_expand.3");
	test(FrontMax == mem + 80, "allocfront_expand.4");

	heap_front = mem;
	heap_tail = mem + 80;
	test(allocfront_expand(80) == mem, "allocfront_expand.5");

	heap_front = mem;
	heap_tail = mem + 79;
	lisp_info_enable = 0; /* infoerror */
	test(allocfront_expand(80) == NULL, "allocfront_expand.6");
	lisp_info_enable = 1;

	RETURN;
}

static int test_allocfront(void)
{
	byte mem[0x1000];
	addr pos;
	size_t size, spacesize;

	aatype(mem);
	heap_pos = heap_front = mem;
	heap_tail = mem + 0x1000;
	pos = allocfront_object(800);
	test(pos == mem, "allocfront.1");
	test(heap_pos == mem + 800, "allocfront.2");
	test(heap_front == mem + 800, "allocfront.3");

	aatype(mem);
	heap_pos = mem;
	heap_front = mem + 0x1000;
	heap_tail = mem + 0x1000;
	SetType(mem, LISPTYPE_CONS);
	SetStatus(mem, LISPSIZE_ARRAY2);
	*PtrValue2L(mem) = 32;
	size = 32;
	SetType(mem + size, LISPSYSTEM_SPACE1);
	SetSizeSpace1(mem + size, 80);
	SetType(mem + size + 80, LISPTYPE_CONS);
	SetStatus(mem + size + 80, LISPSIZE_ARRAY2);
	pos = allocfront(48);
	test(pos == mem + size, "allocfront.4");
	test(heap_pos == mem + size + 48, "allocfront.5");
	test(mem[size + 48] == LISPSYSTEM_SPACE, "allocfront.6");
	spacesize = 0;
	GetSizeSpace(mem + size + 48, &spacesize);
	test(spacesize == 80 - 48, "allocfront7");

	RETURN;
}

static int test_alloctail(void)
{
	byte mem[10000];
	addr pos;
	struct heap_addr *ret;

	aatype(mem);
	Align8Front(mem, &pos);
	heap_tail = pos + 8000;
	heap_front = pos;
	ret = alloctail();
	test(EqualPointer(ret, heap_tail), "alloctail.1");
	test(EqualPointer(ret, pos + 8000 - alloctail_size), "alloctail.2");

	heap_tail = pos + alloctail_size;
	heap_front = pos;
	ret = alloctail();
	test(EqualPointer(ret, heap_tail), "alloctail.3");
	test(EqualPointer(ret, pos), "alloctail.4");

	RETURN;
}

#define heap_debug_size  0x10000
static void gccheck_debug()
{
	GcCounter = 0;
	GcCheck1 = GcCheck2 = GcCheck3 = GcCheck4 = (heap_pos + heap_debug_size - 80);
}

static int test_allocheap(void)
{
	byte mem[heap_debug_size];
	addr pos, temp;

	aatype(mem);
	Align8Front(mem, &pos);
	heap_pos = heap_front = pos;
	gccheck_debug();
	heap_tail = pos + heap_debug_size - 8;

	/* size2 */
	allocheap(256, LISPTYPE_CONS, &temp, 1);
	test(temp[0] == LISPTYPE_CONS, "allocheap.1");
	test(*PtrValue2L(temp) == 256, "allocheap.2");

	allocheap(64, LISPTYPE_SYMBOL, &temp, 1);
	test(temp[0] == LISPTYPE_SYMBOL, "allocheap.3");
	test(*PtrValue2L(temp) == 64, "allocheap.4");

	/* size */
	allocheap(256, LISPTYPE_CONS, &temp, 0);
	test(temp[0] == LISPTYPE_CONS, "allocheap.5");
	test(*PtrValueL(temp) == 256, "allocheap.6");

	allocheap(64, LISPTYPE_SYMBOL, &temp, 0);
	test(temp[0] == LISPTYPE_SYMBOL, "allocheap.7");
	test(*PtrValueL(temp) == 64, "allocheap.8");

	RETURN;
}

static int test_alloc_heap(void)
{
	lisp_info_enable = 0; /* infoerror */
	test(alloc_heap(10000) == 1, "alloc_heap.1");
	lisp_info_enable = 1;
	free_heap();
	test(alloc_heap(1000*1000) == 0, "alloc_heap.2");
	free_heap();

	RETURN;
}


/*
 *  heap_memory
 */
int test_heap_memory(void)
{
	TITLE;

	TestBreak(test_GcCheck);
	TestBreak(test_allocfront_size);
	TestBreak(test_allocfront_search);
	TestBreak(test_makespace_heap);
	TestBreak(test_writereserved_heap);
	TestBreak(test_allocfront_expand);
	TestBreak(test_allocfront);
	TestBreak(test_alloctail);
	TestBreak(test_allocheap);
	TestBreak(test_alloc_heap);

	return 0;
}

