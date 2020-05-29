#include "heap.c"
#include "heap_memory.h"
#include "degrade.h"

static int test_heap_cons(void)
{
	addr root, cons;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	test(heap_cons_count == 0, "heap_cons.1");
	heap_cons(&root);
	test(heap_cons_count == 1, "heap_cons.2");
	test(GetType(root) == LISPTYPE_CONS, "heap_cons.3");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_cons.4");
	test(GetLenArrayA2(root) == 2, "heap_cons.5");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "heap_cons.6");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "heap_cons.7");
	free_heap();

	RETURN;
}

static int test_heap_symbol(void)
{
	addr root, symbol;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	test(heap_symbol_count == 0, "heap_symbol.1");
	heap_symbol(&root);
	test(heap_symbol_count == 1, "heap_symbol.2");
	test(GetType(root) == LISPTYPE_SYMBOL, "heap_symbol.3");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_symbol.4");
	test(GetLenArrayA2(root) == SYMBOL_INDEX_SIZE, "heap_symbol.5");
	symbol = 0;
	symbol = PtrArrayA2(root)[0];
	test(symbol == Unbound, "heap_symbol.6");
	symbol = 0;
	symbol = PtrArrayA2(root)[1];
	test(symbol == Unbound, "heap_symbol.7");
	free_heap();

	RETURN;
}

static int test_heap_smallsize(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_smallsize(&root, LISPTYPE_VECTOR, 10, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_smallsize.1");
	test(GetStatus(root) == LISPSIZE_SMALLSIZE, "heap_smallsize.2");
	size = MemoryLengthSS(10, 20);
	test(*PtrValue2L(root) == size_split(size), "heap_smallsize.3");
	cons = PtrBodySS(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArraySS(root)[0];
	test(cons == Unbound, "heap_smallsize.4");
	cons = 0;
	cons = PtrArraySS(root)[1];
	test(cons == Unbound, "heap_smallsize.5");
	cons = 0;
	cons = PtrArraySS(root)[9];
	test(cons == Unbound, "heap_smallsize.6");

	size = GetLenArraySS(root);
	test(size == 10, "heap_smallsize.7");
	size = GetLenBodySS(root);
	test(size == 20, "heap_smallsize.8");
	free_heap();

	RETURN;
}

static int test_heap_array2(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array2(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array2.1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_array2.2");
	size = MemoryLengthA2(10);
	test(*PtrValue2L(root) == size_split(size), "heap_array2.3");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "heap_array2.4");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "heap_array2.5");
	cons = 0;
	cons = PtrArrayA2(root)[9];
	test(cons == Unbound, "heap_array2.6");
	size = GetLenArrayA2(root);
	test(size == 10, "heap_array2.7");
	free_heap();

	RETURN;
}

static int test_heap_body2(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body2(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB2(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body2.1");
	test(GetStatus(root) == LISPSIZE_BODY2, "heap_body2.2");
	size = MemoryLengthB2(20);
	test(*PtrValue2L(root) == size_split(size), "heap_body2.3");
	size = GetLenBodyB2(root);
	test(size == 20, "heap_body2.4");
	free_heap();

	RETURN;
}

static int test_heap_arraybody(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_arraybody(&root, LISPTYPE_VECTOR, 10, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_arraybody.1");
	test(GetStatus(root) == LISPSIZE_ARRAYBODY, "heap_arraybody.2");
	size = MemoryLengthAB(10, 20);
	test(*PtrValueL(root) == size_split(size), "heap_arraybody.3");
	cons = PtrBodyAB(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArrayAB(root)[0];
	test(cons == Unbound, "heap_arraybody.4");
	cons = 0;
	cons = PtrArrayAB(root)[1];
	test(cons == Unbound, "heap_arraybody.5");
	cons = 0;
	cons = PtrArrayAB(root)[9];
	test(cons == Unbound, "heap_arraybody.6");

	size = GetLenArrayAB(root);
	test(size == 10, "heap_arraybody.7");
	size = GetLenBodyAB(root);
	test(size == 20, "heap_arraybody.8");
	free_heap();

	RETURN;
}

static int test_heap_array4(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array4(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array4.1");
	test(GetStatus(root) == LISPSIZE_ARRAY4, "heap_array4.2");
	size = MemoryLengthA4(10);
	test(*PtrValueL(root) == size_split(size), "heap_array4.3");
	cons = 0;
	cons = PtrArrayA4(root)[0];
	test(cons == Unbound, "heap_array4.4");
	cons = 0;
	cons = PtrArrayA4(root)[1];
	test(cons == Unbound, "heap_array4.5");
	cons = 0;
	cons = PtrArrayA4(root)[9];
	test(cons == Unbound, "heap_array4.6");
	size = GetLenArrayA4(root);
	test(size == 10, "heap_array4.7");
	free_heap();

	RETURN;
}

static int test_heap_body4(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body4(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB4(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body4.1");
	test(GetStatus(root) == LISPSIZE_BODY4, "heap_body4.2");
	size = MemoryLengthB4(20);
	test(*PtrValueL(root) == size_split(size), "heap_body4.3");
	size = GetLenBodyB4(root);
	test(size == 20, "heap_body4.4");
	free_heap();

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_heap_array8(void)
{
	addr root, cons;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array8(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array8.1");
	test(GetStatus(root) == LISPSIZE_ARRAY8, "heap_array8.2");
	size = MemoryLengthA8(10);
	test(*PtrValueL(root) == size_split(size), "heap_array8.3");
	cons = 0;
	cons = PtrArrayA8(root)[0];
	test(cons == Unbound, "heap_array8.4");
	cons = 0;
	cons = PtrArrayA8(root)[1];
	test(cons == Unbound, "heap_array8.5");
	cons = 0;
	cons = PtrArrayA8(root)[9];
	test(cons == Unbound, "heap_array8.6");
	size = GetLenArrayA8(root);
	test(size == 10, "heap_array8.7");
	free_heap();

	RETURN;
}

static int test_heap_body8(void)
{
	addr root, body;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body8(&root, LISPTYPE_VECTOR, 20);
	body = PtrBodyB8(root);
	memset(body, 0xBB, 20);

	test(GetType(root) == LISPTYPE_VECTOR, "heap_body8.1");
	test(GetStatus(root) == LISPSIZE_BODY8, "heap_body8.2");
	size = MemoryLengthB8(20);
	test(*PtrValueL(root) == size_split(size), "heap_body8.3");
	size = GetLenBodyB8(root);
	test(size == 20, "heap_body8.4");
	free_heap();

	RETURN;
}
#endif

static int test_heap_array(void)
{
	addr root;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_array(&root, LISPTYPE_VECTOR, 10);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_array.1");
	test(GetStatus(root) == LISPSIZE_ARRAY2, "heap_array.2");
	size = GetLenArrayA2(root);
	test(size == 10, "heap_array.3");
	free_heap();

	RETURN;
}

static int test_heap_body(void)
{
	addr root;
	size_t size;

	alloc_heap(1024UL*1024UL*10UL);
	Nil = Unbound;
	heap_body(&root, LISPTYPE_VECTOR, 20);
	test(GetType(root) == LISPTYPE_VECTOR, "heap_body.1");
	test(GetStatus(root) == LISPSIZE_BODY2, "heap_body.2");
	size = GetLenBodyB2(root);
	test(size == 20, "heap_body.3");
	free_heap();

	RETURN;
}


/*
 *  heap
 */
int test_heap(void)
{
	TITLE;

	TestBreak(test_heap_cons);
	TestBreak(test_heap_symbol);
	TestBreak(test_heap_smallsize);
	TestBreak(test_heap_array2);
	TestBreak(test_heap_body2);
	TestBreak(test_heap_arraybody);
	TestBreak(test_heap_array4);
	TestBreak(test_heap_body4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_heap_array8);
	TestBreak(test_heap_body8);
#endif
	TestBreak(test_heap_array);
	TestBreak(test_heap_body);

	return 0;
}

