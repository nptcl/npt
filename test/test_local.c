#include "local.c"
#include "degrade.h"
#include "heap.h"

static int test_lowlevel_unsafe(void)
{
	byte mem[100000];
	addr pos, ret;
	struct localroot local;

	Align8Front(mem, &pos);
	local.front = pos;
	local.tail = pos + 16;
	local.size = 100000;
	ret = lowlevel_unsafe(&local, 10);
	test(ret == pos, "lowlevel_unsafe1");
	test(local.front == pos + 16, "lowlevel_unsafe2");

	local.front = pos;
	local.tail = pos + 15;
	ret = lowlevel_unsafe(&local, 10);
	test(ret == NULL, "lowlevel_unsafe3");

	RETURN;
}

static int test_alloc_local(void)
{
	byte mem[100000];
	addr pos, ret;
	struct localroot local;
	struct localcell cell;

	Align8Front(mem, &pos);
	local.front = pos;
	local.tail = pos + 17;
	local.cell = &cell;
	cell.count = 0;
	ret = alloc_local(&local, 10);
	test(ret == pos, "alloc_local1");
	test(local.front == pos + 16, "alloc_local2");

	RETURN;
}

static int test_make_local(void)
{
	struct localroot *local;

	local = make_local(0x100000);
	test(local, "make_local1");
	test(local->size == 0x100000, "make_local2");
	test(local->alloc, "make_local3");
	test(local->front, "make_local4");
	test(local->tail == local->size + (addr)local->alloc, "make_local5");
	test(local->stack == NULL, "make_local6");
	test(Align8Out(local->front) == 0, "make_local7");
	test(Align8Out(local->stack) == 0, "make_local8");
	free_local(local);

	RETURN;
}

static int test_unsafe_push_local(void)
{
	struct localroot *local;
	struct localstack *stack;
	addr front;

	local = make_local(0x100000);
	test(local->stack == NULL, "unsafe_push_local1");
	front = local->front;
	unsafe_push_local(local);
	test(local->stack, "unsafe_push_local2");
	test((addr)local->stack == front, "unsafe_push_local3");
	test(local->stack->stack == NULL, "unsafe_push_local4");

	front = local->front;
	stack = local->stack;
	unsafe_push_local(local);
	test((addr)local->stack == front, "unsafe_push_local5");
	test(local->stack->stack == stack, "unsafe_push_local6");

	free_local(local);

	RETURN;
}

static int test_unsafe_pop_local(void)
{
	struct localroot *local;
	struct localstack *stack;
	addr front, first;

	local = make_local(0x100000);
	first = local->front;
	test(local->stack == NULL, "unsafe_pop_local1");
	front = local->front;
	unsafe_pop_local(local);
	test(local->front == front, "unsafe_pop_local2");
	test(local->stack == NULL, "unsafe_pop_local3");

	alloc_local(local, 100);
	test(local->front != front, "unsafe_pop_local3");
	unsafe_pop_local(local);
	test(local->front == front, "unsafe_pop_local4");

	front = local->front;
	unsafe_push_local(local);
	test(local->stack, "unsafe_pop_local5");
	alloc_local(local, 100);
	unsafe_pop_local(local);
	test(local->stack == NULL, "unsafe_pop_local6");
	test(local->front == front, "unsafe_pop_local7");

	unsafe_push_local(local);
	alloc_local(local, 200);
	stack = local->stack;
	front = local->front;
	unsafe_push_local(local);
	alloc_local(local, 300);
	test(local->stack != stack, "unsafe_pop_local8");
	unsafe_pop_local(local);
	test(local->stack == stack, "unsafe_pop_local9");
	test(local->front == front, "unsafe_pop_local10");
	unsafe_pop_local(local);
	unsafe_pop_local(local);
	unsafe_pop_local(local);
	unsafe_pop_local(local);
	unsafe_pop_local(local);
	test(local->front == first, "unsafe_pop_local11");

	free_local(local);

	RETURN;
}

static int test_unsafe_pop_local_error(void)
{
	struct localroot *local;
	addr front;

	local = make_local(0x100000);
	front = local->front;
	unsafe_push_local(local);
	unsafe_pop_local(local);
	test(local->front == front, "unsafe_pop_local_error1");
	free_local(local);

	RETURN;
}

static int test_rollback_local(void)
{
	struct localroot *local;
	struct localstack *stack, *back;
	struct localcell *cell;
	addr check, first;
	size_t count, i;

	local = make_local(0x10000000);
	/* NULL */
	first = local->front;
	rollback_local(local, NULL);
	test(local->stack == NULL, "rollback_local1");
	test(local->front == first, "rollback_local2");
	test(local->cell, "rollback_local2a");
	test(local->cell->next == NULL, "rollback_local2b");
	test(local->cell->count == 0, "rollback_local2c");

	/* NULL delete */
	alloc_local(local, 2000);
	rollback_local(local, NULL);
	test(local->stack == NULL, "rollback_local3");
	test(local->front == first, "rollback_local4");
	test(local->cell, "rollback_local4a");
	test(local->cell->next == NULL, "rollback_local4b");
	test(local->cell->count == 0, "rollback_local4c");

	/* rollback, 1object */
	alloc_local(local, 300);
	check = local->front;
	unsafe_push_local(local);
	stack = local->stack;
	cell = local->cell;
	count = local->cell->count;
	alloc_local(local, 400);
	test(local->front != check, "rollback_local5");
	rollback_local(local, stack);
	test(local->front == check, "rollback_local6");
	test(local->stack == NULL, "rollback_local7");
	test(local->cell == cell, "rollback_local7a");
	test(local->cell->next == NULL, "rollback_local7b");
	test(local->cell->count == count, "rollback_local7c");

	/* rollback, 2object */
	check = local->front;
	unsafe_push_local(local);
	stack = local->stack;
	cell = local->cell;
	count = cell->count;
	alloc_local(local, 400);
	alloc_local(local, 400);
	unsafe_push_local(local);
	unsafe_push_local(local);
	unsafe_push_local(local);
	rollback_local(local, stack);
	test(local->front == check, "rollback_local8");
	test(local->stack == NULL, "rollback_local9");
	test(local->cell == cell, "rollback_local9a");
	test(local->cell->next == NULL, "rollback_local9b");
	test(local->cell->count == count, "rollback_local9c");

	/* all delete */
	unsafe_push_local(local);
	unsafe_push_local(local);
	alloc_local(local, 400);
	alloc_local(local, 400);
	unsafe_push_local(local);
	unsafe_push_local(local);
	alloc_local(local, 400);
	rollback_local(local, NULL);
	test(local->front == first, "rollback_local10");
	test(local->stack == NULL, "rollback_local11");
	test(local->cell->next == NULL, "rollback_local11b");
	test(local->cell->count == 0, "rollback_local11c");

	/* many object */
	unsafe_push_local(local);
	unsafe_push_local(local);
	unsafe_push_local(local);
	alloc_local(local, 100);
	alloc_local(local, 400);
	alloc_local(local, 400);
	alloc_local(local, 400);
	check = local->front;
	back = local->stack;
	unsafe_push_local(local);
	stack = local->stack;
	cell = local->cell;
	count = local->cell->count;
	for (i = 0; i < LocalCount * 20 + 10; i++)
		alloc_local(local, 100);
	unsafe_push_local(local);
	for (i = 0; i < LocalCount * 30 + 10; i++)
		alloc_local(local, 200);
	rollback_local(local, stack);
	test(local->stack == back, "rollback_local12");
	test(local->front == check, "rollback_local13");
	test(local->cell == cell, "rollback_local13a");
	test(local->cell->next == NULL, "rollback_local13b");
	test(local->cell->count == count, "rollback_local13c");

	free_local(local);

	RETURN;
}

static int statustest(addr body)
{
	return GetStatusDynamic(body)? GetStatusSize(body): -1;
}

static int test_local_cons(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_cons(local, &root);
	test(GetType(root) == LISPTYPE_CONS, "local_cons1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_cons2");
	test(GetCheckSize2(root), "local_cons2a");
	test(GetCheckArray(root), "local_cons2b");
	size = MemoryLengthA2(2);
	test(*PtrValue2L(root) == size_split(size), "local_cons2s");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "local_cons5");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "local_cons6");
	free_local(local);

	RETURN;
}

static int test_local_smallsize(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_smallsize(local, &root, LISPTYPE_SYMBOL, 10, 20);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_smallsize1");
	test(statustest(root) == LISPSIZE_SMALLSIZE, "local_smallsize2");
	test(GetCheckSize2(root), "local_smallsize2a");
	test(GetCheckArray(root), "local_smallsize2b");
	test(GetCheckBody(root), "local_smallsize2c");
	test(GetCheckArrayBody(root), "local_smallsize2d");
	size = MemoryLengthSS(10, 20);
	test(*PtrValue2L(root) == size_split(size), "local_smallsize2s");
	cons = PtrBodySS(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArraySS(root)[0];
	test(cons == Unbound, "local_smallsize5");
	cons = 0;
	cons = PtrArraySS(root)[1];
	test(cons == Unbound, "local_smallsize6");
	cons = 0;
	cons = PtrArraySS(root)[9];
	test(cons == Unbound, "local_smallsize7");

	size = GetLenArraySS(root);
	test(size == 10, "local_smallsize8");
	size = GetLenBodySS(root);
	test(size == 20, "local_smallsize9");
	free_local(local);

	RETURN;
}

static int test_local_array2(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_array2(local, &root, LISPTYPE_SYMBOL, 10);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array2-1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_array2-2");
	test(GetCheckSize2(root), "local_array2-2a");
	test(GetCheckArray(root), "local_array2-2b");
	size = MemoryLengthA2(10);
	test(*PtrValue2L(root) == size_split(size), "local_array2-2s");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "local_array2-5");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "local_array2-6");
	cons = 0;
	cons = PtrArrayA2(root)[9];
	test(cons == Unbound, "local_array2-7");
	size = GetLenArrayA2(root);
	test(size == 10, "local_array2-8");
	free_local(local);

	RETURN;
}

static int test_local_body2(void)
{
	addr root, pos;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_body2(local, &root, LISPTYPE_SYMBOL, 20);
	pos = PtrBodyB2(root);
	memset(pos, 0xBB, 20);

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body2-1");
	test(statustest(root) == LISPSIZE_BODY2, "local_body2-2");
	test(GetCheckSize2(root), "local_body2-2a");
	test(GetCheckBody(root), "local_body2-2b");
	size = MemoryLengthB2(20);
	test(*PtrValue2L(root) == size_split(size), "local_body2-2s");
	size = GetLenBodyB2(root);
	test(size == 20, "local_body2-8");
	free_local(local);

	RETURN;
}

static int test_local_arraybody(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_arraybody(local, &root, LISPTYPE_SYMBOL, 10, 20);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_arraybody1");
	test(statustest(root) == LISPSIZE_ARRAYBODY, "local_arraybody2");
	test(GetCheckSize4(root), "local_arraybody2a");
	test(GetCheckArray(root), "local_arraybody2b");
	test(GetCheckBody(root), "local_arraybody2c");
	test(GetCheckArrayBody(root), "local_arraybody2d");
	size = MemoryLengthAB(10, 20);
	test(*PtrValueL(root) == size_split(size), "local_arraybody2-2s");
	cons = PtrBodyAB(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArrayAB(root)[0];
	test(cons == Unbound, "local_arraybody5");
	cons = 0;
	cons = PtrArrayAB(root)[1];
	test(cons == Unbound, "local_arraybody6");
	cons = 0;
	cons = PtrArrayAB(root)[9];
	test(cons == Unbound, "local_arraybody7");

	size = GetLenArrayAB(root);
	test(size == 10, "local_arraybody8");
	size = GetLenBodyAB(root);
	test(size == 20, "local_arraybody9");
	free_local(local);

	RETURN;
}

static int test_local_array4(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_array4(local, &root, LISPTYPE_SYMBOL, 10);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array4-1");
	test(statustest(root) == LISPSIZE_ARRAY4, "local_array4-2");
	test(GetCheckSize4(root), "local_array4-2a");
	test(GetCheckArray(root), "local_array4-2b");
	size = MemoryLengthA4(10);
	test(*PtrValueL(root) == size_split(size), "local_array4-2s");
	cons = 0;
	cons = PtrArrayA4(root)[0];
	test(cons == Unbound, "local_array4-5");
	cons = 0;
	cons = PtrArrayA4(root)[1];
	test(cons == Unbound, "local_array4-6");
	cons = 0;
	cons = PtrArrayA4(root)[9];
	test(cons == Unbound, "local_array4-7");
	size = GetLenArrayA4(root);
	test(size == 10, "local_array4-8");
	free_local(local);

	RETURN;
}

static int test_local_body4(void)
{
	addr root, pos;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_body4(local, &root, LISPTYPE_SYMBOL, 20);
	pos = PtrBodyB4(root);
	memset(pos, 0xBB, 20);

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body4-1");
	test(statustest(root) == LISPSIZE_BODY4, "local_body4-2");
	test(GetCheckSize4(root), "local_body4-2a");
	test(GetCheckBody(root), "local_body4-2b");
	size = MemoryLengthB4(20);
	test(*PtrValueL(root) == size_split(size), "local_body4-2s");
	size = GetLenBodyB4(root);
	test(size == 20, "local_body4-8");
	free_local(local);

	RETURN;
}

#ifdef LISP_ARCH_64BIT
static int test_local_array8(void)
{
	addr root, cons;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_array8(local, &root, LISPTYPE_SYMBOL, 10);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array8-1");
	test(statustest(root) == LISPSIZE_ARRAY8, "local_array8-2");
	test(GetCheckSize8(root), "local_array8-2a");
	test(GetCheckArray(root), "local_array8-2b");
	size = MemoryLengthA8(10);
	test(*PtrValueL(root) == size_split(size), "local_array8-2s");
	cons = 0;
	cons = PtrArrayA8(root)[0];
	test(cons == Unbound, "local_array8-5");
	cons = 0;
	cons = PtrArrayA8(root)[1];
	test(cons == Unbound, "local_array8-6");
	cons = 0;
	cons = PtrArrayA8(root)[9];
	test(cons == Unbound, "local_array8-7");
	size = GetLenArrayA8(root);
	test(size == 10, "local_array8-8");
	free_local(local);

	RETURN;
}

static int test_local_body8(void)
{
	addr root, pos;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_body8(local, &root, LISPTYPE_SYMBOL, 20);
	pos = PtrBodyB8(root);
	memset(pos, 0xBB, 20);

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body8-1");
	test(statustest(root) == LISPSIZE_BODY8, "local_body8-2");
	test(GetCheckSize8(root), "local_body8-2a");
	test(GetCheckBody(root), "local_body8-2b");
	size = MemoryLengthB8(20);
	test(*PtrValueL(root) == size_split(size), "local_body8-2s");
	size = GetLenBodyB8(root);
	test(size == 20, "local_body8-8");
	free_local(local);

	RETURN;
}
#endif

static int test_local_array(void)
{
	addr root;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_array(local, &root, LISPTYPE_SYMBOL, 10);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_array2");
	test(GetCheckSize2(root), "local_array2a");
	test(GetCheckArray(root), "local_array2b");
	size = GetLenArrayA2(root);
	test(size == 10, "local_array3");
	free_local(local);

	RETURN;
}

static int test_local_body(void)
{
	addr root;
	size_t size;
	struct localroot *local;

	local = make_local(0x100000);
	Nil = Unbound;
	local_body(local, &root, LISPTYPE_SYMBOL, 20);
	test(GetType(root) == LISPTYPE_SYMBOL, "local_body1");
	test(statustest(root) == LISPSIZE_BODY2, "local_body2");
	test(GetCheckSize2(root), "local_body2a");
	test(GetCheckBody(root), "local_body2b");
	size = GetLenBodyB2(root);
	test(size == 20, "local_body2");
	free_local(local);

	RETURN;
}


/*
 *  main
 */
int test_local(void)
{
	TITLE;

	/* allocate */
	TestBreak(test_lowlevel_unsafe);
	TestBreak(test_alloc_local);
	TestBreak(test_make_local);
	TestBreak(test_unsafe_push_local);
	TestBreak(test_unsafe_pop_local);
	TestBreak(test_unsafe_pop_local_error);
	TestBreak(test_rollback_local);
	TestBreak(test_local_cons);

	TestBreak(test_local_smallsize);
	TestBreak(test_local_array2);
	TestBreak(test_local_body2);
	TestBreak(test_local_arraybody);
	TestBreak(test_local_array4);
	TestBreak(test_local_body4);
#ifdef LISP_ARCH_64BIT
	TestBreak(test_local_array8);
	TestBreak(test_local_body8);
#endif
	TestBreak(test_local_array);
	TestBreak(test_local_body);

	return 0;
}

