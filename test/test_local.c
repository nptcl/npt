#include "local.c"
#include "degrade.h"
#include "heap.h"

#ifndef LISP_MEMORY_MALLOC
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
	test(ret == pos, "lowlevel_unsafe.1");
	test(local.front == pos + 16, "lowlevel_unsafe.2");

	local.front = pos;
	local.tail = pos + 15;
	lisp_info_enable = 0; /* infoerror */
	ret = lowlevel_unsafe(&local, 10);
	lisp_info_enable = 1;
	test(ret == NULL, "lowlevel_unsafe.3");

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
	test(ret == pos, "alloc_local.1");
	test(local.front == pos + 16, "alloc_local.2");

	RETURN;
}

static int test_make_local(void)
{
	struct localroot *local;

	local = make_local(0x100000);
	test(local, "make_local.1");
	test(local->size == 0x100000, "make_local.2");
	test(local->alloc, "make_local.3");
	test(local->front, "make_local.4");
	test(local->tail == local->size + (addr)local->alloc, "make_local.5");
	test(local->stack == NULL, "make_local.6");
	test(Align8Out(local->front) == 0, "make_local.7");
	test(Align8Out(local->stack) == 0, "make_local.8");
	free_local(local);

	RETURN;
}

static int test_push_local(void)
{
	struct localroot *local;
	struct localstack *stack;
	LocalStack temp;
	addr front;

	local = make_local(0x100000);
	test(local->stack == NULL, "push_local.1");
	front = local->front;
	push_local(local, &temp);
	test(local->stack, "push_local.2");
	test((addr)local->stack == front, "push_local.3");
	test(local->stack->stack == NULL, "push_local.4");

	front = local->front;
	stack = local->stack;
	push_local(local, &temp);
	test((addr)local->stack == front, "push_local.5");
	test(local->stack->stack == stack, "push_local.6");

	free_local(local);

	RETURN;
}

static void test_rollback_dummy(addr pos)
{
	pos[0] = LISPTYPE_VECTOR;
	pos[1] = LISPSIZE_ARRAY2;
	pos[2] = 0xFF;
	pos[3] = 0x00;
	*PtrLenArrayA2(pos) = 0;
}

static int test_rollback_local(void)
{
	struct localroot *local;
	struct localstack *stack, *back, *temp;
	struct localcell *cell;
	addr check, first, pos;
	size_t count, i;

	local = make_local(0x10000000);
	/* NULL */
	first = local->front;
	push_local(local, &stack);
	rollback_local(local, stack);
	test(local->stack == NULL, "rollback_local.1");
	test(local->front == first, "rollback_local.2");
	test(local->cell, "rollback_local.3");
	test(local->cell->next == NULL, "rollback_local.4");
	test(local->cell->count == 0, "rollback_local.5");

	/* NULL delete */
	push_local(local, &stack);
	pos = alloc_local(local, 2000);
	test_rollback_dummy(pos);
	rollback_local(local, stack);
	test(local->stack == NULL, "rollback_local.6");
	test(local->front == first, "rollback_local.7");
	test(local->cell, "rollback_local.8");
	test(local->cell->next == NULL, "rollback_local.9");
	test(local->cell->count == 0, "rollback_local.10");

	/* rollback, 1object */
	pos = alloc_local(local, 300);
	test_rollback_dummy(pos);
	check = local->front;
	push_local(local, &stack);
	cell = local->cell;
	count = local->cell->count;
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	test(local->front != check, "rollback_local.11");
	rollback_local(local, stack);
	test(local->front == check, "rollback_local.12");
	test(local->stack == NULL, "rollback_local.13");
	test(local->cell == cell, "rollback_local.14");
	test(local->cell->next == NULL, "rollback_local.15");
	test(local->cell->count == count, "rollback_local.16");

	/* rollback, 2object */
	check = local->front;
	push_local(local, &stack);
	cell = local->cell;
	count = cell->count;
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	push_local(local, &temp);
	push_local(local, &temp);
	push_local(local, &temp);
	rollback_local(local, stack);
	test(local->front == check, "rollback_local.17");
	test(local->stack == NULL, "rollback_local.18");
	test(local->cell == cell, "rollback_local.19");
	test(local->cell->next == NULL, "rollback_local.20");
	test(local->cell->count == count, "rollback_local.21");

	/* all delete */
	first = local->front;
	push_local(local, &stack);
	push_local(local, &temp);
	push_local(local, &temp);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	push_local(local, &temp);
	push_local(local, &temp);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	rollback_local(local, stack);
	test(local->front == first, "rollback_local.22");
	test(local->stack == NULL, "rollback_local.23");
	test(local->cell->next == NULL, "rollback_local.24");

	/* many object */
	push_local(local, &temp);
	push_local(local, &temp);
	push_local(local, &temp);
	pos = alloc_local(local, 100);
	test_rollback_dummy(pos);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	pos = alloc_local(local, 400);
	test_rollback_dummy(pos);
	check = local->front;
	back = local->stack;
	push_local(local, &temp);
	stack = local->stack;
	cell = local->cell;
	count = local->cell->count;
	for (i = 0; i < LocalCount * 20 + 10; i++) {
		pos = alloc_local(local, 100);
		test_rollback_dummy(pos);
	}
	push_local(local, &temp);
	for (i = 0; i < LocalCount * 30 + 10; i++) {
		pos = alloc_local(local, 200);
		test_rollback_dummy(pos);
	}
	rollback_local(local, stack);
	test(local->stack == back, "rollback_local.25");
	test(local->front == check, "rollback_local.26");
	test(local->cell == cell, "rollback_local.27");
	test(local->cell->next == NULL, "rollback_local.28");
	test(local->cell->count == count, "rollback_local.29");

	free_local(local);

	RETURN;
}
#endif

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
	test(GetType(root) == LISPTYPE_CONS, "local_cons.1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_cons.2");
	size = MemoryLengthA2(2);
	test(*PtrValue2L(root) == size_split(size), "local_cons.3");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "local_cons.4");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "local_cons.5");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_smallsize.1");
	test(statustest(root) == LISPSIZE_SMALLSIZE, "local_smallsize.2");
	size = MemoryLengthSS(10, 20);
	test(*PtrValue2L(root) == size_split(size), "local_smallsize.3");
	cons = PtrBodySS(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArraySS(root)[0];
	test(cons == Unbound, "local_smallsize.4");
	cons = 0;
	cons = PtrArraySS(root)[1];
	test(cons == Unbound, "local_smallsize.5");
	cons = 0;
	cons = PtrArraySS(root)[9];
	test(cons == Unbound, "local_smallsize.6");

	size = GetLenArraySS(root);
	test(size == 10, "local_smallsize.7");
	size = GetLenBodySS(root);
	test(size == 20, "local_smallsize.8");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array2.1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_array2.2");
	size = MemoryLengthA2(10);
	test(*PtrValue2L(root) == size_split(size), "local_array2.3");
	cons = 0;
	cons = PtrArrayA2(root)[0];
	test(cons == Unbound, "local_array2.4");
	cons = 0;
	cons = PtrArrayA2(root)[1];
	test(cons == Unbound, "local_array2.5");
	cons = 0;
	cons = PtrArrayA2(root)[9];
	test(cons == Unbound, "local_array2.6");
	size = GetLenArrayA2(root);
	test(size == 10, "local_array2.7");
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

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body2.1");
	test(statustest(root) == LISPSIZE_BODY2, "local_body2.2");
	size = MemoryLengthB2(20);
	test(*PtrValue2L(root) == size_split(size), "local_body2.3");
	size = GetLenBodyB2(root);
	test(size == 20, "local_body2.4");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_arraybody.1");
	test(statustest(root) == LISPSIZE_ARRAYBODY, "local_arraybody.2");
	size = MemoryLengthAB(10, 20);
	test(*PtrValueL(root) == size_split(size), "local_arraybody.3");
	cons = PtrBodyAB(root);
	memset(cons, 0xBB, 20);

	cons = 0;
	cons = PtrArrayAB(root)[0];
	test(cons == Unbound, "local_arraybody.4");
	cons = 0;
	cons = PtrArrayAB(root)[1];
	test(cons == Unbound, "local_arraybody.5");
	cons = 0;
	cons = PtrArrayAB(root)[9];
	test(cons == Unbound, "local_arraybody.6");

	size = GetLenArrayAB(root);
	test(size == 10, "local_arraybody.7");
	size = GetLenBodyAB(root);
	test(size == 20, "local_arraybody.8");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array4.1");
	test(statustest(root) == LISPSIZE_ARRAY4, "local_array4.2");
	size = MemoryLengthA4(10);
	test(*PtrValueL(root) == size_split(size), "local_array4.3");
	cons = 0;
	cons = PtrArrayA4(root)[0];
	test(cons == Unbound, "local_array4.4");
	cons = 0;
	cons = PtrArrayA4(root)[1];
	test(cons == Unbound, "local_array4.5");
	cons = 0;
	cons = PtrArrayA4(root)[9];
	test(cons == Unbound, "local_array4.6");
	size = GetLenArrayA4(root);
	test(size == 10, "local_array4.7");
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

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body4.1");
	test(statustest(root) == LISPSIZE_BODY4, "local_body4.2");
	size = MemoryLengthB4(20);
	test(*PtrValueL(root) == size_split(size), "local_body4.3");
	size = GetLenBodyB4(root);
	test(size == 20, "local_body4.4");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array8.1");
	test(statustest(root) == LISPSIZE_ARRAY8, "local_array8.2");
	size = MemoryLengthA8(10);
	test(*PtrValueL(root) == size_split(size), "local_array8.3");
	cons = 0;
	cons = PtrArrayA8(root)[0];
	test(cons == Unbound, "local_array8.4");
	cons = 0;
	cons = PtrArrayA8(root)[1];
	test(cons == Unbound, "local_array8.5");
	cons = 0;
	cons = PtrArrayA8(root)[9];
	test(cons == Unbound, "local_array8.6");
	size = GetLenArrayA8(root);
	test(size == 10, "local_array8.7");
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

	test(GetType(root) == LISPTYPE_SYMBOL, "local_body8.1");
	test(statustest(root) == LISPSIZE_BODY8, "local_body8.2");
	size = MemoryLengthB8(20);
	test(*PtrValueL(root) == size_split(size), "local_body8.3");
	size = GetLenBodyB8(root);
	test(size == 20, "local_body8.4");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_array.1");
	test(statustest(root) == LISPSIZE_ARRAY2, "local_array.2");
	size = GetLenArrayA2(root);
	test(size == 10, "local_array.3");
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
	test(GetType(root) == LISPTYPE_SYMBOL, "local_body.1");
	test(statustest(root) == LISPSIZE_BODY2, "local_body.2");
	size = GetLenBodyB2(root);
	test(size == 20, "local_body.3");
	free_local(local);

	RETURN;
}


/*
 *  local
 */
int test_local(void)
{
	DegradeTitle;

#ifndef LISP_MEMORY_MALLOC
	TestBreak(test_lowlevel_unsafe);
	TestBreak(test_alloc_local);
	TestBreak(test_make_local);
	TestBreak(test_push_local);
	TestBreak(test_rollback_local);
#endif

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

