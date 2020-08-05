#include "sort.c"
#include "cons.h"
#include "constant.h"
#include "degrade.h"

/*
 *  unsafe
 */
static int test_compare2_(addr left, addr right, int *ret)
{
	fixnum value1, value2;
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	return Result(ret, value1 < value2);
}

static int test_simplesort_cons_unsafe(void)
{
	addr cons, value1, value2, value3, left, left2, left3;

	simplesort_cons_unsafe_(&cons, Nil, test_compare2_);
	test(cons == Nil, "simplesort_cons_unsafe.1");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value1, NULL);
	conscar_heap(&cons, value1);
	simplesort_cons_unsafe_(&cons, cons, test_compare2_);
	test(cons != Nil, "simplesort_cons_unsafe.2");
	GetCons(cons, &left, &cons);
	test(left == value1, "simplesort_cons_unsafe.3");
	test(cons == Nil, "simplesort_cons_unsafe.4");

	/* test */
	list_heap(&cons, value1, value2, value3, NULL);
	simplesort_cons_unsafe_(&cons, cons, test_compare2_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe.5");

	/* test */
	list_heap(&cons, value3, value2, value1, NULL);
	conscar_heap(&cons, value1);
	cons_heap(&cons, value2, cons);
	cons_heap(&cons, value3, cons);
	simplesort_cons_unsafe_(&cons, cons, test_compare2_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe.6");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_cons_unsafe_(&cons, cons, test_compare2_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_cons_unsafe.7");

	RETURN;
}

static int test_compare3_(addr info, addr left, addr right, int *ret)
{
	fixnum value1, value2;
	if (info == T)
		return Result(ret, 1);
	GetFixnum(left, &value1);
	GetFixnum(right, &value2);
	return Result(ret, value1 < value2);
}

static int test_simplesort_info_cons_unsafe(void)
{
	addr cons, value1, value2, value3, left, left2, left3;

	simplesort_info_cons_unsafe_(&cons, Nil, Nil, test_compare3_);
	test(cons == Nil, "simplesort_info_cons_unsafe.1");

	fixnum_heap(&value1, 100);
	fixnum_heap(&value2, 200);
	fixnum_heap(&value3, 300);
	list_heap(&cons, value1, NULL);
	simplesort_info_cons_unsafe_(&cons, cons, Nil, test_compare3_);
	test(cons != Nil, "simplesort_info_cons_unsafe.2");
	GetCons(cons, &left, &cons);
	test(left == value1, "simplesort_info_cons_unsafe.3");
	test(cons == Nil, "simplesort_info_cons_unsafe.4");

	/* test */
	list_heap(&cons, value1, value2, value3, NULL);
	simplesort_info_cons_unsafe_(&cons, cons, Nil, test_compare3_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe.5");

	/* test */
	list_heap(&cons, value3, value2, value1, NULL);
	simplesort_info_cons_unsafe_(&cons, cons, Nil, test_compare3_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe.6");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_info_cons_unsafe_(&cons, cons, Nil, test_compare3_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value1 && left2 == value2 && left3 == value3 && cons == Nil,
			"simplesort_info_cons_unsafe.7");

	/* test */
	list_heap(&cons, value2, value3, value1, NULL);
	simplesort_info_cons_unsafe_(&cons, cons, T, test_compare3_);
	/* check */
	GetCons(cons, &left, &cons);
	GetCons(cons, &left2, &cons);
	GetCons(cons, &left3, &cons);
	test(left == value2 && left2 == value3 && left3 == value1 && cons == Nil,
			"simplesort_info_cons_unsafe.8");

	RETURN;
}


/*
 *  main
 */
static int testcase_sort(void)
{
	TestBreak(test_simplesort_cons_unsafe);
	TestBreak(test_simplesort_info_cons_unsafe);

	return 0;
}

static void testinit_sort(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
}

int test_sort(void)
{
	TITLE;
	return degrade_code(
			testinit_sort,
			testcase_sort);
}

