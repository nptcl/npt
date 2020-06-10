#include "clos_cache.c"
#include "clos.h"
#include "cons.h"
#include "constant.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"

static int test_hashindex_cache(void)
{
	addr pos;
	size_t value1, value2;

	hashindex_cache(Nil, 1000, &value1);
	test(value1 == 0, "hashindex_cache1");

	list_heap(&pos, Nil, NULL);
	hashindex_cache(pos, 1000, &value1);
	list_heap(&pos, Nil, NULL);
	hashindex_cache(pos, 1000, &value2);
	test(value1 == value2, "hashindex_cache2");

	list_heap(&pos, Nil, Nil, NULL);
	hashindex_cache(pos, 1000, &value2);
	test(value1 != value2, "hashindex_cache3");

	list_heap(&pos, T, NULL);
	hashindex_cache(pos, 1000, &value2);
	test(value1 != value2, "hashindex_cache4");

	RETURN;
}

static int test_cache_equal_function(void)
{
	addr left, right;

	test(cache_equal_function(Nil, Nil), "cache_equal_function1");
	test(! cache_equal_function(T, Nil), "cache_equal_function2");
	test(! cache_equal_function(Nil, T), "cache_equal_function3");
	list_heap(&left, Nil, T, NULL);
	list_heap(&right, Nil, T, NULL);
	test(cache_equal_function(left, right), "cache_equal_function4");

	list_heap(&left, T, NULL);
	list_heap(&right, Nil, T, NULL);
	test(! cache_equal_function(left, right), "cache_equal_function5");

	list_heap(&left, T, T, NULL);
	list_heap(&right, Nil, T, NULL);
	test(! cache_equal_function(left, right), "cache_equal_function6");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_cache(void)
{
	TestBreak(test_hashindex_cache);
	TestBreak(test_cache_equal_function);

	return 0;
}

int test_clos_cache(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_setjmp(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		build_package();
		build_clos(ptr);
		lisp_initialize = 1;
		result = testbreak_clos_cache();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

