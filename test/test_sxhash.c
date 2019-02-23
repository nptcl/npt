#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "sxhash.c"

static int test_fixed_pointer(void)
{
	addr cons1, cons2;

	consnil_heap(&cons1);
	consnil_heap(&cons2);
	test(fixed_pointer(cons1, -1) == fixed_pointer(cons1, -1), "fixed_pointer1");
	test(fixed_pointer(cons1, -1) != fixed_pointer(cons2, -1), "fixed_pointer2");

	RETURN;
}

static int test_fixed_equal_cons(void)
{
	addr cons1, cons2, cons3, cons4, v1, v2;

	consnil_heap(&cons1);
	consnil_heap(&cons2);
	test(fixed_equal_cons(cons1, -1) == fixed_equal_cons(cons2, -1), "fixed_equal_cons1");
	SetCons(cons1, T, Nil);
	test(fixed_equal_cons(cons1, -1) != fixed_equal_cons(cons2, -1), "fixed_equal_cons2");
	test(fixed_equal_cons(cons1, 0) == fixed_equal_cons(cons2, 0), "fixed_equal_cons3");
	test(fixed_equal_cons(cons1, 1) != fixed_equal_cons(cons2, 1), "fixed_equal_cons4");

	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	SetCons(cons1, Nil, v1);
	SetCons(cons2, Nil, v2);
	test(fixed_equal_cons(cons1, -1) != fixed_equal_cons(cons2, -1), "fixed_equal_cons5");
	SetStatusValue(v2, LISPSTATUS_READONLY, 0);
	SetFixnum(v2, 100);
	test(fixed_equal_cons(cons1, -1) == fixed_equal_cons(cons2, -1), "fixed_equal_cons6");

	consnil_heap(&cons3);
	consnil_heap(&cons4);
	SetFixnum(v2, 200);
	SetCons(cons1, cons3, Nil);
	SetCons(cons2, cons4, Nil);
	SetCons(cons3, T, v1);
	SetCons(cons4, T, v2);
	test(fixed_equal_cons(cons1, -1) != fixed_equal_cons(cons2, -1), "fixed_equal_cons7");
	test(fixed_equal_cons(cons1, 0) == fixed_equal_cons(cons2, 0), "fixed_equal_cons8");
	test(fixed_equal_cons(cons1, 1) == fixed_equal_cons(cons2, 1), "fixed_equal_cons9");
	test(fixed_equal_cons(cons1, 2) != fixed_equal_cons(cons2, 2), "fixed_equal_cons10");

	SetFixnum(v2, 100);
	test(fixed_equal_cons(cons1, -1) == fixed_equal_cons(cons2, -1), "fixed_equal_cons11");
	test(fixed_equal_cons(cons1, 0) == fixed_equal_cons(cons2, 0), "fixed_equal_cons12");
	test(fixed_equal_cons(cons1, 1) == fixed_equal_cons(cons2, 1), "fixed_equal_cons13");
	test(fixed_equal_cons(cons1, 2) == fixed_equal_cons(cons2, 2), "fixed_equal_cons14");

	RETURN;
}

static int test_fixed_vector(void)
{
	addr pos;

	vector2_heap(&pos, 100);
	test(fixed_vector(pos, -1) == 100, "fixed_vector1");

	RETURN;
}

static int test_fixed_character(void)
{
	addr pos;

	character_heap(&pos, 'a');
	test(fixed_character(pos, -1) == 'a', "fixed_character1");
	character_heap(&pos, 10000);
	test(fixed_character(pos, -1) == 10000, "fixed_character2");

	RETURN;
}

static int test_fixed_string(void)
{
	addr pos;
	fixed v1, v2;

	strvect_char_heap(&pos, "");
	v1 = fixed_string(pos, -1);
	strvect_char_heap(&pos, "");
	v2 = fixed_string(pos, -1);
	test(v1 == v2, "fixed_string1");

	strvect_char_heap(&pos, "a");
	v2 = fixed_string(pos, -1);
	test(v1 != v2, "fixed_string2");

	strvect_char_heap(&pos, "a");
	v1 = fixed_string(pos, -1);
	test(v1 == v2, "fixed_string3");

	strvect_char_heap(&pos, "A");
	v2 = fixed_string(pos, -1);
	test(v1 != v2, "fixed_string4");

	strvect_char_heap(&pos, "HelloAAAABBB");
	v1 = fixed_string(pos, -1);
	test(v1 != v2, "fixed_string5");

	strvect_char_heap(&pos, "zzzyyy");
	v2 = fixed_string(pos, -1);
	test(v1 != v2, "fixed_string6");

	strvect_char_heap(&pos, "HelloAAAABBBB");
	v2 = fixed_string(pos, -1);
	test(v1 != v2, "fixed_string7");

	strvect_char_heap(&pos, "HelloAAAABBB");
	v2 = fixed_string(pos, -1);
	test(v1 == v2, "fixed_string8");

	RETURN;
}

static int test_fixed_fixnum(void)
{
	addr pos;
	fixed v1, v2;

	fixnum_heap(&pos, 100);
	test(fixed_fixnum(pos, -1) == 100, "fixed_fixnum1");
	fixnum_heap(&pos, -10);
	v1 = fixed_fixnum(pos, -1);
	fixnum_heap(&pos, -11);
	v2 = fixed_fixnum(pos, -1);
	test(v1 != v2, "fixed_fixnum2");
	
	RETURN;
}

static int testbreak_sxhash(void)
{
	TestBreak(test_fixed_pointer);
	TestBreak(test_fixed_equal_cons);
	TestBreak(test_fixed_vector);
	TestBreak(test_fixed_character);
	TestBreak(test_fixed_string);
	TestBreak(test_fixed_fixnum);

	return 0;
}

int test_sxhash(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	ptr = Execute_Thread;
	lisp_info_enable = 1;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
		build_lisproot(ptr);
		build_constant();
		build_object();
		lisp_init = 1;
		result = testbreak_sxhash();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

