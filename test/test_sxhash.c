#include "character.h"
#include "constant.h"
#include "degrade.h"
#include "object.h"
#include "sxhash.c"

static int test_sxfixed_eq(void)
{
	addr cons1, cons2;

	consnil_heap(&cons1);
	consnil_heap(&cons2);
	test(sxfixed_eq(cons1, -1) == sxfixed_eq(cons1, -1), "sxfixed_eq1");
	test(sxfixed_eq(cons1, -1) != sxfixed_eq(cons2, -1), "sxfixed_eq2");

	RETURN;
}

static int test_sxfixed_cons(void)
{
	addr cons1, cons2, cons3, cons4, v1, v2;

	consnil_heap(&cons1);
	consnil_heap(&cons2);
	test(sxfixed_cons(cons1, -1) == sxfixed_cons(cons2, -1), "sxfixed_cons1");
	SetCons(cons1, T, Nil);
	test(sxfixed_cons(cons1, -1) != sxfixed_cons(cons2, -1), "sxfixed_cons2");
	test(sxfixed_cons(cons1, 0) == sxfixed_cons(cons2, 0), "sxfixed_cons3");
	test(sxfixed_cons(cons1, 1) != sxfixed_cons(cons2, 1), "sxfixed_cons4");

	fixnum_heap(&v1, 100);
	fixnum_heap(&v2, 200);
	SetCons(cons1, Nil, v1);
	SetCons(cons2, Nil, v2);
	test(sxfixed_cons(cons1, -1) != sxfixed_cons(cons2, -1), "sxfixed_cons5");
	SetStatusValue(v2, LISPSTATUS_READONLY, 0);
	SetFixnum(v2, 100);
	test(sxfixed_cons(cons1, -1) == sxfixed_cons(cons2, -1), "sxfixed_cons6");

	consnil_heap(&cons3);
	consnil_heap(&cons4);
	SetFixnum(v2, 200);
	SetCons(cons1, cons3, Nil);
	SetCons(cons2, cons4, Nil);
	SetCons(cons3, T, v1);
	SetCons(cons4, T, v2);
	test(sxfixed_cons(cons1, -1) != sxfixed_cons(cons2, -1), "sxfixed_cons7");
	test(sxfixed_cons(cons1, 0) == sxfixed_cons(cons2, 0), "sxfixed_cons8");
	test(sxfixed_cons(cons1, 1) == sxfixed_cons(cons2, 1), "sxfixed_cons9");
	test(sxfixed_cons(cons1, 2) != sxfixed_cons(cons2, 2), "sxfixed_cons10");

	SetFixnum(v2, 100);
	test(sxfixed_cons(cons1, -1) == sxfixed_cons(cons2, -1), "sxfixed_cons11");
	test(sxfixed_cons(cons1, 0) == sxfixed_cons(cons2, 0), "sxfixed_cons12");
	test(sxfixed_cons(cons1, 1) == sxfixed_cons(cons2, 1), "sxfixed_cons13");
	test(sxfixed_cons(cons1, 2) == sxfixed_cons(cons2, 2), "sxfixed_cons14");

	RETURN;
}

static int test_sxfixed_vector(void)
{
	addr pos;

	vector2_heap(&pos, 100);
	test(sxfixed_vector(pos, -1) == 100, "sxfixed_vector1");

	RETURN;
}

static int test_sxfixed_character(void)
{
	addr pos;

	character_heap(&pos, 'a');
	test(sxfixed_character(pos, -1) == 'a', "sxfixed_character1");
	character_heap(&pos, 10000);
	test(sxfixed_character(pos, -1) == 10000, "sxfixed_character2");

	RETURN;
}

static int test_sxfixed_string(void)
{
	addr pos;
	fixed v1, v2;

	strvect_char_heap(&pos, "");
	v1 = sxfixed_string(pos, -1);
	strvect_char_heap(&pos, "");
	v2 = sxfixed_string(pos, -1);
	test(v1 == v2, "sxfixed_string1");

	strvect_char_heap(&pos, "a");
	v2 = sxfixed_string(pos, -1);
	test(v1 != v2, "sxfixed_string2");

	strvect_char_heap(&pos, "a");
	v1 = sxfixed_string(pos, -1);
	test(v1 == v2, "sxfixed_string3");

	strvect_char_heap(&pos, "A");
	v2 = sxfixed_string(pos, -1);
	test(v1 != v2, "sxfixed_string4");

	strvect_char_heap(&pos, "HelloAAAABBB");
	v1 = sxfixed_string(pos, -1);
	test(v1 != v2, "sxfixed_string5");

	strvect_char_heap(&pos, "zzzyyy");
	v2 = sxfixed_string(pos, -1);
	test(v1 != v2, "sxfixed_string6");

	strvect_char_heap(&pos, "HelloAAAABBBB");
	v2 = sxfixed_string(pos, -1);
	test(v1 != v2, "sxfixed_string7");

	strvect_char_heap(&pos, "HelloAAAABBB");
	v2 = sxfixed_string(pos, -1);
	test(v1 == v2, "sxfixed_string8");

	RETURN;
}

static int test_sxfixed_fixnum(void)
{
	addr pos;
	fixed v1, v2;

	fixnum_heap(&pos, 100);
	test(sxfixed_fixnum(pos, -1) == 100, "sxfixed_fixnum1");
	fixnum_heap(&pos, -10);
	v1 = sxfixed_fixnum(pos, -1);
	fixnum_heap(&pos, -11);
	v2 = sxfixed_fixnum(pos, -1);
	test(v1 != v2, "sxfixed_fixnum2");

	RETURN;
}

static int testbreak_sxhash(void)
{
	TestBreak(test_sxfixed_eq);
	TestBreak(test_sxfixed_cons);
	TestBreak(test_sxfixed_vector);
	TestBreak(test_sxfixed_character);
	TestBreak(test_sxfixed_string);
	TestBreak(test_sxfixed_fixnum);

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
		build_character();
		lisp_initialize = 1;
		result = testbreak_sxhash();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

