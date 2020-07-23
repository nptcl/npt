#include "clos_type.c"
#include "bignum_data.h"
#include "bignum_object.h"
#include "character.h"
#include "clos.h"
#include "code_object.h"
#include "common.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"
#include "reader.h"
#include "real.h"
#include "stream.h"
#include "stream_broadcast.h"
#include "strtype.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"

static int test_clos_class_of(void)
{
	addr x, y;

	clos_class_of(Nil, &x);
	GetConst(CLOS_NULL, &y);
	test(x == y, "clos_class_of-nil");

	clos_class_of(T, &x);
	GetConst(CLOS_SYMBOL, &y);
	test(x == y, "clos_class_of-t");

	clos_class_of(readr("(a b c)"), &x);
	GetConst(CLOS_CONS, &y);
	test(x == y, "clos_class_of-cons");

	clos_class_of(readr("#1a(a b c)"), &x);
	GetConst(CLOS_SIMPLE_VECTOR, &y);
	test(x == y, "clos_class_of-array1");

	clos_class_of(readr("#2a((a b c) (c d e))"), &x);
	GetConst(CLOS_SIMPLE_ARRAY, &y);
	test(x == y, "clos_class_of-array2");

	clos_class_of(readr("#(a b c)"), &x);
	GetConst(CLOS_SIMPLE_VECTOR, &y);
	test(x == y, "clos_class_of-vector");

	clos_class_of(readr("#\\a"), &x);
	GetConst(CLOS_CHARACTER, &y);
	test(x == y, "clos_class_of-character");

	clos_class_of(readr("\"Hello\""), &x);
	GetConst(CLOS_SIMPLE_BASE_STRING, &y);
	test(x == y, "clos_class_of-string");

	hashtable_heap(&x);
	clos_class_of(x, &x);
	GetConst(CLOS_HASH_TABLE, &y);
	test(x == y, "clos_class_of-hash-table");

	GetConst(SPECIAL_READTABLE, &x);
	GetValueSymbol(x, &x);
	clos_class_of(x, &x);
	GetConst(CLOS_READTABLE, &y);
	test(x == y, "clos_class_of-readtable");

	clos_class_of(readr("hello"), &x);
	GetConst(CLOS_SYMBOL, &y);
	test(x == y, "clos_class_of-symbol");

	clos_class_of(readr(":hello"), &x);
	GetConst(CLOS_KEYWORD, &y);
	test(x == y, "clos_class_of-keyword");

	fixnum_heap(&x, 10);
	clos_class_of(x, &x);
	GetConst(CLOS_FIXNUM, &y);
	test(x == y, "clos_class_of-fixnum");

	bignum_value_heap(&x, SignPlus, 10);
	clos_class_of(x, &x);
	GetConst(CLOS_BIGNUM, &y);
	test(x == y, "clos_class_of-bignum");

	clos_class_of(readr("2/3"), &x);
	GetConst(CLOS_RATIO, &y);
	test(x == y, "clos_class_of-ratio");

	clos_class_of(readr("1.23f0"), &x);
	GetConst(CLOS_SINGLE_FLOAT, &y);
	test(x == y, "clos_class_of-single-float");

	clos_class_of(readr("1.23d0"), &x);
	GetConst(CLOS_DOUBLE_FLOAT, &y);
	test(x == y, "clos_class_of-double-float");

	clos_class_of(readr("1.23L0"), &x);
	GetConst(CLOS_LONG_FLOAT, &y);
	test(x == y, "clos_class_of-long-float");

	clos_class_of(readr("#c(1 2)"), &x);
	GetConst(CLOS_COMPLEX, &y);
	test(x == y, "clos_class_of-complex");

	function_empty_heap(&x, readr("hello"));
	clos_class_of(x, &x);
	GetConst(CLOS_FUNCTION, &y);
	test(x == y, "clos_class_of-function");

	GetConst(COMMON_CAR, &x);
	GetFunctionSymbol(x, &x);
	clos_class_of(x, &x);
	GetConst(CLOS_COMPILED_FUNCTION, &y);
	test(x == y, "clos_class_of-compiled-function");

	GetConst(SPECIAL_PACKAGE, &x);
	GetValueSymbol(x, &x);
	clos_class_of(x, &x);
	GetConst(CLOS_PACKAGE, &y);
	test(x == y, "clos_class_of-package");

	GetConst(SPECIAL_RANDOM_STATE, &x);
	GetValueSymbol(x, &x);
	clos_class_of(x, &x);
	GetConst(CLOS_RANDOM_STATE, &y);
	test(x == y, "clos_class_of-random-state");

	x = readr("#p\"Hello\"");
	clos_class_of(x, &x);
	GetConst(CLOS_PATHNAME, &y);
	test(x == y, "clos_class_of-pathname");

	x = readr("#p\"Hello\"");
	SetLogicalPathname(x, 1);
	clos_class_of(x, &x);
	GetConst(CLOS_LOGICAL_PATHNAME, &y);
	test(x == y, "clos_class_of-logical-pathname");

	open_broadcast_stream(&x, Nil);
	clos_class_of(x, &x);
	GetConst(CLOS_BROADCAST_STREAM, &y);
	test(x == y, "clos_class_of-broadcast-stream");

	clos_class_of(readr("#*11011"), &x);
	GetConst(CLOS_SIMPLE_BIT_VECTOR, &y);
	test(x == y, "clos_class_of-bit-vector");

	RETURN;
}

static int test_clos_intern_specializer(void)
{
	addr value, check, check2;

	clos_forget_all_specializer_unsafe();

	fixnum_heap(&value, 100);
	clos_intern_specializer_(value, &check);
	test(closp(check), "clos_intern_specializer1");
	clos_intern_specializer_(value, &check2);
	test(check == check2, "clos_intern_specializer2");

	fixnum_heap(&value, 101);
	clos_intern_specializer_(value, &check2);
	test(check != check2, "clos_intern_specializer3");

	character_heap(&value, 100);
	clos_intern_specializer_(value, &check);
	character_heap(&value, 100);
	clos_intern_specializer_(value, &check2);
	test(check == check2, "clos_intern_specializer4");

	character_heap(&value, 101);
	clos_intern_specializer_(value, &check2);
	test(check != check2, "clos_intern_specializer5");

	fixnum_heap(&value, 100);
	clos_intern_specializer_(value, &check);
	test(clos_specializer_p(check), "eql_specializer_p1");
	GetConst(CLOS_STANDARD_CLASS, &check);
	test(! clos_specializer_p(check), "eql_specializer_p2");

	clos_forget_all_specializer_unsafe();

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_type(void)
{
	TestBreak(test_clos_class_of);
	TestBreak(test_clos_intern_specializer);

	return 0;
}

int test_clos_type(void)
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
		build_character();
		build_real();
		build_package();
		build_stream();
		build_symbol();
		build_clos(ptr);
		build_condition(ptr);
		build_type();
		build_syscall();
		build_common();
		build_reader();
		lisp_initialize = 1;
		result = testbreak_clos_type();
	}
	end_setjmp(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

