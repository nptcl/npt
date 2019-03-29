#include "clos_method.c"
#include "character.h"
#include "common.h"
#include "degrade.h"
#include "execute.h"
#include "readlite.h"
#include "real.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "syscall.h"
#include "symbol.h"
#include "type.h"

/*
 *  access
 */
static int test_stdget_call(addr pos,
		constindex name,
		void (*set)(addr, addr),
		void (*get)(addr, addr *))
{
	addr check, k, v;

	v = readr("aaa");
	GetConstant(name, &k);
	clos_set(pos, k, v);
	(*get)(pos, &check);
	if (check != v)
		return 0;

	(*set)(pos, T);
	clos_get(pos, k, &check);
	if (check != T)
		return 0;

	return 1;
}

#define CheckStdGetMethod(a,b,c) { \
	test(test_stdget_call((a), CONSTANT_CLOSKEY_##b, \
				stdset_method_##c, stdget_method_##c), "method_" #c); \
}

static int test_stdget_method(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_METHOD, &pos);
	clos_instance_heap(pos, &pos);

	CheckStdGetMethod(pos, FUNCTION, function);
	CheckStdGetMethod(pos, GENERIC_FUNCTION, generic_function);
	CheckStdGetMethod(pos, LAMBDA_LIST, lambda_list);
	CheckStdGetMethod(pos, QUALIFIERS, qualifiers);
	CheckStdGetMethod(pos, SPECIALIZERS, specializers);

	RETURN;
}


/*
 *  defmethod
 */
static int test_method_instance_heap(void)
{
	addr instance, clos;

	method_instance_heap(&instance, Nil, Nil, Nil, Nil, Nil);
	GetConst(CLOS_STANDARD_METHOD, &clos);
	test(clos_subtype_p(instance, clos), "method_instance_heap1");

	RETURN;
}

static int test_method_instance_call(void)
{
	addr instance, clos;

	method_instance_call(NULL, &instance, Nil, T);
	GetConst(CLOS_STANDARD_METHOD, &clos);
	test(clos_subtype_p(instance, clos), "method_instance_call1");
	stdget_method_function(instance, &clos);
	test(clos == T, "method_instance_call2");

	RETURN;
}

static void argument_method_char(addr *ret, const char *str)
{
	argument_method_heap(Local_Thread, ret, readr(str));
}

static int test_method_specializer_list(void)
{
	addr lambda, left, right;

	argument_method_char(&lambda, "(a (b integer))");
	method_specializer_list(&lambda, lambda);
	GetCons(lambda, &left, &lambda);
	GetConst(CLOS_T, &right);
	test(left == right, "method_specializer_list1");
	GetCons(lambda, &left, &lambda);
	GetConst(CLOS_INTEGER, &right);
	test(left == right, "method_specializer_list2");
	test(lambda == Nil, "method_specializer_list3");

	argument_method_char(&lambda, "((a (eql hello)) b)");
	method_specializer_list(&lambda, lambda);
	GetCons(lambda, &left, &lambda);
	right = readr("hello");
	clos_find_specializer(right, &right);
	test(left == right, "method_specializer_list4");
	GetCons(lambda, &left, &lambda);
	GetConst(CLOS_T, &right);
	test(left == right, "method_specializer_list5");
	test(lambda == Nil, "method_specializer_list6");

	RETURN;
}

static int test_method_instance_lambda(void)
{
	addr lambda, check;
	LocalRoot local;

	local = Local_Thread;
	lambda = readr("(a (b integer))");
	method_instance_lambda(local, &lambda, Nil, lambda);

	stdget_method_lambda_list(lambda, &check);
	test(check != Nil, "method_instance_lambda1");
	stdget_method_specializers(lambda, &check);
	test(check != Nil, "method_instance_lambda2");

	RETURN;
}


/*
 *  add-method
 */
static int test_method_check_method_class(void)
{
	addr generic, method;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	GetConst(CLOS_STANDARD_METHOD, &method);
	stdset_generic_method_class(generic, method);
	method_instance_call(NULL, &method, Nil, Nil);
	method_check_method_class(generic, method);
	test(1, "method_check_method_class1");

	RETURN;
}

static int test_method_check_method_qualifiers(void)
{
	addr generic, method, combination, cons;

	/* generic */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	GetConst(COMMON_STANDARD, &combination);
	clos_find_combination(combination, &combination);
	stdset_generic_method_combination(generic, combination);
	/* method */
	method_instance_call(NULL, &method, Nil, Nil);
	GetConst(KEYWORD_AFTER, &cons);
	conscar_heap(&cons, cons);
	stdset_method_qualifiers(method, cons);
	/* check */
	method_check_method_qualifiers(Execute_Thread, generic, method);
	test(1, "method_check_method_qualifiers1");

	RETURN;
}

static addr test_lambda_key(addr lambda)
{
	GetArgument(lambda, ArgumentIndex_key, &lambda);
	return lambda;
}

static int test_null_set_diffkey(addr x, addr y)
{
	return method_null_set_difference(test_lambda_key(x), test_lambda_key(y));
}

static void argument_generic_char(addr *ret, const char *str)
{
	argument_generic_heap(Local_Thread, ret, readr(str));
}

static int test_method_null_set_difference(void)
{
	addr generic, method;

	test(method_null_set_difference(Nil, T), "method_null_set_difference1");

	argument_generic_char(&generic, "(a b &key c)");
	argument_method_char(&method, "(b &key c d e)");
	test(test_null_set_diffkey(generic, method), "method_null_set_difference2");

	argument_generic_char(&generic, "(a b &key c d e)");
	argument_method_char(&method, "(b &key c)");
	test(! test_null_set_diffkey(generic, method), "method_null_set_difference3");

	argument_generic_char(&generic, "(&key c)");
	argument_method_char(&method, "(&key c d)");
	test(test_null_set_diffkey(generic, method), "method_null_set_difference4");

	argument_generic_char(&generic, "(&key c ((:hello d)))");
	argument_method_char(&method, "(&key c hello)");
	test(test_null_set_diffkey(generic, method), "method_null_set_difference5");

	argument_generic_char(&generic, "(&key d)");
	argument_method_char(&method, "(&key d)");
	test(test_null_set_diffkey(generic, method), "method_null_set_difference6");

	RETURN;
}

static int test_method_arguments_check1(void)
{
	addr x, y;

	argument_generic_char(&x, "(a b c &optional d e)");
	argument_method_char(&y, "(x y z &key w)");
	method_arguments_check1(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check1");

	RETURN;
}

static int test_method_arguments_check2(void)
{
	addr x, y;

	argument_generic_char(&x, "(&optional d e)");
	argument_method_char(&y, "(x y z &optional g (h 10) &key w)");
	method_arguments_check2(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check2");

	RETURN;
}

static int test_method_arguments_check3(void)
{
	addr x, y;

	argument_generic_char(&x, "(&key d e)");
	argument_method_char(&y, "(x y z &key)");
	method_arguments_check3(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check3-1");

	argument_generic_char(&x, "(d e)");
	argument_method_char(&y, "(x &optional y z)");
	method_arguments_check3(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check3-2");

	argument_generic_char(&x, "(d &rest e)");
	argument_method_char(&y, "(x &key y z)");
	method_arguments_check3(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check3-3");

	argument_generic_char(&x, "(a b &key)");
	argument_method_char(&y, "(c &key)");
	method_arguments_check3(ArgumentStruct(x), ArgumentStruct(y));
	test(1, "method_arguments_check3-4");

	RETURN;
}

static void test_method_arguments_check4_args(const char *left, const char *right)
{
	addr x, y;

	argument_generic_char(&x, left);
	argument_method_char(&y, right);
	method_arguments_check4(x, ArgumentStruct(x), y, ArgumentStruct(y));
}

static int test_method_arguments_check4(void)
{
	test_method_arguments_check4_args("(a)", "(b)");
	test(1, "method_arguments_check4-1");
	test_method_arguments_check4_args("(a)", "(z &key a b c)");
	test(1, "method_arguments_check4-2");
	test_method_arguments_check4_args("(a &key b)", "(z &rest args)");
	test(1, "method_arguments_check4-3");
	test_method_arguments_check4_args("(a &key b)", "(z &key &allow-other-keys)");
	test(1, "method_arguments_check4-4");
	test_method_arguments_check4_args("(a &key b)", "(z &key a b c)");
	test(1, "method_arguments_check4-5");
	RETURN;
}

static void test_method_arguments(const char *left, const char *right)
{
	addr generic, method, lambda;

	/* generic */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	argument_generic_char(&lambda, left);
	stdset_generic_lambda_list(generic, lambda);
	/* method */
	method_instance_call(NULL, &method, Nil, Nil);
	argument_method_char(&lambda, right);
	stdset_method_lambda_list(method, lambda);
	/* check */
	method_check_method_arguments(generic, method);
}

static int test_method_check_method_arguments(void)
{
	test_method_arguments("(a)", "(b)");
	test(1, "method_check_method_arguments1-1");
	test_method_arguments("(a &key x y)", "(b &key x z y)");
	test(1, "method_check_method_arguments1-2");

	RETURN;
}

static int test_method_check_method_arguments_keys(void)
{
	test_method_arguments("(&key)", "(&key)");
	test(1, "method_check_method_arguments2-1");
	test_method_arguments("(&key)", "(&key a b c)");
	test(1, "method_check_method_arguments2-2");
	test_method_arguments("(a b &key c)", "(b z &key c d e)");
	test(1, "method_check_method_arguments2-3");

	RETURN;
}

static int test_method_eqlcheck(void)
{
	addr method, check;
	LocalRoot local;

	local = Local_Thread;
	method = readr("((a (eql hello)) b (c integer))");
	method_instance_lambda(local, &method, Nil, method);
	method_eqlcheck(method, &method);

	GetCons(method, &check, &method);
	test(check == T, "method_eqlcheck1");
	GetCons(method, &check, &method);
	test(check == Nil, "method_eqlcheck2");
	GetCons(method, &check, &method);
	test(check == Nil, "method_eqlcheck3");
	test(method == Nil, "method_eqlcheck4");

	RETURN;
}

static int test_method_update_eqlcheck(void)
{
	addr generic, method, key, cons, cache, value;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	method_instance_call(NULL, &method, Nil, Nil);
	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	stdset_generic_cache(generic, cache);

	list_heap(&cons, Nil, T, Nil, NULL);
	stdset_generic_eqlcheck(generic, cons);
	GetConst(CLOS_INTEGER, &cons);
	list_heap(&key, cons, cons, cons, NULL);
	stdset_method_specializers(method, key);

	stdget_generic_cache(generic, &cache);
	intern_hashheap(cache, key, &value);
	method_update_eqlcheck(generic, method, 1);
	test(findvalue_hashtable(cache, key, &value) == 0, "method_update_eqlcheck1");

	stdget_generic_eqlcheck(generic, &cons);
	GetCons(cons, &value, &cons);
	test(value == Nil, "method_update_eqlcheck2");
	GetCons(cons, &value, &cons);
	test(value == T, "method_update_eqlcheck3");
	GetCons(cons, &value, &cons);
	test(value == Nil, "method_update_eqlcheck4");
	test(cons == Nil, "method_update_eqlcheck5");

	fixnum_heap(&value, 100);
	clos_intern_specializer(value, &value);
	GetConst(CLOS_INTEGER, &cons);
	list_heap(&cons, cons, cons, value, NULL);
	stdset_method_specializers(method, cons);
	method_update_eqlcheck(generic, method, 1);
	test(findvalue_hashtable(cache, key, &value) != 0, "method_update_eqlcheck6");

	stdget_generic_eqlcheck(generic, &cons);
	GetCons(cons, &value, &cons);
	test(value == Nil, "method_update_eqlcheck7");
	GetCons(cons, &value, &cons);
	test(value == T, "method_update_eqlcheck8");
	GetCons(cons, &value, &cons);
	test(value == T, "method_update_eqlcheck9");
	test(cons == Nil, "method_update_eqlcheck10");

	RETURN;
}

static int test_method_update_check(void)
{
	addr generic, method, cons, cache;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	method_instance_call(NULL, &method, Nil, Nil);
	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	stdset_generic_cache(generic, cache);

	GetConst(CLOS_INTEGER, &cons);
	list_heap(&cons, cons, cons, cons, NULL);
	stdset_method_specializers(method, cons);

	test(! stdboundp_generic_eqlcheck(generic), "method_update_check1");
	method_update_check(generic, method, 1);
	test(stdboundp_generic_eqlcheck(generic), "method_update_check2");

	method_update_check(generic, method, 1);
	test(stdboundp_generic_eqlcheck(generic), "method_update_check3");

	RETURN;
}

static int test_method_push_generic(void)
{
	addr generic, combination, array, method, check;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	GetConst(COMMON_STANDARD, &combination);
	clos_find_combination(combination, &combination);
	stdset_generic_method_combination(generic, combination);
	vector4_heap(&array, 4);
	stdset_generic_methods(generic, array);

	method_instance_call(NULL, &method, Nil, Nil);
	stdset_method_qualifiers(method, Nil);
	method_push_generic(ptr, generic, method);
	stdget_generic_methods(generic, &array);
	GetArrayA4(array, 2, &array);
	test(array != Nil, "method_push_generic1");
	GetCons(array, &check, &array);
	test(check == method, "method_push_generic2");
	test(array == Nil, "method_push_generic3");

	RETURN;
}

static int test_method_cache_check(void)
{
	addr eqlcheck, args, keys, fixnum, integer, real;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&eqlcheck, Nil, Nil, Nil, NULL);
	list_heap(&args, integer, integer, integer, NULL);
	list_heap(&keys, integer, integer, integer, NULL);
	test(method_cache_check(eqlcheck, args, keys), "method_cache_check1");

	list_heap(&args, integer, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! method_cache_check(eqlcheck, args, keys), "method_cache_check2");

	list_heap(&args, real, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! method_cache_check(eqlcheck, args, keys), "method_cache_check3");

	list_heap(&args, real, integer, integer, NULL);
	list_heap(&keys, integer, fixnum, fixnum, NULL);
	test(method_cache_check(eqlcheck, args, keys), "method_cache_check4");

	list_heap(&eqlcheck, T, T, T, NULL);
	list_heap(&args, integer, integer, integer, NULL);
	list_heap(&keys, integer, integer, integer, NULL);
	test(method_cache_check(eqlcheck, args, keys), "method_cache_check5");

	list_heap(&args, integer, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! method_cache_check(eqlcheck, args, keys), "method_cache_check6");

	list_heap(&args, real, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! method_cache_check(eqlcheck, args, keys), "method_cache_check7");

	list_heap(&args, real, integer, integer, NULL);
	list_heap(&keys, integer, fixnum, fixnum, NULL);
	test(method_cache_check(eqlcheck, args, keys), "method_cache_check8");

	RETURN;
}

static int test_method_cache_remove(void)
{
	addr generic, method, key, cons, cache, fixnum, integer, real, value;
	LocalRoot local;

	local = Local_Thread;
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	method_cache_remove(local, generic, NULL);
	test(1, "method_cache_remove1");

	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	stdset_generic_cache(generic, cache);

	list_heap(&cons, T, Nil, NULL);
	stdset_generic_eqlcheck(generic, cons);

	method_instance_call(NULL, &method, Nil, Nil);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, real, real, NULL);
	intern_hashheap(cache, key, &value);
	stdset_method_specializers(method, cons);
	method_cache_remove(local, generic, method);
	test(findvalue_hashtable(cache, key, &value), "method_cache_remove2");

	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, fixnum, fixnum, NULL);
	intern_hashheap(cache, key, &value);
	stdset_method_specializers(method, cons);
	method_cache_remove(local, generic, method);
	test(findvalue_hashtable(cache, key, &value) == 0, "method_cache_remove3");

	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, real, fixnum, NULL);
	intern_hashheap(cache, key, &value);
	stdset_method_specializers(method, cons);
	method_cache_remove(local, generic, method);
	test(findvalue_hashtable(cache, key, &value) == 0, "method_cache_remove4");

	RETURN;
}

static void test_generic_array(addr *ret, addr *array)
{
	addr generic, pos;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	clos_instance_heap(generic, &generic);
	GetConst(COMMON_STANDARD, &pos);
	clos_find_combination(pos, &pos);
	stdset_generic_method_combination(generic, pos);
	vector4_heap(array, 4);
	stdset_generic_methods(generic, *array);
	*ret = generic;
}

static int test_method_find_method_nil(void)
{
	addr generic, method, pos, fixnum, integer, array, cons, qua;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	method_instance_call(NULL, &method, Nil, Nil);
	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	stdset_method_qualifiers(method, qua);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	stdset_method_specializers(method, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);
	/* check */
	ptr = Execute_Thread;
	list_heap(&pos, T, T, NULL);
	method_find_method_nil(ptr, generic, pos, T, &pos);
	test(pos == Nil, "method_find_method_nil1");
	method_find_method_nil(ptr, generic, Nil, T, &pos);
	test(pos == Nil, "method_find_method_nil2");
	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	list_heap(&cons, fixnum, fixnum, NULL);
	method_find_method_nil(ptr, generic, qua, cons, &pos);
	test(pos == Nil, "method_find_method_nil3");
	list_heap(&cons, fixnum, integer, NULL);
	method_find_method_nil(ptr, generic, qua, cons, &pos);
	test(pos == method, "method_find_method_nil4");

	RETURN;
}

static int test_method_find_method(void)
{
	addr generic, method, pos, fixnum, integer, array, cons, qua;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	method_instance_call(NULL, &method, Nil, Nil);
	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	stdset_method_qualifiers(method, qua);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	stdset_method_specializers(method, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);
	/* check */
	ptr = Execute_Thread;
	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	list_heap(&cons, fixnum, integer, NULL);
	method_find_method(ptr, generic, qua, cons, &pos);
	test(pos == method, "method_find_method1");

	RETURN;
}

static int test_method_remove_method_execute(void)
{
	addr generic, method, pos, fixnum, integer, array, qua;
	addr temp;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	method_instance_call(NULL, &method, Nil, Nil);
	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	stdset_method_qualifiers(method, qua);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	stdset_method_specializers(method, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);

	/* check */
	ptr = Execute_Thread;
	method_instance_call(NULL, &temp, Nil, Nil);
	GetConst(KEYWORD_AFTER, &qua);
	list_heap(&qua, qua, NULL);
	stdset_method_qualifiers(temp, qua);
	test(! method_remove_method_execute(ptr, generic, temp),
			"method_remove_method_execute1");

	GetConst(KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	stdset_method_qualifiers(temp, qua);
	test(! method_remove_method_execute(ptr, generic, temp),
			"method_remove_method_execute2");

	test(method_remove_method_execute(ptr, generic, method),
			"method_remove_method_execute3");
	GetArrayA4(array, 1, &pos);
	test(pos == Nil, "method_remove_method_execute4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_method(void)
{
	/* access */
	TestBreak(test_stdget_method);
	/* defmethod */
	TestBreak(test_method_instance_heap);
	TestBreak(test_method_instance_call);
	TestBreak(test_method_specializer_list);
	TestBreak(test_method_instance_lambda);
	/* add-method */
	TestBreak(test_method_check_method_class);
	TestBreak(test_method_check_method_qualifiers);
	TestBreak(test_method_null_set_difference);
	TestBreak(test_method_arguments_check1);
	TestBreak(test_method_arguments_check2);
	TestBreak(test_method_arguments_check3);
	TestBreak(test_method_arguments_check4);
	TestBreak(test_method_check_method_arguments);
	TestBreak(test_method_check_method_arguments_keys);
	TestBreak(test_method_eqlcheck);
	TestBreak(test_method_update_eqlcheck);
	TestBreak(test_method_update_check);
	TestBreak(test_method_push_generic);
	TestBreak(test_method_cache_check);
	TestBreak(test_method_cache_remove);
	TestBreak(test_method_find_method_nil);
	TestBreak(test_method_find_method);
	TestBreak(test_method_remove_method_execute);

	return 0;
}

int test_clos_method(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
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
		build_readtable();
		lisp_init = 1;
		result = testbreak_clos_method();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

