#include "clos_method.c"
#include "degrade.h"
#include "execute.h"
#include "readlite.h"
#include "package.h"
#include "readtable.h"

static int test_make_instance_standard_method(void)
{
	addr instance, clos;

	make_instance_standard_method(NULL, &instance, Nil, Nil, Nil, Nil, Nil, Nil);
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &clos);
	test(std_subtype_p(instance, clos), "make_instance_standard_method1");

	RETURN;
}

static int test_make_instance_standard_method_function(void)
{
	addr instance, clos;

	make_instance_standard_method_function(NULL, &instance, Nil, T);
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &clos);
	test(std_subtype_p(instance, clos), "make_instance_standard_method_function1");
	clos_elt(instance, Clos_method_function, &clos);
	test(clos == T, "make_instance_standard_method_function2");

	RETURN;
}

static void import_constant_test(addr package, enum CONSTANT_INDEX index)
{
	addr symbol;
	GetConstant(index, &symbol);
	import_package(package, symbol);
}

static void import_test(void)
{
	addr package;

	find_char_package(LISP_PACKAGE, &package);
	import_package(package, Nil);
	import_package(package, T);
	import_constant_test(package, CONSTANT_COMMON_EQL);
	import_constant_test(package, CONSTANT_COMMON_INTEGER);
	import_constant_test(package, CONSTANT_AMPERSAND_OPTIONAL);
	import_constant_test(package, CONSTANT_AMPERSAND_REST);
	import_constant_test(package, CONSTANT_AMPERSAND_KEY);
	import_constant_test(package, CONSTANT_AMPERSAND_ALLOW);
	import_constant_test(package, CONSTANT_AMPERSAND_AUX);
}

static void readlite_heaptest(addr *ret, const char *str)
{
	readlite_package_heap(ret, LISP_PACKAGE, str);
}

static void lambda_specialized_test(addr *ret, const char *str)
{
	addr lambda;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	import_test();
	readlite_heaptest(&lambda, str);
	lambda_specialized(local, ret, lambda);
	rollback_local(local, stack);
}

static void lambda_generic_function_test(addr *ret, const char *str)
{
	addr lambda;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	import_test();
	readlite_heaptest(&lambda, str);
	lambda_generic_function(local, ret, lambda);
	rollback_local(local, stack);
}

static int test_specializer_list(void)
{
	addr lambda, left, right;

	lambda_specialized_test(&lambda, "(a (b integer))");
	specializer_list(&lambda, lambda);
	GetCons(lambda, &left, &lambda);
	GetConstant(CONSTANT_CLOS_T, &right);
	test(left == right, "specializer_list1");
	GetCons(lambda, &left, &lambda);
	GetConstant(CONSTANT_CLOS_INTEGER, &right);
	test(left == right, "specializer_list2");
	test(lambda == Nil, "specializer_list3");

	lambda_specialized_test(&lambda, "((a (eql hello)) b)");
	specializer_list(&lambda, lambda);
	GetCons(lambda, &left, &lambda);
	internchar(LISP_PACKAGE, "HELLO", &right);
	right = find_eql_specializer(right);
	test(left == right, "specializer_list4");
	GetCons(lambda, &left, &lambda);
	GetConstant(CONSTANT_CLOS_T, &right);
	test(left == right, "specializer_list5");
	test(lambda == Nil, "specializer_list6");

	RETURN;
}

static int test_make_instance_standard_method_lambda(void)
{
	addr lambda, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	import_test();
	readlite_heaptest(&lambda, "(a (b integer))");
	make_instance_standard_method_lambda(local, &lambda, Nil, lambda);
	rollback_local(local, stack);

	clos_elt(lambda, Clos_method_lambda_list, &check);
	test(check != Nil, "make_instance_standard_method_lambda1");
	clos_elt(lambda, Clos_method_lambda_parse, &check);
	test(check != Nil, "make_instance_standard_method_lambda2");
	clos_elt(lambda, Clos_method_specializers, &check);
	test(check != Nil, "make_instance_standard_method_lambda3");

	RETURN;
}


/*
 *  add-method
 */
static int test_check_method_class(void)
{
	addr generic, method;

	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &method);
	setf_clos_elt(generic, Clos_generic_method_class, method);
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	check_method_class(generic, method);
	test(1, "check_method_class1");

	RETURN;
}

static int test_check_method_qualifiers(void)
{
	addr generic, method, combination, cons;

	/* generic */
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	GetConstant(CONSTANT_COMMON_STANDARD, &combination);
	combination = find_method_combination(combination);
	setf_clos_elt(generic, Clos_generic_method_combination, combination);
	/* method */
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	GetConstant(CONSTANT_KEYWORD_AFTER, &cons);
	conscar_heap(&cons, cons);
	setf_clos_elt(method, Clos_method_qualifiers, cons);
	/* check */
	check_method_qualifiers(Execute_Thread, generic, method);
	test(1, "check_method_qualifiers1");

	RETURN;
}

static int test_list_length_compare(void)
{
	addr left, right;

	test(list_length_compare(Nil, Nil) == 0, "list_length_compare1");
	test(list_length_compare(T, Nil) > 0, "list_length_compare2");
	test(list_length_compare(Nil, T) < 0, "list_length_compare3");

	list_heap(&left, T, T, Nil, NULL);
	list_heap(&right, T, T, NULL);
	test(list_length_compare(left, right) > 0, "list_length_compare4");
	test(list_length_compare(right, left) < 0, "list_length_compare5");
	list_heap(&left, Nil, T, Nil, NULL);
	list_heap(&right, Nil, T, T, NULL);
	test(list_length_compare(left, left) == 0, "list_length_compare6");

	RETURN;
}

static addr test_lambda_key(addr lambda)
{
	/* (var opt rest key . _) */
	GetCdr(lambda, &lambda);
	GetCdr(lambda, &lambda);
	GetCdr(lambda, &lambda);
	GetCar(lambda, &lambda);
	return lambda;
}

static int test_null_set_diffkey(addr left, addr right)
{
	return null_set_difference(test_lambda_key(left), test_lambda_key(right));
}

static int test_null_set_difference(void)
{
	addr generic, method;

	test(null_set_difference(Nil, T), "null_set_difference1");

	lambda_generic_function_test(&generic, "(a b &key c)");
	lambda_specialized_test(&method, "(b &key c d e)");
	test(test_null_set_diffkey(generic, method), "null_set_difference2");

	lambda_generic_function_test(&generic, "(a b &key c d e)");
	lambda_specialized_test(&method, "(b &key c)");
	test(! test_null_set_diffkey(generic, method), "null_set_difference3");

	lambda_generic_function_test(&generic, "(&key c)");
	lambda_specialized_test(&method, "(&key c d)");
	test(test_null_set_diffkey(generic, method), "null_set_difference4");

	lambda_generic_function_test(&generic, "(&key c ((:hello d)))");
	lambda_specialized_test(&method, "(&key c hello)");
	test(test_null_set_diffkey(generic, method), "null_set_difference5");

	lambda_generic_function_test(&generic, "(&key d)");
	lambda_specialized_test(&method, "(&key d)");
	test(test_null_set_diffkey(generic, method), "null_set_difference6");

	RETURN;
}

static int test_check_all_keys(void)
{
	addr right, generic, method;

	test(check_all_keys(T, T), "check_all_keys1");
	consnil_heap(&right);
	test(check_all_keys(T, right), "check_all_keys2");
	test(! check_all_keys(right, T), "check_all_keys3");

	lambda_generic_function_test(&generic, "(a b &key c)");
	lambda_specialized_test(&method, "(b &key c d e)");
	test(check_all_keys(test_lambda_key(generic), test_lambda_key(method)),
			"check_all_keys4");

	lambda_generic_function_test(&generic, "(a b &key c d e)");
	lambda_specialized_test(&method, "(b &key c)");
	test(! check_all_keys(test_lambda_key(generic), test_lambda_key(method)),
			"check_all_keys5");

	RETURN;
}

static int test_arguments_check1(void)
{
	addr left, right;

	list_heap(&left, Nil, Nil, T, NULL);
	list_heap(&right, Nil, T, Nil, NULL);
	arguments_check1(left, right);
	test(1, "arguments_check1");

	RETURN;
}

static int test_arguments_check2(void)
{
	addr left, right;

	list_heap(&left, Nil, Nil, T, NULL);
	list_heap(&right, Nil, T, Nil, NULL);
	arguments_check1(left, right);
	test(1, "arguments_check2");

	RETURN;
}

static int test_arguments_check3(void)
{
	arguments_check3(Nil, T, Nil, T);
	test(1, "arguments_check3-1");
	arguments_check3(Nil, Nil, Nil, Nil);
	test(1, "arguments_check3-2");
	arguments_check3(T, Nil, Nil, T);
	test(1, "arguments_check3-3");

	RETURN;
}

static void test_arguments_check4_args(const char *left, const char *right)
{
	addr key1, rest2, key2, allow2;

	lambda_generic_function_test(&key1, left);
	lambda_specialized_test(&rest2, right);
	GetCdr(key1, &key1);
	GetCdr(key1, &key1);
	GetCdr(key1, &key1);
	GetCar(key1, &key1);
	GetCdr(rest2, &rest2);
	GetCdr(rest2, &rest2);
	GetCdr(rest2, &key2);
	GetCdr(key2, &allow2);
	GetCar(rest2, &rest2);
	GetCar(key2, &key2);
	GetCar(allow2, &allow2);
	arguments_check4(key1, rest2, key2, allow2);
}

static int test_arguments_check4(void)
{
	test_arguments_check4_args("(a)", "(b)");
	test_arguments_check4_args("(a)", "(z &key a b c)");
	test_arguments_check4_args("(a &key b)", "(z &rest args)");
	test_arguments_check4_args("(a &key b)", "(z &key &allow-other-keys)");
	test_arguments_check4_args("(a &key b)", "(z &key a b c)");
	test(1, "arguments_check4");
	RETURN;
}

static void test_method_arguments(const char *left, const char *right)
{
	addr generic, method, lambda;

	/* generic */
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	lambda_generic_function_test(&lambda, left);
	setf_clos_elt(generic, Clos_generic_lambda_list, lambda);
	/* method */
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	lambda_specialized_test(&lambda, right);
	setf_clos_elt(method, Clos_method_lambda_parse, lambda);
	/* check */
	check_method_arguments(generic, method);
}

static int test_check_method_arguments(void)
{
	test_method_arguments("(a)", "(b)");
	test_method_arguments("(a &key x y)", "(b &key x z y)");
	test(1, "check_method_arguments1");

	RETURN;
}

static int test_method_eqlcheck(void)
{
	addr method, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	import_test();
	readlite_heaptest(&method, "((a (eql hello)) b (c integer))");
	make_instance_standard_method_lambda(local, &method, Nil, method);
	rollback_local(local, stack);
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

static int test_update_method_and_cache_eqlcheck(void)
{
	addr generic, method, key, cons, cache, value;

	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	setf_clos_elt(generic, Clos_generic_cache, cache);

	list_heap(&cons, Nil, T, Nil, NULL);
	setf_clos_elt(generic, Clos_generic_eqlcheck, cons);
	GetConstant(CONSTANT_CLOS_INTEGER, &cons);
	list_heap(&key, cons, cons, cons, NULL);
	setf_clos_elt(method, Clos_method_specializers, key);

	clos_elt(generic, Clos_generic_cache, &cache);
	intern_hashheap(cache, key, &value);
	update_method_and_cache_eqlcheck(generic, method);
	test(findvalue_hashtable(cache, key, &value) == 0,
			"update_method_and_cache_eqlcheck1");

	clos_elt(generic, Clos_generic_eqlcheck, &cons);
	GetCons(cons, &value, &cons);
	test(value == Nil, "update_method_and_cache_eqlcheck2");
	GetCons(cons, &value, &cons);
	test(value == T, "update_method_and_cache_eqlcheck3");
	GetCons(cons, &value, &cons);
	test(value == Nil, "update_method_and_cache_eqlcheck4");
	test(cons == Nil, "update_method_and_cache_eqlcheck5");

	fixnum_heap(&value, 100);
	intern_eql_specializer(value, &value);
	GetConstant(CONSTANT_CLOS_INTEGER, &cons);
	list_heap(&cons, cons, cons, value, NULL);
	setf_clos_elt(method, Clos_method_specializers, cons);
	update_method_and_cache_eqlcheck(generic, method);
	test(findvalue_hashtable(cache, key, &value) != 0,
			"update_method_and_cache_eqlcheck6");

	clos_elt(generic, Clos_generic_eqlcheck, &cons);
	GetCons(cons, &value, &cons);
	test(value == Nil, "update_method_and_cache_eqlcheck7");
	GetCons(cons, &value, &cons);
	test(value == T, "update_method_and_cache_eqlcheck8");
	GetCons(cons, &value, &cons);
	test(value == T, "update_method_and_cache_eqlcheck9");
	test(cons == Nil, "update_method_and_cache_eqlcheck10");

	RETURN;
}

static int test_update_method_check(void)
{
	addr generic, method, cons, cache;

	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	setf_clos_elt(generic, Clos_generic_cache, cache);

	GetConstant(CONSTANT_CLOS_INTEGER, &cons);
	list_heap(&cons, cons, cons, cons, NULL);
	setf_clos_elt(method, Clos_method_specializers, cons);

	test(! clos_elt_boundp(generic, Clos_generic_eqlcheck), "update_method_check1");
	update_method_check(generic, method);
	test(clos_elt_boundp(generic, Clos_generic_eqlcheck), "update_method_check2");

	update_method_check(generic, method);
	test(clos_elt_boundp(generic, Clos_generic_eqlcheck), "update_method_check3");

	RETURN;
}

static int test_generic_function_push_method(void)
{
	addr generic, combination, array, method, check;
	Execute ptr;

	ptr = Execute_Thread;
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	GetConstant(CONSTANT_COMMON_STANDARD, &combination);
	combination = find_method_combination(combination);
	setf_clos_elt(generic, Clos_generic_method_combination, combination);
	vector4_heap(&array, 4);
	setf_clos_elt(generic, Clos_generic_methods, array);

	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	setf_clos_elt(method, Clos_method_qualifiers, Nil);
	generic_function_push_method(ptr, generic, method);
	clos_elt(generic, Clos_generic_methods, &array);
	GetArrayA4(array, 2, &array);
	test(array != Nil, "generic_function_push_method1");
	GetCons(array, &check, &array);
	test(check == method, "generic_function_push_method2");
	test(array == Nil, "generic_function_push_method3");

	RETURN;
}

static int test_every_remove_cache_check(void)
{
	addr eqlcheck, args, keys, fixnum, integer, real;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&eqlcheck, Nil, Nil, Nil, NULL);
	list_heap(&args, integer, integer, integer, NULL);
	list_heap(&keys, integer, integer, integer, NULL);
	test(every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check1");

	list_heap(&args, integer, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check2");

	list_heap(&args, real, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check3");

	list_heap(&args, real, integer, integer, NULL);
	list_heap(&keys, integer, fixnum, fixnum, NULL);
	test(every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check4");

	list_heap(&eqlcheck, T, T, T, NULL);
	list_heap(&args, integer, integer, integer, NULL);
	list_heap(&keys, integer, integer, integer, NULL);
	test(every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check5");

	list_heap(&args, integer, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check6");

	list_heap(&args, real, fixnum, integer, NULL);
	list_heap(&keys, integer, integer, real, NULL);
	test(! every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check7");

	list_heap(&args, real, integer, integer, NULL);
	list_heap(&keys, integer, fixnum, fixnum, NULL);
	test(every_remove_cache_check(eqlcheck, args, keys),
			"every_remove_cache_check8");

	RETURN;
}

static int test_generic_function_remove_cache(void)
{
	addr generic, method, key, cons, cache, fixnum, integer, real, value;
	LocalRoot local;

	local = Local_Thread;
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	generic_function_remove_cache(local, generic, NULL);
	test(1, "generic_function_remove_cache1");

	hashtable_full_heap(&cache, HASHTABLE_TEST_CACHE, 8,
			HASHTABLE_REHASH_SIZE_DEFAULT, HASHTABLE_REHASH_THRESHOLD_DEFAULT);
	setf_clos_elt(generic, Clos_generic_cache, cache);

	list_heap(&cons, T, Nil, NULL);
	setf_clos_elt(generic, Clos_generic_eqlcheck, cons);

	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, real, real, NULL);
	intern_hashheap(cache, key, &value);
	setf_clos_elt(method, Clos_method_specializers, cons);
	generic_function_remove_cache(local, generic, method);
	test(findvalue_hashtable(cache, key, &value),
			"generic_function_remove_cache2");

	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, fixnum, fixnum, NULL);
	intern_hashheap(cache, key, &value);
	setf_clos_elt(method, Clos_method_specializers, cons);
	generic_function_remove_cache(local, generic, method);
	test(findvalue_hashtable(cache, key, &value) == 0,
			"generic_function_remove_cache3");

	list_heap(&key, integer, integer, NULL);
	list_heap(&cons, real, fixnum, NULL);
	intern_hashheap(cache, key, &value);
	setf_clos_elt(method, Clos_method_specializers, cons);
	generic_function_remove_cache(local, generic, method);
	test(findvalue_hashtable(cache, key, &value) == 0,
			"generic_function_remove_cache4");

	RETURN;
}

static void test_generic_array(addr *ret, addr *array)
{
	addr generic, pos;

	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &generic);
	make_instance_restrict_heap(generic, &generic);
	GetConstant(CONSTANT_COMMON_STANDARD, &pos);
	pos = find_method_combination(pos);
	setf_clos_elt(generic, Clos_generic_method_combination, pos);
	vector4_heap(array, 4);
	setf_clos_elt(generic, Clos_generic_methods, *array);
	*ret = generic;
}

static int test_std_find_method_nil(void)
{
	addr generic, method, pos, fixnum, integer, array, cons, qua;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	setf_clos_elt(method, Clos_method_qualifiers, qua);
	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	setf_clos_elt(method, Clos_method_specializers, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);
	/* check */
	ptr = Execute_Thread;
	list_heap(&pos, T, T, NULL);
	std_find_method_nil(ptr, generic, pos, T, &pos);
	test(pos == Nil, "std_find_method_nil1");
	std_find_method_nil(ptr, generic, Nil, T, &pos);
	test(pos == Nil, "std_find_method_nil2");
	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	list_heap(&cons, fixnum, fixnum, NULL);
	std_find_method_nil(ptr, generic, qua, cons, &pos);
	test(pos == Nil, "std_find_method_nil3");
	list_heap(&cons, fixnum, integer, NULL);
	std_find_method_nil(ptr, generic, qua, cons, &pos);
	test(pos == method, "std_find_method_nil4");

	RETURN;
}

static int test_std_find_method(void)
{
	addr generic, method, pos, fixnum, integer, array, cons, qua;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	setf_clos_elt(method, Clos_method_qualifiers, qua);
	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	setf_clos_elt(method, Clos_method_specializers, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);
	/* check */
	ptr = Execute_Thread;
	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	list_heap(&cons, fixnum, integer, NULL);
	std_find_method(ptr, generic, qua, cons, &pos);
	test(pos == method, "std_find_method1");

	RETURN;
}

static int test_std_remove_method_execute(void)
{
	addr generic, method, pos, fixnum, integer, array, qua;
	addr temp;
	Execute ptr;

	/* generic */
	test_generic_array(&generic, &array);
	/* method */
	make_instance_standard_method_function(NULL, &method, Nil, Nil);
	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	setf_clos_elt(method, Clos_method_qualifiers, qua);
	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	list_heap(&pos, fixnum, integer, NULL);
	setf_clos_elt(method, Clos_method_specializers, pos);
	list_heap(&pos, method, NULL);
	SetArrayA4(array, 1, pos);

	/* check */
	ptr = Execute_Thread;
	make_instance_standard_method_function(NULL, &temp, Nil, Nil);
	GetConstant(CONSTANT_KEYWORD_AFTER, &qua);
	list_heap(&qua, qua, NULL);
	setf_clos_elt(temp, Clos_method_qualifiers, qua);
	test(! std_remove_method_execute(ptr, generic, temp),
			"std_remove_method_execute1");

	GetConstant(CONSTANT_KEYWORD_BEFORE, &qua);
	list_heap(&qua, qua, NULL);
	setf_clos_elt(temp, Clos_method_qualifiers, qua);
	test(! std_remove_method_execute(ptr, generic, temp),
			"std_remove_method_execute2");

	test(std_remove_method_execute(ptr, generic, method),
			"std_remove_method_execute3");
	GetArrayA4(array, 1, &pos);
	test(pos == Nil, "std_remove_method_execute4");

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_method(void)
{
	/* defmethod */
	TestBreak(test_make_instance_standard_method);
	TestBreak(test_make_instance_standard_method_function);
	TestBreak(test_specializer_list);
	TestBreak(test_make_instance_standard_method_lambda);
	/* add-method */
	TestBreak(test_check_method_class);
	TestBreak(test_check_method_qualifiers);
	TestBreak(test_list_length_compare);
	TestBreak(test_null_set_difference);
	TestBreak(test_check_all_keys);
	TestBreak(test_arguments_check1);
	TestBreak(test_arguments_check2);
	TestBreak(test_arguments_check3);
	TestBreak(test_arguments_check4);
	TestBreak(test_check_method_arguments);
	TestBreak(test_method_eqlcheck);
	TestBreak(test_update_method_and_cache_eqlcheck);
	TestBreak(test_update_method_check);
	TestBreak(test_generic_function_push_method);
	TestBreak(test_every_remove_cache_check);
	TestBreak(test_generic_function_remove_cache);
	TestBreak(test_std_find_method_nil);
	TestBreak(test_std_find_method);
	TestBreak(test_std_remove_method_execute);

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
		build_package();
		build_clos(ptr);
		lisp_init = 1;
		result = testbreak_clos_method();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

