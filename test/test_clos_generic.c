#include "clos_generic.c"
#include "character.h"
#include "common.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"
#include "readtable.h"
#include "real.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
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

#define CheckStdGetGeneric(a,b,c) { \
	test(test_stdget_call((a), CONSTANT_CLOSNAME_##b, \
				stdset_generic_##c, stdget_generic_##c), "generic_" #c); \
}

static int test_stdget_generic(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);

	CheckStdGetGeneric(pos, NAME, name);
	CheckStdGetGeneric(pos, LAMBDA_LIST, lambda_list);
	CheckStdGetGeneric(pos, METHODS, methods);
	CheckStdGetGeneric(pos, METHOD_CLASS, method_class);
	CheckStdGetGeneric(pos, ARGUMENT_PRECEDENCE_ORDER, argument_precedence_order);
	CheckStdGetGeneric(pos, DECLARATIONS, declarations);
	CheckStdGetGeneric(pos, METHOD_COMBINATION, method_combination);
	CheckStdGetGeneric(pos, EQLCHECK, eqlcheck);
	CheckStdGetGeneric(pos, CACHE, cache);
	CheckStdGetGeneric(pos, CALL, call);

	RETURN;
}

static int test_stdboundp_generic(void)
{
	int check;
	addr pos;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);

	check = stdboundp_generic_argument_precedence_order(pos);
	test(! check, "stdboundp_generic1");
	stdset_generic_argument_precedence_order(pos, Nil);
	check = stdboundp_generic_argument_precedence_order(pos);
	test(check, "stdboundp_generic2");

	check = stdboundp_generic_eqlcheck(pos);
	test(! check, "stdboundp_generic3");
	stdset_generic_eqlcheck(pos, Nil);
	check = stdboundp_generic_eqlcheck(pos);
	test(check, "stdboundp_generic4");

	RETURN;
}

#define CheckStdGetSpecializer(a,b,c) { \
	test(test_stdget_call((a), CONSTANT_CLOSNAME_##b, \
				stdset_specializer_##c, stdget_specializer_##c), "specializer_" #c); \
}

static int test_stdget_specializer(void)
{
	addr pos;

	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	clos_instance_heap(pos, &pos);

	CheckStdGetSpecializer(pos, OBJECT, object);
	CheckStdGetSpecializer(pos, TYPE, type);

	RETURN;
}


/*
 *  call object
 */
static int test_call(Execute ptr, addr instance, addr generic, addr args)
{
	/* dummy */
	return 0;
}

static int test_clos_generic_call_heap(void)
{
	addr pos;
	clos_generic_call call;

	ClosGenericTable[gen_debug] = test_call;
	clos_generic_call_heap(&pos, gen_debug, 10);
	test(GetType(pos) == LISPSYSTEM_GENERIC, "clos_generic_call_heap1");
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "clos_generic_call_heap2");
	CallClosGenericCall(pos, &call);
	test(call == test_call, "clos_generic_call_heap3");

	RETURN;
}


/*
 *  default method-combination
 */
static void test_make_generic(addr *ret)
{
	addr clos;
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &clos);
	clos_instance_heap(clos, ret);
}

static void test_make_method(addr *ret)
{
	addr clos;
	GetConst(CLOS_STANDARD_METHOD, &clos);
	clos_instance_heap(clos, ret);
}

static void test_comb_standard_method_call(Execute ptr, addr right)
{
	/* method next . args */
	GetCdr(right, &right); /* next */
	GetCdr(right, &right); /* args */
	GetCar(right, &right); /* t */
	setresult_control(ptr, right);
}

static int test_comb_standard_method(void)
{
	addr method, args, call, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);

	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &call);
	compiled_heap(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function(method, call);
	list_heap(&args, T, NULL);
	comb_standard_method(ptr, method, Nil, args);
	getresult_control(ptr, &call);
	test(call == T, "comb_standard_method1");

	free_control(ptr, control);

	RETURN;
}

static void test_comb_standard_funcall_call(Execute ptr, addr right)
{
	addr left, check;
	fixnum value;

	/* method */
	GetCons(right, &left, &right);
	GetConst(CLOS_STANDARD_METHOD, &check);
	if (! clos_subtype_p(left, check)) goto error;

	GetCons(right, &left, &right);
	GetCons(left, &check, &left);
	GetFixnum(check, &value);
	if (value != 10) goto error;
	GetCons(left, &check, &left);
	GetFixnum(check, &value);
	if (value != 20) goto error;
	GetCons(left, &check, &left);
	GetFixnum(check, &value);
	if (value != 30) goto error;
	if (left != Nil) goto error;

	/* result */
	GetCar(right, &right);
	setresult_control(ptr, right);
	return;

error:
	setresult_control(ptr, Nil);
	return;
}

static int test_comb_standard_funcall(void)
{
	addr method, call, control, around, primary, args, value1, value2, value3;
	Execute ptr;
	LocalRoot local;

	ptr = Execute_Thread;
	local = ptr->local;
	push_close_control(ptr, &control);

	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC2", &call);
	compiled_heap(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_funcall_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function(method, call);

	fixnum_local(local, &value1, 10);
	fixnum_local(local, &value2, 20);
	fixnum_local(local, &value3, 30);
	list_local(local, &around, method, value1, NULL);
	list_local(local, &primary, value2, value3, NULL);
	list_local(local, &args, T, NULL);
	comb_standard_funcall(ptr, args, around, primary);
	getresult_control(ptr, &call);
	test(call == T, "comb_standard_funcall1");
	free_control(ptr, control);

	RETURN;
}

static int test_function_standard_lambda(void)
{
	addr control, method, call, data, args;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);

	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC3", &call);
	compiled_heap(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function(method, call);
	list_heap(&args, Nil, Nil, T, NULL);
	list_heap(&data, method, NULL);
	list_heap(&data, Nil, data, Nil, Nil, NULL);
	setdata_control(ptr, data);
	setargs_list_control(ptr, args);
	function_standard_lambda(ptr);

	getresult_control(ptr, &call);
	test(call == T, "function_standard_lambda1");
	free_control(ptr, control);

	RETURN;
}

static int test_comb_standard_qualifiers(void)
{
	addr generic, method, call, primary, control, args;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = ptr->local;
	push_local(local, &stack);
	/* generic function */
	test_make_generic(&generic);
	GetConst(CLOS_STANDARD_METHOD, &method);
	stdset_generic_method_class(generic, method);
	/* make method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC4", &call);
	compiled_heap(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function(method, call);
	list_heap(&primary, method, NULL);
	comb_standard_qualifiers(local, &method, generic, Nil, primary, Nil);
	/* run method */
	push_close_control(ptr, &control);
	list_heap(&args, T, NULL);
	comb_standard_method(ptr, method, Nil, args);

	getresult_control(ptr, &call);
	test(call == T, "comb_standard_qualifiers1");
	free_control(ptr, control);
	rollback_local(local, stack);

	RETURN;
}

static int test_comb_standard(void)
{
	addr data, method, call, control, generic, args;
	Execute ptr;
	clos_generic_call callproc;

	ptr = Execute_Thread;
	/* data */
	vector4_heap(&data, 4);
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC5", &call);
	compiled_heap(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function(method, call);
	list_heap(&method, method, NULL);
	SetArrayA4(data, 2, method);
	/* call */
	comb_standard(&call, data);
	/* clos_generic_call */
	push_close_control(ptr, &control);
	CallClosGenericCall(call, &callproc);
	test_make_generic(&generic);
	GetConst(CLOS_STANDARD_METHOD, &args);
	stdset_generic_method_class(generic, args);
	list_heap(&args, T, NULL);
	callproc(ptr, call, generic, args);

	getresult_control(ptr, &call);
	test(call == T, "comb_standard1");
	free_control(ptr, control);

	RETURN;
}

static int test_comb_lambda(void)
{
	addr call;

	comb_lambda(Execute_Thread, &call, Nil, Nil, Nil);
	test(call, "comb_lambda1");

	RETURN;
}


/*
 *  generic-finalize
 */
static int test_generic_eql_specializer(void)
{
	addr left, right, value;

	fixnum_heap(&value, 40);
	clos_intern_specializer(value, &left);
	fixnum_heap(&value, 40);
	clos_intern_specializer(value, &right);
	test(generic_eql_specializer(left, right, 1), "generic_eql_specializer1");

	GetConst(CLOS_REAL, &right);
	test(generic_eql_specializer(left, right, 1), "generic_eql_specializer2");

	fixnum_heap(&value, 40);
	clos_intern_specializer(value, &right);
	GetConst(CLOS_FIXNUM, &left);
	test(generic_eql_specializer(left, right, 1), "generic_eql_specializer3");
	test(! generic_eql_specializer(left, right, 0), "generic_eql_specializer4");

	GetConst(CLOS_FIXNUM, &left);
	GetConst(CLOS_REAL, &right);
	test(generic_eql_specializer(left, right, 1), "generic_eql_specializer5");

	RETURN;
}

static int test_generic_make_method_check(void)
{
	addr method, pos1, pos2, argtype;

	test_make_method(&method);
	GetConst(CLOS_REAL, &pos1);
	GetConst(CLOS_INTEGER, &pos2);
	list_heap(&pos1, pos1, pos2, NULL);
	stdset_method_specializers(method, pos1);

	GetConst(CLOS_INTEGER, &pos1);
	fixnum_heap(&pos2, 10);
	clos_intern_specializer(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	test(generic_make_method_check(argtype, method), "generic_make_method_check1");

	character_heap(&pos2, 10);
	clos_intern_specializer(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	test(! generic_make_method_check(argtype, method), "generic_make_method_check2");

	RETURN;
}

static int test_generic_compare_class(void)
{
	addr left, right;

	GetConst(CLOS_INTEGER, &left);
	test(generic_compare_class(left, left) == 0, "generic_compare_class1");

	GetConst(CLOS_REAL, &right);
	test(generic_compare_class(left, right) < 0, "generic_compare_class2");

	GetConst(CLOS_FIXNUM, &right);
	test(generic_compare_class(left, right) > 0, "generic_compare_class3");

	RETURN;
}

static int test_generic_compare_eql(void)
{
	test(generic_compare_eql(T, T) == 0, "generic_compare_eql1");
	RETURN;
}

static int test_generic_compare_eql_type(void)
{
	addr left, right, pos;

	fixnum_heap(&left, 10);
	clos_intern_specializer(left, &left);
	GetConst(CLOS_REAL, &right);
	test(generic_compare_eql_type(left, right) < 0, "generic_compare_eql_type1");

	GetConst(CLOS_FIXNUM, &right);
	test(generic_compare_eql_type(left, right) < 0, "generic_compare_eql_type2");

	GetConst(CLOS_INTEGER, &pos);
	stdset_specializer_type(left, pos);
	test(generic_compare_eql_type(left, right) > 0, "generic_compare_eql_type3");
	clos_forget_all_specializer_unsafe();

	RETURN;
}

static int test_generic_compare_type_eql(void)
{
	addr left, right, pos;

	fixnum_heap(&right, 10);
	clos_intern_specializer(right, &right);
	GetConst(CLOS_REAL, &left);
	test(generic_compare_type_eql(left, right) > 0, "generic_compare_type_eql1");

	GetConst(CLOS_FIXNUM, &left);
	test(generic_compare_type_eql(left, right) > 0, "generic_compare_type_eql2");

	GetConst(CLOS_INTEGER, &pos);
	stdset_specializer_type(right, pos);
	test(generic_compare_type_eql(left, right) < 0, "generic_compare_type_eql3");
	clos_forget_all_specializer_unsafe();

	RETURN;
}

static int test_generic_compare_specializer(void)
{
	addr left, right;

	fixnum_heap(&left, 10);
	clos_intern_specializer(left, &left);
	test(generic_compare_specializer(left, left) == 0, "generic_compare_specializer1");

	GetConst(CLOS_REAL, &right);
	test(generic_compare_specializer(left, right) < 0, "generic_compare_specializer2");
	test(generic_compare_specializer(right, left) > 0, "generic_compare_specializer3");
	test(generic_compare_specializer(right, right) == 0, "generic_compare_specializer4");
	GetConst(CLOS_INTEGER, &left);
	test(generic_compare_specializer(left, right) < 0, "generic_compare_specializer5");

	RETURN;
}

static int test_generic_sort_compare(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	check = generic_sort_compare(generic_compare_specializer, cons1, cons2);
	test(! check, "generic_sort_compare1");
	check = generic_sort_compare(generic_compare_specializer, cons2, cons1);
	test(check, "generic_sort_compare2");
	check = generic_sort_compare(generic_compare_specializer, cons1, cons1);
	test(check, "generic_sort_compare3");

	RETURN;
}

static int test_generic_sort_call(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;

	test_make_method(&method1);
	test_make_method(&method2);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	stdset_method_specializers(method1, cons1);
	stdset_method_specializers(method2, cons2);

	check = generic_sort_call(method1, method2);
	test(! check, "generic_sort_call1");
	check = generic_sort_call(method2, method1);
	test(check, "generic_sort_call2");
	check = generic_sort_call(method1, method1);
	test(check, "generic_sort_call3");

	RETURN;
}

static int test_generic_sort(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;
	addr order, value1, value2, value3;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	check = generic_sort(order, generic_compare_specializer, cons1, cons2);
	test(! check, "generic_sort1");
	check = generic_sort(order, generic_compare_specializer, cons2, cons1);
	test(check, "generic_sort2");
	check = generic_sort(order, generic_compare_specializer, cons1, cons1);
	test(check, "generic_sort3");

	list_heap(&order, value2, value1, value3, NULL);
	check = generic_sort(order, generic_compare_specializer, cons1, cons2);
	test(! check, "generic_sort4");
	list_heap(&order, value3, value2, value1, NULL);
	check = generic_sort(order, generic_compare_specializer, cons1, cons2);
	test(check, "generic_sort5");

	RETURN;
}

static int test_generic_sort_order_call(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;
	addr order, value1, value2, value3;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	stdset_method_specializers(method1, cons1);
	stdset_method_specializers(method2, cons2);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	check = generic_sort_order_call(order, method1, method2);
	test(! check, "generic_sort_order_call1");
	check = generic_sort_order_call(order, method2, method1);
	test(check, "generic_sort_order_call2");
	check = generic_sort_order_call(order, method1, method1);
	test(check, "generic_sort_order_call3");

	list_heap(&order, value2, value1, value3, NULL);
	check = generic_sort_order_call(order, method1, method2);
	test(! check, "generic_sort_order_call4");
	list_heap(&order, value3, value2, value1, NULL);
	check = generic_sort_order_call(order, method1, method2);
	test(check, "generic_sort_order_call5");

	RETURN;
}

static int test_generic_specializers_sort(void)
{
	addr fixnum, integer, real;
	addr cons1, cons2, cons3, method1, method2, method3;
	addr order, value1, value2, value3;
	addr generic;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	list_heap(&cons3, integer, integer, real, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	test_make_method(&method3);
	stdset_method_specializers(method1, cons1);
	stdset_method_specializers(method2, cons2);
	stdset_method_specializers(method3, cons3);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	test_make_generic(&generic);
	list_heap(&order, value2, value1, value3, NULL);
	stdset_generic_precedence_index(generic, order);
	list_heap(&cons1, method1, method2, method3, NULL);
	generic_specializers_sort(&cons1, generic, cons1);

	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "generic_specializers_sort1");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "generic_specializers_sort2");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "generic_specializers_sort3");

	stdset_generic_precedence_index(generic, Nil);
	list_heap(&cons1, method1, method2, method3, NULL);
	generic_specializers_sort(&cons1, generic, cons1);
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "generic_specializers_sort4");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "generic_specializers_sort5");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "generic_specializers_sort6");

	RETURN;
}

static int test_generic_make_type(void)
{
	addr generic, method, pos, args, control, check;
	clos_generic_call call;
	Execute ptr;

	test_make_generic(&generic);
	stdset_generic_argument_precedence_order(generic, Nil);
	stdset_generic_precedence_index(generic, Nil);
	/* method-combination */
	stdset_generic_method_combination(generic, Nil);
	/* method-class */
	GetConst(CLOS_STANDARD_METHOD, &pos);
	stdset_generic_method_class(generic, pos);
	/* method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &pos);
	compiled_heap(&pos, pos);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(pos, p_debug1);
	stdset_method_function(method, pos);
	GetConst(CLOS_T, &check);
	list_heap(&pos, check, NULL);
	stdset_method_specializers(method, pos);
	list_heap(&method, method, NULL);
	/* methods */
	vector4_heap(&pos, 4);
	SetArrayA4(pos, 2, method); /* primary */
	stdset_generic_methods(generic, pos);
	/* call */
	GetConst(CLOS_T, &check);
	list_heap(&pos, check, NULL);
	generic_make_type(Execute_Thread, &pos, generic, pos);
	CallClosGenericCall(pos, &call);
	list_heap(&args, T, NULL);
	ptr = Execute_Thread;
	/* others */

	push_close_control(ptr, &control);
	call(ptr, pos, generic, args);
	/* check */
	getresult_control(ptr, &pos);
	test(pos == T, "generic_make_type1");
	free_control(ptr, control);

	RETURN;
}

static int test_generic_make_mapcar_class_of(void)
{
	addr pos, eql, eqlchecks, args, value1, value2, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	clos_forget_all_specializer_unsafe();
	clos_intern_specializer(value1, &eql);
	list_heap(&eqlchecks, T, T, Nil, NULL);
	list_heap(&args, value2, value1, value1, value1, value1, NULL);
	generic_make_mapcar_class_of(local, &args, eqlchecks, args);

	GetCons(args, &pos, &args);
	GetConst(CLOS_FIXNUM, &check);
	test(pos == check, "generic_make_mapcar_class_of1");
	GetCons(args, &pos, &args);
	test(pos == eql, "generic_make_mapcar_class_of2");
	GetCons(args, &pos, &args);
	test(pos == check, "generic_make_mapcar_class_of3");
	rollback_local(local, stack);

	RETURN;
}

static int test_hello_call(Execute ptr, addr instance, addr generic, addr args)
{
	fixnum value;

	GetCar(args, &args);
	GetFixnum(args, &value);
	setresult_control(ptr, value == 100? T: Nil);

	return 0;
}

static int test_generic_make_lambda_call(void)
{
	addr instance, eqlcheck, cache, call, pos, control;
	Execute ptr;

	/* argument */
	clos_generic_call_heap(&instance, gen_debug, 2);
	list_heap(&eqlcheck, T, NULL);
	SetClosGenericCallArray(instance, 0, eqlcheck);
	generic_instance_cache(&cache);
	SetClosGenericCallArray(instance, 1, cache);

	/* intern cache */
	ClosGenericTable[gen_debug] = test_hello_call;
	clos_generic_call_heap(&call, gen_debug, 2);
	GetConst(CLOS_FIXNUM, &pos);
	list_heap(&pos, pos, NULL);
	intern_hashheap(cache, pos, &pos);
	SetCdr(pos, call);

	/* call */
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	fixnum_heap(&pos, 100);
	list_heap(&pos, pos, NULL);
	generic_make_lambda_call(ptr, instance, Nil, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "generic_make_lambda_call1");
	free_control(ptr, control);

	RETURN;
}


/*
 *  execute-clos
 */
static int test_closrun_execute(void)
{
	addr generic, method, name, lambda, control, args, pos, value;
	Execute ptr;

	ptr = Execute_Thread;
	internchar(LISP_PACKAGE, "HELLO", &name);
	SetFunctionSymbol(name, Unbound);
	lambda = readr("(values)");

	argument_generic_heap(ptr->local, &lambda, lambda);
	parse_callname_error(&name, name);
	generic_empty(name, lambda, &generic);
	list_heap(&pos, Nil, NULL);
	stdset_generic_eqlcheck(generic, pos);
	/* method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &pos);
	compiled_heap(&pos, pos);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(pos, p_debug1);
	stdset_method_function(method, pos);
	GetConst(CLOS_T, &value);
	list_heap(&pos, value, NULL);
	stdset_method_specializers(method, pos);
	/* methods */
	stdget_generic_methods(generic, &pos);
	list_heap(&method, method, NULL);
	SetArrayA4(pos, 2, method); /* primary */
	/* finalize */
	generic_finalize(generic);
	/* execute */
	push_close_control(ptr, &control);
	fixnum_heap(&value, 100);
	list_heap(&args, value, NULL);
	getcallname_global(name, &name);
	apply_control(ptr, name, args);

	getresult_control(ptr, &pos);
	test(pos == value, "closrun_execute1");
	free_control(ptr, control);

	RETURN;
}


/*
 *  main
 */
static int testbreak_clos_generic(void)
{
	/* access */
	TestBreak(test_stdget_generic);
	TestBreak(test_stdboundp_generic);
	TestBreak(test_stdget_specializer);
	/* call object */
	TestBreak(test_clos_generic_call_heap);
	/* default method-combination */
	TestBreak(test_comb_standard_method);
	TestBreak(test_comb_standard_funcall);
	TestBreak(test_function_standard_lambda);
	TestBreak(test_comb_standard_qualifiers);
	TestBreak(test_comb_standard);
	TestBreak(test_comb_lambda);
	/* finalize-generic-function */
	TestBreak(test_generic_eql_specializer);
	TestBreak(test_generic_make_method_check);
	TestBreak(test_generic_compare_class);
	TestBreak(test_generic_compare_eql);
	TestBreak(test_generic_compare_eql_type);
	TestBreak(test_generic_compare_type_eql);
	TestBreak(test_generic_compare_specializer);
	TestBreak(test_generic_sort_compare);
	TestBreak(test_generic_sort_call);
	TestBreak(test_generic_sort);
	TestBreak(test_generic_sort_order_call);
	TestBreak(test_generic_specializers_sort);
	TestBreak(test_generic_make_type);
	TestBreak(test_generic_make_mapcar_class_of);
	TestBreak(test_generic_make_lambda_call);
	/* execute clos */
	TestBreak(test_closrun_execute);

	return 0;
}

int test_clos_generic(void)
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
		lisp_initialize = 1;
		result = testbreak_clos_generic();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

