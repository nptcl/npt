#include "clos_generic.c"
#include "character.h"
#include "clos_defgeneric.h"
#include "common.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"
#include "package_intern.h"
#include "reader.h"
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
		int (*set_)(addr, addr),
		int (*get_)(addr, addr *))
{
	addr check, k, v;

	v = readr_debug("aaa");
	GetConstant(name, &k);
	clos_set_(pos, k, v);
	(*get_)(pos, &check);
	if (check != v)
		return 0;

	(*set_)(pos, T);
	clos_get_(pos, k, &check);
	if (check != T)
		return 0;

	return 1;
}

#define CheckStdGetGeneric(a,b,c) { \
	test(test_stdget_call((a), CONSTANT_CLOSNAME_##b, \
				stdset_generic_##c##_, stdget_generic_##c##_), "generic_" #c); \
}

static int test_stdget_generic(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap_(pos, &pos);

	CheckStdGetGeneric(pos, NAME, name);
	CheckStdGetGeneric(pos, METHODS, methods);
	CheckStdGetGeneric(pos, LAMBDA_LIST, lambda_list);
	CheckStdGetGeneric(pos, ARGUMENT_PRECEDENCE_ORDER, argument_precedence_order);
	CheckStdGetGeneric(pos, DECLARATIONS, declarations);
	CheckStdGetGeneric(pos, METHOD_CLASS, method_class);
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
	clos_instance_heap_(pos, &pos);

	check = 0;
	stdboundp_generic_argument_precedence_order_(pos, &check);
	test(! check, "stdboundp_generic1");
	stdset_generic_argument_precedence_order_(pos, Nil);
	stdboundp_generic_argument_precedence_order_(pos, &check);
	test(check, "stdboundp_generic2");

	stdboundp_generic_eqlcheck_(pos, &check);
	test(! check, "stdboundp_generic3");
	stdset_generic_eqlcheck_(pos, Nil);
	stdboundp_generic_eqlcheck_(pos, &check);
	test(check, "stdboundp_generic4");

	RETURN;
}

#define CheckStdGetSpecializer(a,b,c) { \
	test(test_stdget_call((a), CONSTANT_CLOSNAME_##b, \
				stdset_specializer_##c##_, stdget_specializer_##c##_), \
				"specializer_" #c); \
}

static int test_stdget_specializer(void)
{
	addr pos;

	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	clos_instance_heap_(pos, &pos);

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
	clos_instance_heap_(clos, ret);
}

static void test_make_method(addr *ret)
{
	addr clos;
	GetConst(CLOS_STANDARD_METHOD, &clos);
	clos_instance_heap_(clos, ret);
}

static int test_comb_standard_method_call(Execute ptr, addr right)
{
	/* method next . args */
	GetCdr(right, &right); /* next */
	GetCdr(right, &right); /* args */
	GetCar(right, &right); /* t */
	setresult_control(ptr, right);
	return 0;
}

static int test_comb_standard_method(void)
{
	addr method, args, call, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);

	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC1", &call);
	compiled_system(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function_(method, call);
	list_heap(&args, T, NULL);
	comb_standard_method_(ptr, method, Nil, args);
	getresult_control(ptr, &call);
	test(call == T, "comb_standard_method1");

	pop_control_(ptr, control);

	RETURN;
}

static int test_comb_standard_funcall_call(Execute ptr, addr right)
{
	addr left, check;
	fixnum value;

	/* method */
	GetCons(right, &left, &right);
	GetConst(CLOS_STANDARD_METHOD, &check);
	if (! clos_subtype_p_debug(left, check)) goto error;

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
	return 0;

error:
	setresult_control(ptr, Nil);
	return 0;
}

static int test_comb_standard_funcall(void)
{
	addr method, call, control, around, primary, args, value1, value2, value3;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);

	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC2", &call);
	compiled_system(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_funcall_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function_(method, call);

	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	fixnum_heap(&value3, 30);
	list_heap(&around, method, value1, NULL);
	list_heap(&primary, value2, value3, NULL);
	list_heap(&args, T, NULL);
	comb_standard_funcall_(ptr, args, around, primary);
	getresult_control(ptr, &call);
	test(call == T, "comb_standard_funcall1");
	pop_control_(ptr, control);

	RETURN;
}

static int test_function_standard_lambda(void)
{
	addr control, method, call, data, args;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);

	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC3", &call);
	compiled_system(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function_(method, call);
	list_heap(&args, Nil, Nil, T, NULL);
	list_heap(&data, method, NULL);
	list_heap(&data, Nil, data, Nil, Nil, NULL);
	setdata_control(ptr, data);
	setargs_list_control(ptr, args);
	function_standard_lambda(ptr);

	getresult_control(ptr, &call);
	test(call == T, "function_standard_lambda1");
	pop_control_(ptr, control);

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
	stdset_generic_method_class_(generic, method);
	/* make method */
	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC4", &call);
	compiled_system(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function_(method, call);
	list_heap(&primary, method, NULL);
	comb_standard_qualifiers_(local, &method, generic, Nil, primary, Nil);
	/* run method */
	push_control(ptr, &control);
	list_heap(&args, T, NULL);
	comb_standard_method_(ptr, method, Nil, args);

	getresult_control(ptr, &call);
	test(call == T, "comb_standard_qualifiers1");
	pop_control_(ptr, control);
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
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC5", &call);
	compiled_system(&call, call);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(call, p_debug1);
	stdset_method_function_(method, call);
	list_heap(&method, method, NULL);
	SetArrayA4(data, 2, method);
	/* call */
	comb_standard_(&call, data);
	/* clos_generic_call */
	push_control(ptr, &control);
	CallClosGenericCall(call, &callproc);
	test_make_generic(&generic);
	GetConst(CLOS_STANDARD_METHOD, &args);
	stdset_generic_method_class_(generic, args);
	list_heap(&args, T, NULL);
	callproc(ptr, call, generic, args);

	getresult_control(ptr, &call);
	test(call == T, "comb_standard1");
	pop_control_(ptr, control);

	RETURN;
}

static int test_comb_lambda(void)
{
	addr call;

	comb_lambda_(Execute_Thread, &call, Nil, Nil, Nil);
	test(call, "comb_lambda1");

	RETURN;
}


/*
 *  generic-finalize
 */
static int test_generic_eql_specializer(void)
{
	int check;
	addr left, right, value;

	check = 0;
	fixnum_heap(&value, 40);
	clos_intern_specializer_(value, &left);
	fixnum_heap(&value, 40);
	clos_intern_specializer_(value, &right);
	generic_eql_specializer_(left, right, 1, &check);
	test(check, "generic_eql_specializer1");

	GetConst(CLOS_REAL, &right);
	generic_eql_specializer_(left, right, 1, &check);
	test(check, "generic_eql_specializer2");

	fixnum_heap(&value, 40);
	clos_intern_specializer_(value, &right);
	GetConst(CLOS_FIXNUM, &left);
	generic_eql_specializer_(left, right, 1, &check);
	test(check, "generic_eql_specializer3");
	generic_eql_specializer_(left, right, 0, &check);
	test(! check, "generic_eql_specializer4");

	GetConst(CLOS_FIXNUM, &left);
	GetConst(CLOS_REAL, &right);
	generic_eql_specializer_(left, right, 1, &check);
	test(check, "generic_eql_specializer5");

	RETURN;
}

static int test_generic_make_method_check(void)
{
	int check;
	addr method, pos1, pos2, argtype;

	check = 0;
	test_make_method(&method);
	GetConst(CLOS_REAL, &pos1);
	GetConst(CLOS_INTEGER, &pos2);
	list_heap(&pos1, pos1, pos2, NULL);
	stdset_method_specializers_(method, pos1);

	GetConst(CLOS_INTEGER, &pos1);
	fixnum_heap(&pos2, 10);
	clos_intern_specializer_(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	generic_make_method_check_(argtype, method, &check);
	test(check, "generic_make_method_check1");

	character_heap(&pos2, 10);
	clos_intern_specializer_(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	generic_make_method_check_(argtype, method, &check);
	test(! check, "generic_make_method_check2");

	RETURN;
}

static int test_generic_compare_class(void)
{
	int check;
	addr left, right;

	check = 0;
	GetConst(CLOS_INTEGER, &left);
	generic_compare_class_(left, left, &check);
	test(check == 0, "generic_compare_class1");

	GetConst(CLOS_REAL, &right);
	generic_compare_class_(left, right, &check);
	test(check < 0, "generic_compare_class2");

	GetConst(CLOS_FIXNUM, &right);
	generic_compare_class_(left, right, &check);
	test(check > 0, "generic_compare_class3");

	RETURN;
}

static int test_generic_compare_eql(void)
{
	int check;

	check = 0;
	generic_compare_eql_(T, T, &check);
	test(check == 0, "generic_compare_eql1");

	RETURN;
}

static int test_generic_compare_eql_type(void)
{
	int check;
	addr left, right, pos;

	check = 0;
	fixnum_heap(&left, 10);
	clos_intern_specializer_(left, &left);
	GetConst(CLOS_REAL, &right);
	generic_compare_eql_type_(left, right, &check);
	test(check < 0, "generic_compare_eql_type1");

	GetConst(CLOS_FIXNUM, &right);
	generic_compare_eql_type_(left, right, &check);
	test(check < 0, "generic_compare_eql_type2");

	GetConst(CLOS_INTEGER, &pos);
	stdset_specializer_type_(left, pos);
	generic_compare_eql_type_(left, right, &check);
	test(check > 0, "generic_compare_eql_type3");
	clos_forget_all_specializer_unsafe();

	RETURN;
}

static int test_generic_compare_type_eql(void)
{
	int check;
	addr left, right, pos;

	check = 0;

	fixnum_heap(&right, 10);
	clos_intern_specializer_(right, &right);
	GetConst(CLOS_REAL, &left);
	generic_compare_type_eql_(left, right, &check);
	test(check > 0, "generic_compare_type_eql1");

	GetConst(CLOS_FIXNUM, &left);
	generic_compare_type_eql_(left, right, &check);
	test(check > 0, "generic_compare_type_eql2");

	GetConst(CLOS_INTEGER, &pos);
	stdset_specializer_type_(right, pos);
	generic_compare_type_eql_(left, right, &check);
	test(check < 0, "generic_compare_type_eql3");
	clos_forget_all_specializer_unsafe();

	RETURN;
}

static int test_generic_compare_specializer(void)
{
	int check;
	addr left, right;

	check = 0;

	fixnum_heap(&left, 10);
	clos_intern_specializer_(left, &left);
	generic_compare_specializer_(left, left, &check);
	test(check == 0, "generic_compare_specializer1");

	GetConst(CLOS_REAL, &right);
	generic_compare_specializer_(left, right, &check);
	test(check < 0, "generic_compare_specializer2");
	generic_compare_specializer_(right, left, &check);
	test(check > 0, "generic_compare_specializer3");
	generic_compare_specializer_(right, right, &check);
	test(check == 0, "generic_compare_specializer4");
	GetConst(CLOS_INTEGER, &left);
	generic_compare_specializer_(left, right, &check);
	test(check < 0, "generic_compare_specializer5");

	RETURN;
}

static int test_generic_sort_compare(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;

	check = 0;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	check = 0;
	generic_sort_compare_(cons1, cons2, &check, generic_compare_specializer_);
	test(! check, "generic_sort_compare1");
	generic_sort_compare_(cons2, cons1, &check, generic_compare_specializer_);
	test(check, "generic_sort_compare2");
	generic_sort_compare_(cons1, cons1, &check, generic_compare_specializer_);
	test(check, "generic_sort_compare3");

	RETURN;
}

static int test_generic_sort_call(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;

	check = 0;

	test_make_method(&method1);
	test_make_method(&method2);
	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	stdset_method_specializers_(method1, cons1);
	stdset_method_specializers_(method2, cons2);

	generic_sort_call_(method1, method2, &check);
	test(! check, "generic_sort_call1");
	generic_sort_call_(method2, method1, &check);
	test(check, "generic_sort_call2");
	generic_sort_call_(method1, method1, &check);
	test(check, "generic_sort_call3");

	RETURN;
}

static int test_generic_sort(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;
	addr order, value1, value2, value3;

	check = 0;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	generic_sort_(order, generic_compare_specializer_, cons1, cons2, &check);
	test(! check, "generic_sort1");
	generic_sort_(order, generic_compare_specializer_, cons2, cons1, &check);
	test(check, "generic_sort2");
	generic_sort_(order, generic_compare_specializer_, cons1, cons1, &check);
	test(check, "generic_sort3");

	list_heap(&order, value2, value1, value3, NULL);
	generic_sort_(order, generic_compare_specializer_, cons1, cons2, &check);
	test(! check, "generic_sort4");
	list_heap(&order, value3, value2, value1, NULL);
	generic_sort_(order, generic_compare_specializer_, cons1, cons2, &check);
	test(check, "generic_sort5");

	RETURN;
}

static int test_generic_sort_order_call(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;
	addr order, value1, value2, value3;

	check = 0;

	GetConst(CLOS_FIXNUM, &fixnum);
	GetConst(CLOS_INTEGER, &integer);
	GetConst(CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	stdset_method_specializers_(method1, cons1);
	stdset_method_specializers_(method2, cons2);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	generic_sort_order_call_(order, method1, method2, &check);
	test(! check, "generic_sort_order_call1");
	generic_sort_order_call_(order, method2, method1, &check);
	test(check, "generic_sort_order_call2");
	generic_sort_order_call_(order, method1, method1, &check);
	test(check, "generic_sort_order_call3");

	list_heap(&order, value2, value1, value3, NULL);
	generic_sort_order_call_(order, method1, method2, &check);
	test(! check, "generic_sort_order_call4");
	list_heap(&order, value3, value2, value1, NULL);
	generic_sort_order_call_(order, method1, method2, &check);
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
	stdset_method_specializers_(method1, cons1);
	stdset_method_specializers_(method2, cons2);
	stdset_method_specializers_(method3, cons3);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	test_make_generic(&generic);
	list_heap(&order, value2, value1, value3, NULL);
	stdset_generic_precedence_index_(generic, order);
	list_heap(&cons1, method1, method2, method3, NULL);
	generic_specializers_sort_(&cons1, generic, cons1);

	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "generic_specializers_sort1");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "generic_specializers_sort2");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "generic_specializers_sort3");

	stdset_generic_precedence_index_(generic, Nil);
	list_heap(&cons1, method1, method2, method3, NULL);
	generic_specializers_sort_(&cons1, generic, cons1);
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
	stdset_generic_argument_precedence_order_(generic, Nil);
	stdset_generic_precedence_index_(generic, Nil);
	/* method-combination */
	stdset_generic_method_combination_(generic, Nil);
	/* method-class */
	GetConst(CLOS_STANDARD_METHOD, &pos);
	stdset_generic_method_class_(generic, pos);
	/* method */
	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC1", &pos);
	compiled_system(&pos, pos);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(pos, p_debug1);
	stdset_method_function_(method, pos);
	GetConst(CLOS_T, &check);
	list_heap(&pos, check, NULL);
	stdset_method_specializers_(method, pos);
	list_heap(&method, method, NULL);
	/* vector */
	vector4_heap(&pos, 4);
	SetArrayA4(pos, 2, method); /* primary */
	stdset_generic_vector_(generic, pos);
	/* call */
	GetConst(CLOS_T, &check);
	list_heap(&pos, check, NULL);
	generic_make_type_(Execute_Thread, &pos, generic, pos);
	CallClosGenericCall(pos, &call);
	list_heap(&args, T, NULL);
	ptr = Execute_Thread;
	/* others */

	push_control(ptr, &control);
	call(ptr, pos, generic, args);
	/* check */
	getresult_control(ptr, &pos);
	test(pos == T, "generic_make_type1");
	pop_control_(ptr, control);

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
	clos_intern_specializer_(value1, &eql);
	list_heap(&eqlchecks, T, T, Nil, NULL);
	list_heap(&args, value2, value1, value1, value1, value1, NULL);
	generic_make_mapcar_class_of_(local, &args, eqlchecks, args);

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
	generic_cache_heap(&cache);
	SetClosGenericCallArray(instance, 1, cache);

	/* intern cache */
	ClosGenericTable[gen_debug] = test_hello_call;
	clos_generic_call_heap(&call, gen_debug, 2);
	GetConst(CLOS_FIXNUM, &pos);
	list_heap(&pos, pos, NULL);
	intern_hashheap_(cache, pos, &pos);
	SetCdr(pos, call);

	/* call */
	ptr = Execute_Thread;
	push_control(ptr, &control);
	fixnum_heap(&pos, 100);
	list_heap(&pos, pos, NULL);
	generic_make_lambda_call_(ptr, instance, Nil, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "generic_make_lambda_call1");
	pop_control_(ptr, control);

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
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	SetFunctionSymbol(name, Unbound);
	lambda = readr_debug("(values)");

	argument_generic_heap_(ptr->local, &lambda, lambda);
	parse_callname_error_(&name, name);
	generic_make_empty_(name, lambda, &generic);
	list_heap(&pos, Nil, NULL);
	stdset_generic_eqlcheck_(generic, pos);
	/* method */
	test_make_method(&method);
	internchar_debug(LISP_COMMON_USER, "TEST-GENERIC1", &pos);
	compiled_system(&pos, pos);
	SetPointer(p_debug1, dynamic, test_comb_standard_method_call);
	setcompiled_dynamic(pos, p_debug1);
	stdset_method_function_(method, pos);
	GetConst(CLOS_T, &value);
	list_heap(&pos, value, NULL);
	stdset_method_specializers_(method, pos);
	/* vector */
	stdget_generic_vector_(generic, &pos);
	list_heap(&method, method, NULL);
	SetArrayA4(pos, 2, method); /* primary */
	/* finalize */
	generic_finalize_(generic);
	/* execute */
	push_control(ptr, &control);
	fixnum_heap(&value, 100);
	list_heap(&args, value, NULL);
	getglobal_parse_callname(name, &name);
	apply_control(ptr, name, args);

	getresult_control(ptr, &pos);
	test(pos == value, "closrun_execute1");
	pop_control_(ptr, control);

	RETURN;
}


/*
 *  main
 */
static int testcase_clos_generic(void)
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

static void testinit_clos_generic(Execute ptr)
{
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
}

int test_clos_generic(void)
{
	DegradeTitle;
	return DegradeCode(clos_generic);
}

