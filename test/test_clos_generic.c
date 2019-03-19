#include "clos_generic.c"
#include "character.h"
#include "degrade.h"
#include "execute.h"
#include "package.h"
#include "readlite.h"
#include "readtable.h"
#include "symbol.h"

/*
 *  call object
 */
static void test_call(Execute ptr, addr instance, addr generic, addr args)
{
	/* dummy */
}

static int test_clos_generic_call_alloc(void)
{
	addr pos;
	clos_generic_call call;

	clos_generic_call_alloc(NULL, &pos, test_call, 10);
	test(GetStatusSize(pos) == LISPSIZE_SMALLSIZE, "clos_generic_call_alloc1");
	GetClosGenericCall(pos, &call);
	test(call == test_call, "clos_generic_call_alloc2");

	RETURN;
}


/*
 *  default method-combination
 */
static void test_make_generic(addr *ret)
{
	addr clos;
	GetConstant(CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION, &clos);
	make_instance_restrict_heap(clos, ret);
}

static int test_combination_arguments_order(void)
{
	addr generic, pos;

	test_make_generic(&generic);
	GetConstant(CONSTANT_KEYWORD_MOST_SPECIFIC_FIRST, &pos);
	list_heap(&pos, pos, NULL);
	setf_clos_elt(generic, Clos_generic_combination_arguments, pos);
	combination_arguments_order(&pos, generic);
	test(pos == Nil, "combination_arguments_order1");

	GetConstant(CONSTANT_KEYWORD_MOST_SPECIFIC_LAST, &pos);
	list_heap(&pos, pos, NULL);
	setf_clos_elt(generic, Clos_generic_combination_arguments, pos);
	combination_arguments_order(&pos, generic);
	test(pos == T, "combination_arguments_order2");

	RETURN;
}

static int test_order_methods(void)
{
	addr pos, left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	list_local(local, &pos, Nil, T, NULL);

	order_methods(local, &pos, Nil, pos);
	GetCons(pos, &left, &right);
	test(left == Nil, "order_methods1");
	GetCons(right, &left, &right);
	test(left == T, "order_methods2");
	test(right == Nil, "order_methods3");

	order_methods(local, &pos, T, pos);
	GetCons(pos, &left, &right);
	test(left == T, "order_methods4");
	GetCons(right, &left, &right);
	test(left == Nil, "order_methods5");
	test(right == Nil, "order_methods6");

	rollback_local(local, stack);

	RETURN;
}

static void test_make_method(addr *ret)
{
	addr clos;
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &clos);
	make_instance_restrict_heap(clos, ret);
}

static void test_funcall_method_call(Execute ptr, addr right)
{
	/* method next args */
	GetCdr(right, &right); /* (next . _) */
	GetCdr(right, &right); /* (args . _) */
	GetCar(right, &right); /* args */
	GetCar(right, &right); /* t */
	setresult_control(ptr, right);
}

static int test_funcall_method(void)
{
	addr method, args, call, control;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);

	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &call);
	compiled_heap(&call, call);
	setcompiled_dynamic(call, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, call);
	list_heap(&args, T, NULL);
	funcall_method(ptr, method, Nil, args);
	getresult_control(ptr, &call);
	test(call == T, "funcall_method1");

	free_control(ptr, control);

	RETURN;
}

static void test_funcall_around_primary_call(Execute ptr, addr right)
{
	addr left, check;
	fixnum value;

	/* method */
	GetCons(right, &left, &right);
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &check);
	if (! std_subtype_p(left, check)) goto error;

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
	GetCar(right, &right);
	setresult_control(ptr, right);
	return;

error:
	setresult_control(ptr, Nil);
	return;
}

static int test_funcall_around_primary(void)
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
	setcompiled_dynamic(call, test_funcall_around_primary_call);
	setf_clos_elt(method, Clos_method_function, call);

	fixnum_local(local, &value1, 10);
	fixnum_local(local, &value2, 20);
	fixnum_local(local, &value3, 30);
	list_local(local, &around, method, value1, NULL);
	list_local(local, &primary, value2, value3, NULL);
	list_local(local, &args, T, NULL);
	funcall_around_primary(ptr, args, around, primary);
	getresult_control(ptr, &call);
	test(call == T, "funcall_around_primary1");
	free_control(ptr, control);

	RETURN;
}

static int test_make_standard_method_for_standard_lambda(void)
{
	addr control, method, call, data, args;
	Execute ptr;

	ptr = Execute_Thread;
	push_close_control(ptr, &control);

	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC3", &call);
	compiled_heap(&call, call);
	setcompiled_dynamic(call, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, call);
	list_heap(&args, T, NULL);
	list_heap(&args, Nil, Nil, args, NULL);
	list_heap(&data, method, NULL);
	list_heap(&data, Nil, data, Nil, Nil, NULL);
	setdata_control(ptr, data);
	setargs_list_control(ptr, args);
	make_standard_method_for_standard_lambda(ptr);

	getresult_control(ptr, &call);
	test(call == T, "make_standard_method_for_standard_lambda1");
	free_control(ptr, control);

	RETURN;
}

static int test_make_standard_method_for_standard(void)
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
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &method);
	setf_clos_elt(generic, Clos_generic_method_class, method);
	/* make method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC4", &call);
	compiled_heap(&call, call);
	setcompiled_dynamic(call, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, call);
	list_heap(&primary, method, NULL);
	make_standard_method_for_standard(local,
			&method, generic, Nil, primary, Nil, Nil);
	/* run method */
	push_close_control(ptr, &control);
	list_heap(&args, T, NULL);
	funcall_method(ptr, method, Nil, args);

	getresult_control(ptr, &call);
	test(call == T, "make_standard_method_for_standard1");
	free_control(ptr, control);
	rollback_local(local, stack);

	RETURN;
}

static int test_combination_lambda_standard(void)
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
	setcompiled_dynamic(call, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, call);
	list_heap(&method, method, NULL);
	SetArrayA4(data, 2, method);
	/* call */
	combination_lambda_standard(&call, data);
	/* clos_generic_call */
	push_close_control(ptr, &control);
	GetClosGenericCall(call, &callproc);
	test_make_generic(&generic);
	setf_clos_elt(generic, Clos_generic_combination_arguments, Nil);
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &args);
	setf_clos_elt(generic, Clos_generic_method_class, args);
	list_heap(&args, T, NULL);
	callproc(ptr, call, generic, args);

	getresult_control(ptr, &call);
	test(call == T, "combination_lambda_standard1");
	free_control(ptr, control);

	RETURN;
}

static int test_combination_lambda(void)
{
	addr call, pos;

	GetConstant(CONSTANT_COMMON_STANDARD, &pos);
	pos = find_method_combination(pos);
	combination_lambda(&call, Nil, pos, Nil);
	test(call, "combination_lambda1");

	RETURN;
}


/*
 *  finalize-generic-function
 */
static int test_subclass_eql_specializer(void)
{
	addr left, right, value;

	fixnum_heap(&value, 40);
	intern_eql_specializer(value, &left);
	fixnum_heap(&value, 40);
	intern_eql_specializer(value, &right);
	test(subclass_eql_specializer(left, right, 1), "subclass_eql_specializer1");

	GetConstant(CONSTANT_CLOS_REAL, &right);
	test(subclass_eql_specializer(left, right, 1), "subclass_eql_specializer2");

	fixnum_heap(&value, 40);
	intern_eql_specializer(value, &right);
	GetConstant(CONSTANT_CLOS_FIXNUM, &left);
	test(subclass_eql_specializer(left, right, 1), "subclass_eql_specializer3");
	test(! subclass_eql_specializer(left, right, 0), "subclass_eql_specializer4");

	GetConstant(CONSTANT_CLOS_FIXNUM, &left);
	GetConstant(CONSTANT_CLOS_REAL, &right);
	test(subclass_eql_specializer(left, right, 1), "subclass_eql_specializer5");

	RETURN;
}

static int test_method_specializers_check(void)
{
	addr method, pos1, pos2, argtype;

	test_make_method(&method);
	GetConstant(CONSTANT_CLOS_REAL, &pos1);
	GetConstant(CONSTANT_CLOS_INTEGER, &pos2);
	list_heap(&pos1, pos1, pos2, NULL);
	setf_clos_elt(method, Clos_method_specializers, pos1);

	GetConstant(CONSTANT_CLOS_INTEGER, &pos1);
	fixnum_heap(&pos2, 10);
	intern_eql_specializer(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	test(method_specializers_check(argtype, method), "method_specializers_check1");

	character_heap(&pos2, 10);
	intern_eql_specializer(pos2, &pos2);
	list_heap(&argtype, pos1, pos2, NULL);
	test(! method_specializers_check(argtype, method), "method_specializers_check2");

	RETURN;
}

static int test_std_compareclass_p(void)
{
	addr left, right;

	GetConstant(CONSTANT_CLOS_INTEGER, &left);
	test(std_compareclass_p(left, left) == 0, "std_compareclass_p1");

	GetConstant(CONSTANT_CLOS_REAL, &right);
	test(std_compareclass_p(left, right) < 0, "std_compareclass_p2");

	GetConstant(CONSTANT_CLOS_FIXNUM, &right);
	test(std_compareclass_p(left, right) > 0, "std_compareclass_p3");

	RETURN;
}

static int test_compare_eql_specializer(void)
{
	test(compare_eql_specializer(T, T) == 0, "compare_eql_specializer1");
	RETURN;
}

static int test_compare_eql_type(void)
{
	addr left, right, pos;

	fixnum_heap(&left, 10);
	intern_eql_specializer(left, &left);
	GetConstant(CONSTANT_CLOS_REAL, &right);
	test(compare_eql_type(left, right) < 0, "compare_eql_type1");

	GetConstant(CONSTANT_CLOS_FIXNUM, &right);
	test(compare_eql_type(left, right) < 0, "compare_eql_type2");

	GetConstant(CONSTANT_CLOS_INTEGER, &pos);
	setf_clos_elt(left, Clos_specializer_type, pos);
	test(compare_eql_type(left, right) > 0, "compare_eql_type3");
	forget_all_eql_specializer();

	RETURN;
}

static int test_compare_type_eql(void)
{
	addr left, right, pos;

	fixnum_heap(&right, 10);
	intern_eql_specializer(right, &right);
	GetConstant(CONSTANT_CLOS_REAL, &left);
	test(compare_type_eql(left, right) > 0, "compare_type_eql1");

	GetConstant(CONSTANT_CLOS_FIXNUM, &left);
	test(compare_type_eql(left, right) > 0, "compare_type_eql2");

	GetConstant(CONSTANT_CLOS_INTEGER, &pos);
	setf_clos_elt(right, Clos_specializer_type, pos);
	test(compare_type_eql(left, right) < 0, "compare_type_eql3");
	forget_all_eql_specializer();

	RETURN;
}

static int test_compare_eql_specializers(void)
{
	addr left, right;

	fixnum_heap(&left, 10);
	intern_eql_specializer(left, &left);
	test(compare_eql_specializers(left, left) == 0, "intern_eql_specializer1");

	GetConstant(CONSTANT_CLOS_REAL, &right);
	test(compare_eql_specializers(left, right) < 0, "intern_eql_specializer2");
	test(compare_eql_specializers(right, left) > 0, "intern_eql_specializer3");
	test(compare_eql_specializers(right, right) == 0, "intern_eql_specializer4");
	GetConstant(CONSTANT_CLOS_INTEGER, &left);
	test(compare_eql_specializers(left, right) < 0, "intern_eql_specializer5");

	RETURN;
}

static int test_sortcompare(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	check = sortcompare(compare_eql_specializers, cons1, cons2);
	test(! check, "sortcompare1");
	check = sortcompare(compare_eql_specializers, cons2, cons1);
	test(check, "sortcompare2");
	check = sortcompare(compare_eql_specializers, cons1, cons1);
	test(check, "sortcompare3");

	RETURN;
}

static int test_specializer_sort(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;

	test_make_method(&method1);
	test_make_method(&method2);
	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, integer, NULL);
	list_heap(&cons2, fixnum, integer, fixnum, NULL);
	setf_clos_elt(method1, Clos_method_specializers, cons1);
	setf_clos_elt(method2, Clos_method_specializers, cons2);

	check = specializer_sort(method1, method2);
	test(! check, "specializer_sort1");
	check = specializer_sort(method2, method1);
	test(check, "specializer_sort2");
	check = specializer_sort(method1, method1);
	test(check, "specializer_sort3");

	RETURN;
}

static int test_sortconsorder(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2;
	addr order, value1, value2, value3;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	check = sortconsorder(order, compare_eql_specializers, cons1, cons2);
	test(! check, "sortconsorder1");
	check = sortconsorder(order, compare_eql_specializers, cons2, cons1);
	test(check, "sortconsorder2");
	check = sortconsorder(order, compare_eql_specializers, cons1, cons1);
	test(check, "sortconsorder3");

	list_heap(&order, value2, value1, value3, NULL);
	check = sortconsorder(order, compare_eql_specializers, cons1, cons2);
	test(! check, "sortconsorder4");
	list_heap(&order, value3, value2, value1, NULL);
	check = sortconsorder(order, compare_eql_specializers, cons1, cons2);
	test(check, "sortconsorder5");

	RETURN;
}

static int test_specializer_sort_argument_precedence_order(void)
{
	int check;
	addr fixnum, integer, real, cons1, cons2, method1, method2;
	addr order, value1, value2, value3;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	setf_clos_elt(method1, Clos_method_specializers, cons1);
	setf_clos_elt(method2, Clos_method_specializers, cons2);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	list_heap(&order, value1, value2, value3, NULL);
	check = specializer_sort_argument_precedence_order(order, method1, method2);
	test(! check, "specializer_sort_argument_precedence_order1");
	check = specializer_sort_argument_precedence_order(order, method2, method1);
	test(check, "specializer_sort_argument_precedence_order2");
	check = specializer_sort_argument_precedence_order(order, method1, method1);
	test(check, "specializer_sort_argument_precedence_order3");

	list_heap(&order, value2, value1, value3, NULL);
	check = specializer_sort_argument_precedence_order(order, method1, method2);
	test(! check, "specializer_sort_argument_precedence_order4");
	list_heap(&order, value3, value2, value1, NULL);
	check = specializer_sort_argument_precedence_order(order, method1, method2);
	test(check, "specializer_sort_argument_precedence_order5");

	RETURN;
}

static int test_sort_argument_precedence_order(void)
{
	addr fixnum, integer, real;
	addr cons1, cons2, cons3, method1, method2, method3;
	addr order, value1, value2, value3;
	addr generic;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	list_heap(&cons3, integer, integer, real, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	test_make_method(&method3);
	setf_clos_elt(method1, Clos_method_specializers, cons1);
	setf_clos_elt(method2, Clos_method_specializers, cons2);
	setf_clos_elt(method3, Clos_method_specializers, cons3);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	test_make_generic(&generic);
	list_heap(&order, value2, value1, value3, NULL);
	setf_clos_elt(generic, Clos_generic_argument_precedence_order, order);
	list_heap(&cons1, method1, method2, method3, NULL);
	sort_argument_precedence_order(&cons1, generic, cons1);

	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "sort_argument_precedence_order1");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "sort_argument_precedence_order2");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "sort_argument_precedence_order3");

	RETURN;
}

static int test_specializers_sort(void)
{
	addr fixnum, integer, real;
	addr cons1, cons2, cons3, method1, method2, method3;
	addr order, value1, value2, value3;
	addr generic;

	GetConstant(CONSTANT_CLOS_FIXNUM, &fixnum);
	GetConstant(CONSTANT_CLOS_INTEGER, &integer);
	GetConstant(CONSTANT_CLOS_REAL, &real);
	list_heap(&cons1, fixnum, real, fixnum, NULL);
	list_heap(&cons2, fixnum, integer, integer, NULL);
	list_heap(&cons3, integer, integer, real, NULL);
	test_make_method(&method1);
	test_make_method(&method2);
	test_make_method(&method3);
	setf_clos_elt(method1, Clos_method_specializers, cons1);
	setf_clos_elt(method2, Clos_method_specializers, cons2);
	setf_clos_elt(method3, Clos_method_specializers, cons3);

	index_heap(&value1, 0);
	index_heap(&value2, 1);
	index_heap(&value3, 2);
	test_make_generic(&generic);
	list_heap(&order, value2, value1, value3, NULL);
	setf_clos_elt(generic, Clos_generic_argument_precedence_order, order);
	list_heap(&cons1, method1, method2, method3, NULL);
	specializers_sort(&cons1, generic, cons1);

	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "specializers_sort1");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "specializers_sort2");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "specializers_sort3");

	setf_clos_elt(generic, Clos_generic_argument_precedence_order, Unbound);
	list_heap(&cons1, method1, method2, method3, NULL);
	specializers_sort(&cons1, generic, cons1);
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method2, "specializers_sort4");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method1, "specializers_sort5");
	GetCons(cons1, &cons2, &cons1);
	test(cons2 == method3, "specializers_sort6");

	RETURN;
}

static int test_make_generic_function_type(void)
{
	addr generic, method, pos, args, control;
	clos_generic_call call;
	Execute ptr;

	test_make_generic(&generic);
	/* method-combination */
	GetConstant(CONSTANT_COMMON_STANDARD, &pos);
	pos = find_method_combination(pos);
	setf_clos_elt(generic, Clos_generic_method_combination, pos);
	/* combination-arguments */
	setf_clos_elt(generic, Clos_generic_combination_arguments, Nil);
	/* method-class */
	GetConstant(CONSTANT_CLOS_STANDARD_METHOD, &pos);
	setf_clos_elt(generic, Clos_generic_method_class, pos);
	/* method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &pos);
	compiled_heap(&pos, pos);
	setcompiled_dynamic(pos, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, pos);
	list_heap(&pos, find_class(T), NULL);
	setf_clos_elt(method, Clos_method_specializers, pos);
	list_heap(&method, method, NULL);
	/* methods */
	vector4_heap(&pos, 4);
	SetArrayA4(pos, 2, method); /* primary */
	setf_clos_elt(generic, Clos_generic_methods, pos);
	/* call */
	list_heap(&pos, find_class(T), NULL);
	make_generic_function_type(&pos, generic, pos);
	GetClosGenericCall(pos, &call);
	list_heap(&args, T, NULL);
	ptr = Execute_Thread;

	push_close_control(ptr, &control);
	call(ptr, pos, generic, args);
	/* check */
	getresult_control(ptr, &pos);
	test(pos == T, "make_generic_function_type1");
	free_control(ptr, control);

	RETURN;
}

static int test_mapcar_cons_class_of(void)
{
	addr pos, eql, eqlchecks, args, value1, value2, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);
	fixnum_heap(&value1, 10);
	fixnum_heap(&value2, 20);
	forget_all_eql_specializer();
	intern_eql_specializer(value1, &eql);
	list_heap(&eqlchecks, T, T, Nil, NULL);
	list_heap(&args, value2, value1, value1, value1, value1, NULL);
	mapcar_cons_class_of(local, &args, eqlchecks, args);

	GetCons(args, &pos, &args);
	GetConstant(CONSTANT_CLOS_FIXNUM, &check);
	test(pos == check, "mapcar_cons_class_of1");
	GetCons(args, &pos, &args);
	test(pos == eql, "mapcar_cons_class_of2");
	GetCons(args, &pos, &args);
	test(pos == check, "mapcar_cons_class_of3");
	rollback_local(local, stack);

	RETURN;
}

static void test_hello_call(Execute ptr,
		addr instance, addr generic, addr args)
{
	fixnum value;

	GetCar(args, &args);
	GetFixnum(args, &value);
	setresult_control(ptr, value == 100? T: Nil);
}

static int test_make_generic_function_lambda_call(void)
{
	addr instance, eqlcheck, cache, call, pos, control;
	Execute ptr;

	/* argument */
	clos_generic_call_heap(&instance, NULL, 2);
	list_heap(&eqlcheck, T, NULL);
	SetClosGenericCallArray(instance, 0, eqlcheck);
	make_generic_function_cache(&cache);
	SetClosGenericCallArray(instance, 1, cache);

	/* intern cache */
	clos_generic_call_heap(&call, test_hello_call, 2);
	GetConstant(CONSTANT_CLOS_FIXNUM, &pos);
	list_heap(&pos, pos, NULL);
	intern_hashheap(cache, pos, &pos);
	SetCdr(pos, call);

	/* call */
	ptr = Execute_Thread;
	push_close_control(ptr, &control);
	fixnum_heap(&pos, 100);
	list_heap(&pos, pos, NULL);
	make_generic_function_lambda_call(ptr, instance, Nil, pos);
	getresult_control(ptr, &pos);
	test(pos == T, "make_generic_function_lambda_call1");
	free_control(ptr, control);

	RETURN;
}


/*
 *  execute-clos
 */
static int test_execute_clos(void)
{
	addr generic, method, name, lambda, control, args, pos, value;
	Execute ptr;

	ptr = Execute_Thread;
	internchar(LISP_PACKAGE, "HELLO", &name);
	SetFunctionSymbol(name, Unbound);
	readlite_package_heap(&lambda, LISP_PACKAGE, "(value)");
	generic_function_instance(ptr, &generic, name, lambda);
	list_heap(&pos, Nil, NULL);
	setf_clos_elt(generic, Clos_generic_eqlcheck, pos);
	/* method */
	test_make_method(&method);
	internchar(LISP_PACKAGE, "TEST-GENERIC1", &pos);
	compiled_heap(&pos, pos);
	setcompiled_dynamic(pos, test_funcall_method_call);
	setf_clos_elt(method, Clos_method_function, pos);
	list_heap(&pos, find_class(T), NULL);
	setf_clos_elt(method, Clos_method_specializers, pos);
	/* methods */
	clos_elt(generic, Clos_generic_methods, &pos);
	list_heap(&method, method, NULL);
	SetArrayA4(pos, 2, method); /* primary */
	/* finalize */
	std_finalize_generic_function(ptr, generic);
	/* execute */
	push_close_control(ptr, &control);
	fixnum_heap(&value, 100);
	list_heap(&args, value, NULL);
	getcallname_global(name, &name);
	apply_control(ptr, name, args);

	getresult_control(ptr, &pos);
	test(pos == value, "execute_clos1");
	free_control(ptr, control);

	RETURN;
}



/*
 *  main
 */
static int testbreak_clos_generic(void)
{
	/* call object */
	TestBreak(test_clos_generic_call_alloc);
	/* default method-combination */
	TestBreak(test_combination_arguments_order);
	TestBreak(test_order_methods);
	TestBreak(test_funcall_method);
	TestBreak(test_funcall_around_primary);
	TestBreak(test_make_standard_method_for_standard_lambda);
	TestBreak(test_make_standard_method_for_standard);
	TestBreak(test_combination_lambda_standard);
	TestBreak(test_combination_lambda);
	/* finalize-generic-function */
	TestBreak(test_subclass_eql_specializer);
	TestBreak(test_method_specializers_check);
	TestBreak(test_std_compareclass_p);
	TestBreak(test_compare_eql_specializer);
	TestBreak(test_compare_eql_type);
	TestBreak(test_compare_type_eql);
	TestBreak(test_compare_eql_specializers);
	TestBreak(test_sortcompare);
	TestBreak(test_specializer_sort);
	TestBreak(test_sortconsorder);
	TestBreak(test_specializer_sort_argument_precedence_order);
	TestBreak(test_sort_argument_precedence_order);
	TestBreak(test_specializers_sort);
	TestBreak(test_make_generic_function_type);
	TestBreak(test_mapcar_cons_class_of);
	TestBreak(test_make_generic_function_lambda_call);
	/* execute clos */
	TestBreak(test_execute_clos);

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
		build_package();
		build_clos(ptr);
		lisp_init = 1;
		result = testbreak_clos_generic();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

