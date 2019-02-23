#include "clos.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_object.h"
#include "clos_standard.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
#include "equal.h"
#include "function.h"
#include "hashtable.h"
#include "lambda.h"
#include "sequence.h"

/* clos_generic_call */
#define GetClosGenericCall(x,y)  (*(y) = *(clos_generic_call *)PtrBodySS(x))
#define SetClosGenericCall(x,y)  (*(clos_generic_call *)PtrBodySS(x) = (y))
#define GetClosGenericCallArray(x,i,y)  GetArraySS(x,i,y)
#define SetClosGenericCallArray(x,i,y)  SetArraySS(x,i,y)


/*****************************************************************************
 *  clos-generic-call
 *****************************************************************************/
void clos_generic_call_alloc(LocalRoot local,
		addr *ret, clos_generic_call call, int size)
{
	addr pos;

	Check(size < 0, "size error");
	Check(255 < size, "size error");
	alloc_smallsize(local, &pos, LISPTYPE_SYSTEM, size, sizeof(clos_generic_call));
	SetClosGenericCall(pos, call);
	*ret = pos;
}

void clos_generic_call_heap(addr *ret, clos_generic_call call, int size)
{
	clos_generic_call_alloc(NULL, ret, call, size);
}

void clos_generic_call_local(LocalRoot local,
		addr *ret, clos_generic_call call, int size)
{
	Check(local == NULL, "local error");
	clos_generic_call_alloc(local, ret, call, size);
}


/*****************************************************************************
 *  default method-combination
 *****************************************************************************/
static void combination_arguments_order(addr *ret, addr generic)
{
	addr cons, car, cdr, check;

	clos_elt(generic, Clos_generic_combination_arguments, &cons);
	if (cons == Nil) {
		*ret = Nil;
		return;
	}
	if (GetType(cons) != LISPTYPE_CONS) {
		fmte("The method-combination argument don't allow dot-list.", NULL);
		goto error;
	}
	GetCons(cons, &car, &cdr);
	if (cdr != Nil) {
		fmte("The method-combination argument ~S must be one.", cons, NULL);
		goto error;
	}
	GetConst(KEYWORD_MOST_SPECIFIC_FIRST, &check);
	if (car == check) {
		*ret = Nil;
		return;
	}
	GetConst(KEYWORD_MOST_SPECIFIC_LAST, &check);
	if (car == check) {
		*ret = T;
		return;
	}

	fmte("The order ~S must be a :most-specific-first or "
			":most-specific-last.", car, NULL);
error:
	*ret = NULL;
}

static void order_methods(LocalRoot local,
		addr *ret, addr order, addr methods)
{
	if (order == Nil)
		*ret = methods;
	else
		reverse_list_local_unsafe(local, ret, methods);
}

static void funcall_method(Execute ptr, addr car, addr cdr, addr rest)
{
	addr call;
	clos_elt(car, Clos_method_function, &call);
	funcall_control(ptr, call, car, cdr, rest, NULL);
}

static void funcall_around_primary(Execute ptr,
		addr rest, addr around, addr primary)
{
	append_cons_local_unsafe(ptr->local, &around, around, primary);
	GetCons(around, &around, &primary);
	funcall_method(ptr, around, primary, rest);
}

static void make_standard_method_for_standard_lambda(Execute ptr)
{
	addr args, data, before, primary, after, order, one, control, car, cdr;

	/*
	 *  (lambda (method next args)
	 *    (declare (ignore method next))
	 *    ...)
	 */
	getargs_control(ptr, 2, &args); /* args */

	/* closure */
	getdata_control(ptr, &data);
	GetCons(data, &before, &data);
	GetCons(data, &primary, &data);
	GetCons(data, &after, &data);
	GetCar(data, &order);

	/* before */
	while (before != Nil) {
		GetCons(before, &one, &before);
		funcall_method(ptr, one, Nil, args);
	}

	/* primary */
	setvalues_nil_control(ptr);
	order_methods(ptr->local, &primary, order, primary);
	GetCons(primary, &car, &cdr);
	funcall_method(ptr, car, cdr, args);

	/* after */
	if (after != Nil) {
		push_close_control(ptr, &control);
		while (after != Nil) {
			GetCons(after, &one, &before);
			funcall_method(ptr, one, Nil, args);
		}
		free_control(ptr, control);
	}
}

static void make_standard_method_for_standard(LocalRoot local,
		addr *ret, addr generic, addr before, addr primary, addr after, addr order)
{
	addr clos, call, data, name;

	clos_elt(generic, Clos_generic_method_class, &clos);
	GetConst(SYSTEM_STANDARD, &name);
	compiled_local(local, &call, name);
	setcompiled_any(call, make_standard_method_for_standard_lambda);
	list_local(local, &data, before, primary, after, order, NULL);
	SetDataFunction(call, data);
	make_instance_standard_method_function(local, ret, clos, call);
}

static void combination_lambda_standard_execute(Execute ptr,
		addr instance, addr generic, addr rest)
{
	addr data, around, before, primary, after, order, temp;
	LocalRoot local;

	GetClosGenericCallArray(instance, 0, &data);
	GetArrayA4(data, 0, &around);
	GetArrayA4(data, 1, &before);
	GetArrayA4(data, 2, &primary);
	GetArrayA4(data, 3, &after);
	combination_arguments_order(&order, generic);

	if (primary == Nil)
		fmte("The primary method must be more than one.", NULL);
	local = ptr->local;
	GetCdr(primary, &temp);
	if (before != Nil || after != Nil || temp != Nil) {
		make_standard_method_for_standard(local, &temp,
				generic, before, primary, after, order);
		list_local(local, &primary, temp, NULL);
	}
	funcall_around_primary(ptr, rest, around, primary);
}

static void combination_lambda_standard_call(Execute ptr,
		addr instance, addr generic, addr rest)
{
	addr control;

	push_return_control(ptr, &control);
	combination_lambda_standard_execute(ptr, instance, generic, rest);
	free_control(ptr, control);
}

static void combination_lambda_standard(addr *ret, addr data)
{
	addr pos;

	clos_generic_call_heap(&pos, combination_lambda_standard_call, 1);
	SetClosGenericCallArray(pos, 0, data);
	*ret = pos;
}

static void combination_lambda(addr *ret, addr generic, addr combination, addr data)
{
	addr name, check;

	clos_elt(combination, Clos_combination_name, &name);
	GetConst(COMMON_STANDARD, &check);
	if (name == check) {
		combination_lambda_standard(ret, data);
		return;
	}

	fmte("TODO: method-combination is not support yet.", NULL);
}


/*****************************************************************************
 *  finalize-generic-function
 *****************************************************************************/
int subclass_eql_specializer(addr left, addr right, int check)
{
	int check1, check2;

	check1 = eql_specializer_p(left);
	check2 = eql_specializer_p(right);
	if (check1 && check2) {
		return left == right;
	}
	if (check1) {
		clos_elt(left, Clos_specializer_type, &left);
		return std_subclass_p(left, right);
	}
	if (check2) {
		if (! check) return 0;
		clos_elt(right, Clos_specializer_type, &right);
		return std_subclass_p(left, right);
	}

	return std_subclass_p(left, right);
}

static int method_specializers_check(addr argtype, addr method)
{
	addr left, right;

	clos_elt(method, Clos_method_specializers, &method);
	while (argtype != Nil || method != Nil) {
		Check(argtype == Nil || method == Nil, "argument error");
		GetCons(argtype, &left, &argtype);
		GetCons(method, &right, &method);
		if (! subclass_eql_specializer(left, right, 0)) return 0;
	}

	return 1;
}

static int std_compareclass_p(addr left, addr right)
{
	if (left == right) return 0;
	if (std_subclass_p(left, right)) return -1;
	return 1;
}

static int compare_eql_specializer(addr left, addr right)
{
	if (left == right) return 0;
	clos_elt(left, Clos_specializer_object, &left);
	clos_elt(right, Clos_specializer_object, &right);
	fmte("The eql-specializers have a difference value ~S /= ~S.", left, right, NULL);
	return 1;
}

static int compare_eql_type(addr left, addr right)
{
	int check;
	clos_elt(left, Clos_specializer_type, &left);
	check = std_compareclass_p(left, right);
	return check == 0? -1: check;
}

static int compare_type_eql(addr left, addr right)
{
	int check;
	clos_elt(right, Clos_specializer_type, &right);
	check = std_compareclass_p(left, right);
	return check == 0? 1: check;
}

static int compare_eql_specializers(addr left, addr right)
{
	int check1, check2;

	check1 = eql_specializer_p(left);
	check2 = eql_specializer_p(right);
	if (check1 && check2)
		return compare_eql_specializer(left, right);
	if (check1)
		return compare_eql_type(left, right);
	if (check2)
		return compare_type_eql(left, right);
	if (left == right)
		return 0;

	return std_compareclass_p(left, right);
}

static int sortcompare(int (*call)(addr, addr), addr cdr1, addr cdr2)
{
	int value;
	addr car1, car2;

	for (;;) {
		if (cdr1 == Nil || cdr2 == Nil) break;
		GetCons(cdr1, &car1, &cdr1);
		GetCons(cdr2, &car2, &cdr2);
		value = call(car1, car2);
		if (value < 0) return 1;
		if (0 < value) return 0;
	}

	return 1;
}

static int specializer_sort(addr left, addr right)
{
	clos_elt(left, Clos_method_specializers, &left);
	clos_elt(right, Clos_method_specializers, &right);
	return sortcompare(compare_eql_specializers, left, right);
}

static int sortconsorder(addr order, int (*call)(addr, addr), addr cons1, addr cons2)
{
	int value;
	addr pos, car1, car2;
	size_t index;

	while (order != Nil) {
		GetCons(order, &pos, &order);
		GetIndex(pos, &index);
		nth_unsafe(&car1, index, cons1);
		nth_unsafe(&car2, index, cons2);
		value = call(car1, car2);
		if (value < 0) return 1;
		if (0 < value) return 0;
	}

	return 1;
}

static int specializer_sort_argument_precedence_order(addr order,
		addr left, addr right)
{
	clos_elt(left, Clos_method_specializers, &left);
	clos_elt(right, Clos_method_specializers, &right);
	return sortconsorder(order, compare_eql_specializers, left, right);
}

static void sort_argument_precedence_order(addr *ret, addr generic, addr filter)
{
	addr order;
	clos_elt(generic, Clos_generic_argument_precedence_order, &order);
	simplesort_info_cons_unsafe(ret, filter, order,
			specializer_sort_argument_precedence_order);
}

static void specializers_sort(addr *ret, addr generic, addr filter)
{
	if (clos_elt_boundp(generic, Clos_generic_argument_precedence_order))
		sort_argument_precedence_order(ret, generic, filter);
	else
		simplesort_cons_unsafe(ret, filter, specializer_sort);
}

static void make_generic_function_type(addr *ret, addr generic, addr argtype)
{
	addr array, data, combination, methods, method, cons;
	size_t size, index;

	clos_elt(generic, Clos_generic_method_combination, &combination);
	clos_elt(generic, Clos_generic_methods, &array);
	LenArrayA4(array, &size);
	vector4_heap(&data, size);
	for (index = 0; index < size; index++) {
		GetArrayA4(array, index, &methods);
		/* remove-if-not */
		for (cons = Nil; methods != Nil; ) {
			GetCons(methods, &method, &methods);
			if (method_specializers_check(argtype, method))
				cons_heap(&cons, method, cons);
		}
		/* sort */
		specializers_sort(&cons, generic, cons);
		SetArrayA4(data, index, cons);
	}
	/* combination */
	combination_lambda(ret, generic, combination, data);
}

static void mapcar_cons_class_of(LocalRoot local,
		addr *ret, addr eqlchecks, addr args)
{
	addr result, eqlcheck, arg, check;

	for (result = Nil; eqlchecks != Nil; ) {
		GetCons(eqlchecks, &eqlcheck, &eqlchecks);
		if (args == Nil)
			fmte("Too few arguments.", NULL);
		GetCons(args, &arg, &args);
		check = eqlcheck != Nil? find_eql_specializer_nil(arg): Nil;
		if (check == Nil)
			class_of(arg, &check);
		cons_local(local, &result, check, result);
	}
	nreverse_list_unsafe(ret, result);
}

static void make_generic_function_lambda_call(Execute ptr,
		addr instance, addr generic, addr args)
{
	addr eqlcheck, cache, key, value, cons;
	LocalRoot local;
	LocalStack stack;
	clos_generic_call call;

	local = ptr->local;
	push_local(local, &stack);

	GetClosGenericCallArray(instance, 0, &eqlcheck);
	GetClosGenericCallArray(instance, 1, &cache);
	mapcar_cons_class_of(local, &key, eqlcheck, args);
	if (findvalue_hashtable(cache, key, &value)) {
		/* not found, tranlate to heap-list from dynamic list */
		copy_list_heap_unsafe(&key, key);
		make_generic_function_type(&value, generic, key);
		intern_hashheap(cache, key, &cons);
		SetCdr(cons, value);
	}
	rollback_local(local, stack);

	/* clos_generic_call */
	GetClosGenericCall(value, &call);
	call(ptr, value, generic, args);
}

static void make_generic_function_lambda(addr generic, addr *ret)
{
	addr eqlcheck, cache, call;

	clos_elt(generic, Clos_generic_eqlcheck, &eqlcheck);
	clos_elt(generic, Clos_generic_cache, &cache);
	clos_generic_call_heap(&call, make_generic_function_lambda_call, 2);
	SetClosGenericCallArray(call, 0, eqlcheck);
	SetClosGenericCallArray(call, 1, cache);
	*ret = call;
}

static void generic_function_no_method(Execute ptr,
		addr instance, addr generic, addr right)
{
	fmte("This generic function have no method.", NULL);
}

static void make_generic_function_call(Execute ptr, addr generic, addr *ret)
{
	addr call;

	if (clos_elt_boundp(generic, Clos_generic_eqlcheck)) {
		make_generic_function_lambda(generic, ret);
	}
	else {
		GetConst(CLOSDATA_NO_METHOD, &call);
		if (call == Nil) {
			clos_generic_call_heap(&call, generic_function_no_method, 0);
			SetConstant(CONSTANT_CLOSDATA_NO_METHOD, call);
		}
		*ret = call;
	}
}

void std_finalize_generic_function(Execute ptr, addr generic)
{
	addr pos;

	make_generic_function_call(ptr, generic, &pos);
	setf_clos_elt(generic, Clos_generic_call, pos);
}


/*****************************************************************************
 *  execute clos
 *****************************************************************************/
static void execute_standard_generic_function(Execute ptr,
		addr clos, addr args)
{
	addr pos;
	clos_generic_call call;

	clos_elt(clos, Clos_generic_call, &pos);
	Check(GetType(pos) != LISPTYPE_SYSTEM, "type error");
	GetClosGenericCall(pos, &call);
	call(ptr, pos, clos, args);
}

int funcallable_p(addr pos)
{
	addr clos;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &clos);

	return std_subtype_p(pos, clos);
}

void execute_clos(Execute ptr, addr pos, addr args)
{
	addr gfclass, class_of;

	Check(! funcallable_p(pos), "type error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &gfclass);
	clos_class_of(pos, &class_of);
	if (gfclass == class_of) {
		execute_standard_generic_function(ptr, pos, args);
	}
	else {
		fmte("The clos system can't run this funcallable instance ~S.", pos, NULL);
	}
}


/*****************************************************************************
 *  defgeneric
 *****************************************************************************/
static void parse_input_name(addr *ret, addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME)
		*ret = name;
	else
		parse_callname_heap(ret, name);
}

static void make_generic_function_cache(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_CACHE,
			8,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

static void make_methods_array(addr *ret, addr combination)
{
	size_t size;
	method_combination_qualifiers_count(combination, &size);
	vector4_heap(ret, size);
}

void generic_function_instance(Execute ptr, addr *ret, addr name, addr lambda)
{
	addr clos, instance, pos, combination;

	parse_input_name(&name, name);
	clos = find_generic_function_nil(name);
	if (clos != Nil)
		fmte("TODO: The generic-function already exists. (no implemented yet)", NULL);
	GetConst(COMMON_STANDARD, &combination);
	combination = find_method_combination(combination);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &clos);
	make_instance_restrict_heap(clos, &instance);
	std_update_class_of(instance, clos);
	/* setf name */
	setf_clos_elt(instance, Clos_generic_name, name);
	/* setf lambda-list */
	lambda_generic_function(ptr->local, &lambda, lambda);
	setf_clos_elt(instance, Clos_generic_lambda_list, lambda);
	/* setf order */
	setf_clos_elt(instance, Clos_generic_argument_precedence_order, Unbound);
	/* setf methods */
	make_methods_array(&pos, combination);
	setf_clos_elt(instance, Clos_generic_methods, pos);
	/* setf method-class */
	GetConst(CLOS_STANDARD_METHOD, &pos);
	setf_clos_elt(instance, Clos_generic_method_class, pos);
	/* setf method-combination */
	setf_clos_elt(instance, Clos_generic_method_combination, combination);
	/* setf combination-arguments */
	setf_clos_elt(instance, Clos_generic_combination_arguments, Nil);
	/* setf cache */
	make_generic_function_cache(&pos);
	setf_clos_elt(instance, Clos_generic_cache, pos);
	/* setf call */
	setf_clos_elt(instance, Clos_generic_call, Nil);
	/* finalize */
	std_finalize_generic_function(ptr, instance);
	/* setf fdefinition */
	setcallname_global(name, instance);
	*ret = instance;
}

