#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
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
typedef int (*clos_generic_call)(Execute, addr, addr, addr);
#define GetClosGenericCall(x,y)  (*(y) = *(clos_generic_call *)PtrBodySS(x))
#define SetClosGenericCall(x,y)  (*(clos_generic_call *)PtrBodySS(x) = (y))
#define GetClosGenericCallArray(x,i,y)  GetArraySS(x,i,y)
#define SetClosGenericCallArray(x,i,y)  SetArraySS(x,i,y)

/*
 *  standard-generic-function
 */
static void stdget_generic_constant(addr pos, addr *ret,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_generic_constant(addr pos, addr value,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetGeneric(p,r,a,b) \
	stdget_generic_constant((p), (r), Clos_generic_##a, CONSTANT_CLOSKEY_##b)
#define StdSetGeneric(p,r,a,b) \
	stdset_generic_constant((p), (r), Clos_generic_##a, CONSTANT_CLOSKEY_##b)

void stdget_generic_name(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, name, NAME);
}
void stdset_generic_name(addr pos, addr value)
{
	StdSetGeneric(pos, value, name, NAME);
}

void stdget_generic_lambda_list(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, lambda_list, LAMBDA_LIST);
}
void stdset_generic_lambda_list(addr pos, addr value)
{
	StdSetGeneric(pos, value, lambda_list, LAMBDA_LIST);
}

void stdget_generic_methods(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, methods, METHODS);
}
void stdset_generic_methods(addr pos, addr value)
{
	StdSetGeneric(pos, value, methods, METHODS);
}

void stdget_generic_method_class(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, method_class, METHOD_CLASS);
}
void stdset_generic_method_class(addr pos, addr value)
{
	StdSetGeneric(pos, value, method_class, METHOD_CLASS);
}

void stdget_generic_argument_precedence_order(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}
void stdset_generic_argument_precedence_order(addr pos, addr value)
{
	StdSetGeneric(pos, value, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

void stdget_generic_declarations(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, declarations, DECLARATIONS);
}
void stdset_generic_declarations(addr pos, addr value)
{
	StdSetGeneric(pos, value, declarations, DECLARATIONS);
}

void stdget_generic_method_combination(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, method_combination, METHOD_COMBINATION);
}
void stdset_generic_method_combination(addr pos, addr value)
{
	StdSetGeneric(pos, value, method_combination, METHOD_COMBINATION);
}

void stdget_generic_combination_arguments(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, combination_arguments, COMBINATION_ARGUMENTS);
}
void stdset_generic_combination_arguments(addr pos, addr value)
{
	StdSetGeneric(pos, value, combination_arguments, COMBINATION_ARGUMENTS);
}

void stdget_generic_eqlcheck(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, eqlcheck, EQLCHECK);
}
void stdset_generic_eqlcheck(addr pos, addr value)
{
	StdSetGeneric(pos, value, eqlcheck, EQLCHECK);
}

void stdget_generic_cache(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, cache, CACHE);
}
void stdset_generic_cache(addr pos, addr value)
{
	StdSetGeneric(pos, value, cache, CACHE);
}

void stdget_generic_call(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, call, CALL);
}
void stdset_generic_call(addr pos, addr value)
{
	StdSetGeneric(pos, value, call, CALL);
}

static int stdboundp_generic_constant(addr pos,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	if (clos == check) {
		clos_getelt(pos, (size_t)index1, &pos);
		return pos != Unbound;
	}
	else {
		GetConstant(index2, &check);
		return clos_slot_boundp(pos, check);
	}
}

int stdboundp_generic_argument_precedence_order(addr pos)
{
	return stdboundp_generic_constant(pos,
			Clos_generic_argument_precedence_order,
			CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER);
}

int stdboundp_generic_eqlcheck(addr pos)
{
	return stdboundp_generic_constant(pos,
			Clos_generic_eqlcheck,
			CONSTANT_CLOSKEY_EQLCHECK);
}


/*
 *  eql-specializer
 */
static void stdget_specializer_constant(addr pos, addr *ret,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_EQL_SPECIALIZER, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_specializer_constant(addr pos, addr value,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_EQL_SPECIALIZER, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetSpecializer(p,r,a,b) \
	stdget_specializer_constant((p), (r), Clos_specializer_##a, CONSTANT_CLOSKEY_##b)
#define StdSetSpecializer(p,r,a,b) \
	stdset_specializer_constant((p), (r), Clos_specializer_##a, CONSTANT_CLOSKEY_##b)

void stdget_specializer_object(addr pos, addr *ret)
{
	StdGetSpecializer(pos, ret, object, OBJECT);
}
void stdset_specializer_object(addr pos, addr value)
{
	StdSetSpecializer(pos, value, object, OBJECT);
}

void stdget_specializer_type(addr pos, addr *ret)
{
	StdGetSpecializer(pos, ret, type, TYPE);
}
void stdset_specializer_type(addr pos, addr value)
{
	StdSetSpecializer(pos, value, type, TYPE);
}


/*****************************************************************************
 *  clos-generic-call
 *****************************************************************************/
static void clos_generic_call_alloc(LocalRoot local,
		addr *ret, clos_generic_call call, int size)
{
	addr pos;

	Check(size < 0, "size error");
	Check(255 < size, "size error");
	alloc_smallsize(local, &pos,
			LISPSYSTEM_GENERIC, size, sizeoft(clos_generic_call));
	SetClosGenericCall(pos, call);
	*ret = pos;
}

static void clos_generic_call_heap(addr *ret, clos_generic_call call, int size)
{
	clos_generic_call_alloc(NULL, ret, call, size);
}


/*****************************************************************************
 *  default method-combination
 *****************************************************************************/
static void comb_standard_getorder(addr *ret, addr generic)
{
	addr cons, car, cdr, check;

	stdget_generic_combination_arguments(generic, &cons);
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

static void comb_standard_order(LocalRoot local,
		addr *ret, addr order, addr methods)
{
	if (order == Nil)
		*ret = methods;
	else
		reverse_list_local_unsafe(local, ret, methods);
}

static int comb_standard_method(Execute ptr, addr car, addr cdr, addr rest)
{
	addr call, args;
	LocalRoot local;
	LocalStack stack;

	stdget_method_function(car, &call);
	local = ptr->local;
	push_local(local, &stack);
	lista_local(local, &args, car, cdr, rest, NULL);
	if (apply_control(ptr, call, args))
		return 1;
	rollback_local(local, stack);

	return 0;
}

static int comb_standard_funcall(Execute ptr, addr rest, addr around, addr primary)
{
	append_cons_local_unsafe(ptr->local, &around, around, primary);
	GetCons(around, &around, &primary);
	return comb_standard_method(ptr, around, primary, rest);
}

static void comb_standard_lambda(Execute ptr)
{
	addr args, data, before, primary, after, order, one, control, car, cdr;

	/*
	 *  (lambda (method next &rest args)
	 *    (declare (ignore method next))
	 *    ...)
	 */
	getargs_list_control_unsafe(ptr, 2, &args);

	/* closure */
	getdata_control(ptr, &data);
	GetCons(data, &before, &data);
	GetCons(data, &primary, &data);
	GetCons(data, &after, &data);
	GetCar(data, &order);

	/* before */
	while (before != Nil) {
		GetCons(before, &one, &before);
		comb_standard_method(ptr, one, Nil, args);
	}

	/* primary */
	setvalues_nil_control(ptr);
	comb_standard_order(ptr->local, &primary, order, primary);
	GetCons(primary, &car, &cdr);
	comb_standard_method(ptr, car, cdr, args);

	/* after */
	if (after != Nil) {
		push_close_control(ptr, &control);
		while (after != Nil) {
			GetCons(after, &one, &before);
			comb_standard_method(ptr, one, Nil, args);
		}
		free_control(ptr, control);
	}
}

static void comb_standard_qualifiers(LocalRoot local,
		addr *ret, addr generic, addr before, addr primary, addr after, addr order)
{
	addr clos, call, data, name;

	stdget_generic_method_class(generic, &clos);
	GetConst(SYSTEM_STANDARD, &name);
	compiled_local(local, &call, name);
	setcompiled_any(call, comb_standard_lambda);
	list_local(local, &data, before, primary, after, order, NULL);
	SetDataFunction(call, data);
	method_instance_call(local, ret, clos, call);
}

static void comb_standard_call_error(Execute ptr, addr instance, addr generic)
{
	/* TODO: no-applicable-method */
	infoprint(generic);
	infoprint(instance);
	stdget_generic_name(generic, &generic);
	infoprint(generic);
	fmte("There is no primary method.", NULL);
}

static int comb_standard_execute(Execute ptr, addr instance, addr generic, addr rest)
{
	addr data, around, before, primary, after, order, temp;
	LocalRoot local;

	GetClosGenericCallArray(instance, 0, &data);
	GetArrayA4(data, 0, &around);
	GetArrayA4(data, 1, &before);
	GetArrayA4(data, 2, &primary);
	GetArrayA4(data, 3, &after);
	comb_standard_getorder(&order, generic);

	if (primary == Nil) {
		comb_standard_call_error(ptr, instance, generic);
		return 0;
	}
	local = ptr->local;
	GetCdr(primary, &temp);
	if (before != Nil || after != Nil || temp != Nil) {
		comb_standard_qualifiers(local, &temp, generic, before, primary, after, order);
		list_local(local, &primary, temp, NULL);
	}
	return comb_standard_funcall(ptr, rest, around, primary);
}

static int comb_standard_call(Execute ptr, addr instance, addr generic, addr rest)
{
	int result;
	addr control;

	push_return_control(ptr, &control);
	result = comb_standard_execute(ptr, instance, generic, rest);
	free_control(ptr, control);

	return result;
}

static void comb_standard(addr *ret, addr data)
{
	addr pos;

	clos_generic_call_heap(&pos, comb_standard_call, 1);
	SetClosGenericCallArray(pos, 0, data);
	*ret = pos;
}

static void comb_lambda(addr *ret, addr generic, addr combination, addr data)
{
	addr name, check;

	stdget_combination_name(combination, &name);
	GetConst(COMMON_STANDARD, &check);
	if (name == check) {
		comb_standard(ret, data);
		return;
	}

	fmte("TODO: method-combination is not support yet.", NULL);
}


/*****************************************************************************
 *  generic-finalize
 *****************************************************************************/
int generic_eql_specializer(addr left, addr right, int check)
{
	int check1, check2;

	check1 = clos_specializer_p(left);
	check2 = clos_specializer_p(right);
	if (check1 && check2) {
		return left == right;
	}
	if (check1) {
		stdget_specializer_type(left, &left);
		return clos_subclass_p(left, right);
	}
	if (check2) {
		if (! check) return 0;
		stdget_specializer_type(right, &right);
		return clos_subclass_p(left, right);
	}

	return clos_subclass_p(left, right);
}

static int generic_make_method_check(addr argtype, addr method)
{
	addr left, right;

	stdget_method_specializers(method, &method);
	while (argtype != Nil || method != Nil) {
		Check(argtype == Nil || method == Nil, "argument error");
		GetCons(argtype, &left, &argtype);
		GetCons(method, &right, &method);
		if (! generic_eql_specializer(left, right, 0)) return 0;
	}

	return 1;
}

static int generic_compare_class(addr left, addr right)
{
	if (left == right) return 0;
	if (clos_subclass_p(left, right)) return -1;
	return 1;
}

static int generic_compare_eql(addr left, addr right)
{
	if (left == right) return 0;
	stdget_specializer_object(left, &left);
	stdget_specializer_object(right, &right);
	fmte("The eql-specializers have a difference value ~S /= ~S.", left, right, NULL);
	return 1;
}

static int generic_compare_eql_type(addr left, addr right)
{
	int check;
	stdget_specializer_type(left, &left);
	check = generic_compare_class(left, right);
	return check == 0? -1: check;
}

static int generic_compare_type_eql(addr left, addr right)
{
	int check;
	stdget_specializer_type(right, &right);
	check = generic_compare_class(left, right);
	return check == 0? 1: check;
}

static int generic_compare_specializer(addr left, addr right)
{
	int check1, check2;

	check1 = clos_specializer_p(left);
	check2 = clos_specializer_p(right);
	if (check1 && check2)
		return generic_compare_eql(left, right);
	if (check1)
		return generic_compare_eql_type(left, right);
	if (check2)
		return generic_compare_type_eql(left, right);
	if (left == right)
		return 0;

	return generic_compare_class(left, right);
}

static int generic_sort_compare(int (*call)(addr, addr), addr a, addr b)
{
	int value;
	addr x, y;

	for (;;) {
		if (a == Nil || b == Nil) break;
		GetCons(a, &x, &a);
		GetCons(b, &y, &b);
		value = call(x, y);
		if (value < 0) return 1;
		if (0 < value) return 0;
	}

	return 1;
}

static int generic_sort_call(addr left, addr right)
{
	stdget_method_specializers(left, &left);
	stdget_method_specializers(right, &right);
	return generic_sort_compare(generic_compare_specializer, left, right);
}

static int generic_sort(addr order, int (*call)(addr, addr), addr a, addr b)
{
	int value;
	addr pos, x, y;
	size_t i;

	while (order != Nil) {
		GetCons(order, &pos, &order);
		GetIndex(pos, &i);
		nth_unsafe(&x, i, a);
		nth_unsafe(&y, i, b);
		value = call(x, y);
		if (value < 0) return 1;
		if (0 < value) return 0;
	}

	return 1;
}

static int generic_sort_order_call(addr order, addr left, addr right)
{
	stdget_method_specializers(left, &left);
	stdget_method_specializers(right, &right);
	return generic_sort(order, generic_compare_specializer, left, right);
}

static void generic_specializer_order(addr *ret, addr gen, addr list)
{
	addr order;
	stdget_generic_argument_precedence_order(gen, &order);
	simplesort_info_cons_unsafe(ret, list, order, generic_sort_order_call);
}

static void generic_specializers_sort(addr *ret, addr gen, addr list)
{
	if (stdboundp_generic_argument_precedence_order(gen))
		generic_specializer_order(ret, gen, list);
	else
		simplesort_cons_unsafe(ret, list, generic_sort_call);
}

static void generic_make_type(addr *ret, addr gen, addr type)
{
	addr array, data, comb, methods, method, list;
	size_t size, index;

	stdget_generic_method_combination(gen, &comb);
	stdget_generic_methods(gen, &array);
	LenArrayA4(array, &size);
	vector4_heap(&data, size);
	for (index = 0; index < size; index++) {
		GetArrayA4(array, index, &methods);
		/* remove-if-not */
		for (list = Nil; methods != Nil; ) {
			GetCons(methods, &method, &methods);
			if (generic_make_method_check(type, method))
				cons_heap(&list, method, list);
		}
		/* sort */
		generic_specializers_sort(&list, gen, list);
		SetArrayA4(data, index, list);
	}
	/* combination */
	comb_lambda(ret, gen, comb, data);
}

static void generic_make_mapcar_class_of(LocalRoot local,
		addr *ret, addr list, addr args)
{
	addr result, eqlcheck, arg, check;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &eqlcheck, &list);
		if (args == Nil)
			fmte("Too few arguments.", NULL);
		GetCons(args, &arg, &args);
		if (eqlcheck == Nil)
			check = Nil;
		else
			clos_find_specializer_nil(arg, &check);
		if (check == Nil)
			clos_class_of(arg, &check);
		cons_local(local, &result, check, result);
	}
	nreverse_list_unsafe(ret, result);
}

static int generic_make_lambda_call(Execute ptr, addr inst, addr gen, addr args)
{
	addr eqlcheck, cache, key, value, cons;
	LocalRoot local;
	LocalStack stack;
	clos_generic_call call;

	local = ptr->local;
	push_local(local, &stack);

	GetClosGenericCallArray(inst, 0, &eqlcheck);
	GetClosGenericCallArray(inst, 1, &cache);
	generic_make_mapcar_class_of(local, &key, eqlcheck, args);
	if (findvalue_hashtable(cache, key, &value)) {
		/* not found, tranlate to heap-list from dynamic list */
		copy_list_heap_unsafe(&key, key);
		generic_make_type(&value, gen, key);
		intern_hashheap(cache, key, &cons);
		SetCdr(cons, value);
	}
	rollback_local(local, stack);

	/* clos_generic_call */
	GetClosGenericCall(value, &call);
	return (*call)(ptr, value, gen, args);
}

static void generic_make_lambda(addr gen, addr *ret)
{
	addr eqlcheck, cache, call;

	stdget_generic_eqlcheck(gen, &eqlcheck);
	stdget_generic_cache(gen, &cache);
	clos_generic_call_heap(&call, generic_make_lambda_call, 2);
	SetClosGenericCallArray(call, 0, eqlcheck);
	SetClosGenericCallArray(call, 1, cache);
	*ret = call;
}

static int generic_no_method(Execute ptr, addr inst, addr gen, addr pos)
{
	fmte("GENERIC-FUNCTION ~S have no method.", gen, NULL);
	return 0;
}

static void generic_make_generic_call(addr gen, addr *ret)
{
	addr call;

	if (stdboundp_generic_eqlcheck(gen)) {
		generic_make_lambda(gen, ret);
		return;
	}

	GetConst(CLOSDATA_NO_METHOD, &call);
	if (call == Nil) {
		clos_generic_call_heap(&call, generic_no_method, 0);
		SetConstant(CONSTANT_CLOSDATA_NO_METHOD, call);
	}
	*ret = call;
}

void generic_finalize(addr gen)
{
	addr pos;
	generic_make_generic_call(gen, &pos);
	stdset_generic_call(gen, pos);
}


/*****************************************************************************
 *  execute clos
 *****************************************************************************/
static void closrun_standard(Execute ptr, addr clos, addr args)
{
	addr pos;
	clos_generic_call call;

	stdget_generic_call(clos, &pos);
	CheckType(pos, LISPSYSTEM_GENERIC);
	GetClosGenericCall(pos, &call);
	call(ptr, pos, clos, args);
}

int funcallable_p(addr pos)
{
	addr clos;

	if (! closp(pos)) return 0;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &clos);

	return clos_subtype_p(pos, clos);
}

void closrun_execute(Execute ptr, addr pos, addr args)
{
	addr check, class_of;

	Check(! funcallable_p(pos), "type error");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &check);
	clos_class_of(pos, &class_of);
	if (check == class_of)
		closrun_standard(ptr, pos, args);
	else
		fmte("Cannot exexute a funcallable instance ~S.", pos, NULL);
}


/*****************************************************************************
 *  defgeneric
 *****************************************************************************/
static void generic_instance_name(addr *ret, addr name)
{
	if (GetType(name) == LISPTYPE_CALLNAME)
		*ret = name;
	else
		parse_callname_heap(ret, name);
}

static void generic_instance_cache(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_CACHE,
			8,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

static void generic_instance_methods(addr *ret, addr combination)
{
	size_t size;
	method_combination_qualifiers_count(combination, &size);
	vector4_heap(ret, size);
}

void generic_instance_heap(LocalRoot local, addr *ret, addr name, addr lambda)
{
	addr pos, comb, methods, method, cache;

	generic_instance_name(&name, name);
	clos_find_generic_nil(name, &pos);
	if (pos != Nil)
		fmte("TODO: The generic-function already exists. (no implemented yet)", NULL);
	GetConst(COMBINATION_STANDARD, &comb);
	argument_generic_heap(local, &lambda, lambda);
	generic_instance_methods(&methods, comb);
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_instance_cache(&cache);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	stdset_generic_name(pos, name);
	stdset_generic_lambda_list(pos, lambda);
	stdset_generic_argument_precedence_order(pos, Unbound);
	stdset_generic_methods(pos, methods);
	stdset_generic_method_class(pos, method);
	stdset_generic_method_combination(pos, comb);
	stdset_generic_combination_arguments(pos, Nil);
	stdset_generic_cache(pos, cache);
	stdset_generic_call(pos, Nil);

	/* result */
	generic_finalize(pos);
	setcallname_global(name, pos);
	*ret = pos;
}

void generic_common_instance(addr *ret, addr name, addr args)
{
	addr pos, comb, methods, method, cache;

	CheckType(name, LISPTYPE_CALLNAME);
	CheckType(args, LISPSYSTEM_ARGUMENT);
#ifdef LISP_DEBUG
	clos_find_generic_nil(name, &pos);
	Check(pos != Nil, "generic function error");
#endif
	GetConst(COMBINATION_STANDARD, &comb);
	generic_instance_methods(&methods, comb);
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_instance_cache(&cache);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	stdset_generic_name(pos, name);
	stdset_generic_lambda_list(pos, args);
	stdset_generic_argument_precedence_order(pos, Unbound);
	stdset_generic_methods(pos, methods);
	stdset_generic_method_class(pos, method);
	stdset_generic_method_combination(pos, comb);
	stdset_generic_combination_arguments(pos, Nil);
	stdset_generic_cache(pos, cache);
	stdset_generic_call(pos, Nil);

	/* result */
	setcallname_global(name, pos);
	*ret = pos;
}

