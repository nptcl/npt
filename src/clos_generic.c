#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control.h"
#include "define.h"
#include "equal.h"
#include "function.h"
#include "gc.h"
#include "hashtable.h"
#include "lambda.h"
#include "pointer.h"
#include "sequence.h"
#include "sort.h"
#include "symbol.h"

/* clos_generic_call */
enum ClosGenericIndex {
#ifdef LISP_DEGRADE
	gen_debug,
#endif
	gen_comb_standard_call,
	gen_comb_define_call,
	gen_generic_make_lambda_call,
	gen_generic_no_method,
	gen_size
};
typedef int (*clos_generic_call)(Execute, addr, addr, addr);
static clos_generic_call ClosGenericTable[gen_size];
#define CallClosGenericCall(x,y) \
	(*(y) = ClosGenericTable[*(enum ClosGenericIndex *)PtrBodySS(x)])
#define GetClosGenericCall(x,y)  (*(y) = *(enum ClosGenericIndex *)PtrBodySS(x))
#define SetClosGenericCall(x,y)  (*(enum ClosGenericIndex *)PtrBodySS(x) = (y))
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
		clos_getelt(pos, (size_t)index1, &check);
	}
	else {
		GetConstant(index2, &check);
		clos_get(pos, check, &check);
	}
	/* Unbound check */
	if (check == Unbound)
		_fmte("There is no applicable methods in ~S.", pos, NULL);
	*ret = check;
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
	stdget_generic_constant((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)
#define StdSetGeneric(p,r,a,b) \
	stdset_generic_constant((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

_g void stdget_generic_name(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, name, NAME);
}
_g void stdset_generic_name(addr pos, addr value)
{
	StdSetGeneric(pos, value, name, NAME);
}

_g void stdget_generic_lambda_list(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, lambda_list, LAMBDA_LIST);
}
_g void stdset_generic_lambda_list(addr pos, addr value)
{
	StdSetGeneric(pos, value, lambda_list, LAMBDA_LIST);
}

_g void stdget_generic_methods(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, methods, METHODS);
}
_g void stdset_generic_methods(addr pos, addr value)
{
	StdSetGeneric(pos, value, methods, METHODS);
}

_g void stdget_generic_method_class(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, method_class, METHOD_CLASS);
}
_g void stdset_generic_method_class(addr pos, addr value)
{
	StdSetGeneric(pos, value, method_class, METHOD_CLASS);
}

_g void stdget_generic_argument_precedence_order(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}
_g void stdset_generic_argument_precedence_order(addr pos, addr value)
{
	StdSetGeneric(pos, value, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

_g void stdget_generic_declarations(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, declarations, DECLARATIONS);
}
_g void stdset_generic_declarations(addr pos, addr value)
{
	StdSetGeneric(pos, value, declarations, DECLARATIONS);
}

_g void stdget_generic_method_combination(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, method_combination, METHOD_COMBINATION);
}
_g void stdset_generic_method_combination(addr pos, addr value)
{
	StdSetGeneric(pos, value, method_combination, METHOD_COMBINATION);
}

_g void stdget_generic_eqlcheck(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, eqlcheck, EQLCHECK);
}
_g void stdset_generic_eqlcheck(addr pos, addr value)
{
	StdSetGeneric(pos, value, eqlcheck, EQLCHECK);
}

_g void stdget_generic_cache(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, cache, CACHE);
}
_g void stdset_generic_cache(addr pos, addr value)
{
	StdSetGeneric(pos, value, cache, CACHE);
}

_g void stdget_generic_call(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, call, CALL);
}
_g void stdset_generic_call(addr pos, addr value)
{
	StdSetGeneric(pos, value, call, CALL);
}

_g void stdget_generic_precedence_index(addr pos, addr *ret)
{
	StdGetGeneric(pos, ret, precedence_index, PRECEDENCE_INDEX);
}
_g void stdset_generic_precedence_index(addr pos, addr value)
{
	StdSetGeneric(pos, value, precedence_index, PRECEDENCE_INDEX);
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

_g int stdboundp_generic_argument_precedence_order(addr pos)
{
	return stdboundp_generic_constant(pos,
			Clos_generic_argument_precedence_order,
			CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER);
}

_g int stdboundp_generic_eqlcheck(addr pos)
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
	stdget_specializer_constant((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)
#define StdSetSpecializer(p,r,a,b) \
	stdset_specializer_constant((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

_g void stdget_specializer_object(addr pos, addr *ret)
{
	StdGetSpecializer(pos, ret, object, OBJECT);
}
_g void stdset_specializer_object(addr pos, addr value)
{
	StdSetSpecializer(pos, value, object, OBJECT);
}

_g void stdget_specializer_type(addr pos, addr *ret)
{
	StdGetSpecializer(pos, ret, type, TYPE);
}
_g void stdset_specializer_type(addr pos, addr value)
{
	StdSetSpecializer(pos, value, type, TYPE);
}


/*****************************************************************************
 *  clos-generic-call
 *****************************************************************************/
static void clos_generic_call_heap(addr *ret, enum ClosGenericIndex call, int size)
{
	addr pos;

	Check(size < 0, "size error");
	Check(255 < size, "size error");
	heap_smallsize(&pos, LISPSYSTEM_GENERIC, size, sizeoft(enum ClosGenericIndex));
	SetClosGenericCall(pos, call);
	*ret = pos;
}


/*****************************************************************************
 *  default method-combination
 *****************************************************************************/
/*
 *  standard
 */
static int comb_standard_method_(Execute ptr, addr car, addr cdr, addr rest)
{
	addr call;
	stdget_method_function(car, &call);
	return applya_control(ptr, call, car, cdr, rest, NULL);
}

static int comb_standard_funcall(Execute ptr, addr rest, addr around, addr primary)
{
	append2_local_unsafe(ptr->local, around, primary, &around);
	GetCons(around, &around, &primary);
	return comb_standard_method_(ptr, around, primary, rest);
}

static int function_standard_lambda(Execute ptr)
{
	addr args, data, before, primary, after, one, control, car, cdr;

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
	GetCar(data, &after);

	/* before */
	while (before != Nil) {
		GetCons(before, &one, &before);
		Return(comb_standard_method_(ptr, one, Nil, args));
	}

	/* primary */
	setvalues_nil_control(ptr);
	GetCons(primary, &car, &cdr);
	Return(comb_standard_method_(ptr, car, cdr, args));

	/* after */
	if (after != Nil) {
		push_close_control(ptr, &control);
		while (after != Nil) {
			GetCons(after, &one, &after);
			Return(comb_standard_method_(ptr, one, Nil, args));
		}
		Return(free_control(ptr, control));
	}

	return 0;
}

static void comb_standard_qualifiers(LocalRoot local,
		addr *ret, addr gen, addr before, addr primary, addr after)
{
	addr clos, call, data, name;

	stdget_generic_method_class(gen, &clos);
	GetConst(SYSTEM_STANDARD, &name);
	compiled_local(local, &call, name);
	setcompiled_any(call, p_defun_standard_lambda);
	list_local(local, &data, before, primary, after, NULL);
	SetDataFunction(call, data);
	method_instance_call(local, ret, clos, call);
}

static int generic_no_applicable_method(Execute ptr, addr gen, addr args)
{
	addr call;

	/* call (no-applicable-method generic-function . args) */
	GetConst(COMMON_NO_APPLICABLE_METHOD, &call);
	getfunctioncheck_local(ptr, call, &call);
	return applya_control(ptr, call, gen, args, NULL);
}

static int comb_standard_execute(Execute ptr, addr inst, addr gen, addr rest)
{
	addr data, around, before, primary, after, temp;
	LocalRoot local;

	GetClosGenericCallArray(inst, 0, &data);
	GetArrayA4(data, Clos_standard_around, &around);
	GetArrayA4(data, Clos_standard_before, &before);
	GetArrayA4(data, Clos_standard_primary, &primary);
	GetArrayA4(data, Clos_standard_after, &after);

	if (primary == Nil)
		return generic_no_applicable_method(ptr, gen, rest);
	local = ptr->local;
	GetCdr(primary, &temp);
	if (before != Nil || after != Nil || temp != Nil) {
		comb_standard_qualifiers(local, &temp, gen, before, primary, after);
		list_local(local, &primary, temp, NULL);
	}
	return comb_standard_funcall(ptr, rest, around, primary);
}

static int comb_standard_call(Execute ptr, addr inst, addr gen, addr rest)
{
	int result;
	addr control;

	push_return_control(ptr, &control);
	result = comb_standard_execute(ptr, inst, gen, rest);
	free_control(ptr, control);

	return result;
}

static int comb_standard(addr *ret, addr data)
{
	addr pos;

	clos_generic_call_heap(&pos, gen_comb_standard_call, 1);
	SetClosGenericCallArray(pos, 0, data);
	*ret = pos;

	return 0;
}


/*
 *  execute combination
 */
static int comb_define_call(Execute ptr, addr inst, addr gen, addr rest)
{
	addr control, call;

	push_return_control(ptr, &control);
	GetClosGenericCallArray(inst, 0, &call);
	if (apply_control(ptr, call, rest)) return 1;
	free_control(ptr, control);

	return 0;
}

static int comb_long(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	if (comb_longform(ptr, &data, gen, comb, data))
		return 1;
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);
	*ret = pos;

	return 0;
}

static int comb_short(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	if (comb_shortform(ptr, &data, gen, comb, data))
		return 1;
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);
	*ret = pos;

	return 0;
}

static int comb_lambda(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	/* standard */
	if (comb == Nil)
		return comb_standard(ret, data);

	/* long-form */
	if (clos_long_combination_p(comb))
		return comb_long(ptr, ret, gen, comb, data);

	/* short-form */
	if (clos_short_combination_p(comb))
		return comb_short(ptr, ret, gen, comb, data);

	/* error */
	_fmte("Invalid method-combination ~S.", comb, NULL);
	return 0;
}


/*****************************************************************************
 *  generic-finalize
 *****************************************************************************/
_g int generic_eql_specializer(addr left, addr right, int check)
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
	_fmte("The eql-specializers have a difference value ~S /= ~S.", left, right, NULL);
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
		value = (*call)(x, y);
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
		getnth_unsafe(a, i, &x);
		getnth_unsafe(b, i, &y);
		value = (*call)(x, y);
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

static void generic_specializers_sort(addr *ret, addr gen, addr list)
{
	addr order;

	stdget_generic_precedence_index(gen, &order);
	if (order != Nil)
		simplesort_info_cons_unsafe(ret, list, order, generic_sort_order_call);
	else
		simplesort_cons_unsafe(ret, list, generic_sort_call);
}

static void generic_make_array(addr *ret, addr gen, addr type)
{
	addr array, data, methods, method, list;
	size_t size, index;

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
	*ret = data;
}

static int generic_make_type(Execute ptr, addr *ret, addr gen, addr type)
{
	addr comb, data;
	LocalHold hold;

	stdget_generic_method_combination(gen, &comb);
	generic_make_array(&data, gen, type);
	hold = LocalHold_local_push(ptr, data);
	Return(comb_lambda(ptr, ret, gen, comb, data));
	localhold_end(hold);

	return 0;
}

static void generic_make_mapcar_class_of(LocalRoot local,
		addr *ret, addr list, addr args)
{
	addr result, eqlcheck, arg, check;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &eqlcheck, &list);
		if (args == Nil)
			_fmte("Too few arguments.", NULL);
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
	clos_generic_call call;
	LocalRoot local;
	LocalHold hold;

	local = ptr->local;
	GetClosGenericCallArray(inst, 0, &eqlcheck);
	GetClosGenericCallArray(inst, 1, &cache);
	generic_make_mapcar_class_of(local, &key, eqlcheck, args);
	if (! findvalue_hashtable(cache, key, &value)) {
		/* not found, tranlate to heap-list from dynamic list */
		copy_list_heap_unsafe(&key, key);
		hold = LocalHold_local_push(ptr, key);
		Return(generic_make_type(ptr, &value, gen, key));
		localhold_end(hold);
		intern_hashheap(cache, key, &cons);
		SetCdr(cons, value);
	}

	/* clos_generic_call */
	CallClosGenericCall(value, &call);
	return (*call)(ptr, value, gen, args);
}

static void generic_make_lambda(addr gen, addr *ret)
{
	addr eqlcheck, cache, call;

	stdget_generic_eqlcheck(gen, &eqlcheck);
	stdget_generic_cache(gen, &cache);
	clos_generic_call_heap(&call, gen_generic_make_lambda_call, 2);
	SetClosGenericCallArray(call, 0, eqlcheck);
	SetClosGenericCallArray(call, 1, cache);
	*ret = call;
}

static int generic_no_method(Execute ptr, addr inst, addr gen, addr args)
{
	return generic_no_applicable_method(ptr, gen, args);
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
		clos_generic_call_heap(&call, gen_generic_no_method, 0);
		SetConstant(CONSTANT_CLOSDATA_NO_METHOD, call);
	}
	*ret = call;
}

_g void generic_finalize(addr gen)
{
	addr pos;
	generic_make_generic_call(gen, &pos);
	stdset_generic_call(gen, pos);
}


/*****************************************************************************
 *  execute clos
 *****************************************************************************/
_g int funcallable_p(addr pos)
{
	addr clos;

	if (! closp(pos)) return 0;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &clos);

	return clos_subtype_p(pos, clos);
}

_g void closrun_execute(Execute ptr, addr clos, addr args)
{
	addr pos;
	clos_generic_call call;

	if (! funcallable_p(clos))
		_fmte("Cannot exexute a funcallable instance ~S.", clos, NULL);
	stdget_generic_call(clos, &pos);
	CheckType(pos, LISPSYSTEM_GENERIC);
	CallClosGenericCall(pos, &call);
	(*call)(ptr, pos, clos, args);
}


/*****************************************************************************
 *  defgeneric
 *****************************************************************************/
static void generic_instance_cache(addr *ret)
{
	hashtable_full_heap(ret,
			HASHTABLE_TEST_CACHE,
			8,
			HASHTABLE_REHASH_SIZE_DEFAULT,
			HASHTABLE_REHASH_THRESHOLD_DEFAULT);
}

static void generic_instance_methods(addr *ret, addr comb)
{
	size_t size;
	method_combination_qualifiers_count(comb, &size);
	vector4_heap(ret, size);
}

_g void generic_common_instance(addr *ret, addr name, addr args)
{
	addr pos, methods, method, cache;

	CheckType(name, LISPTYPE_CALLNAME);
	CheckType(args, LISPSYSTEM_ARGUMENT);
#ifdef LISP_DEBUG
	clos_find_generic_nil(name, &pos);
	Check(pos != Nil, "generic function error");
#endif
	generic_instance_methods(&methods, Nil);
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_instance_cache(&cache);

	/* make-instance */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	stdset_generic_name(pos, name);
	stdset_generic_lambda_list(pos, args);
	stdset_generic_argument_precedence_order(pos, Nil);
	stdset_generic_methods(pos, methods);
	stdset_generic_method_class(pos, method);
	stdset_generic_method_combination(pos, Nil);
	stdset_generic_cache(pos, cache);
	stdset_generic_precedence_index(pos, Nil);

	/* result */
	setcallname_global(name, pos);
	*ret = pos;
}

_g void generic_common_order(addr gen, addr order, addr list)
{
#ifdef LISP_DEBUG
	addr var, x, y, root;
	size_t size1, size2, size3, index, check;

	stdget_generic_lambda_list(gen, &var);
	GetArgument(var, ArgumentIndex_var, &var);

	size1 = length_list_safe(var);
	size2 = length_list_safe(order);
	size3 = length_list_safe(list);
	if (size1 != size2)
		_fmte("Length of :argument-precedence-order is not equal to lambda-list.", NULL);
	if (size1 != size3)
		_fmte("Length of :precedence-index is not equal to lambda-list.", NULL);
	for (root = list; order != Nil; ) {
		GetCons(order, &x, &order);
		getcons(root, &y, &root);
		if (! position_list_eq_unsafe(x, var, &index))
			_fmte("The variable ~S is not exist in :argument-precedence-order", x, NULL);
		GetIndex(y, &check);
		if (index != check)
			_fmte("Invalid precedence-index list.", NULL);
	}
#endif
	stdset_generic_argument_precedence_order(gen, order);
	stdset_generic_precedence_index(gen, list);
}


/*****************************************************************************
 *  ensure-generic-function
 *****************************************************************************/
_g int ensure_generic_function_common(Execute ptr, addr name, addr rest, addr *ret)
{
	addr call, check, clos, ensure;

	/* symbol or (setf name) */
	parse_callname_error(&call, name);
	/* symbol check */
	if (symbol_callname_p(call)) {
		/* special-operator */
		if (get_special_operator(name))
			_fmte("ENSURE-GENERIC-FUNCTION don't accept "
					"a special symbol ~S.", name, NULL);
		/* macro */
		getmacro_symbol(name, &check);
		if (check != Unbound)
			_fmte("ENSURE-GENERIC-FUNCTION don't accept "
					"a macro symbol ~S.", check, NULL);
	}
	/* class-of */
	getcallname_global(call, &check);
	if (check == Unbound) {
		clos = Nil;
	}
	else if (clos_generic_p(check)) {
		clos_class_of(check, &clos);
	}
	else {
		/* error */
		_fmte("Invalid generic-function argument ~S.", name, NULL);
		return 0;
	}

	/* (apply #'ensure-generic-function-using-class ...) */
	GetConst(CLOSNAME_ENSURE_GENERIC_FUNCTION_USING_CLASS, &ensure);
	getfunctioncheck_local(ptr, ensure, &ensure);
	return applya_control(ptr, ensure, clos, name, rest, NULL);
}


/*
 *  generic-empty
 */
_g void generic_empty(addr name, addr lambda, addr *ret)
{
	addr pos, method, methods, cache;

	Check(! callnamep(name), "type error");
	Check(! argumentp(lambda), "type error");
	Check(ArgumentStruct(lambda)->type != ArgumentType_generic, "argument error");

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	generic_instance_methods(&methods, Nil);
	GetConst(CLOS_STANDARD_METHOD, &method);
	generic_instance_cache(&cache);

	/* setf */
	stdset_generic_name(pos, name);
	stdset_generic_lambda_list(pos, lambda);
	stdset_generic_methods(pos, methods);
	stdset_generic_method_class(pos, method);
	stdset_generic_argument_precedence_order(pos, Nil);
	stdset_generic_method_combination(pos, Nil);
	stdset_generic_cache(pos, cache);
	stdset_generic_precedence_index(pos, Nil);

	/* result */
	generic_finalize(pos);
	setcallname_global(name, pos);
	*ret = pos;
}


/*
 *  generic-add
 */
static void generic_precedence_order_index(addr lambda, addr order, addr *ret)
{
	addr var, x, list;
	size_t size1, size2, index;

	if (order == Nil) {
		*ret = Nil;
		return;
	}
	GetArgument(lambda, ArgumentIndex_var, &var);
	size1 = length_list_safe(var);
	size2 = length_list_safe(order);
	if (size1 != size2)
		_fmte("Length of :argument-precedence-order is not equal to lambda-list.", NULL);
	for (list = Nil; var != Nil; ) {
		GetCons(var, &x, &var);
		if (! position_list_eq_unsafe(x, order, &index))
			_fmte("The variable ~S is not exist in :argument-precedence-order", x, NULL);
		index_heap(&x, index);
		cons_heap(&list, x, list);
	}
	nreverse_list_unsafe(ret, list);
}

_g int generic_add(struct generic_argument *str, addr *ret)
{
	addr pos, comb, methods, cache, order;

	/* check */
	generic_precedence_order_index(str->lambda, str->order, &order);
	/* (make-instance generic-function-class) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	if (callclang_funcall(str->ptr, &pos, pos, str->generic, NULL))
		return 1;

	/* value */
	comb = str->combination;
	if (comb != Nil)
		clos_find_method_combination(pos, comb, &comb);
	generic_instance_methods(&methods, comb);
	generic_instance_cache(&cache);

	/* setf */
	stdset_generic_name(pos, str->name);
	stdset_generic_lambda_list(pos, str->lambda);
	stdset_generic_methods(pos, methods);
	stdset_generic_method_class(pos, str->method);
	stdset_generic_argument_precedence_order(pos, str->order);
	stdset_generic_declarations(pos, str->declare);
	stdset_generic_method_combination(pos, comb);
	stdset_generic_cache(pos, cache);
	stdset_generic_precedence_index(pos, order);

	/* result */
	generic_finalize(pos);
	setcallname_global(str->name, pos);
	*ret = pos;
	return 0;
}


/*
 *  generic-change
 */
_g int generic_change(struct generic_argument *str, addr *ret)
{
	_fmte("TODO", NULL);
	return 0;
}


/*
 *  common
 */
_g void generic_compute_applicable_methods(LocalRoot local,
		addr gen, addr args, addr *ret)
{
	addr data, root, list, pos;
	LocalStack stack;
	size_t size, i;

	push_local(local, &stack);
	stdget_generic_eqlcheck(gen, &data);
	generic_make_mapcar_class_of(local, &data, data, args);
	generic_make_array(&data, gen, data);
	LenArrayA4(data, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetArrayA4(data, i, &list);
		while (list != Nil) {
			GetCons(list, &pos, &list);
			cons_heap(&root, pos, root);
		}
	}
	rollback_local(local, stack);
	nreverse_list_unsafe(ret, root);
}

static int generic_find_method_equal(addr method, addr spec)
{
	addr left, right, a, b;
	size_t x, y;

	stdget_method_specializers(method, &left);
	right = spec;
	/* length check */
	x = length_list_safe(left);
	y = length_list_safe(right);
	if (x != y) {
		_fmte("The length of specializers ~S "
				"does not match in the method ~S.", spec, method, NULL);
		return 0;
	}
	/* specializer check */
	while (left != Nil) {
		getcons(left, &a, &left);
		getcons(right, &b, &right);
		if (a != b)
			return 0;
	}

	return 1;
}

_g void generic_find_method(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret)
{
	addr comb, list, method;
	size_t index;

	stdget_generic_method_combination(gen, &comb);
	if (qualifiers_position_nil(ptr, qua, comb, &index)) {
		if (errorp != Nil) {
			_fmte("The qualifiers ~S is not found in generic-function ~S.",
					qua, gen, NULL);
		}
		*ret = Nil;
		return;
	}

	stdget_generic_methods(gen, &list);
	GetArrayA4(list, index, &list);
	while (list != Nil) {
		GetCons(list, &method, &list);
		if (generic_find_method_equal(method, spec)) {
			*ret = method;
			return;
		}
	}
	/* not found */
	if (errorp != Nil) {
		_fmte("The specializes ~S is not found in generic-function ~S ~S.",
				spec, gen, qua, NULL);
	}
	*ret = Nil;
}


/*
 *  initialize
 */
_g void init_clos_generic(void)
{
	SetPointerCall(defun, any, standard_lambda);
	ClosGenericTable[gen_comb_standard_call] = comb_standard_call;
	ClosGenericTable[gen_comb_define_call] = comb_define_call;
	ClosGenericTable[gen_generic_make_lambda_call] = generic_make_lambda_call;
	ClosGenericTable[gen_generic_no_method] = generic_no_method;
}

