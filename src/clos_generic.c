#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "define.h"
#include "equal.h"
#include "execute_object.h"
#include "function.h"
#include "hold.h"
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
static int stdget_generic_constant_(addr pos, addr *ret,
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
		Return(clos_get_(pos, check, &check));
	}
	/* Unbound check */
	if (check == Unbound) {
		*ret = Nil;
		return fmte_("There is no applicable methods in ~S.", pos, NULL);
	}

	return Result(ret, check);
}

static int stdset_generic_constant_(addr pos, addr value,
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
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdGetGeneric_(p,r,a,b) \
	stdget_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)
#define StdSetGeneric_(p,r,a,b) \
	stdset_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

int stdget_generic_name_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, name, NAME);
}
int stdset_generic_name_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, name, NAME);
}

int stdget_generic_methods_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, methods, METHODS);
}
int stdset_generic_methods_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, methods, METHODS);
}

int stdget_generic_lambda_list_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, lambda_list, LAMBDA_LIST);
}
int stdset_generic_lambda_list_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdget_generic_argument_precedence_order_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}
int stdset_generic_argument_precedence_order_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

int stdget_generic_declarations_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, declarations, DECLARATIONS);
}
int stdset_generic_declarations_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, declarations, DECLARATIONS);
}

int stdget_generic_method_class_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, method_class, METHOD_CLASS);
}
int stdset_generic_method_class_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, method_class, METHOD_CLASS);
}

int stdget_generic_method_combination_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, method_combination, METHOD_COMBINATION);
}
int stdset_generic_method_combination_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, method_combination, METHOD_COMBINATION);
}

int stdget_generic_vector_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, vector, VECTOR);
}
int stdset_generic_vector_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, vector, VECTOR);
}

int stdget_generic_remove_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, remove, REMOVE);
}
int stdset_generic_remove_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, remove, REMOVE);
}

int stdget_generic_argument_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, argument, ARGUMENT);
}
int stdset_generic_argument_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, argument, ARGUMENT);
}

int stdget_generic_documentation_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, documentation, DOCUMENTATION);
}
int stdset_generic_documentation_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, documentation, DOCUMENTATION);
}

int stdget_generic_eqlcheck_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, eqlcheck, EQLCHECK);
}
int stdset_generic_eqlcheck_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, eqlcheck, EQLCHECK);
}

int stdget_generic_cache_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, cache, CACHE);
}
int stdset_generic_cache_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, cache, CACHE);
}

int stdget_generic_call_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, call, CALL);
}
int stdset_generic_call_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, call, CALL);
}

int stdget_generic_precedence_index_(addr pos, addr *ret)
{
	return StdGetGeneric_(pos, ret, precedence_index, PRECEDENCE_INDEX);
}
int stdset_generic_precedence_index_(addr pos, addr value)
{
	return StdSetGeneric_(pos, value, precedence_index, PRECEDENCE_INDEX);
}

static int stdboundp_generic_constant_(addr pos, int *ret,
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
		return Result(ret, pos != Unbound);
	}
	else {
		GetConstant(index2, &check);
		return clos_slot_boundp_(pos, check, ret);
	}
}

int stdboundp_generic_argument_precedence_order_(addr pos, int *ret)
{
	return stdboundp_generic_constant_(pos, ret,
			Clos_generic_argument_precedence_order,
			CONSTANT_CLOSKEY_ARGUMENT_PRECEDENCE_ORDER);
}

int stdboundp_generic_eqlcheck_(addr pos, int *ret)
{
	return stdboundp_generic_constant_(pos, ret,
			Clos_generic_eqlcheck,
			CONSTANT_CLOSKEY_EQLCHECK);
}


/*
 *  eql-specializer
 */
static int stdget_specializer_constant_(addr pos, addr *ret,
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
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_specializer_constant_(addr pos, addr value,
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
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdGetSpecializer_(p,r,a,b) \
	stdget_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)
#define StdSetSpecializer_(p,r,a,b) \
	stdset_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

int stdget_specializer_object_(addr pos, addr *ret)
{
	return StdGetSpecializer_(pos, ret, object, OBJECT);
}
int stdset_specializer_object_(addr pos, addr value)
{
	return StdSetSpecializer_(pos, value, object, OBJECT);
}

int stdget_specializer_type_(addr pos, addr *ret)
{
	return StdGetSpecializer_(pos, ret, type, TYPE);
}
int stdset_specializer_type_(addr pos, addr value)
{
	return StdSetSpecializer_(pos, value, type, TYPE);
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
	Return(stdget_method_function_(car, &call));
	return applya_control(ptr, call, car, cdr, rest, NULL);
}

static int comb_standard_funcall_(Execute ptr, addr rest, addr around, addr primary)
{
	append2_local_unsafe(ptr->local, around, primary, &around);
	GetCons(around, &around, &primary);
	return comb_standard_method_(ptr, around, primary, rest);
}

static int function_standard_lambda_after_(Execute ptr, addr after, addr args)
{
	addr control, values, one;
	size_t size;

	push_control(ptr, &control);
	save_values_control(ptr, &values, &size);
	while (after != Nil) {
		GetCons(after, &one, &after);
		if (comb_standard_method_(ptr, one, Nil, args))
			goto finish;
	}
	restore_values_control(ptr, values, size);
finish:
	return pop_control_(ptr, control);
}

static int function_standard_lambda(Execute ptr)
{
	addr args, data, before, primary, after, one, car, cdr;

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
		Return(function_standard_lambda_after_(ptr, after, args));
	}

	return 0;
}

static int comb_standard_qualifiers_(LocalRoot local,
		addr *ret, addr gen, addr before, addr primary, addr after)
{
	addr clos, call, data, name;

	Return(stdget_generic_method_class_(gen, &clos));
	GetConst(SYSTEM_STANDARD, &name);
	ParseCallName(local, &name, name);
	compiled_local(local, &call, name);
	setcompiled_any(call, p_defun_standard_lambda);
	list_local(local, &data, before, primary, after, NULL);
	SetDataFunction(call, data);
	return method_instance_call_(local, ret, clos, call);
}

static int generic_no_applicable_method_(Execute ptr, addr gen, addr args)
{
	addr call;

	/* call (no-applicable-method generic-function . args) */
	GetConst(COMMON_NO_APPLICABLE_METHOD, &call);
	Return(getfunction_global_(call, &call));
	return applya_control(ptr, call, gen, args, NULL);
}

static int comb_standard_execute_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr data, around, before, primary, after, temp;
	LocalRoot local;

	GetClosGenericCallArray(inst, 0, &data);
	GetArrayA4(data, Clos_standard_around, &around);
	GetArrayA4(data, Clos_standard_before, &before);
	GetArrayA4(data, Clos_standard_primary, &primary);
	GetArrayA4(data, Clos_standard_after, &after);

	if (primary == Nil)
		return generic_no_applicable_method_(ptr, gen, rest);
	local = ptr->local;
	GetCdr(primary, &temp);
	if (before != Nil || after != Nil || temp != Nil) {
		Return(comb_standard_qualifiers_(local, &temp, gen, before, primary, after));
		list_local(local, &primary, temp, NULL);
	}
	return comb_standard_funcall_(ptr, rest, around, primary);
}

static int comb_standard_call_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr control;

	push_control(ptr, &control);
	(void)comb_standard_execute_(ptr, inst, gen, rest);
	return pop_control_(ptr, control);
}

static int comb_standard_(addr *ret, addr data)
{
	addr pos;

	clos_generic_call_heap(&pos, gen_comb_standard_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}


/*
 *  execute combination
 */
static int comb_define_call_(Execute ptr, addr inst, addr gen, addr rest)
{
	addr control, call;

	push_control(ptr, &control);
	GetClosGenericCallArray(inst, 0, &call);
	(void)apply_control(ptr, call, rest);
	return pop_control_(ptr, control);
}

static int comb_long_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	Return(comb_longform_(ptr, &data, gen, comb, data));
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}

static int comb_short_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	addr pos;

	/* make function */
	Return(comb_shortform_(ptr, &data, gen, comb, data));
	Check(! functionp(data), "type error");
	/* result */
	clos_generic_call_heap(&pos, gen_comb_define_call, 1);
	SetClosGenericCallArray(pos, 0, data);

	return Result(ret, pos);
}

static int comb_lambda_(Execute ptr, addr *ret, addr gen, addr comb, addr data)
{
	int check;

	/* standard */
	if (comb == Nil)
		return comb_standard_(ret, data);

	/* long-form */
	Return(clos_long_combination_p_(comb, &check));
	if (check)
		return comb_long_(ptr, ret, gen, comb, data);

	/* short-form */
	Return(clos_short_combination_p_(comb, &check));
	if (check)
		return comb_short_(ptr, ret, gen, comb, data);

	/* error */
	*ret = Nil;
	return fmte_("Invalid method-combination ~S.", comb, NULL);
}


/*****************************************************************************
 *  generic-finalize
 *****************************************************************************/
int generic_eql_specializer_(addr left, addr right, int check, int *ret)
{
	int check1, check2;

	Return(clos_specializer_p_(left, &check1));
	Return(clos_specializer_p_(right, &check2));
	if (check1 && check2) {
		return Result(ret, left == right);
	}
	if (check1) {
		Return(stdget_specializer_type_(left, &left));
		return clos_subclass_p_(left, right, ret);
	}
	if (check2) {
		if (! check)
			return Result(ret, 0);
		Return(stdget_specializer_type_(right, &right));
		return clos_subclass_p_(left, right, ret);
	}

	return clos_subclass_p_(left, right, ret);
}

static int generic_make_method_check_(addr argtype, addr method, int *ret)
{
	int check;
	addr left, right;

	Return(stdget_method_specializers_(method, &method));
	while (argtype != Nil || method != Nil) {
		Check(argtype == Nil || method == Nil, "argument error");
		GetCons(argtype, &left, &argtype);
		GetCons(method, &right, &method);
		Return(generic_eql_specializer_(left, right, 0, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_compare_class_(addr left, addr right, int *ret)
{
	int check;

	if (left == right)
		return Result(ret, 0);
	Return(clos_subclass_p_(left, right, &check));

	return Result(ret, check? -1: 1);
}

static int generic_compare_eql_(addr left, addr right, int *ret)
{
	if (left == right)
		return Result(ret, 0);
	Return(stdget_specializer_object_(left, &left));
	Return(stdget_specializer_object_(right, &right));
	*ret = 1;
	return fmte_("The eql-specializers have "
			"a difference value ~S /= ~S.", left, right, NULL);
}

static int generic_compare_eql_type_(addr left, addr right, int *ret)
{
	int check;

	Return(stdget_specializer_type_(left, &left));
	Return(generic_compare_class_(left, right, &check));

	return Result(ret, check == 0? -1: check);
}

static int generic_compare_type_eql_(addr left, addr right, int *ret)
{
	int check;

	Return(stdget_specializer_type_(right, &right));
	Return(generic_compare_class_(left, right, &check));

	return Result(ret, check == 0? 1: check);
}

static int generic_compare_specializer_(addr left, addr right, int *ret)
{
	int check1, check2;

	Return(clos_specializer_p_(left, &check1));
	Return(clos_specializer_p_(right, &check2));
	if (check1 && check2)
		return generic_compare_eql_(left, right, ret);
	if (check1)
		return generic_compare_eql_type_(left, right, ret);
	if (check2)
		return generic_compare_type_eql_(left, right, ret);
	if (left == right)
		return Result(ret, 0);

	return generic_compare_class_(left, right, ret);
}

static int generic_sort_compare_(addr a, addr b, int *ret,
		int (*call_)(addr, addr, int *))
{
	int check;
	addr x, y;

	for (;;) {
		if (a == Nil || b == Nil)
			break;
		GetCons(a, &x, &a);
		GetCons(b, &y, &b);
		Return((*call_)(x, y, &check));
		if (check < 0)
			return Result(ret, 1);
		if (0 < check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_sort_call_(addr left, addr right, int *ret)
{
	Return(stdget_method_specializers_(left, &left));
	Return(stdget_method_specializers_(right, &right));
	return generic_sort_compare_(left, right, ret, generic_compare_specializer_);
}

static int generic_sort_(addr order,
		int (*call_)(addr, addr, int *),
		addr a, addr b, int *ret)
{
	int check;
	addr pos, x, y;
	size_t i;

	while (order != Nil) {
		GetCons(order, &pos, &order);
		GetIndex(pos, &i);
		getnth_unsafe(a, i, &x);
		getnth_unsafe(b, i, &y);
		Return((*call_)(x, y, &check));
		if (check < 0)
			return Result(ret, 1);
		if (0 < check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

static int generic_sort_order_call_(addr order, addr left, addr right, int *ret)
{
	Return(stdget_method_specializers_(left, &left));
	Return(stdget_method_specializers_(right, &right));
	return generic_sort_(order, generic_compare_specializer_, left, right, ret);
}

static int generic_specializers_sort_(addr *ret, addr gen, addr list)
{
	addr order;

	Return(stdget_generic_precedence_index_(gen, &order));
	if (order != Nil) {
		return simplesort_info_cons_unsafe_(ret,
				list, order, generic_sort_order_call_);
	}
	else {
		return simplesort_cons_unsafe_(ret,
				list, generic_sort_call_);
	}
}

static int generic_make_array_(addr *ret, addr gen, addr type)
{
	int check;
	addr array, data, methods, method, list;
	size_t size, index;

	Return(stdget_generic_vector_(gen, &array));
	LenArrayA4(array, &size);
	vector4_heap(&data, size);
	for (index = 0; index < size; index++) {
		GetArrayA4(array, index, &methods);
		/* remove-if-not */
		for (list = Nil; methods != Nil; ) {
			GetCons(methods, &method, &methods);
			Return(generic_make_method_check_(type, method, &check));
			if (check)
				cons_heap(&list, method, list);
		}
		/* sort */
		Return(generic_specializers_sort_(&list, gen, list));
		SetArrayA4(data, index, list);
	}

	return Result(ret, data);
}

static int generic_make_type_(Execute ptr, addr *ret, addr gen, addr type)
{
	addr comb, data;
	LocalHold hold;

	Return(stdget_generic_method_combination_(gen, &comb));
	Return(generic_make_array_(&data, gen, type));
	hold = LocalHold_local_push(ptr, data);
	Return(comb_lambda_(ptr, ret, gen, comb, data));
	localhold_end(hold);

	return 0;
}

static int generic_make_mapcar_class_of_(LocalRoot local,
		addr *ret, addr list, addr args)
{
	addr result, eqlcheck, arg, check;

	for (result = Nil; list != Nil; ) {
		GetCons(list, &eqlcheck, &list);
		if (args == Nil) {
			*ret = Nil;
			return fmte_("Too few arguments.", NULL);
		}
		GetCons(args, &arg, &args);
		if (eqlcheck == Nil) {
			check = Nil;
		}
		else {
			Return(clos_find_specializer_nil_(arg, &check));
		}
		if (check == Nil) {
			Return(clos_class_of_(arg, &check));
		}
		cons_local(local, &result, check, result);
	}
	nreverse(ret, result);

	return 0;
}

static int generic_make_lambda_call_(Execute ptr, addr inst, addr gen, addr args)
{
	addr eqlcheck, cache, key, value, cons;
	clos_generic_call call;
	LocalRoot local;
	LocalHold hold;

	local = ptr->local;
	GetClosGenericCallArray(inst, 0, &eqlcheck);
	GetClosGenericCallArray(inst, 1, &cache);
	Return(generic_make_mapcar_class_of_(local, &key, eqlcheck, args));
	Return(find_hashtable_(cache, key, &value));
	if (value == Unbound) {
		/* not found, tranlate to heap-list from dynamic list */
		copy_list_heap_unsafe(&key, key);
		hold = LocalHold_local_push(ptr, key);
		Return(generic_make_type_(ptr, &value, gen, key));
		localhold_end(hold);
		Return(intern_hashheap_(cache, key, &cons));
		SetCdr(cons, value);
	}

	/* clos_generic_call */
	CallClosGenericCall(value, &call);
	return (*call)(ptr, value, gen, args);
}

static int generic_make_lambda_(addr gen, addr *ret)
{
	addr eqlcheck, cache, call;

	Return(stdget_generic_eqlcheck_(gen, &eqlcheck));
	Return(stdget_generic_cache_(gen, &cache));
	clos_generic_call_heap(&call, gen_generic_make_lambda_call, 2);
	SetClosGenericCallArray(call, 0, eqlcheck);
	SetClosGenericCallArray(call, 1, cache);

	return Result(ret, call);
}

static int generic_no_method_(Execute ptr, addr inst, addr gen, addr args)
{
	return generic_no_applicable_method_(ptr, gen, args);
}

static int generic_make_generic_call_(addr gen, addr *ret)
{
	int check;
	addr call;

	Return(stdboundp_generic_eqlcheck_(gen, &check));
	if (check)
		return generic_make_lambda_(gen, ret);

	GetConst(CLOSDATA_NO_METHOD, &call);
	if (call == Unbound) {
		clos_generic_call_heap(&call, gen_generic_no_method, 0);
		SetConstant(CONSTANT_CLOSDATA_NO_METHOD, call);
	}

	return Result(ret, call);
}

int generic_finalize_(addr gen)
{
	addr pos;

	Return(generic_make_generic_call_(gen, &pos));
	Return(stdset_generic_call_(gen, pos));

	return 0;
}


/*****************************************************************************
 *  execute clos
 *****************************************************************************/
static int funcallable_p_(addr pos, int *ret)
{
	addr clos;

	if (! closp(pos))
		return 0;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &clos);

	return clos_subtype_p_(pos, clos, ret);
}

int closrun_execute_(Execute ptr, addr clos, addr args)
{
	int check;
	addr pos;
	clos_generic_call call;

	Return(funcallable_p_(clos, &check));
	if (! check)
		return fmte_("Cannot exexute a funcallable instance ~S.", clos, NULL);
	Return(stdget_generic_call_(clos, &pos));
	CheckType(pos, LISPSYSTEM_GENERIC);
	CallClosGenericCall(pos, &call);
	return (*call)(ptr, pos, clos, args);
}

int generic_order_(addr gen, addr order, addr list)
{
#ifdef LISP_DEBUG
	addr var, x, y, root;
	size_t size1, size2, size3, index, check;

	Return(stdget_generic_argument_(gen, &var));
	GetArgument(var, ArgumentIndex_var, &var);

	Return(length_list_safe_(var, &size1));
	Return(length_list_safe_(order, &size2));
	Return(length_list_safe_(list, &size3));
	if (size1 != size2) {
		return fmte_("Length of :argument-precedence-order is "
				"not equal to lambda-list.", NULL);
	}
	if (size1 != size3) {
		return fmte_("Length of :precedence-index is "
				"not equal to lambda-list.", NULL);
	}
	for (root = list; order != Nil; ) {
		GetCons(order, &x, &order);
		Return_getcons(root, &y, &root);
		if (! position_list_eq_unsafe(x, var, &index)) {
			return fmte_("The variable ~S "
					"is not exist in :argument-precedence-order", x, NULL);
		}
		GetIndex(y, &check);
		if (index != check)
			return fmte_("Invalid precedence-index list.", NULL);
	}
#endif
	Return(stdset_generic_argument_precedence_order_(gen, order));
	Return(stdset_generic_precedence_index_(gen, list));

	return 0;
}


/*
 *  common
 */
int generic_compute_applicable_methods_(LocalRoot local,
		addr gen, addr args, addr *ret)
{
	addr data, root, list, pos;
	LocalStack stack;
	size_t size, i;

	push_local(local, &stack);
	Return(stdget_generic_eqlcheck_(gen, &data));
	Return(generic_make_mapcar_class_of_(local, &data, data, args));
	Return(generic_make_array_(&data, gen, data));
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
	nreverse(ret, root);

	return 0;
}

static int generic_find_method_equal_(addr method, addr spec, int *ret)
{
	addr left, right, a, b;
	size_t x, y;

	Return(stdget_method_specializers_(method, &left));
	right = spec;
	/* length check */
	Return(length_list_safe_(left, &x));
	Return(length_list_safe_(right, &y));
	if (x != y) {
		*ret = 0;
		return fmte_("The length of specializers ~S "
				"does not match in the method ~S.", spec, method, NULL);
	}
	/* specializer check */
	while (left != Nil) {
		Return_getcons(left, &a, &left);
		Return_getcons(right, &b, &right);
		if (a != b)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int generic_find_method_(Execute ptr,
		addr gen, addr qua, addr spec, addr errorp, addr *ret)
{
	int check;
	addr comb, list, method;
	size_t index;

	Return(stdget_generic_method_combination_(gen, &comb));
	Return(qualifiers_position_nil_(ptr, qua, comb, &index, &check));
	if (check) {
		if (errorp != Nil) {
			*ret = Nil;
			return fmte_("The qualifiers ~S is not found in generic-function ~S.",
					qua, gen, NULL);
		}
		return Result(ret, Nil);
	}

	Return(stdget_generic_vector_(gen, &list));
	GetArrayA4(list, index, &list);
	while (list != Nil) {
		GetCons(list, &method, &list);
		Return(generic_find_method_equal_(method, spec, &check));
		if (check)
			return Result(ret, method);
	}
	/* not found */
	if (errorp != Nil) {
		*ret = Nil;
		return fmte_("The specializes ~S is not found in generic-function ~S ~S.",
				spec, gen, qua, NULL);
	}

	return Result(ret, Nil);
}


/*
 *  documentation
 */
int get_documentation_function_object_(addr pos, addr *ret)
{
	if (functionp(pos)) {
		getdocumentation_function(pos, ret);
		return 0;
	}

	/* generic-function */
	return stdget_generic_documentation_(pos, ret);
}

int set_documentation_function_object_(addr pos, addr value)
{
	if (functionp(pos)) {
		setdocumentation_function(pos, value);
		return 0;
	}

	/* generic-function */
	return stdset_generic_documentation_(pos, value);
}


/*
 *  initialize
 */
void init_clos_generic(void)
{
	SetPointerCall(defun, any, standard_lambda);
	ClosGenericTable[gen_comb_standard_call] = comb_standard_call_;
	ClosGenericTable[gen_comb_define_call] = comb_define_call_;
	ClosGenericTable[gen_generic_make_lambda_call] = generic_make_lambda_call_;
	ClosGenericTable[gen_generic_no_method] = generic_no_method_;
}

