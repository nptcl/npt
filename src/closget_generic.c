#include "clos_define.h"
#include "clos_object.h"
#include "clos_variable.h"
#include "closget.h"
#include "closget_generic.h"
#include "condition.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  slot-value
 */
static int stdget_generic_constant_(addr pos, addr *ret,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_generic_function) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}
	/* Unbound check */
	if (pos == Unbound) {
		*ret = Nil;
		return fmte_("There is no applicable methods in ~S.", pos, NULL);
	}

	return Result(ret, pos);
}
#define StdGetGeneric_(p,r,a,b) \
	stdget_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

int stdget_generic_name_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, name, NAME);
}

int stdget_generic_methods_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, methods, METHODS);
}

int stdget_generic_lambda_list_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdget_generic_argument_precedence_order_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

int stdget_generic_declarations_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, declarations, DECLARATIONS);
}

int stdget_generic_method_class_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, method_class, METHOD_CLASS);
}

int stdget_generic_method_combination_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, method_combination, METHOD_COMBINATION);
}

int stdget_generic_vector_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, vector, VECTOR);
}

int stdget_generic_remove_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, remove, REMOVE);
}

int stdget_generic_argument_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, argument, ARGUMENT);
}

int stdget_generic_documentation_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, documentation, DOCUMENTATION);
}

int stdget_generic_eqlcheck_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, eqlcheck, EQLCHECK);
}

int stdget_generic_cache_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, cache, CACHE);
}

int stdget_generic_call_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, call, CALL);
}

int stdget_generic_precedence_index_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetGeneric_(pos, ret, precedence_index, PRECEDENCE_INDEX);
}


/*
 *  (setf slot-value)
 */
static int stdset_generic_constant_(addr pos, addr value,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_generic_function) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetGeneric_(p,r,a,b) \
	stdset_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

int stdset_generic_name_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, name, NAME);
}

int stdset_generic_methods_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, methods, METHODS);
}

int stdset_generic_lambda_list_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdset_generic_argument_precedence_order_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

int stdset_generic_declarations_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, declarations, DECLARATIONS);
}

int stdset_generic_method_class_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, method_class, METHOD_CLASS);
}

int stdset_generic_method_combination_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, method_combination, METHOD_COMBINATION);
}

int stdset_generic_vector_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, vector, VECTOR);
}

int stdset_generic_remove_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, remove, REMOVE);
}

int stdset_generic_argument_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, argument, ARGUMENT);
}

int stdset_generic_documentation_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, documentation, DOCUMENTATION);
}

int stdset_generic_eqlcheck_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, eqlcheck, EQLCHECK);
}

int stdset_generic_cache_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, cache, CACHE);
}

int stdset_generic_call_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, call, CALL);
}

int stdset_generic_precedence_index_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetGeneric_(pos, value, precedence_index, PRECEDENCE_INDEX);
}


/*
 *  slot-boundp
 */
static int stdboundp_generic_constant_(addr pos, int *ret,
		enum Clos_generic_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_generic_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_generic_function) {
		clos_getelt(pos, (size_t)index1, &pos);
		return Result(ret, pos != Unbound);
	}
	else {
		GetConstant(index2, &check);
		return clos_slot_boundp_(pos, check, ret);
	}
}
#define StdBoundpGeneric_(p,r,a,b) \
	stdboundp_generic_constant_((p), (r), Clos_generic_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_generic_name_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, name, NAME);
}

int stdboundp_generic_methods_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, methods, METHODS);
}

int stdboundp_generic_lambda_list_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdboundp_generic_argument_precedence_order_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, argument_precedence_order, ARGUMENT_PRECEDENCE_ORDER);
}

int stdboundp_generic_declarations_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, declarations, DECLARATIONS);
}

int stdboundp_generic_method_class_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, method_class, METHOD_CLASS);
}

int stdboundp_generic_method_combination_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, method_combination, METHOD_COMBINATION);
}

int stdboundp_generic_vector_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, vector, VECTOR);
}

int stdboundp_generic_remove_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, remove, REMOVE);
}

int stdboundp_generic_argument_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, argument, ARGUMENT);
}

int stdboundp_generic_documentation_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, documentation, DOCUMENTATION);
}

int stdboundp_generic_eqlcheck_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, eqlcheck, EQLCHECK);
}

int stdboundp_generic_cache_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, cache, CACHE);
}

int stdboundp_generic_call_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, call, CALL);
}

int stdboundp_generic_precedence_index_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpGeneric_(pos, ret, precedence_index, PRECEDENCE_INDEX);
}

