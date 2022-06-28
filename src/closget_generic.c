#include "clos_define.h"
#include "clos_object.h"
#include "closget.h"
#include "closget_generic.h"
#include "condition.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

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

