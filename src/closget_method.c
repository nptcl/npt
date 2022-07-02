#include "clos_define.h"
#include "clos_object.h"
#include "clos_variable.h"
#include "closget.h"
#include "closget_method.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  slot-value
 */
static int stdget_method_constant_(addr pos, addr *ret,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_method) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetMethod_(p,r,a,b) \
	stdget_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

int stdget_method_function_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetMethod_(pos, ret, function, FUNCTION);
}

int stdget_method_generic_function_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetMethod_(pos, ret, generic_function, GENERIC_FUNCTION);
}

int stdget_method_lambda_list_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetMethod_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdget_method_qualifiers_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetMethod_(pos, ret, qualifiers, QUALIFIERS);
}

int stdget_method_specializers_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetMethod_(pos, ret, specializers, SPECIALIZERS);
}


/*
 *  (setf slot-value)
 */
static int stdset_method_constant_(addr pos, addr value,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_method) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetMethod_(p,r,a,b) \
	stdset_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

int stdset_method_function_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetMethod_(pos, value, function, FUNCTION);
}

int stdset_method_generic_function_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetMethod_(pos, value, generic_function, GENERIC_FUNCTION);
}

int stdset_method_lambda_list_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetMethod_(pos, value, lambda_list, LAMBDA_LIST);
}

int stdset_method_qualifiers_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetMethod_(pos, value, qualifiers, QUALIFIERS);
}

int stdset_method_specializers_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetMethod_(pos, value, specializers, SPECIALIZERS);
}


/*
 *  slot-boundp
 */
static int stdboundp_method_constant_(addr pos, int *ret,
		enum Clos_method_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_method_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_standard_method) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpMethod_(p,r,a,b) \
	stdboundp_method_constant_((p), (r), Clos_method_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_method_function_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpMethod_(pos, ret, function, FUNCTION);
}

int stdboundp_method_generic_function_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpMethod_(pos, ret, generic_function, GENERIC_FUNCTION);
}

int stdboundp_method_lambda_list_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpMethod_(pos, ret, lambda_list, LAMBDA_LIST);
}

int stdboundp_method_qualifiers_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpMethod_(pos, ret, qualifiers, QUALIFIERS);
}

int stdboundp_method_specializers_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpMethod_(pos, ret, specializers, SPECIALIZERS);
}

