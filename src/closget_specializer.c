#include "clos_define.h"
#include "clos_object.h"
#include "clos_variable.h"
#include "closget.h"
#include "closget_specializer.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  slot-value
 */
static int stdget_specializer_constant_(addr pos, addr *ret,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_eql_specializer) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetSpecializer_(p,r,a,b) \
	stdget_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

int stdget_specializer_object_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetSpecializer_(pos, ret, object, OBJECT);
}

int stdget_specializer_type_(Execute ptr, addr pos, addr *ret)
{
	Check(ptr == NULL, "execute error");
	return StdGetSpecializer_(pos, ret, type, TYPE);
}


/*
 *  (setf slot-value)
 */
static int stdset_specializer_constant_(addr pos, addr value,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_eql_specializer) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
		return 0;
	}
	else {
		GetConstant(index2, &check);
		return clos_set_(pos, check, value);
	}
}
#define StdSetSpecializer_(p,r,a,b) \
	stdset_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

int stdset_specializer_object_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetSpecializer_(pos, value, object, OBJECT);
}

int stdset_specializer_type_(Execute ptr, addr pos, addr value)
{
	Check(ptr == NULL, "execute error");
	return StdSetSpecializer_(pos, value, type, TYPE);
}


/*
 *  slot-boundp
 */
static int stdboundp_specializer_constant_(addr pos, int *ret,
		enum Clos_specializer_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_specializer_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	if (clos == Clos_eql_specializer) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_getelt(pos, (size_t)index1, &pos);
	}
	else {
		GetConstant(index2, &check);
		Return(clos_get_(pos, check, &pos));
	}

	return Result(ret, pos != Unbound);
}
#define StdBoundpSpecializer_(p,r,a,b) \
	stdboundp_specializer_constant_((p), (r), Clos_specializer_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_specializer_object_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpSpecializer_(pos, ret, object, OBJECT);
}

int stdboundp_specializer_type_(Execute ptr, addr pos, int *ret)
{
	Check(ptr == NULL, "execute error");
	return StdBoundpSpecializer_(pos, ret, type, TYPE);
}

