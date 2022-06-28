#include "clos_define.h"
#include "clos_object.h"
#include "closget.h"
#include "closget_specializer.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

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


