#include "clos_define.h"
#include "clos_object.h"
#include "closget.h"
#include "closget_class.h"
#include "constant.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  slot-value
 */
static int stdget_class_constant_(addr pos, addr *ret,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}
#define StdGetClass_(p,r,a,b) \
	stdget_class_constant_((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)

void stdget_class_name_check(addr pos, addr *ret)
{
	addr key;

	if (GetType(pos) != LISPTYPE_CLOS)
		goto unbound;
	GetConstant(CONSTANT_CLOSNAME_NAME, &key);
	if (! clos_getp(pos, key, &pos))
		goto unbound;
	if (pos == Unbound)
		goto unbound;
	*ret = pos;
	return;

unbound:
	*ret = Unbound;
}

int stdget_class_name_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, name, NAME);
}

int stdget_class_direct_slots_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_slots, DIRECT_SLOTS);
}

int stdget_class_direct_subclasses_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_subclasses, DIRECT_SUBCLASSES);
}

int stdget_class_direct_superclasses_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_superclasses, DIRECT_SUPERCLASSES);
}

int stdget_class_precedence_list_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdget_class_slots_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, slots, EFFECTIVE_SLOTS);
}

int stdget_class_finalized_p_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, finalized_p, FINALIZED_P);
}

int stdget_class_prototype_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, prototype, PROTOTYPE);
}

int stdget_class_default_initargs_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, default_initargs, DEFAULT_INITARGS);
}

int stdget_class_direct_default_initargs_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}

int stdget_class_version_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, version, VERSION);
}

int stdget_class_document_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, document, DOCUMENTATION);
}

int stdget_class_redefined_class_(addr pos, addr *ret)
{
	return StdGetClass_(pos, ret, redefined_class, REDEFINED_CLASS);
}


/*
 *  (set slot-value)
 */
static int stdset_class_constant_(addr pos, addr value,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
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
#define StdSetClass_(p,r,a,b) \
	stdset_class_constant_((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)

int stdset_class_name_(addr pos, addr value)
{
	return StdSetClass_(pos, value, name, NAME);
}

int stdset_class_direct_slots_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_slots, DIRECT_SLOTS);
}

int stdset_class_direct_subclasses_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_subclasses, DIRECT_SUBCLASSES);
}

int stdset_class_direct_superclasses_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_superclasses, DIRECT_SUPERCLASSES);
}

int stdset_class_precedence_list_(addr pos, addr value)
{
	return StdSetClass_(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdset_class_slots_(addr pos, addr value)
{
	return StdSetClass_(pos, value, slots, EFFECTIVE_SLOTS);
}

int stdset_class_finalized_p_(addr pos, addr value)
{
	return StdSetClass_(pos, value, finalized_p, FINALIZED_P);
}

int stdset_class_prototype_(addr pos, addr value)
{
	return StdSetClass_(pos, value, prototype, PROTOTYPE);
}

int stdset_class_default_initargs_(addr pos, addr value)
{
	return StdSetClass_(pos, value, default_initargs, DEFAULT_INITARGS);
}

int stdset_class_direct_default_initargs_(addr pos, addr value)
{
	return StdSetClass_(pos, value, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}

int stdset_class_version_(addr pos, addr value)
{
	return StdSetClass_(pos, value, version, VERSION);
}

int stdset_class_document_(addr pos, addr value)
{
	return StdSetClass_(pos, value, document, DOCUMENTATION);
}
int stdset_class_redefined_class_(addr pos, addr value)
{
	return StdSetClass_(pos, value, redefined_class, REDEFINED_CLASS);
}


/*
 *  slot-boundp
 */
static int stdboundp_class_constant_(addr pos, int *ret,
		enum Clos_class_Index index1, constindex index2)
{
	Return(stdget_class_constant_(pos, &pos, index1, index2));
	return Result(ret, pos != Unbound);
}
#define StdBoundpClass_(p,r,a,b) \
	stdboundp_class_constant_((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)

int stdboundp_class_name_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, name, NAME);
}

int stdboundp_class_direct_slots_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, direct_slots, DIRECT_SLOTS);
}

int stdboundp_class_direct_subclasses_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, direct_subclasses, DIRECT_SUBCLASSES);
}

int stdboundp_class_direct_superclasses_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, direct_superclasses, DIRECT_SUPERCLASSES);
}

int stdboundp_class_precedence_list_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdboundp_class_slots_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, slots, EFFECTIVE_SLOTS);
}

int stdboundp_class_finalized_p_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, finalized_p, FINALIZED_P);
}

int stdboundp_class_prototype_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, prototype, PROTOTYPE);
}

int stdboundp_class_default_initargs_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, default_initargs, DEFAULT_INITARGS);
}

int stdboundp_class_direct_default_initargs_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}

int stdboundp_class_version_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, version, VERSION);
}

int stdboundp_class_document_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, document, DOCUMENTATION);
}

int stdboundp_class_redefined_class_(addr pos, int *ret)
{
	return StdBoundpClass_(pos, ret, redefined_class, REDEFINED_CLASS);
}

