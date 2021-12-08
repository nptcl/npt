#include "clos.h"
#include "constant.h"
#include "structure_access.h"
#include "typedef.h"

static int stdget_structure_constant_(addr pos, addr *ret,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		return clos_checkelt_(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		return clos_check_(pos, check, ret);
	}
}

static int stdset_structure_constant_(addr pos, addr value,
		enum Clos_structure_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_structure_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STRUCTURE_CLASS, &check);
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
#define StdGetStructure_(p,r,a,b) \
	stdget_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)
#define StdSetStructure_(p,r,a,b) \
	stdset_structure_constant_((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)

int stdget_structure_name_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, name, NAME);
}
int stdset_structure_name_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, name, NAME);
}

int stdget_structure_direct_slots_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, direct_slots, DIRECT_SLOTS);
}
int stdset_structure_direct_slots_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, direct_slots, DIRECT_SLOTS);
}

int stdget_structure_slots_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, slots, SLOTS);
}
int stdset_structure_slots_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, slots, SLOTS);
}

int stdget_structure_documentation_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, documentation, DOCUMENTATION);
}
int stdset_structure_documentation_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, documentation, DOCUMENTATION);
}

int stdget_structure_include_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, include, INCLUDE);
}
int stdset_structure_include_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, include, INCLUDE);
}

int stdget_structure_precedence_list_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
int stdset_structure_precedence_list_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

int stdget_structure_value_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, value, VALUE);
}
int stdset_structure_value_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, value, VALUE);
}

int stdget_structure_predicate_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, predicate, PREDICATE);
}
int stdset_structure_predicate_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, predicate, PREDICATE);
}

int stdget_structure_access_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, access, ACCESS);
}
int stdset_structure_access_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, access, ACCESS);
}

int stdget_structure_copier_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, copier, COPIER);
}
int stdset_structure_copier_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, copier, COPIER);
}

int stdget_structure_constructor_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, constructor, CONSTRUCTOR);
}
int stdset_structure_constructor_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, constructor, CONSTRUCTOR);
}

