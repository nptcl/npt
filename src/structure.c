#include "clos.h"
#include "clos_instance.h"
#include "clos_object.h"
#include "clos_type.h"
#include "closget_structure.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "equal.h"
#include "execute.h"
#include "hold.h"
#include "integer.h"
#include "structure.h"
#include "structure_define.h"
#include "structure_define1.h"
#include "structure_define2.h"
#include "structure_make.h"
#include "structure_object.h"
#include "symbol.h"
#include "type_object.h"
#include "type_upgraded.h"
#include "typedef.h"

/*
 *  control
 */
int structure_class_p(addr pos)
{
	addr x, y;

	if (! closp(pos))
		return 0;
	GetClassOfClos(pos, &x);
	GetConst(CLOS_STRUCTURE_CLASS, &y);

	return x == y;
}

int structure_class_object_p(addr pos)
{
	return structure_object_p(pos) || structure_class_p(pos);
}

int structure_instance_p(addr pos)
{
	if (! closp(pos))
		return 0;
	GetClassOfClos(pos, &pos);
	return structure_class_p(pos);
}

static int equalcall_structure_(addr a, addr b, int *ret,
		int (*call)(addr, addr, int *))
{
	int check;
	addr c, d;
	size_t x, y;

	/* class-of */
	if (! closp(a))
		return Result(ret, 0);
	if (! closp(b))
		return Result(ret, 0);
	GetClassOfClos(a, &c);
	GetClassOfClos(b, &d);
	if (c != d)
		return Result(ret, 0);
	if (! structure_class_p(c))
		return Result(ret, 0);

	/* slots */
	GetValueClos(a, &a);
	GetValueClos(b, &b);
	LenClosValue(a, &x);
	LenClosValue(b, &y);
	if (x != y)
		return Result(ret, 0);
	for (x = 0; x < y; x++) {
		GetClosValue(a, x, &c);
		GetClosValue(b, x, &d);
		Return((*call)(c, d, &check));
		if (! check)
			return Result(ret, 0);
	}

	return Result(ret, 1);
}

int equalp_structure_(addr a, addr b, int *ret)
{
	return equalcall_structure_(a, b, ret, equalp_function_);
}

int equalrt_structure_(addr a, addr b, int *ret)
{
	return equalcall_structure_(a, b, ret, equalrt_function_);
}

int typep_structure_(addr instance, addr clos, int *ret)
{
	addr list;

	Check(! structure_class_p(clos), "type error");
	if (! structure_instance_p(instance))
		return Result(ret, 0);

	/* subtypep */
	GetClassOfClos(instance, &list);
	Return(stdget_structure_precedence_list_(list, &list));
	return Result(ret, find_list_eq_unsafe(clos, list));
}

int structure_get_object(addr pos, addr *ret)
{
	if (! symbolp(pos)) {
		*ret = Nil;
		return 0;
	}

	getstructure_symbol(pos, &pos);
	if (pos == Nil) {
		*ret = Nil;
		return 0;
	}

	*ret = pos;
	return 1;
}

int structure_get_class(addr pos, addr *ret)
{
	if (! symbolp(pos)) {
		*ret = Nil;
		return 0;
	}

	clos_find_class_nil(pos, &pos);
	if (pos == Nil) {
		*ret = Nil;
		return 0;
	}

	*ret = pos;
	return 1;
}

int structure_get(addr pos, addr *ret)
{
	return structure_get_class(pos, ret)
		|| structure_get_object(pos, ret);
}

static int structure_get_type_upgraded_(Execute ptr, addr pos, addr *ret)
{
	enum ARRAY_TYPE type1;
	int type2;
	addr vector;

	gettype_structure(pos, &type1, &type2);
	upgraded_array_object(type1, type2, &pos);
	Return(type_object_(ptr, &pos, pos));
	GetConst(COMMON_VECTOR, &vector);
	list_heap(ret, vector, pos, NULL);

	return 0;
}

int structure_get_type_(Execute ptr, addr pos, addr *ret)
{
	addr value;

	/* class */
	if (structure_get_class(pos, &value)) {
		GetConst(COMMON_CLASS, ret);
		return 0;
	}

	/* object */
	if (structure_get_object(pos, &value)) {
		if (structure_list_p(value)) {
			GetConst(COMMON_LIST, ret);
			return 0;
		}
		if (structure_vector_p(value))
			return structure_get_type_upgraded_(ptr, value, ret);
	}

	/* nil */
	return Result(ret, Nil);
}


/*
 *  documentation
 */
static int getdoc_structure_class_(addr symbol, addr *ret)
{
	addr pos;

	clos_find_class_nil(symbol, &pos);
	if (pos == Nil)
		return Result(ret, Unbound);
	if (! structure_class_p(pos))
		return Result(ret, Unbound);

	return stdget_structure_documentation_(pos, ret);
}

static int getdoc_structure_symbol_(addr symbol, addr *ret)
{
	addr pos;

	getstructure_symbol(symbol, &pos);
	if (pos == Nil)
		return Result(ret, Unbound);
	GetDocStructure(pos, ret);

	return 0;
}

int getdoc_structure_(addr symbol, addr *ret)
{
	addr pos;

	/* class */
	Return(getdoc_structure_class_(symbol, &pos));
	if (pos != Unbound)
		return Result(ret, pos);

	/* symbol */
	Return(getdoc_structure_symbol_(symbol, &pos));
	if (pos != Unbound)
		return Result(ret, pos);

	/* error */
	return TypeError_(symbol, STRUCTURE_CLASS);
}

static int setdoc_structure_class_(addr symbol, addr value, int *ret)
{
	addr pos;

	clos_find_class_nil(symbol, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	if (! structure_class_p(pos))
		return Result(ret, 0);
	Return(stdset_structure_documentation_(pos, value));

	return Result(ret, 1);
}

static int setdoc_structure_symbol_(addr symbol, addr value, int *ret)
{
	addr pos;

	getstructure_symbol(symbol, &pos);
	if (pos == Nil)
		return Result(ret, 0);
	SetDocStructure(pos, value);

	return Result(ret, 1);
}

int setdoc_structure_(addr symbol, addr value)
{
	int check;

	/* class */
	Return(setdoc_structure_class_(symbol, value, &check));
	if (check)
		return 0;

	/* symbol */
	Return(setdoc_structure_symbol_(symbol, value, &check));
	if (check)
		return 0;

	/* error */
	return TypeError_(symbol, STRUCTURE_CLASS);
}


/*
 *  structure-constructor
 */
int structure_reader_(Execute ptr, addr symbol, addr rest, addr *ret)
{
	return structure_constructor_common_(ptr, symbol, rest, ret);
}

int structure_constructor_common_(Execute ptr, addr symbol, addr rest, addr *ret)
{
	addr instance;

	if (! symbolp(symbol)) {
		*ret = Nil;
		return fmte_("The first argument ~S must be a symbol type.", symbol, NULL);
	}

	/* class */
	if (structure_get_class(symbol, &instance)) {
		if (! structure_class_p(instance)) {
			*ret = Nil;
			return fmte_("The class ~S don't be a structure-class.", symbol, NULL);
		}
		return make_structure1_common_(ptr, ret, instance, rest, 0, 1);
	}

	/* object */
	if (structure_get_object(symbol, &instance)) {
		if (structure_object_p(instance)) {
			if (structure_list_p(instance))
				return make_structure2_common_(ptr, ret, instance, rest, 0, 1);
			if (structure_vector_p(instance))
				return make_structure3_common_(ptr, ret, instance, rest, 0, 1);
		}
	}

	/* error */
	*ret = Nil;
	return fmte_("The value ~S must be a structure object.", symbol, NULL);
}

int allocate_instance_structure_(Execute ptr, addr clos, addr *ret)
{
	return make_structure1_common_(ptr, ret, clos, Nil, 1, 0);
}

int make_instance_structure_(Execute ptr, addr rest, addr *ret)
{
	addr instance;
	Return_getcons(rest, &instance, &rest);
	return make_structure1_common_(ptr, ret, instance, rest, 1, 1);
}


/*
 *  copy-structure
 */
void copy_structure_common(addr inst, addr *ret)
{
	addr class_of, slots, clos, src, dst, pos;
	size_t size, i;

	Check(! structure_instance_p(inst), "type error");
	/* source */
	GetClassOfClos(inst, &class_of);
	GetSlotClos(inst, &slots);
	GetValueClos(inst, &src);
	/* destination */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, class_of);
	GetValueClos(clos, &dst);
	/* value */
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetClosValue(src, i, &pos);
		SetClosValue(dst, i, pos);
	}
	/* result */
	*ret = clos;
}


/*
 *  initialize
 */
void init_structure(void)
{
	init_structure_define();
}

