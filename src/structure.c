#include "clos.h"
#include "clos_class.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "equal.h"
#include "execute.h"
#include "hold.h"
#include "integer.h"
#include "structure.h"
#include "structure_define.h"
#include "structure_make.h"
#include "structure_object.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  control
 */
int structure_class_p_(addr pos, int *ret)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	Return(clos_class_of_(pos, &pos));
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	return clos_subclass_p_(pos, check, ret);
}

int structure_class_p_debug(addr pos)
{
	int check;
	check = 0;
	Error(structure_class_p_(pos, &check));
	return check;
}

int structure_instance_p_(addr pos, int *ret)
{
	addr right;

	if (GetType(pos) != LISPTYPE_CLOS)
		return Result(ret, 0);
	Return(clos_class_of_(pos, &pos));
	GetConst(CLOS_STRUCTURE_OBJECT, &right);
	return clos_subclass_p_(pos, right, ret);
}

int structure_instance_p_debug(addr pos)
{
	int check;
	check = 0;
	Error(structure_instance_p_(pos, &check));
	return check;
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
	Return(clos_class_of_(a, &c));
	Return(clos_class_of_(b, &d));
	if (c != d)
		return Result(ret, 0);
	GetConst(CLOS_STRUCTURE_OBJECT, &d);
	Return(clos_subclass_p_(c, d, &check));
	if (! check)
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

int typep_structure_(addr value, addr instance, int *ret)
{
	int check;

	Check(! structure_class_p_debug(instance), "type error");
	Return(structure_instance_p_(value, &check));
	if (! check)
		return Result(ret, 0);
	else
		return clos_subtype_p_(value, instance, ret);
}


/*
 *  structure-constructor
 */
static int make_structure_common_clos_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, value;
	LocalHold hold;

	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	Return(stdget_structure_slots_(instance, &value));
	SetSlotStructureType(pos, value);
	SetErrorpStructureType(pos, errorp);

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_clos_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_vector_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, slots, value, type, name, named;
	size_t size;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	Return(stdget_structure_slots_(instance, &slots));
	Return(stdget_structure_value_(instance, &value));
	Return(stdget_structure_vector_(instance, &type));
	Return(stdget_structure_name_(instance, &name));
	Return(stdget_structure_named_(instance, &named));
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	SetVectorStructureType(pos, type);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != Nil);
	str->size = size;
	Return(getindex_integer_(value, &size));
	str->size_value = size;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_vector_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_list_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr pos, slots, value, name, named;
	size_t size;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	Return(stdget_structure_slots_(instance, &slots));
	Return(stdget_structure_value_(instance, &value));
	Return(stdget_structure_name_(instance, &name));
	Return(stdget_structure_named_(instance, &named));
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != Nil);
	str->size = size;
	Return(getindex_integer_(value, &size));
	str->size_value = size;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure_list_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

static int make_structure_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	addr type, check;

	/* clos */
	Return(stdget_structure_type_(instance, &type));
	GetConst(COMMON_CLASS, &check);
	if (type == check)
		return make_structure_common_clos_(ptr, ret, instance, rest, errorp, initp);

	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (type == check)
		return make_structure_common_vector_(ptr, ret, instance, rest, errorp, initp);

	/* list */
	GetConst(COMMON_LIST, &check);
	if (type == check)
		return make_structure_common_list_(ptr, ret, instance, rest, errorp, initp);

	/* error */
	return fmte_("Invalid type value ~S.", type, NULL);
}

int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret)
{
	int check;
	addr instance;

	if (! symbolp(symbol))
		return fmte_("The first argument ~S must be a symbol type.", symbol, NULL);
	Return(clos_find_class_(symbol, &instance));
	Return(structure_class_p_(instance, &check));
	if (! check)
		return fmte_("The class ~S don't be a structure-class.", symbol, NULL);

	return make_structure_common_(ptr, ret, instance, rest, 0, 1);
}

int allocate_instance_structure_(Execute ptr, addr clos, addr *ret)
{
	return make_structure_common_(ptr, ret, clos, Nil, 1, 0);
}

int make_instance_structure(Execute ptr, addr rest, addr *ret)
{
	addr instance;
	Return_getcons(rest, &instance, &rest);
	return make_structure_common_(ptr, ret, instance, rest, 1, 1);
}


/*
 *  copy-structure
 */
void copy_structure_common(addr inst, addr *ret)
{
	addr class_of, slots, clos, src, dst, pos;
	size_t size, i;

	Check(! structure_instance_p_debug(inst), "type error");
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

