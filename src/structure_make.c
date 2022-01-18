#include "array_make.h"
#include "condition.h"
#include "clos.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "execute.h"
#include "sequence.h"
#include "structure_access.h"
#include "structure_direct.h"
#include "structure_make.h"
#include "structure_object.h"
#include "strtype.h"
#include "symbol.h"
#include "typedef.h"

/*
 *  find
 */
static int make_structure_dynamic_find_(addr key, addr slots, int *ret)
{
	int check;
	addr value;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(key, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int make_structure_dynamic_p_(int errorp, addr key, addr slots, int *ret)
{
	int check;

	if (! errorp)
		return Result(ret, 0);
	Return(make_structure_dynamic_find_(key, slots, &check));
	return Result(ret, ! check);
}

static int make_structure_dynamic_(addr instance, addr slots, addr list, int errorp)
{
	int check;
	addr key;

	while (list != Nil) {
		if (! consp_getcons(list, &key, &list))
			return fmte_("Invalid keyword-argumets ~S.", list, NULL);
		if (! consp(list))
			return fmte_("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			return fmte_("The key ~S must be a symbol type.", key, NULL);
		Return(make_structure_dynamic_p_(errorp, key, slots, &check));
		if (check) {
			return fmte_("There is no slot ~S "
					"in the structure ~S.", key, instance, NULL);
		}
		GetCdr(list, &list);
	}

	return 0;
}

static int make_structure_find_(addr key, addr list, addr *value, int *ret)
{
	int check;
	addr left, right, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		Return_getcons(list, &left, &list);
		Return_getcons(list, &right, &list);
		Check(! symbolp(left), "type error");
		if (right == g)
			continue;
		GetNameSymbol(left, &left);
		Return(string_equal_(key, left, &check));
		if (check) {
			*value = right;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}


/*
 *  init
 */
static int make_structure_value_(Execute ptr, addr slot, addr *ret)
{
	addr pos;

	/* initfunction */
	GetFunctionSlot(slot, &pos);
	if (pos != Nil)
		return callclang_apply(ptr, ret, pos, Nil);

	/* initform */
	GetFormSlot(slot, ret);
	return 0;
}

static int make_structure1_init_(Execute ptr, addr clos, addr args, int initp)
{
	int check;
	addr slots, slot, pos;
	size_t size, i;

	GetSlotClos(clos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (! initp) {
			Return(structure_write1_(ptr, clos, slot, Unbound));
			continue;
		}
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write1_(ptr, clos, slot, pos));
	}

	return 0;
}

static int make_structure2_init_(Execute ptr, addr list, addr slots, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write2_(ptr, list, slot, pos));
	}

	return 0;
}

static int make_structure3_init_(Execute ptr,
		addr vector, addr slots, addr vector_type, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(make_structure_find_(slot, args, &pos, &check));
		if (! check) {
			Return(make_structure_value_(ptr, slot, &pos));
		}
		Return(structure_write3_(ptr, vector, slot, vector_type, pos));
	}

	return 0;
}


/*
 *  make
 */
int make_structure1_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	int errorp;
	addr instance, slots, clos;
	LocalHold hold;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	Return(make_structure_dynamic_(instance, slots, args, errorp));
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);

	hold = LocalHold_local_push(ptr, clos);
	Return(make_structure1_init_(ptr, clos, args, initp));
	localhold_end(hold);

	return Result(ret, clos);
}

static void make_structure2_nil(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

int make_structure2_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, list;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	Return(make_structure_dynamic_(instance, slots, args, str->errorp));
	make_structure2_nil(&list, str->size_all);

	if (initp) {
		hold = LocalHold_local_push(ptr, list);
		Return(make_structure2_init_(ptr, list, slots, args));
		localhold_end(hold);
	}

	return Result(ret, list);
}

static int make_structure3_vector_(addr *ret,
		enum ARRAY_TYPE type1, int type2, size_t size)
{
	struct array_struct *str;
	addr pos;

	/* vector */
	if (type1 == ARRAY_TYPE_T) {
		vector_heap(ret, size);
		return 0;
	}

	/* array */
	array_empty_heap(&pos);
	str = ArrayInfoStruct(pos);
	str->type = type1;
	str->bytesize = type2;
	str->dimension = 1;
	str->size = str->front = size;
	array_set_type(pos);
	array_set_element_size(pos);
	array_set_simple(pos);
	Return(array_make_memory_(pos, Nil, Nil, Nil, Nil));
	Return(array_make_clear_(pos));
	return Result(ret, pos);
}

int make_structure3_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, vector, type;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	Return(make_structure_dynamic_(instance, slots, args, str->errorp));
	Return(make_structure3_vector_(&vector, str->type1, str->type2, str->size_all));

	if (initp) {
		hold = LocalHold_local_push(ptr, vector);
		Return(make_structure3_init_(ptr, vector, slots, type, args));
		localhold_end(hold);
	}

	return Result(ret, vector);
}


/*
 *  common
 */
int make_structure1_common_(Execute ptr, addr *ret,
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
	Return(make_structure1_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

int make_structure2_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	int named;
	addr pos, name, slots;
	size_t size, size_all;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	GetNameStructure(instance, &name);
	GetSlotsStructure(instance, &slots);
	named = structure_named_p(instance);
	size_all = get_size_all_structure(instance);
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != 0);
	str->size = size;
	str->size_all = size_all;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure2_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

int make_structure3_common_(Execute ptr, addr *ret,
		addr instance, addr rest, int errorp, int initp)
{
	enum ARRAY_TYPE type1;
	int named, type2;
	addr pos, name, slots, type;
	size_t size, size_all;
	struct structure_type_struct *str;
	LocalHold hold;

	/* value */
	GetNameStructure(instance, &name);
	GetSlotsStructure(instance, &slots);
	GetSpecializedStructure(instance, &type);
	named = structure_named_p(instance);
	size_all = get_size_all_structure(instance);
	gettype_structure(instance, &type1, &type2);

	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	SetVectorStructureType(pos, type);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = (errorp != 0);
	str->named = (named != 0);
	str->size = size;
	str->size_all = size_all;
	str->type1 = type1;
	str->type2 = type2;

	hold = LocalHold_local_push(ptr, pos);
	Return(make_structure3_(ptr, ret, pos, rest, initp));
	localhold_end(hold);

	return 0;
}

