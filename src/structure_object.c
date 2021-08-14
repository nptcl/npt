#include "array_vector.h"
#include "condition.h"
#include "constant.h"
#include "clos.h"
#include "cons_list.h"
#include "heap.h"
#include "hold.h"
#include "sequence.h"
#include "structure_object.h"
#include "type_typep.h"
#include "type_parse.h"
#include "typedef.h"

/*
 *  access
 */
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

int stdget_structure_type_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, type, TYPE);
}
int stdset_structure_type_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, type, TYPE);
}

int stdget_structure_vector_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, vector, VECTOR);
}
int stdset_structure_vector_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, vector, VECTOR);
}

int stdget_structure_named_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, named, NAMED);
}
int stdset_structure_named_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, named, NAMED);
}

int stdget_structure_named_index_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, named_index, NAMED_INDEX);
}
int stdset_structure_named_index_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, named_index, NAMED_INDEX);
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

int stdget_structure_print_(addr pos, addr *ret)
{
	return StdGetStructure_(pos, ret, print, PRINT);
}
int stdset_structure_print_(addr pos, addr value)
{
	return StdSetStructure_(pos, value, print, PRINT);
}


/*
 *  structure
 */
void localhold_destruct(LocalHold hold, struct defstruct *str)
{
	localhold_pushva_force(hold, str->instance, str->env, str->doc, str->slots,
			str->name, str->conc_name, str->copier, str->predicate,
			str->constructor, str->iname, str->iargs,
			str->print_object, str->print_function,
			str->type_vector, str->initial_offset, NULL);
}

void defstruct_clean(struct defstruct *str)
{
	clearpoint(str);
	str->conc_name = Unbound;
	str->copier = Nil;
	str->predicate = Nil;
	str->iname = Nil;
	str->iargs = Nil;
	str->constructor = Nil;
	str->type_vector = Unbound;
	str->print_function = Unbound;
	str->print_object = Unbound;
	str->size = 0;
	str->size_value = 0;
	str->offset = 0;
	str->named_index = 0;
	str->change = Nil;
}


/*
 *  structure-type object
 */
struct structure_type_struct *ptrstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return PtrStructureType_Low(pos);
}

void getinstancestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetInstanceStructureType_Low(pos, ret);
}

void setinstancestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetInstanceStructureType_Low(pos, value);
}

void getnamestructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNameStructureType_Low(pos, ret);
}

void setnamestructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNameStructureType_Low(pos, value);
}

void getslotstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetSlotStructureType_Low(pos, ret);
}

void setslotstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetSlotStructureType_Low(pos, value);
}

void getvectorstructuretype(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetVectorStructureType_Low(pos, ret);
}

void setvectorstructuretype(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetVectorStructureType_Low(pos, value);
}

int refnamedstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefNamedStructureType_Low(pos);
}

void getnamedstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetNamedStructureType_Low(pos, ret);
}

void setnamedstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetNamedStructureType_Low(pos, value);
}

int referrorpstructuretype(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	return RefErrorpStructureType_Low(pos);
}

void geterrorpstructuretype(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	GetErrorpStructureType_Low(pos, ret);
}

void seterrorpstructuretype(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE_TYPE);
	SetErrorpStructureType_Low(pos, value);
}

static void structure_type_heap_unsafe(addr *ret)
{
	heap_smallsize(ret, LISPSYSTEM_STRUCTURE_TYPE,
			StructureType_size, sizeoft(struct structure_type_struct));
}


/*
 *  access vector
 */
int structure_getdirect_(Execute ptr, addr vector, size_t i, addr type, addr *ret)
{
	int check;
	addr value;

	Return(getelt_sequence_(NULL, vector, i, &value));
	Return(parse_type(ptr, &type, type, Nil));
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return fmte_("The value ~S don't match ~A type.", value, type, NULL);
	}

	return Result(ret, value);
}

int structure_setdirect_(Execute ptr, addr vector, size_t i, addr type, addr value)
{
	int check;

	Return(parse_type(ptr, &type, type, Nil));
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return fmte_("The value ~S don't match ~A type.", value, type, NULL);
	}

	return setelt_sequence_(vector, i, value);
}

int structure_getarray_(Execute ptr, addr vector, addr slot, addr type, addr *ret)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_getdirect_(ptr, vector, i, type, ret);
}

int structure_setarray_(Execute ptr, addr vector, addr slot, addr type, addr value)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_setdirect_(ptr, vector, i, type, value);
}


/*
 *  structure-type
 */
void structure_type_heap(addr *ret)
{
	addr pos;
	structure_type_heap_unsafe(&pos);
	clearpoint(PtrStructureType(pos));
	*ret = pos;
}

static void structure_type_parameter(addr *ret,
		addr instance, addr name, addr slot, addr vector,
		size_t size, size_t value, unsigned named, size_t named_index)
{
	addr pos;
	struct structure_type_struct *str;

	structure_type_heap_unsafe(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slot);
	SetVectorStructureType(pos, vector);
	str = PtrStructureType(pos);
	str->size = size;
	str->size_value = value;
	str->named = named;
	str->named_index = named_index;
	str->errorp = 0;
	*ret = pos;
}

void structure_type(struct defstruct *str, addr slot, addr *ret)
{
	structure_type_parameter(ret,
			str->instance, str->name, slot, str->type_vector,
			str->size, str->size_value, str->named_p, str->named_index);
}

int structure_type_list_p(addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	size_t size;

	/* listp */
	str = PtrStructureType(type);
	if (length_list_p(var, &size))
		return Result(ret, 0);
	/* length */
	if (size < str->size_value)
		return Result(ret, 0);
	/* check */
	if (str->named) {
		GetNameStructureType(type, &type);
		Return(getnth_(var, str->named_index, &var));
		return Result(ret, var == type);
	}

	return Result(ret, 1);
}

int structure_type_vector_p(Execute ptr, addr type, addr var, int *ret)
{
	struct structure_type_struct *str;
	addr check;
	size_t size;

	/* vectorp */
	if (! vector_type_p(var)) {
		*ret = 0;
		return 0;
	}
	Return(length_sequence_(var, 1, &size));
	/* length */
	str = PtrStructureType(type);
	if (size < str->size_value) {
		*ret = 0;
		return 0;
	}
	/* check */
	if (str->named) {
		GetVectorStructureType(type, &check);
		Return(structure_getdirect_(ptr, var, str->named_index, check, &var));
		GetNameStructureType(type, &type);
		*ret = (var == type);
		return 0;
	}
	*ret = 1;
	return 0;
}

