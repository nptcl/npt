#include "array_vector.h"
#include "clos.h"
#include "clos_slot.h"
#include "condition.h"
#include "cons_list.h"
#include "heap.h"
#include "sequence.h"
#include "structure_direct.h"
#include "type_typep.h"
#include "typedef.h"

/*
 *  structure-type
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
 *  access
 */
static int structure_getdirect_(addr vector, size_t index, addr *ret)
{
	return getelt_sequence_(NULL, vector, index, ret);
}

int structure_getarray_(addr vector, addr slot, addr *ret)
{
	size_t index;
	GetAccessSlot(slot, &index);
	return structure_getdirect_(vector, index, ret);
}

static int structure_setcheck_error_(Execute ptr, addr type, addr value)
{
	int check;

	CheckType(type, LISPTYPE_TYPE);
	Return(typep_clang_(ptr, value, type, &check));
	if (! check) {
		return call_type_error_va_(ptr, value, type,
				"The value ~S don't match ~A type.", value, type, NULL);
	}

	return 0;
}

int structure_write1_(Execute ptr, addr instance, addr slot, addr value)
{
	addr type, var;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type);
	if (value != Unbound) {
		Return(structure_setcheck_error_(ptr, type, value));
	}
	else {
		value = Nil;
	}

	/* write */
	GetLocationSlot(slot, &index);
	GetValueClos(instance, &var);
	SetClosValue(var, index, value);
	return 0;
}

int structure_write2_(Execute ptr, addr list, addr slot, addr value)
{
	addr type;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type);
	Return(structure_setcheck_error_(ptr, type, value));

	/* write */
	GetAccessSlot(slot, &index);
	return setnth_(list, index, value);
}

int structure_write3_(Execute ptr, addr vector, addr slot, addr type1, addr value)
{
	addr type2;
	size_t index;

	/* check */
	GetTypeSlot(slot, &type2);
	Return(structure_setcheck_error_(ptr, type1, value));
	Return(structure_setcheck_error_(ptr, type2, value));

	/* write */
	GetAccessSlot(slot, &index);
	return setelt_sequence_(vector, index, value);
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
		size_t size, size_t size_all, unsigned named, size_t named_index,
		enum ARRAY_TYPE type1, int type2)
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
	str->size_all = size_all;
	str->named = named;
	str->named_index = named_index;
	str->errorp = 0;
	str->type1 = type1;
	str->type2 = type2;
	*ret = pos;
}

void structure_type(struct defstruct *str, addr slot, addr *ret)
{
	structure_type_parameter(ret,
			str->instance, str->name, slot, str->type_vector,
			str->size, str->size_all, str->named_p, str->named_index,
			str->type1, str->type2);
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
	if (size < str->size_all)
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
	size_t size;

	/* vectorp */
	if (! vector_type_p(var)) {
		*ret = 0;
		return 0;
	}
	Return(length_sequence_(var, 1, &size));
	/* length */
	str = PtrStructureType(type);
	if (size < str->size_all) {
		*ret = 0;
		return 0;
	}
	/* check */
	if (str->named) {
		Return(structure_getdirect_(var, str->named_index, &var));
		GetNameStructureType(type, &type);
		*ret = (var == type);
		return 0;
	}
	*ret = 1;
	return 0;
}

