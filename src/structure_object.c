#include "heap.h"
#include "structure_object.h"
#include "typedef.h"

int structure_object_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_STRUCTURE;
}

struct structure_struct *ptrstructure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure_Low(pos);
}

void getnamestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetNameStructure_Low(pos, ret);
}

void setnamestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetNameStructure_Low(pos, value);
}

void getslotsstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetSlotsStructure_Low(pos, ret);
}

void setslotsstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetSlotsStructure_Low(pos, value);
}

void getdirectstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetDirectStructure_Low(pos, ret);
}

void setdirectstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetDirectStructure_Low(pos, value);
}

void getdocstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetDocStructure_Low(pos, ret);
}

void setdocstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetDocStructure_Low(pos, value);
}

void getincludestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetIncludeStructure_Low(pos, ret);
}

void setincludestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetIncludeStructure_Low(pos, value);
}

void getprecedencestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetPrecedenceStructure_Low(pos, ret);
}

void setprecedencestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetPrecedenceStructure_Low(pos, value);
}

void getspecializedstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetSpecializedStructure_Low(pos, ret);
}

void setspecializedstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetSpecializedStructure_Low(pos, value);
}

void getpredicatestructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetPredicateStructure_Low(pos, ret);
}

void setpredicatestructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetPredicateStructure_Low(pos, value);
}

void getaccessstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetAccessStructure_Low(pos, ret);
}

void setaccessstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetAccessStructure_Low(pos, value);
}

void getcopierstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetCopierStructure_Low(pos, ret);
}

void setcopierstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetCopierStructure_Low(pos, value);
}

void getconstructorstructure(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	GetConstructorStructure_Low(pos, ret);
}

void setconstructorstructure(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	SetConstructorStructure_Low(pos, value);
}

void structure_heap(addr *ret)
{
	addr pos;
	struct structure_struct *str;

	heap_smallsize(&pos, LISPSYSTEM_STRUCTURE,
			Structure_size, sizeoft(struct structure_struct));
	str = PtrStructure_Low(pos);
	clearpoint(str);
	*ret = pos;
}

int structure_named_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->named_p;
}

int structure_list_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->list_p;
}

int structure_vector_p(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->vector_p;
}

void set_named_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->named_p = (value != 0);
}

void set_list_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->list_p = (value != 0);
}

void set_vector_p_structure(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->vector_p = (value != 0);
}

size_t get_size_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->size;
}

size_t get_size_all_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->size_all;
}

size_t get_offset_structure(addr pos)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	return PtrStructure(pos)->offset;
}

void set_size_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->size = value;
}

void set_size_all_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->size_all = value;
}

void set_offset_structure(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_STRUCTURE);
	PtrStructure(pos)->offset = value;
}

void gettype_structure(addr pos, enum ARRAY_TYPE *rtype1, int *rtype2)
{
	struct structure_struct *str;

	str = PtrStructure(pos);
	*rtype1 = str->type1;
	*rtype2 = str->type2;
}

void settype_structure(addr pos, enum ARRAY_TYPE type1, int type2)
{
	struct structure_struct *str;

	str = PtrStructure(pos);
	str->type1 = type1;
	str->type2 = type2;
}

void structure_swap(addr x, addr y)
{
	int i;
	addr array[Structure_size], z;
	struct structure_struct *strx, *stry, str;

	CheckType(x, LISPSYSTEM_STRUCTURE);
	CheckType(y, LISPSYSTEM_STRUCTURE);
	strx = PtrStructure(x);
	stry = PtrStructure(y);

	/* x -> temp */
	str = *strx;
	for (i = 0; i < Structure_size; i++) {
		GetArraySS(x, i, &z);
		array[i] = z;
	}

	/* y -> x */
	*strx = *stry;
	for (i = 0; i < Structure_size; i++) {
		GetArraySS(y, i, &z);
		SetArraySS(x, i, z);
	}

	/* temp -> y */
	*stry = str;
	for (i = 0; i < Structure_size; i++) {
		z = array[i];
		SetArraySS(y, i, z);
	}
}

