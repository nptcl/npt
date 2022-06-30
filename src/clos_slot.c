#include "clos_define.h"
#include "clos_slot.h"
#include "closget_slot.h"
#include "condition.h"
#include "memory.h"
#include "typedef.h"

#define PtrSlotStruct_Low(x)	PtrBodySSa((x),SLOT_INDEX_SIZE)
#define SlotStruct_Low(x)		((struct slot_struct *)PtrSlotStruct_Low(x))

struct slot_struct *struct_slot(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return SlotStruct_Low(pos);
}

int slotp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT;
}

static inline void slot_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPSYSTEM_SLOT, SLOT_INDEX_SIZE, sizeoft(struct slot_struct));
}
int slot_alloc_(Execute ptr, LocalRoot local, addr *ret)
{
	addr pos;

	slot_unsafe(local, &pos);
	Return(setallocation_slot_(ptr, pos, 0));
	Return(setlocation_slot_(ptr, pos, 0));
	Return(setaccess_slot_(ptr, pos, 0));
	Return(setname_slot_(ptr, pos, Unbound));
	Return(setform_slot_(ptr, pos, Unbound));

	return Result(ret, pos);
}
int slot_local_(Execute ptr, addr *ret)
{
	return slot_alloc_(ptr, ptr->local, ret);
}
int slot_heap_(Execute ptr, addr *ret)
{
	return slot_alloc_(ptr, NULL, ret);
}

int slot_copy_alloc_(Execute ptr, LocalRoot local, addr *ret, addr slot)
{
	int check;
	addr pos, value;
	struct slot_struct *str1, *str2;
	size_t i;

	CheckType(slot, LISPSYSTEM_SLOT);
	slot_unsafe(local, &pos);

	/* value */
	str1 = SlotStruct_Low(slot);
	str2 = SlotStruct_Low(pos);
	str2->location = str1->location;
	str2->access = str1->access;
	Return(getallocation_slot_(ptr, slot, &check));
	Return(setallocation_slot_(ptr, pos, check));

	/* array */
	for (i = 0; i < SLOT_INDEX_SIZE; i++) {
		GetArraySS(slot, i, &value);
		SetArraySS(pos, i, value);
	}

	/* result */
	return Result(ret, pos);
}
int slot_copy_local_(Execute ptr, addr *ret, addr slot)
{
	return slot_copy_alloc_(ptr, ptr->local, ret, slot);
}
int slot_copy_heap_(Execute ptr, addr *ret, addr slot)
{
	return slot_copy_alloc_(ptr, NULL, ret, slot);
}


/*
 *  control
 */
int slot_class_p_(Execute ptr, addr pos, int *ret)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	Return(getallocation_slot_(ptr, pos, &check));

	return Result(ret, check != 0);
}

int slot_instance_p_(Execute ptr, addr pos, int *ret)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	Return(getallocation_slot_(ptr, pos, &check));

	return Result(ret, check == 0);
}

int slot_set_class_(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return setallocation_slot_(ptr, pos, 1);
}

int slot_set_instance_(Execute ptr, addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return setallocation_slot_(ptr, pos, 0);
}

int slot_set_allocation_(Execute ptr, addr pos, addr value)
{
	addr check;

	CheckType(pos, LISPSYSTEM_SLOT);

	/* instance */
	GetConst(KEYWORD_INSTANCE, &check);
	if (check == value)
		return slot_set_instance_(ptr, pos);

	/* class */
	GetConst(KEYWORD_CLASS, &check);
	if (check == value)
		return slot_set_class_(ptr, pos);

	/* error */
	return fmte_("Invalid :allocation value ~S.", value, NULL);
}

