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
void slot_alloc(LocalRoot local, addr *ret)
{
	addr pos;

	slot_unsafe(local, &pos);
	setallocation_slot(pos, 0);
	setlocation_slot(pos, 0);
	setaccess_slot(pos, 0);
	setname_slot(pos, Unbound);
	setform_slot(pos, Unbound);
	*ret = pos;
}
void slot_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	slot_alloc(local, ret);
}
void slot_heap(addr *ret)
{
	slot_alloc(NULL, ret);
}

void slot_copy_alloc(LocalRoot local, addr *ret, addr slot)
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
	getallocation_slot(slot, &check);
	setallocation_slot(pos, check);

	/* array */
	for (i = 0; i < SLOT_INDEX_SIZE; i++) {
		GetArraySS(slot, i, &value);
		SetArraySS(pos, i, value);
	}

	/* result */
	*ret = pos;
}
void slot_copy_local(LocalRoot local, addr *ret, addr slot)
{
	CheckLocal(local);
	slot_copy_alloc(local, ret, slot);
}
void slot_copy_heap(addr *ret, addr slot)
{
	slot_copy_alloc(NULL, ret, slot);
}


/*
 *  control
 */
int slot_class_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	getallocation_slot(pos, &check);

	return check != 0;
}

int slot_instance_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	getallocation_slot(pos, &check);

	return check == 0;
}

void slot_set_class(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	setallocation_slot(pos, 1);
}

void slot_set_instance(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	setallocation_slot(pos, 0);
}

int slot_set_allocation_(addr pos, addr value)
{
	addr check;

	CheckType(pos, LISPSYSTEM_SLOT);

	/* instance */
	GetConst(KEYWORD_INSTANCE, &check);
	if (check == value) {
		slot_set_instance(pos);
		return 0;
	}

	/* class */
	GetConst(KEYWORD_CLASS, &check);
	if (check == value) {
		slot_set_class(pos);
		return 0;
	}

	/* error */
	return fmte_("Invalid :allocation value ~S.", value, NULL);
}

