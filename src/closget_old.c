#include "clos_object.h"
#include "closget.h"
#include "closget_old.h"
#include "closget_slot.h"
#include "constant.h"
#include "condition.h"
#include "execute.h"
#include "symbol.h"
#include "typedef.h"

int clos_errorp(addr pos, size_t index, constindex name)
{
	addr key, slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	GetConstant(name, &key);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		getname_slot_unsafe(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key)
			return index != i;
	}

	return 1;
}

int clos_getp(addr pos, addr key, addr *ret)
{
	addr slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		getname_slot_unsafe(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key) {
			GetClosValue(vector, i, ret);
			return 1;
		}
	}

	return 0;
}

int clos_setp(addr pos, addr key, addr value)
{
	addr slot, vector, check;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	Check(! symbolp(key), "type error");
	GetSlotClos_Low(pos, &slot);
	GetValueClos_Low(pos, &vector);
	LenSlotVector(slot, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slot, i, &pos);
		getname_slot_unsafe(pos, &check);
		Check(! symbolp(check), "type error");
		if (check == key) {
			SetClosValue(vector, i, value);
			return 1;
		}
	}

	return 0;
}

int clos_checkp_(addr pos, addr key, addr *value, int *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, &check))
		return Result(ret, 0);
	if (check == Unbound) {
		*ret = 0;
		return call_unbound_slot_(NULL, pos, key);
	}
	*value = check;
	return Result(ret, 1);
}

int clos_get_(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, ret)) {
		*ret = Nil;
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);
	}

	return 0;
}

int clos_set_(addr pos, addr key, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_setp(pos, key, value))
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);

	return 0;
}

int clos_check_(addr pos, addr key, addr *ret)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	Return(clos_checkp_(pos, key, ret, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("The slot name ~S don't exist in the ~S.", key, pos, NULL);
	}

	return 0;
}

void clos_getelt(addr pos, size_t index, addr *ret)
{
	addr vector;
#ifdef LISP_DEBUG
	size_t size;
#endif

	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, &vector);
#ifdef LISP_DEBUG
	LenClosValue(vector, &size);
	Check(size <= index, "size error");
#endif
	GetClosValue(vector, index, ret);
}

void clos_setelt(addr pos, size_t index, addr value)
{
	addr vector;
#ifdef LISP_DEBUG
	size_t size;
#endif

	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, &vector);
#ifdef LISP_DEBUG
	LenClosValue(vector, &size);
	Check(size <= index, "size error");
#endif
	SetClosValue(vector, index, value);
}

int clos_checkelt_(addr pos, size_t index, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	clos_getelt(pos, index, &check);
	if (check == Unbound) {
		*ret = Nil;
		return call_unbound_slot_(NULL, pos, check);
	}
	else {
		return Result(ret, check);
	}
}

int clos_getconst_(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return clos_get_(pos, key, ret);
}

int clos_setconst_(addr pos, constindex index, addr value)
{
	addr key;
	GetConstant(index, &key);
	return clos_set_(pos, key, value);
}

int clos_checkconst_(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	return clos_check_(pos, key, ret);
}


/*
 *  check
 */
int clos_slot_exists_p(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	return clos_getp(pos, name, &name);
}

int clos_slot_boundp_nil(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_getp(pos, name, &name))
		return name != Unbound;
	else
		return -1; /* error no slot. */
}

int clos_slot_boundp_(addr pos, addr name, int *ret)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	check = clos_slot_boundp_nil(pos, name);
	if (check < 0) {
		*ret = 0;
		return fmte_("The pos object ~S have no ~S slot.", pos, name, NULL);
	}

	return Result(ret, check);
}

int clos_slot_makunbound_nil_(addr pos, addr name, int *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_slot_exists_p(pos, name)) {
		Return(clos_set_(pos, name, Unbound));
		return Result(ret, 0);
	}
	/* error */
	return Result(ret, 1);
}

int clos_slot_makunbound_(addr pos, addr name)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	Return(clos_slot_makunbound_nil_(pos, name, &check));
	if (check)
		return fmte_("The slot have no ~S.", name, NULL);

	return 0;
}

