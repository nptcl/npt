#include "clos_object.h"
#include "clos_slot.h"
#include "cons_list.h"
#include "closget_slot.h"
#include "local.h"
#include "memory.h"
#include "typedef.h"

/*
 *  clos-value
 */
void getclosvalue(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	GetClosValue_Low(pos, index, ret);
}

void setclosvalue(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosValue_Low(pos, index, value);
}

void lenclosvalue(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	LenClosValue_Low(pos, ret);
}

int clos_value_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_CLOS_VALUE;
}

void clos_value_alloc(LocalRoot local, addr *ret, size_t size)
{
	size_t i;

	alloc_array4(local, ret, LISPSYSTEM_CLOS_VALUE, size);
	for (i = 0; i < size; i++) {
		SetClosValue_Low(*ret, i, Unbound);
	}
}

void clos_value_heap(addr *ret, size_t size)
{
	clos_value_alloc(NULL, ret, size);
}

static void clos_value_copy_alloc(LocalRoot local, addr *ret, addr pos, size_t size)
{
	addr instance, value;
	size_t i;

	alloc_array4(local, &instance, LISPSYSTEM_CLOS_VALUE, size);
	for (i = 0; i < size; i++) {
		GetClosValue_Low(pos, i, &value);
		SetClosValue_Low(instance, i, value);
	}
	*ret = instance;
}


/*
 *  clos
 */
struct clos_struct *struct_clos(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return ClosStruct_Low(pos);
}

void getclassof_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos_Low(pos, ret);
}

void setclassof_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassOfClos_Low(pos, value);
}

void getslot_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetSlotClos_Low(pos, ret);
}

void setslot_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotClos_Low(pos, value);
}

void getvalue_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, ret);
}

void setvalue_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetValueClos_Low(pos, value);
}

void getfuncall_clos(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, ret);
}

void setfuncall_clos(addr pos, int value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFuncallClos_Low(pos, value);
}

void getversion_clos(addr pos, fixnum *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetVersionClos_Low(pos, ret);
}

void setversion_clos(addr pos, fixnum value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetVersionClos_Low(pos, value);
}

int closp(addr pos)
{
	return GetType(pos) == LISPTYPE_CLOS;
}

static inline void clos_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPTYPE_CLOS, CLOS_INDEX_SIZE, sizeoft(struct clos_struct));
}

void clos_alloc(LocalRoot local, addr *ret, addr slots)
{
	addr pos, value;
	size_t size;

	CheckType(slots, LISPSYSTEM_SLOT_VECTOR);
	clos_unsafe(local, &pos);

	/* value */
	LenSlotVector_Low(slots, &size);
	clos_value_alloc(local, &value, size);

	/* clos */
	SetClassOfClos_Low(pos, Unbound);
	SetSlotClos_Low(pos, slots);
	SetValueClos_Low(pos, value);
	SetFuncallClos_Low(pos, 0);
	SetVersionClos_Low(pos, 0);

	/* result */
	*ret = pos;
}
void clos_local(LocalRoot local, addr *ret, addr slots)
{
	CheckLocal(local);
	clos_alloc(local, ret, slots);
}
void clos_heap(addr *ret, addr slots)
{
	clos_alloc(NULL, ret, slots);
}

void clos_destroy(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetClassOfClos_Low(pos, Nil);
	SetSlotClos_Low(pos, Nil);
	SetValueClos_Low(pos, Nil);
	SetFuncallClos_Low(pos, 0);
	SetVersionClos_Low(pos, 0);
}

void clos_swap(addr a, addr b)
{
	int i1, i2;
	fixnum f1, f2;
	addr v1, v2;

	CheckType(a, LISPTYPE_CLOS);
	CheckType(b, LISPTYPE_CLOS);
	/* class-of */
	GetClassOfClos_Low(a, &v1);
	GetClassOfClos_Low(b, &v2);
	SetClassOfClos_Low(a, v2);
	SetClassOfClos_Low(b, v1);
	/* slot */
	GetSlotClos_Low(a, &v1);
	GetSlotClos_Low(b, &v2);
	SetSlotClos_Low(a, v2);
	SetSlotClos_Low(b, v1);
	/* value */
	GetValueClos_Low(a, &v1);
	GetValueClos_Low(b, &v2);
	SetValueClos_Low(a, v2);
	SetValueClos_Low(b, v1);
	/* funcall */
	GetFuncallClos_Low(a, &i1);
	GetFuncallClos_Low(b, &i2);
	SetFuncallClos_Low(a, i2);
	SetFuncallClos_Low(b, i1);
	/* version */
	GetVersionClos_Low(a, &f1);
	GetVersionClos_Low(b, &f2);
	SetVersionClos_Low(a, f2);
	SetVersionClos_Low(b, f1);
}

void clos_copy_alloc(LocalRoot local, addr pos, addr *ret)
{
	int y;
	fixnum z;
	addr value, x;

	CheckType(pos, LISPTYPE_CLOS);
	clos_unsafe(local, &value);

	/* class-of */
	GetClassOfClos_Low(pos, &x);
	SetClassOfClos_Low(value, x);
	/* slot */
	GetSlotClos_Low(pos, &x);
	SetSlotClos_Low(value, x);
	/* value */
	GetValueClos_Low(pos, &x);
	SetValueClos_Low(value, x);
	/* funcall */
	GetFuncallClos_Low(pos, &y);
	SetFuncallClos_Low(value, y);
	/* version */
	GetVersionClos_Low(pos, &z);
	SetVersionClos_Low(value, z);
	/* result */
	*ret = value;
}

void clos_allcopy_alloc(LocalRoot local, addr pos, addr *ret)
{
	addr instance, x;
	size_t size;

	/* clos */
	clos_copy_alloc(local, pos, &instance);
	/* slot */
	GetSlotClos_Low(pos, &x);
	LenSlotVector(x, &size);
	slot_vector_copy_alloc(local, &x, x);
	SetSlotClos_Low(instance, x);
	/* value */
	GetValueClos_Low(pos, &x);
	clos_value_copy_alloc(local, &x, x, size);
	SetValueClos_Low(instance, x);
	/* result */
	*ret = instance;
}

void clos_getslots_heap(addr pos, addr *ret)
{
	addr list, slots, x;
	size_t size, i;

	CheckType(pos, LISPTYPE_CLOS);
	GetSlotClos_Low(pos, &slots);
	LenSlotVector(slots, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &x);
		getname_slot(x, &x);
		cons_heap(&list, x, list);
	}
	nreverse(ret, list);
}

int clos_funcall_p(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, &check);

	return check;
}

void clos_set_funcall(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetFuncallClos_Low(pos, 1);
}


/*
 *  slot-vector
 */
void getslotvector(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	GetSlotVector_Low(pos, index, ret);
}

void setslotvector(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotVector_Low(pos, index, value);
}

void lenslotvector(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector_Low(pos, ret);
}

int slot_vector_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT_VECTOR;
}

void slot_vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_array4(local, ret, LISPSYSTEM_SLOT_VECTOR, size);
}
void slot_vector_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	slot_vector_alloc(local, ret, size);
}
void slot_vector_heap(addr *ret, size_t size)
{
	slot_vector_alloc(NULL, ret, size);
}

void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	addr vector, value;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	slot_vector_alloc(local, &vector, size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &value);
		slot_copy_alloc(local, &value, value);
		SetSlotVector(vector, i, value);
	}
	*ret = vector;
}
void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	slot_vector_copy_alloc(local, ret, pos);
}
void slot_vector_copy_heap(addr *ret, addr pos)
{
	slot_vector_copy_alloc(NULL, ret, pos);
}

void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	if (local || (! GetStatusDynamic(pos))) {
		*ret = pos;
		return;
	}

	/* local -> heap */
	slot_vector_copy_alloc(local, ret, pos);
}

void slot_vector_clear(addr pos)
{
	addr slot;
	size_t size, i;

	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector(pos, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &slot);
		setclass_slot(slot, Nil);
	}
}

