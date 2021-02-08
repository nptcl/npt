#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_make.h"
#include "clos_type.h"
#include "condition.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "package.h"
#include "symbol.h"

static int Clos_standard_ignore = 0;

/*
 *  access
 */
struct slot_struct *struct_slot(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return SlotStruct_Low(pos);
}

void getname_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetNameSlot_Low(pos, ret);
}

void setname_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameSlot_Low(pos, value);
}

void gettype_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetTypeSlot_Low(pos, ret);
}

void settype_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeSlot_Low(pos, value);
}

void getargs_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetArgsSlot_Low(pos, ret);
}

void setargs_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArgsSlot_Low(pos, value);
}

void getform_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFormSlot_Low(pos, ret);
}

void setform_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFormSlot_Low(pos, value);
}

void getfunction_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFunctionSlot_Low(pos, ret);
}

void setfunction_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionSlot_Low(pos, value);
}

void getreaders_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadersSlot_Low(pos, ret);
}

void setreaders_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadersSlot_Low(pos, value);
}

void getwriters_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetWritersSlot_Low(pos, ret);
}

void setwriters_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetWritersSlot_Low(pos, value);
}

void getdocument_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetDocumentSlot_Low(pos, ret);
}

void setdocument_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDocumentSlot_Low(pos, value);
}

void getclass_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetClassSlot_Low(pos, ret);
}

void setclass_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassSlot_Low(pos, value);
}

void getreadonly_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadOnlySlot_Low(pos, ret);
}

void setreadonly_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadOnlySlot_Low(pos, value);
}

void getallocation_slot(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, ret);
}

void setallocation_slot(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAllocationSlot_Low(pos, value);
}

void getlocation_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetLocationSlot_Low(pos, ret);
}

void setlocation_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLocationSlot_Low(pos, value);
}

void getaccess_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAccessSlot_Low(pos, ret);
}

void setaccess_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAccessSlot_Low(pos, value);
}

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

void clos_standard_ignore(int value)
{
	Clos_standard_ignore = value;
}

int clos_standard_class_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_class_p_Low(pos);
}

int clos_standard_generic_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_generic_p_Low(pos);
}

int clos_standard_method_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_method_p_Low(pos);
}

int clos_standard_combination_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_combination_p_Low(pos);
}

int clos_standard_specializer_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_specializer_p_Low(pos);
}


/*
 *  allocate
 */
/* slot */
static inline void slot_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPSYSTEM_SLOT, SLOT_INDEX_SIZE, sizeoft(struct slot_struct));
}
void slot_alloc(LocalRoot local, addr *ret)
{
	addr pos;

	slot_unsafe(local, &pos);
	SetAllocationSlot_Low(pos, 0);
	SetLocationSlot_Low(pos, 0);
	SetAccessSlot_Low(pos, 0);
	SetNameSlot_Low(pos, Unbound);
	SetFormSlot_Low(pos, Unbound);
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
	GetAllocationSlot_Low(slot, &check);
	SetAllocationSlot_Low(pos, check);

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

/* slot-vector */
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
		SetClassSlot(slot, Nil);
	}
}


/* clos */
static inline void clos_value_unsafe(LocalRoot local, addr *ret, size_t size)
{
	size_t i;

	alloc_array4(local, ret, LISPSYSTEM_CLOS_VALUE, size);
	for (i = 0; i < size; i++) {
		SetClosValue_Low(*ret, i, Unbound);
	}
}

void clos_value_heap(addr *ret, size_t size)
{
	clos_value_unsafe(NULL, ret, size);
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
	clos_value_unsafe(local, &value, size);

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


/*
 *  control
 */
int closp(addr pos)
{
	return GetType(pos) == LISPTYPE_CLOS;
}

int slotp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT;
}

int slot_vector_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT_VECTOR;
}

int clos_value_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_CLOS_VALUE;
}

int clos_funcall_p(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, &check);

	return check;
}

int slot_class_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check != 0;
}

int slot_instance_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check == 0;
}

void clos_set_funcall(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetFuncallClos_Low(pos, 1);
}

void slot_set_class(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 1);
}

void slot_set_instance(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 0);
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
		GetNameSlot(pos, &check);
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
		GetNameSlot(pos, &check);
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
		GetNameSlot(pos, &check);
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


/*
 *  table
 */
/* clos */
void clos_find_class_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getclass_symbol(name, ret);
}

int clos_find_class_(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	clos_find_class_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No class named ~S.", name, NULL);

	return 0;
}

void clos_define_class(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	Check((! closp(value)) && (value != Nil), "type value error");
	if (value == Nil)
		remclass_symbol(name);
	else
		setclass_symbol(name, value);
}

/* generic */
void clos_find_generic_nil(addr name, addr *ret)
{
	Check(! function_name_p(name), "type error");

	getglobal_parse_callname(name, &name);
	if (name == Unbound || (! closp(name)))
		*ret = Nil;
	else
		*ret = name;
}

int clos_find_generic_(addr name, addr *ret)
{
	clos_find_generic_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No generic function named ~S.", name, NULL);

	return 0;
}

int clos_define_generic_(addr name, addr value)
{
	Check(! function_name_p(name), "type name error");
	return setglobal_parse_callname_(name, value);
}

/* method-combination */
void clos_find_combination_nil(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	getcombination_symbol(name, ret);
}

int clos_find_combination_(addr name, addr *ret)
{
	clos_find_combination_nil(name, ret);
	if (*ret == Nil)
		return fmte_("No method combination named ~S.", name, NULL);

	return 0;
}

void clos_define_combination(addr name, addr value)
{
	Check(! symbolp(name), "type name error");
	setcombination_symbol(name, value);
}

/* eql-specializser */
static void clos_table_specializer(addr *ret)
{
	*ret = LispRoot(SPECIALIZER);
}

int clos_find_specializer_nil_(addr name, addr *ret)
{
	addr table;
	clos_table_specializer(&table);
	return findnil_hashtable_(table, name, ret);
}

int clos_find_specializer_(addr name, addr *ret)
{
	Return(clos_find_specializer_nil_(name, ret));
	if (*ret == Nil)
		return fmte_("No method eql-specializer named ~S.", name, NULL);

	return 0;
}

int clos_define_specializer_(addr name, addr value)
{
	addr table;

	clos_table_specializer(&table);
	Return(intern_hashheap_(table, name, &name));
	SetCdr(name, value);

	return 0;
}

void clos_forget_all_specializer_unsafe(void)
{
	addr table;
	clos_table_specializer(&table);
	clear_hashtable_heap(table);
}


/*
 *  build
 */
void init_clos(void)
{
	init_clos_generic();
	init_clos_make();
	init_clos_type();
}

static void build_clos_table(Execute ptr)
{
	addr pos;

	/* eql-specializer */
	hashtable_size_heap(&pos, CLOS_TABLE_SPECIALIZER_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	SetLispRoot(SPECIALIZER, pos);
}

void build_clos(Execute ptr)
{
	/* Variable */
	Clos_standard_class = 0;
	Clos_standard_generic = 0;
	Clos_standard_method = 0;
	Clos_standard_combination = 0;
	Clos_standard_specializer = 0;
	Clos_standard_ignore = 0;
	/* build */
	build_clos_table(ptr);
	build_clos_class(ptr->local);
	build_clos_combination();
}

