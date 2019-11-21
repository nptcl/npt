#include "condition.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_type.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "package.h"
#include "symbol.h"

static int Clos_standard_ignore = 0;

/*
 *  access
 */
_g struct slot_struct *struct_slot(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	return SlotStruct_Low(pos);
}

_g void getname_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetNameSlot_Low(pos, ret);
}

_g void setname_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetNameSlot_Low(pos, value);
}

_g void gettype_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetTypeSlot_Low(pos, ret);
}

_g void settype_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetTypeSlot_Low(pos, value);
}

_g void getargs_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetArgsSlot_Low(pos, ret);
}

_g void setargs_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetArgsSlot_Low(pos, value);
}

_g void getform_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFormSlot_Low(pos, ret);
}

_g void setform_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFormSlot_Low(pos, value);
}

_g void getfunction_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetFunctionSlot_Low(pos, ret);
}

_g void setfunction_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFunctionSlot_Low(pos, value);
}

_g void getreaders_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadersSlot_Low(pos, ret);
}

_g void setreaders_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadersSlot_Low(pos, value);
}

_g void getwriters_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetWritersSlot_Low(pos, ret);
}

_g void setwriters_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetWritersSlot_Low(pos, value);
}

_g void getdocument_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetDocumentSlot_Low(pos, ret);
}

_g void setdocument_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetDocumentSlot_Low(pos, value);
}

_g void getclass_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetClassSlot_Low(pos, ret);
}

_g void setclass_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassSlot_Low(pos, value);
}

_g void getreadonly_slot(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetReadOnlySlot_Low(pos, ret);
}

_g void setreadonly_slot(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetReadOnlySlot_Low(pos, value);
}

_g void getallocation_slot(addr pos, int *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, ret);
}

_g void setallocation_slot(addr pos, int value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAllocationSlot_Low(pos, value);
}

_g void getlocation_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetLocationSlot_Low(pos, ret);
}

_g void setlocation_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetLocationSlot_Low(pos, value);
}

_g void getaccess_slot(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	GetAccessSlot_Low(pos, ret);
}

_g void setaccess_slot(addr pos, size_t value)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetAccessSlot_Low(pos, value);
}

_g struct clos_struct *struct_clos(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	return ClosStruct_Low(pos);
}

_g void getclassof_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetClassOfClos_Low(pos, ret);
}

_g void setclassof_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClassOfClos_Low(pos, value);
}

_g void getslot_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetSlotClos_Low(pos, ret);
}

_g void setslot_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotClos_Low(pos, value);
}

_g void getvalue_clos(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetValueClos_Low(pos, ret);
}

_g void setvalue_clos(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetValueClos_Low(pos, value);
}

_g void getfuncall_clos(addr pos, int *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, ret);
}

_g void setfuncall_clos(addr pos, int value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetFuncallClos_Low(pos, value);
}

_g void getversion_clos(addr pos, fixnum *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	GetVersionClos_Low(pos, ret);
}

_g void setversion_clos(addr pos, fixnum value)
{
	CheckType(pos, LISPTYPE_CLOS);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetVersionClos_Low(pos, value);
}

_g void getslotvector(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	GetSlotVector_Low(pos, index, ret);
}

_g void setslotvector(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetSlotVector_Low(pos, index, value);
}

_g void lenslotvector(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	LenSlotVector_Low(pos, ret);
}

_g void getclosvalue(addr pos, size_t index, addr *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	GetClosValue_Low(pos, index, ret);
}

_g void setclosvalue(addr pos, size_t index, addr value)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	Check(GetStatusReadOnly(pos), "readonly error");
	SetClosValue_Low(pos, index, value);
}

_g void lenclosvalue(addr pos, size_t *ret)
{
	CheckType(pos, LISPSYSTEM_CLOS_VALUE);
	LenClosValue_Low(pos, ret);
}

_g void clos_standard_ignore(int value)
{
	Clos_standard_ignore = value;
}

_g int clos_standard_class_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_class_p_Low(pos);
}

_g int clos_standard_generic_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_generic_p_Low(pos);
}

_g int clos_standard_method_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_method_p_Low(pos);
}

_g int clos_standard_combination_p_debug(addr pos)
{
	return (! Clos_standard_ignore)
		&& clos_standard_combination_p_Low(pos);
}

_g int clos_standard_specializer_p_debug(addr pos)
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
_g void slot_alloc(LocalRoot local, addr *ret)
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
_g void slot_local(LocalRoot local, addr *ret)
{
	CheckLocal(local);
	slot_alloc(local, ret);
}
_g void slot_heap(addr *ret)
{
	slot_alloc(NULL, ret);
}

_g void slot_copy_alloc(LocalRoot local, addr *ret, addr slot)
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
_g void slot_copy_local(LocalRoot local, addr *ret, addr slot)
{
	CheckLocal(local);
	slot_copy_alloc(local, ret, slot);
}
_g void slot_copy_heap(addr *ret, addr slot)
{
	slot_copy_alloc(NULL, ret, slot);
}

/* slot-vector */
_g void slot_vector_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_array4(local, ret, LISPSYSTEM_SLOT_VECTOR, size);
}
_g void slot_vector_local(LocalRoot local, addr *ret, size_t size)
{
	CheckLocal(local);
	slot_vector_alloc(local, ret, size);
}
_g void slot_vector_heap(addr *ret, size_t size)
{
	slot_vector_alloc(NULL, ret, size);
}

_g void slot_vector_copy_alloc(LocalRoot local, addr *ret, addr pos)
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
_g void slot_vector_copy_local(LocalRoot local, addr *ret, addr pos)
{
	CheckLocal(local);
	slot_vector_copy_alloc(local, ret, pos);
}
_g void slot_vector_copy_heap(addr *ret, addr pos)
{
	slot_vector_copy_alloc(NULL, ret, pos);
}

_g void slot_vector_copyheap_alloc(LocalRoot local, addr *ret, addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT_VECTOR);
	if (local || (! GetStatusDynamic(pos))) {
		*ret = pos;
		return;
	}

	/* local -> heap */
	slot_vector_copy_alloc(local, ret, pos);
}

_g void slot_vector_clear(addr pos)
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
static inline void clos_unsafe(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret,
			LISPTYPE_CLOS, CLOS_INDEX_SIZE, sizeoft(struct clos_struct));
}

static inline void clos_value_unsafe(LocalRoot local, addr *ret, size_t size)
{
	alloc_array4(local, ret, LISPSYSTEM_CLOS_VALUE, size);
}

_g void clos_alloc(LocalRoot local, addr *ret, addr slots)
{
	addr pos, value;
	size_t size, i;

	CheckType(slots, LISPSYSTEM_SLOT_VECTOR);
	clos_unsafe(local, &pos);

	/* value */
	LenSlotVector_Low(slots, &size);
	clos_value_unsafe(local, &value, size);
	for (i = 0; i < size; i++) {
		SetClosValue_Low(value, i, Unbound);
	}

	/* clos */
	SetClassOfClos_Low(pos, Unbound);
	SetSlotClos_Low(pos, slots);
	SetValueClos_Low(pos, value);
	SetFuncallClos_Low(pos, 0);
	SetVersionClos_Low(pos, 0);

	/* result */
	*ret = pos;
}
_g void clos_local(LocalRoot local, addr *ret, addr slots)
{
	CheckLocal(local);
	clos_alloc(local, ret, slots);
}
_g void clos_heap(addr *ret, addr slots)
{
	clos_alloc(NULL, ret, slots);
}


/*
 *  control
 */
_g int closp(addr pos)
{
	return GetType(pos) == LISPTYPE_CLOS;
}

_g int slotp(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT;
}

_g int slot_vector_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_SLOT_VECTOR;
}

_g int clos_value_p(addr pos)
{
	return GetType(pos) == LISPSYSTEM_CLOS_VALUE;
}

_g int clos_funcall_p(addr pos)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	GetFuncallClos_Low(pos, &check);

	return check;
}

_g int slot_class_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check != 0;
}

_g int slot_instance_p(addr pos)
{
	int check;

	CheckType(pos, LISPSYSTEM_SLOT);
	GetAllocationSlot_Low(pos, &check);

	return check == 0;
}

_g void clos_set_funcall(addr pos)
{
	CheckType(pos, LISPTYPE_CLOS);
	SetFuncallClos_Low(pos, 1);
}

_g void slot_set_class(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 1);
}

_g void slot_set_instance(addr pos)
{
	CheckType(pos, LISPSYSTEM_SLOT);
	SetAllocationSlot_Low(pos, 0);
}

_g void slot_set_allocation(addr pos, addr value)
{
	addr check;

	CheckType(pos, LISPSYSTEM_SLOT);

	/* instance */
	GetConst(KEYWORD_INSTANCE, &check);
	if (check == value) {
		slot_set_instance(pos);
		return;
	}

	/* class */
	GetConst(KEYWORD_CLASS, &check);
	if (check == value) {
		slot_set_class(pos);
		return;
	}

	/* error */
	fmte("Invalid :allocation value ~S.", value, NULL);
}

_g int clos_errorp(addr pos, size_t index, constindex name)
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

_g int clos_getp(addr pos, addr key, addr *ret)
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

_g int clos_setp(addr pos, addr key, addr value)
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

_g int clos_checkp(addr pos, addr key, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, &check)) {
		return 0;
	}
	if (check == Unbound) {
		unbound_slot(pos, key);
		return 0;
	}
	*ret = check;

	return 1;
}

_g void clos_get(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, key, ret))
		fmte("The slot name ~S don't exist in the ~S.", key, pos, NULL);
}

_g void clos_set(addr pos, addr key, addr value)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_setp(pos, key, value))
		fmte("The slot name ~S don't exist in the ~S.", key, pos, NULL);
}

_g void clos_check(addr pos, addr key, addr *ret)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_checkp(pos, key, ret))
		fmte("The slot name ~S don't exist in the ~S.", key, pos, NULL);
}

_g void clos_getelt(addr pos, size_t index, addr *ret)
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

_g void clos_setelt(addr pos, size_t index, addr value)
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

_g void clos_checkelt(addr pos, size_t index, addr *ret)
{
	addr check;

	CheckType(pos, LISPTYPE_CLOS);
	clos_getelt(pos, index, &check);
	if (check == Unbound)
		unbound_slot(pos, check);
	else
		*ret = check;
}

_g void clos_getconst(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	clos_get(pos, key, ret);
}

_g void clos_setconst(addr pos, constindex index, addr value)
{
	addr key;
	GetConstant(index, &key);
	clos_set(pos, key, value);
}

_g void clos_checkconst(addr pos, constindex index, addr *ret)
{
	addr key;
	GetConstant(index, &key);
	clos_check(pos, key, ret);
}


/*
 *  check
 */
_g int clos_slot_exists_p(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	return clos_getp(pos, name, &name);
}

_g int clos_slot_boundp_nil(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (! clos_getp(pos, name, &name))
		return -1; /* error no slot. */
	return name != Unbound;
}

_g int clos_slot_boundp(addr pos, addr name)
{
	int check;

	CheckType(pos, LISPTYPE_CLOS);
	check = clos_slot_boundp_nil(pos, name);
	if (check < 0)
		fmte("The pos object ~S have no ~S slot.", pos, name, NULL);
	return check;
}

_g int clos_slot_makunbound_nil(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_slot_exists_p(pos, name)) {
		clos_set(pos, name, Unbound);
		return 0;
	}
	/* error */
	return 1;
}

_g void clos_slot_makunbound(addr pos, addr name)
{
	CheckType(pos, LISPTYPE_CLOS);
	if (clos_slot_makunbound_nil(pos, name))
		fmte("The slot have no ~S.", name, NULL);
}


/*
 *  table
 */
/* clos */
static inline void clos_table_class(addr *ret)
{
	*ret = Root(LISPINDEX_CLOS);
}

_g void clos_find_class_nil(addr name, addr *ret)
{
	addr table;

	Check(! symbolp(name), "type name error");
	clos_table_class(&table);
	findvalue_hashtable(table, name, ret);
}

_g void clos_find_class(addr name, addr *ret)
{
	Check(! symbolp(name), "type name error");
	clos_find_class_nil(name, ret);
	if (*ret == Nil)
		fmte("No class named ~S.", name, NULL);
}

_g void clos_define_class(addr name, addr value)
{
	addr table;

	Check(! symbolp(name), "type name error");
	CheckType(value, LISPTYPE_CLOS);
	clos_table_class(&table);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

_g void clos_forget_all_classes_unsafe(void)
{
	addr table;
	clos_table_class(&table);
	clear_hashtable_heap(table);
}

/* generic */
_g void clos_find_generic_nil(addr name, addr *ret)
{
	Check(! function_name_p(name), "type error");

	getcallname_global(name, &name);
	if (name == Unbound || (! closp(name)))
		*ret = Nil;
	else
		*ret = name;
}

_g void clos_find_generic(addr name, addr *ret)
{
	clos_find_generic_nil(name, ret);
	if (*ret == Nil)
		fmte("No generic function named ~S.", name, NULL);
}

_g void clos_define_generic(addr name, addr value)
{
	Check(! function_name_p(name), "type name error");
	setcallname_global(name, value);
}

/* method-combination */
static void clos_table_combination(addr *ret)
{
	*ret = Root(LISPINDEX_COMBINATION);
}

_g void clos_find_combination_nil(addr name, addr *ret)
{
	addr table;

	Check(! symbolp(name), "type name error");
	clos_table_combination(&table);
	findvalue_hashtable(table, name, ret);
}

_g void clos_find_combination(addr name, addr *ret)
{
	clos_find_combination_nil(name, ret);
	if (*ret == Nil)
		fmte("No method combination named ~S.", name, NULL);
}

_g void clos_define_combination(addr name, addr value)
{
	addr table;

	Check(! symbolp(name), "type name error");
	clos_table_combination(&table);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

_g void clos_forget_all_combination_unsafe(void)
{
	addr table;
	clos_table_combination(&table);
	clear_hashtable_heap(table);
}

/* eql-specializser */
static void clos_table_specializer(addr *ret)
{
	*ret = Root(LISPINDEX_SPECIALIZER);
}

_g void clos_find_specializer_nil(addr name, addr *ret)
{
	addr table;
	clos_table_specializer(&table);
	findvalue_hashtable(table, name, ret);
}

_g void clos_find_specializer(addr name, addr *ret)
{
	clos_find_specializer_nil(name, ret);
	if (*ret == Nil)
		fmte("No method eql-specializer named ~S.", name, NULL);
}

_g void clos_define_specializer(addr name, addr value)
{
	addr table;

	clos_table_specializer(&table);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

_g void clos_forget_all_specializer_unsafe(void)
{
	addr table;
	clos_table_specializer(&table);
	clear_hashtable_heap(table);
}


/*
 *  build
 */
_g void init_clos(void)
{
	init_clos_class();
	init_clos_generic();
	init_clos_type();
}

static void build_clos_table(Execute ptr)
{
	addr pos;

	/* class table */
	hashtable_size_heap(&pos, CLOS_TABLE_CLASS_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	Root(LISPINDEX_CLOS) = pos;

	/* method-combination */
	hashtable_size_heap(&pos, CLOS_TABLE_COMBINATION_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQ);
	Root(LISPINDEX_COMBINATION) = pos;

	/* eql-specializer */
	hashtable_size_heap(&pos, CLOS_TABLE_SPECIALIZER_SIZE);
	settest_hashtable(pos, HASHTABLE_TEST_EQL);
	Root(LISPINDEX_SPECIALIZER) = pos;
}

_g void build_clos(Execute ptr)
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

