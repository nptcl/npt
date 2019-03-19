#include "clos_object.h"
#include "condition.h"
#include "constant.h"
#include "function.h"
#include "hashtable.h"
#include "heap.h"
#include "integer.h"
#include "lisp.h"
#include "local.h"
#include "object.h"
#include "package.h"


/*****************************************************************************
 *  find-class
 *****************************************************************************/
addr find_class_nil(addr name)
{
	addr table;

	Check(! IsSymbol(name), "type name error");
	table = Root(LISPINDEX_CLOS);
	findvalue_hashtable(table, name, &name);

	return name;
}

addr find_class(addr name)
{
	name = find_class_nil(name);
	if (name == Nil)
		fmte("No class named ~S.", name, NULL);
	return name;
}

void setf_find_class(addr name, addr value)
{
	addr table;

	Check(! IsSymbol(name), "type name error");
	Check(GetType(value) != LISPTYPE_CLOS, "type value error");
	table = Root(LISPINDEX_CLOS);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

void forget_all_classes(void)
{
	clear_hashtable_heap(Root(LISPINDEX_CLOS));
}

void unsafe_remove_class(addr name)
{
	addr table;

	Check(! IsSymbol(name), "type name error");
	table = Root(LISPINDEX_CLOS);
	delete_hashtable(table, name);
}


/*
 *  find-generic-function
 */
addr find_generic_function_nil(addr name)
{
	Check(! function_name_p(name), "type error");
	getcallname_global(name, &name);
	if (name == Unbound) return Nil;
	if (GetType(name) != LISPTYPE_CLOS) return Nil;
	return name;
}

addr find_generic_function(addr name)
{
	name = find_generic_function_nil(name);
	if (name == Nil)
		fmte("No generic function named ~S.", name, NULL);
	return name;
}

void setf_find_generic_function(addr name, addr value)
{
	Check(! function_name_p(name), "type name error");
	setcallname_global(name, value);
}


/*
 *  find-method-combination
 */
addr find_method_combination_nil(addr name)
{
	addr table;

	Check(! IsSymbol(name), "type name error");
	table = Root(LISPINDEX_COMBINATION);
	findvalue_hashtable(table, name, &name);

	return name;
}

addr find_method_combination(addr name)
{
	name = find_method_combination_nil(name);
	if (name == Nil)
		fmte("No method combination named ~S.", name, NULL);
	return name;
}

void setf_find_method_combination(addr name, addr value)
{
	addr table;

	Check(! IsSymbol(name), "type name error");
	table = Root(LISPINDEX_COMBINATION);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

void forget_all_method_combination(void)
{
	clear_hashtable_heap(Root(LISPINDEX_COMBINATION));
}


/*
 *  eql-specializser
 */
addr find_eql_specializer_nil(addr name)
{
	addr table;

	table = Root(LISPINDEX_SPECIALIZER);
	findvalue_hashtable(table, name, &name);

	return name;
}

addr find_eql_specializer(addr name)
{
	name = find_eql_specializer_nil(name);
	if (name == Nil)
		fmte("No method eql-specializer named ~S.", name, NULL);
	return name;
}

void setf_find_eql_specializer(addr name, addr value)
{
	addr table;

	table = Root(LISPINDEX_SPECIALIZER);
	intern_hashheap(table, name, &name);
	SetCdr(name, value);
}

void forget_all_eql_specializer(void)
{
	clear_hashtable_heap(Root(LISPINDEX_SPECIALIZER));
}


/*****************************************************************************
 *  slot-definition
 *****************************************************************************/
void slot_alloc(LocalRoot local, addr *ret)
{
	addr pos, instance;

	GetConst(KEYWORD_INSTANCE, &instance);
	alloc_array2(local, &pos, LISPTYPE_SYSTEM, SLOT_INDEX_SIZE);
	SetSlot(pos, SLOT_INDEX_NAME, Unbound);
	SetSlot(pos, SLOT_INDEX_ALLOCATION, instance);
	SetSlot(pos, SLOT_INDEX_TYPE, T);
	SetSlot(pos, SLOT_INDEX_INITFORM, Unbound);
	*ret = pos;
}
void slot_local(LocalRoot local, addr *ret)
{
	Check(local == NULL, "local error");
	slot_alloc(local, ret);
}
void slot_heap(addr *ret)
{
	slot_alloc(NULL, ret);
}

void slot_copy_alloc(LocalRoot local, addr *ret, addr slot)
{
	addr pos, value;
	size_t index;

	slot_alloc(local, &pos);
	for (index = 0; index < SLOT_INDEX_SIZE; index++) {
		GetArrayA2(slot, index, &value);
		SetArrayA2(pos, index, value);
	}
	*ret = pos;
}
void slot_copy_local(LocalRoot local, addr *ret, addr slot)
{
	Check(local == NULL, "local error");
	slot_copy_alloc(local, ret, slot);
}
void slot_copy_heap(addr *ret, addr slot)
{
	slot_copy_alloc(NULL, ret, slot);
}


/*****************************************************************************
 *  clos
 *****************************************************************************/
void clos_alloc(LocalRoot local, addr *ret, addr slots)
{
	addr pos, table, data, cons;
	size_t size, i;

	LenArrayA4(slots, &size);
	alloc_array2(local, &pos, LISPTYPE_CLOS, CLOS_INDEX_SIZE);
	hashtable_full_alloc(local, &table, HASHTABLE_TEST_EQ, size, 1.0, 1.0);
	vector4_alloc(local, &data, size);
	for (i = 0; i < size; i++) {
		conscdr_alloc(local, &cons, Unbound);
		SetArrayA4(data, i, cons);
	}
	fixnum_alloc(local, &cons, 0);
	SetClos(pos, CLOS_INDEX_CLASS_OF, Unbound);
	SetClos(pos, CLOS_INDEX_VERSION, cons);
	SetClos(pos, CLOS_INDEX_TABLE, table);
	SetClos(pos, CLOS_INDEX_SLOTS, slots);
	SetClos(pos, CLOS_INDEX_DATA, data);
	SetUser(pos, 0);
	*ret = pos;
}
void clos_local(LocalRoot local, addr *ret, addr slots)
{
	Check(local == NULL, "local error");
	clos_alloc(local, ret, slots);
}
void clos_heap(addr *ret, addr slots)
{
	clos_alloc(NULL, ret, slots);
}

void clos_copy_alloc(LocalRoot local, addr *ret, addr pos)
{
	Abort("TODO: Hello");
}
void clos_copy_local(LocalRoot local, addr *ret, addr pos)
{
	Check(local == NULL, "local error");
	clos_copy_alloc(local, ret, pos);
}
void clos_copy_heap(addr *ret, addr pos)
{
	clos_copy_alloc(NULL, ret, pos);
}

int closp(addr clos)
{
	Check(clos == Unbound, "unbound error");
	return GetType(clos) == LISPTYPE_CLOS;
}

void clos_swap(addr clos1, addr clos2)
{
	addr a, b;
	size_t i;

	for (i = 0; i < CLOS_INDEX_SIZE; i++) {
		GetClos(clos1, i, &a);
		GetClos(clos2, i, &b);
		SetClos(clos1, i, b);
		SetClos(clos2, i, a);
	}
}

void clos_destroy(addr clos)
{
	SetClos(clos, CLOS_INDEX_CLASS_OF, Unbound);
	SetClos(clos, CLOS_INDEX_VERSION, Unbound);
	SetClos(clos, CLOS_INDEX_TABLE, Unbound);
	SetClos(clos, CLOS_INDEX_SLOTS, Nil);
	SetClos(clos, CLOS_INDEX_DATA, Nil);
}

void clos_class_of(addr clos, addr *ret)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_CLASS_OF, &clos);
	if (clos == Unbound)
		fmte("The class-of slot is unbound.", NULL);
	*ret = clos;
}

void setf_clos_class_of(addr clos, addr value)
{
	Check(! closp(clos), "type clos error");
	Check(! closp(value), "type value error");
	SetClos(clos, CLOS_INDEX_CLASS_OF, value);
}

void clos_version(addr clos, addr *ret)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_VERSION, &clos);
	if (clos == Unbound)
		fmte("The version slot is unbound.", NULL);
	*ret = clos;
}

void setf_clos_version(addr clos, addr value)
{
	Check(! closp(clos), "type clos error");
	Check(GetType(value) != LISPTYPE_FIXNUM, "type value error");
	SetClos(clos, CLOS_INDEX_VERSION, value);
}

void clos_version_value(addr clos, fixnum *ret)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_VERSION, &clos);
	if (clos == Unbound)
		fmte("The version slot is unbound.", NULL);
	GetFixnum(clos, ret);
}


/*
 *  clos-elt
 */
void clos_elt_unbound(addr clos, size_t index, addr *ret)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_DATA, &clos);
	GetArrayA4(clos, index, &clos);
	Check(GetType(clos) != LISPTYPE_CONS, "data error");
	GetCdr(clos, ret);
}

int clos_elt_unbound_p(addr clos, size_t index)
{
	clos_elt_unbound(clos, index, &clos);
	return clos == Unbound;
}

int clos_elt_boundp(addr clos, size_t index)
{
	clos_elt_unbound(clos, index, &clos);
	return clos != Unbound;
}

void clos_elt(addr clos, size_t index, addr *ret)
{
	addr pos;

	clos_elt_unbound(clos, index, ret);
	if (*ret == Unbound) {
		make_index_integer_alloc(NULL, &pos, index);
		fmte("The slot index ~S is unbound in the object ~S.", pos, clos, NULL);
	}
}

void setf_clos_elt(addr clos, size_t index, addr value)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_DATA, &clos);
	GetArrayA4(clos, index, &clos);
	Check(GetType(clos) != LISPTYPE_CONS, "data error");
	SetCdr(clos, value);
}


/*
 *  clos-value
 */
int clos_value_check(addr clos, addr name, addr *ret)
{
	addr pos;
	size_t index;

	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	if (findvalue_hashtable(pos, name, &pos)) return 1;
	GetIndex(pos, &index);
	clos_elt_unbound(clos, index, ret);

	return 0;
}

void clos_value_unbound(addr clos, addr name, addr *ret)
{
	if (clos_value_check(clos, name, ret)) {
		fmte("Slot ~S don't exist in this clos.", name, NULL);
		*ret = NULL;
	}
}

void clos_value(addr clos, addr name, addr *ret)
{
	addr pos;

	clos_value_unbound(clos, name, &pos);
	if (pos == Unbound) {
		fmte("The slot name ~S is unbound in the object ~S.", name, clos, NULL);
		*ret = NULL;
	}
	*ret = pos;
}

void setf_clos_value(addr clos, addr name, addr value)
{
	addr pos;
	size_t index;

	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	if (findvalue_hashtable(pos, name, &pos))
		fmte("The slot name ~S don't exist in the object ~S.", name, clos, NULL);
	GetIndex(pos, &index);
	setf_clos_elt(clos, index, value);
}


/*
 *  make-instance-restrict
 */
static void make_instance_restrict_slots_alloc(LocalRoot local,
		addr clos, addr slots, addr *ret)
{
	addr keyword_instance;
	addr instance, table, data, slot, name, value, cons;
	size_t size, index, check_index;

	/* constant */
	GetConst(KEYWORD_INSTANCE, &keyword_instance);
	/* slots size */
	LenArrayA4(slots, &size);
	/* clos instance */
	clos_alloc(local, &instance, slots);
	GetClos(instance, CLOS_INDEX_TABLE, &table);
	GetClos(instance, CLOS_INDEX_DATA, &data);
	/* class-of */
	setf_clos_class_of(instance, clos);

	/* data */
	for (index = 0; index < size; index++) {
		GetArrayA4(slots, index, &slot);
		/* name check */
		GetSlot(slot, SLOT_INDEX_NAME, &name);
		if (! IsSymbol(name))
			fmte("The slot name ~S must be a symbol.", name, NULL);
		/* table check */
		if (! findvalue_hashtable(table, name, &value))
			fmte("The slot name ~S already exists.", name, NULL);
		/* location check */
		GetSlot(slot, SLOT_INDEX_LOCATION, &value);
		GetIndex(value, &check_index);
		if (check_index != index)
			fmte("The slot location ~S is invalid.", value, NULL);
		/* allocation check */
		GetSlot(slot, SLOT_INDEX_ALLOCATION, &value);
		if (value != keyword_instance)
			fmte("The allocation must be a :instance.", NULL);
		/* setf gethash */
		index_alloc(local, &value, index);
		intern_hashalloc(local, table, name, &cons);
		SetCdr(cons, value);
		/* value */
		GetSlot(slot, SLOT_INDEX_INITFORM, &value);
		GetArrayA4(data, index, &cons);
		SetCons(cons, slot, value);
	}
	*ret = instance;
}

void make_instance_restrict_alloc(LocalRoot local, addr clos, addr *ret)
{
	addr key;
	clos_elt(clos, Clos_class_slots, &key);
	make_instance_restrict_slots_alloc(local, clos, key, ret);
}
void make_instance_restrict_local(LocalRoot local, addr clos, addr *ret)
{
	Check(local == NULL, "local error");
	make_instance_restrict_alloc(local, clos, ret);
}
void make_instance_restrict_heap(addr clos, addr *ret)
{
	make_instance_restrict_alloc(NULL, clos, ret);
}


/*
 *  slot check
 */
int clos_slot_exists_p(addr clos, addr name)
{
	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_TABLE, &clos);
	return findvalue_hashtable(clos, name, &clos) == 0;
}

int clos_slot_boundp_nil(addr clos, addr name)
{
	addr pos;
	size_t index;

	Check(! closp(clos), "type error");
	GetClos(clos, CLOS_INDEX_TABLE, &pos);
	if (findvalue_hashtable(pos, name, &pos))
		return -1; /* error, no slot. */
	GetIndex(pos, &index);

	return clos_elt_boundp(clos, index);
}

int clos_slot_boundp(addr clos, addr name)
{
	int check = clos_slot_boundp_nil(clos, name);
	if (check < 0)
		fmte("The clos object ~S have no ~S slot.", clos, name, NULL);
	return check;
}

int clos_slot_makunbound_nil(addr clos, addr name)
{
	if (clos_slot_exists_p(clos, name)) {
		setf_clos_value(clos, name, Unbound);
		return 0;
	}
	/* error */
	return 1;
}

void clos_slot_makunbound(addr clos, addr name)
{
	if (clos_slot_makunbound_nil(clos, name))
		fmte("The slot have no ~S.", name, NULL);
}


/*****************************************************************************
 *  debug function
 *****************************************************************************/
#ifdef LISP_DEBUG
static enum CONSTANT_INDEX constant_class[] = {
	CONSTANT_CLOSNAME_NAME,
	CONSTANT_CLOSNAME_DIRECT_SLOTS,
	CONSTANT_CLOSNAME_DIRECT_SUBCLASSES,
	CONSTANT_CLOSNAME_DIRECT_SUPERCLASSES,
	CONSTANT_CLOSNAME_CLASS_PRECEDENCE_LIST,
	CONSTANT_CLOSNAME_EFFECTIVE_SLOTS,
	CONSTANT_CLOSNAME_FINALIZED_P,
	CONSTANT_CLOSNAME_PROTOTYPE,
	CONSTANT_CLOSNAME_DIRECT_METHODS,
	CONSTANT_CLOSNAME_DIRECT_SHARED,
	CONSTANT_CLOSNAME_DEFAULT_INITARGS,
	CONSTANT_CLOSNAME_VERSION,
	CONSTANT_CLOSNAME_UPDATE_INFO,
	CONSTANT_EMPTY
};

static enum CONSTANT_INDEX constant_generic[] = {
	CONSTANT_CLOSNAME_NAME,
	CONSTANT_CLOSNAME_LAMBDA_LIST,
	CONSTANT_CLOSNAME_METHODS,
	CONSTANT_CLOSNAME_METHOD_CLASS,
	CONSTANT_CLOSNAME_ARGUMENT_PRECEDENCE_ORDER,
	CONSTANT_CLOSNAME_DECLARATIONS,
	CONSTANT_CLOSNAME_METHOD_COMBINATION,
	CONSTANT_CLOSNAME_COMBINATION_ARGUMENTS,
	CONSTANT_CLOSNAME_EQLCHECK,
	CONSTANT_CLOSNAME_CACHE,
	CONSTANT_CLOSNAME_CALL,
	CONSTANT_EMPTY
};

static enum CONSTANT_INDEX constant_method[] = {
	CONSTANT_CLOSNAME_FUNCTION,
	CONSTANT_CLOSNAME_GENERIC_FUNCTION,
	CONSTANT_CLOSNAME_LAMBDA_LIST,
	CONSTANT_CLOSNAME_LAMBDA_PARSE,
	CONSTANT_CLOSNAME_QUALIFIERS,
	CONSTANT_CLOSNAME_SPECIALIZERS,
	CONSTANT_EMPTY
};

static enum CONSTANT_INDEX constant_combination[] = {
	CONSTANT_CLOSNAME_NAME,
	CONSTANT_CLOSNAME_LONG_P,
	CONSTANT_CLOSNAME_DOCUMENT,
	CONSTANT_CLOSNAME_IDENTITY,
	CONSTANT_CLOSNAME_OPERATOR,
	CONSTANT_CLOSNAME_LAMBDA_LIST,
	CONSTANT_CLOSNAME_QUALIFIERS,
	CONSTANT_CLOSNAME_ARGUMENTS,
	CONSTANT_CLOSNAME_GENERIC,
	CONSTANT_CLOSNAME_FORM,
	CONSTANT_CLOSNAME_FUNCTION,
	CONSTANT_EMPTY
};

static enum CONSTANT_INDEX constant_specializer[] = {
	CONSTANT_CLOSNAME_OBJECT,
	CONSTANT_CLOSNAME_TYPE,
	CONSTANT_EMPTY
};

static void check_elt(addr clos,
		enum CONSTANT_INDEX class_of,
		enum CONSTANT_INDEX array[],
		size_t size,
		size_t index,
		void (*call)(addr, size_t, addr *),
		addr *ret)
{
	addr pos, root;

	/* clos check */
	Check(! closp(clos), "clos type error.");
	/* class-of check */
	GetConstant(class_of, &root);
	clos_class_of(clos, &pos);
	Check(root != pos, "class-of error");
	/* index check */
	Check(size <= index, "index error");
	GetConstant(array[index], &pos);
	Check(! clos_slot_exists_p(clos, pos), "slot error");
	/* result */
	call(clos, index, ret);
}

void class_elt(addr clos, enum Clos_class_Index index, addr *ret)
{
	check_elt(clos,
			CONSTANT_CLOS_STANDARD_CLASS,
			constant_class,
			(size_t)Clos_class_size,
			(size_t)index,
			clos_elt,
			ret);
}

void generic_elt(addr clos, enum Clos_generic_Index index, addr *ret)
{
	check_elt(clos,
			CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION,
			constant_generic,
			(size_t)Clos_generic_size,
			(size_t)index,
			clos_elt,
			ret);
}

void method_elt(addr clos, enum Clos_method_Index index, addr *ret)
{
	check_elt(clos,
			CONSTANT_CLOS_STANDARD_METHOD,
			constant_method,
			(size_t)Clos_method_size,
			(size_t)index,
			clos_elt,
			ret);
}

void combination_elt(addr clos, enum Clos_combination_Index index, addr *ret)
{
	check_elt(clos,
			CONSTANT_CLOS_METHOD_COMBINATION,
			constant_combination,
			(size_t)Clos_combination_size,
			(size_t)index,
			clos_elt,
			ret);
}

void specializer_elt(addr clos, enum Clos_specializer_Index index, addr *ret)
{
	check_elt(clos,
			CONSTANT_CLOS_EQL_SPECIALIZER,
			constant_specializer,
			(size_t)Clos_specializer_size,
			(size_t)index,
			clos_elt,
			ret);
}

static void setf_check_elt(addr clos,
		enum CONSTANT_INDEX class_of,
		enum CONSTANT_INDEX array[],
		size_t size,
		size_t index,
		addr value)
{
	addr pos, root;

	/* clos check */
	Check(! closp(clos), "clos type error.");
	/* class-of check */
	GetConstant(class_of, &root);
	clos_class_of(clos, &pos);
	Check(root != pos, "class-of error");
	/* index check */
	Check(size <= index, "index error");
	GetConstant(array[index], &pos);
	Check(! clos_slot_exists_p(clos, pos), "slot error");
	/* result */
	setf_clos_elt(clos, index, value);
}

void setf_class_elt(addr clos, enum Clos_class_Index index, addr value)
{
	setf_check_elt(clos,
			CONSTANT_CLOS_STANDARD_CLASS,
			constant_class,
			(size_t)Clos_class_size,
			(size_t)index,
			value);
}

void setf_generic_elt(addr clos, enum Clos_generic_Index index, addr value)
{
	setf_check_elt(clos,
			CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION,
			constant_generic,
			(size_t)Clos_generic_size,
			(size_t)index,
			value);
}

void setf_method_elt(addr clos, enum Clos_method_Index index, addr value)
{
	setf_check_elt(clos,
			CONSTANT_CLOS_STANDARD_METHOD,
			constant_method,
			(size_t)Clos_method_size,
			(size_t)index,
			value);
}

void setf_combination_elt(addr clos, enum Clos_combination_Index index, addr value)
{
	setf_check_elt(clos,
			CONSTANT_CLOS_METHOD_COMBINATION,
			constant_combination,
			(size_t)Clos_combination_size,
			(size_t)index,
			value);
}

void setf_specializer_elt(addr clos, enum Clos_specializer_Index index, addr value)
{
	setf_check_elt(clos,
			CONSTANT_CLOS_EQL_SPECIALIZER,
			constant_specializer,
			(size_t)Clos_specializer_size,
			(size_t)index,
			value);
}
#endif

