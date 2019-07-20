#include "clos.h"
#include "clos_class.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "equal.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "mop.h"
#include "object.h"
#include "package.h"
#include "pointer.h"
#include "print_object.h"
#include "sequence.h"
#include "strtype.h"
#include "structure.h"
#include "symbol.h"
#include "type_object.h"
#include "type_subtypep.h"
#include "type_table.h"
#include "type_typep.h"

/*
 *  defstruct-struct
 */
_g void defstruct_clean(struct defstruct *ptr)
{
	clearpoint(ptr);
	ptr->conc_name = Unbound;
	ptr->copier = Nil;
	ptr->predicate = Nil;
	ptr->iname = Nil;
	ptr->iargs = Nil;
	ptr->constructor = Nil;
	ptr->type_vector = Unbound;
	ptr->print_function = Unbound;
	ptr->print_object = Unbound;
	ptr->size = 0;
	ptr->size_value = 0;
	ptr->offset = 0;
	ptr->named_index = 0;
}

static int ensure_structure_constructor(addr args, addr *ret)
{
	addr key, value, keyword, root;

	GetConst(KEYWORD_CONSTRUCTOR, &keyword);
	for (root = Nil; args != Nil; ) {
		getcons(args, &key, &args);
		getcons(args, &value, &args);
		if (key != keyword)
			continue;
		cons_heap(&root, value, root);
	}
	nreverse_list_unsafe(ret, root);

	return root != Nil;
}

static void ensure_structure_struct(struct defstruct *str,
		Execute ptr, addr name, addr slots, addr args)
{
	addr pos, check;

	defstruct_clean(str);
	str->ptr = ptr;
	str->slots = slots;
	str->name = name;
	/* :documentation */
	if (getkeyargs(args, KEYWORD_DOCUMENTATION, &pos)) pos = Nil;
	str->doc = pos;
	/* :conc-name */
	if (! getkeyargs(args, KEYWORD_CONC_NAME, &pos)) {
		str->conc_name_p = 1;
		str->conc_name = pos;
	}
	/* :type */
	if (! getkeyargs(args, KEYWORD_TYPE, &pos)) {
		GetConst(COMMON_LIST, &check);
		if (pos == check) {
			str->type_list_p = 1;
		}
		else {
			str->type_vector_p = 1;
			str->type_vector = pos;
		}
		str->type_p = 1;
	}
	/* :initial-offset */
	if (! getkeyargs(args, KEYWORD_INITIAL_OFFSET, &pos)) {
		str->initial_offset_p = 1;
		str->initial_offset = pos;
		getindex_error(pos, &(str->offset));
	}
	/* :named */
	if (! getkeyargs(args, KEYWORD_NAMED, &pos)) {
		str->named_p = (pos != Nil);
		str->offset++;
	}
	/* :copier */
	if (! getkeyargs(args, KEYWORD_COPIER, &pos)) {
		str->copier_p = 1;
		str->copier = pos;
	}
	/* :predicate */
	if (! getkeyargs(args, KEYWORD_PREDICATE, &pos)) {
		str->predicate_p = 1;
		str->predicate = pos;
	}
	/* :include */
	if (! getkeyargs(args, KEYWORD_INCLUDE, &pos)) {
		if (! consp(pos))
			fmte("Invalid :include format ~S.", pos, NULL);
		GetCons(pos, &pos, &check);
		str->include_p = 1;
		str->iname = pos;
		str->iargs = check;
	}
	/* :print-object */
	if (! getkeyargs(args, KEYWORD_PRINT_OBJECT, &pos)) {
		str->print_object_p = 1;
		str->print_object = pos;
	}
	/* :print-function */
	if (! getkeyargs(args, KEYWORD_PRINT_FUNCTION, &pos)) {
		str->print_function_p = 1;
		str->print_function = pos;
	}
	/* :constructor */
	if (ensure_structure_constructor(args, &pos)) {
		str->constructor_p = 1;
		str->constructor = pos;
	}
}


/*
 *  access
 */
static void stdget_structure_constant(addr pos, addr *ret,
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
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_structure_constant(addr pos, addr value,
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
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetStructure(p,r,a,b) \
	stdget_structure_constant((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)
#define StdSetStructure(p,r,a,b) \
	stdset_structure_constant((p), (r), Clos_structure_##a, CONSTANT_CLOSNAME_##b)

_g void stdget_structure_name(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, name, NAME);
}
_g void stdset_structure_name(addr pos, addr value)
{
	StdSetStructure(pos, value, name, NAME);
}

_g void stdget_structure_slots(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, slots, SLOTS);
}
_g void stdset_structure_slots(addr pos, addr value)
{
	StdSetStructure(pos, value, slots, SLOTS);
}

_g void stdget_structure_documentation(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, documentation, DOCUMENTATION);
}
_g void stdset_structure_documentation(addr pos, addr value)
{
	StdSetStructure(pos, value, documentation, DOCUMENTATION);
}

_g void stdget_structure_include(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, include, INCLUDE);
}
_g void stdset_structure_include(addr pos, addr value)
{
	StdSetStructure(pos, value, include, INCLUDE);
}

_g void stdget_structure_precedence_list(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
_g void stdset_structure_precedence_list(addr pos, addr value)
{
	StdSetStructure(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

_g void stdget_structure_type(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, type, TYPE);
}
_g void stdset_structure_type(addr pos, addr value)
{
	StdSetStructure(pos, value, type, TYPE);
}

_g void stdget_structure_vector(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, vector, VECTOR);
}
_g void stdset_structure_vector(addr pos, addr value)
{
	StdSetStructure(pos, value, vector, VECTOR);
}

_g void stdget_structure_named(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, named, NAMED);
}
_g void stdset_structure_named(addr pos, addr value)
{
	StdSetStructure(pos, value, named, NAMED);
}

_g void stdget_structure_named_index(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, named_index, NAMED_INDEX);
}
_g void stdset_structure_named_index(addr pos, addr value)
{
	StdSetStructure(pos, value, named_index, NAMED_INDEX);
}

_g void stdget_structure_value(addr pos, addr *ret)
{
	StdGetStructure(pos, ret, value, VALUE);
}
_g void stdset_structure_value(addr pos, addr value)
{
	StdSetStructure(pos, value, value, VALUE);
}


/*
 *  control
 */
_g int structure_class_p(addr pos)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	clos_class_of(pos, &pos);
	GetConst(CLOS_STRUCTURE_CLASS, &check);
	return clos_subclass_p(pos, check);
}

_g int structure_instance_p(addr pos)
{
	addr check;

	if (GetType(pos) != LISPTYPE_CLOS) return 0;
	clos_class_of(pos, &pos);
	GetConst(CLOS_STRUCTURE_OBJECT, &check);
	return clos_subclass_p(pos, check);
}

static int equalcall_structure(addr a, addr b, int (*call)(addr, addr))
{
	addr c, d;
	size_t x, y;

	if (! structure_instance_p(a))
		return 0;
	if (! structure_instance_p(b))
		return 0;
	GetValueClos(a, &a);
	GetValueClos(b, &b);
	LenClosValue(a, &x);
	LenClosValue(b, &y);
	if (x != y)
		return 0;
	for (x = 0; x < y; x++) {
		GetClosValue(a, x, &c);
		GetClosValue(b, x, &d);
		if (! (*call)(c, d))
			return 0;
	}

	return 1;
}

_g int equalp_structure(addr a, addr b)
{
	return equalcall_structure(a, b, equalp_function);
}

_g int equalrt_structure(addr a, addr b)
{
	return equalcall_structure(a, b, equalrt_function);
}

_g int typep_structure(addr value, addr instance)
{
	Check(! structure_class_p(instance), "type error");
	return structure_instance_p(value) && clos_subtype_p(value, instance);
}

_g int subtypep_structure(addr left, addr right)
{
	Check(! structure_class_p(left), "type error");
	Check(! structure_class_p(right), "type error");
	return clos_subclass_p(left, right);
}


/*
 *  access vector
 */
static int structure_getarray_direct(addr vector, size_t i, addr type, addr *ret)
{
	int check;
	addr value;

	getarray(vector, i, &value);
	if (typep_clang(value, type, &check))
		return 1;
	if (! check) {
		type_object(&type, type);
		fmte("The value ~S don't match ~A type.", value, type, NULL);
		return 0;
	}
	*ret = value;

	return 0;
}

static int structure_getarray(addr vector, addr slot, addr type, addr *ret)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_getarray_direct(vector, i, type, ret);
}

static int structure_setarray_direct(addr vector, size_t i, addr type, addr value)
{
	int check;

	if (typep_clang(value, type, &check))
		return 1;
	if (! check) {
		type_object(&type, type);
		fmte("The value ~S don't match ~A type.", value, type, NULL);
		return 0;
	}
	setarray(vector, i, value);

	return 0;
}
static int structure_setarray(addr vector, addr slot, addr type, addr value)
{
	size_t i;
	GetAccessSlot(slot, &i);
	return structure_setarray_direct(vector, i, type, value);
}


/*
 *  check-instance
 */
static void structure_check_name(struct defstruct *ptr)
{
	addr pos;

	clos_find_class_nil(ptr->name, &pos);
	if (pos != Nil)
		fmtw("The structure name ~S already exists.", ptr->name, NULL);
}

static void structure_slots_heap(addr list, addr *ret)
{
	addr pos, name, init, type, readonly, root;

	for (root = Nil; list != Nil; ) {
		GetCons(list, &pos, &list);
		list_bind(pos, &name, &init, &type, &readonly, NULL);
		slot_heap(&pos);
		SetNameSlot(pos, name);
		SetTypeSlot(pos, type);
		SetFunctionSlot(pos, init);
		SetReadOnlySlot(pos, readonly);
		cons_heap(&root, pos, root);
	}
	nreverse_list_unsafe(ret, root);
}

static void structure_check_slots(addr list)
{
	addr pos, a, b, tail;

	while (list != Nil) {
		getcons(list, &pos, &list);
		GetNameSlot(pos, &a);
		Check(! symbolp(a), "type error");
		GetNameSymbol(a, &a);
		for (tail = list; tail != Nil; ) {
			getcons(tail, &pos, &tail);
			GetNameSlot(pos, &b);
			Check(! symbolp(b), "type error");
			GetNameSymbol(b, &b);
			if (string_equal(a, b))
				fmte("The slot name ~S is duplicated in the defstruct.", a, NULL);
		}
	}
}

static void structure_check_predicate(struct defstruct *ptr)
{
	if (ptr->type_p && (! ptr->named_p)) {
		/* no-predicate */
		if (! ptr->predicate_p) {
			ptr->predicate_p = 1;
			ptr->predicate = Nil;
			return;
		}
		if (ptr->predicate == Nil)
			return;
		fmte("DEFSTRUCT ~S is defined :PREDICATE, "
				"but the structure is not named.", ptr->name, NULL);
		return;
	}
	else if (ptr->predicate_p && ptr->predicate == T) {
		ptr->predicate_p = 0;
		return;
	}
}

static void structure_include(struct defstruct *ptr)
{
	int invalid;
	addr instance, x, y;

	if (! ptr->include_p)
		return;
	/* instance check */
	clos_find_class_nil(ptr->iname, &instance);
	if (instance == Nil)
		fmte(":INCLUDE ~S structure don't exist.", ptr->iname, NULL);
	if (! structure_class_p(instance))
		fmte(":INCLUDE ~S must be structure type.", instance, NULL);

	/* class check */
	stdget_structure_type(instance, &x);
	GetConst(COMMON_CLASS, &y);
	if (x == y) {
		if (ptr->type_list_p || ptr->type_vector_p)
			fmte(":TYPE option is CLASS, but :INCLUDE type is not CLASS.", NULL);
	}

	/* list check */
	GetConst(COMMON_LIST, &y);
	if (x == y) {
		if (! ptr->type_list_p)
			fmte(":TYPE option is LIST, but :INCLUDE type is not LIST.", NULL);
	}

	/* vector check */
	GetConst(COMMON_VECTOR, &y);
	if (x == y) {
		if (! ptr->type_vector_p)
			fmte(":TYPE option is VECTOR, but :INCLUDE type is not VECTOR.", NULL);
		x = ptr->type_vector;
		stdget_structure_vector(instance, &y);
		if (! subtypep_clang(x, y, &invalid)) {
			type_object(&x, x);
			type_object(&y, y);
			fmte(":TYPE ~A is not in the include ~A type.", x, y, NULL);
		}
	}

	/* instance */
	ptr->iname = instance;
}

static int structure_find_slots(addr instance, addr name, addr *ret)
{
	addr slots, pos, check;
	size_t size, i;

	Check(! structure_class_p(instance), "type error");
	Check(! symbolp(name), "type error");

	/* find */
	GetNameSymbol(name, &name);
	stdget_structure_slots(instance, &slots);
	Check(! slot_vector_p(slots), "type error");
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &check);
		GetNameSymbol(check, &check);
		if (string_equal(name, check)) {
			*ret= pos;
			return 1;
		}
	}

	return 0;
}

static void structure_include_slots(struct defstruct *ptr)
{
	addr name, list, pos, instance;

	if (! ptr->include_p)
		return;
	instance = ptr->iname;
	for (list = ptr->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		if (structure_find_slots(instance, name, &pos))
			fmte("The slot ~S already exist in :INCLUDE structure.", name, NULL);
	}
}

static void structure_include_arguments(struct defstruct *ptr)
{
	int invalid;
	addr name, list, instance, a, b, x, y, gensym;

	if (! ptr->include_p)
		return;
	instance = ptr->iname;
	GetConst(SYSTEM_STRUCTURE_GENSYM, &gensym);
	for (list = ptr->iargs; list != Nil; ) {
		GetCons(list, &a, &list);
		GetNameSlot(a, &name);
		if (! structure_find_slots(instance, name, &b)) {
			fmte("The :include argument ~S don't exist "
					"in :INCLUDE structure.", name, NULL);
			return;
		}
		/* form */
		GetFunctionSlot(a, &x);
		if (x == gensym) {
			GetFunctionSlot(b, &y);
			SetFunctionSlot(a, y);
		}
		/* type */
		GetTypeSlot(a, &x);
		GetTypeSlot(b, &y);
		if (x == gensym) {
			SetTypeSlot(a, y);
		}
		else if (! subtypep_clang(x, y, &invalid)) {
			type_object(&x, x);
			type_object(&y, y);
			fmte("The slot ~S type ~A is not "
					"in the include ~A type.", name, x, y, NULL);
			return;
		}
		/* readonly */
		GetReadOnlySlot(a, &x);
		GetReadOnlySlot(b, &y);
		if (x == gensym) {
			SetReadOnlySlot(a, y);
		}
		else if (x == Nil && y == T) {
			fmte("The slot ~S is readonly "
					"but include slot is not readonly.", name, NULL);
			return;
		}
	}
}

static void structure_print_check(struct defstruct *ptr)
{
	if (ptr->print_function_p && ptr->print_object_p) {
		fmte("The defstruct option must be have "
				"either :PRINT-OBJECT or :PRINT-FUNCTION, "
				"but there are both options", NULL);
	}
}

static void structure_slots_value(struct defstruct *ptr)
{
	addr list, pos, check, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = ptr->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		/* init */
		GetFunctionSlot(pos, &check);
		if (check == g) {
			SetFunctionSlot(pos, Nil);
		}
		/* type */
		GetTypeSlot(pos, &check);
		if (check == g) {
			GetTypeTable(&check, T);
			SetTypeSlot(pos, check);
		}
		/* readonly */
		GetReadOnlySlot(pos, &check);
		if (check == g) {
			SetReadOnlySlot(pos, Nil);
		}
	}
}


/*
 *  make-instance
 */
static void structure_instance_include(struct defstruct *ptr, addr instance)
{
	addr list, pos;

	/* include */
	if (ptr->include_p)
		stdset_structure_include(instance, ptr->iname);

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		stdget_structure_include(pos, &pos);
	}
	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	cons_heap(&list, pos, list);
	GetConst(CLOS_T, &pos);
	cons_heap(&list, pos, list);
	nreverse_list_unsafe(&list, list);
	stdset_structure_precedence_list(instance, list);
}

static void structure_instance(struct defstruct *ptr)
{
	addr clos, instance, pos;

	/* structure */
	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	clos_instance_heap(clos, &instance);
	SetClassOfClos(instance, clos);
	/* name */
	stdset_structure_name(instance, ptr->name);
	/* documentation */
	if (ptr->doc != Nil)
		stdset_structure_documentation(instance, ptr->doc);
	/* include, precedence-list */
	structure_instance_include(ptr, instance);
	/* type */
	if (ptr->type_list_p) {
		GetConst(COMMON_LIST, &pos);
		stdset_structure_type(instance, pos);
	}
	else if (ptr->type_vector_p) {
		GetConst(COMMON_VECTOR, &pos);
		stdset_structure_type(instance, pos);
		stdset_structure_vector(instance, ptr->type_vector);
	}
	else {
		GetConst(COMMON_CLASS, &pos);
		stdset_structure_type(instance, pos);
	}
	/* named */
	stdset_structure_named(instance, ptr->named_p? T: Nil);
	/* result */
	ptr->instance = instance;
}


/*
 *  slots-make
 */
static int structure_pushnew_local(LocalRoot local, addr *ret, addr value, addr list)
{
	addr root, check;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		GetCons(list, &check, &list);
		if (string_equal(check, value))
			return 0;
	}
	cons_local(local, ret, value, root);

	return 1;
}

static int structure_find_slotslist(addr name, addr list, addr *ret)
{
	addr pos, check;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &check);
		GetNameSymbol(check, &check);
		if (string_equal(name, check)) {
			*ret = pos;
			return 1;
		}
	}

	return 0;
}

static void structure_slots_make(struct defstruct *ptr)
{
	addr root, slots, list, pos, name, args, instance;
	LocalRoot local;
	LocalStack stack;
	size_t size, value, i, count;

	local = ptr->ptr->local;
	push_local(local, &stack);
	root = slots = Nil;
	size = value = 0;
	/* include */
	if (ptr->include_p) {
		pos = ptr->iname;
		stdget_structure_slots(pos, &list);
		stdget_structure_value(pos, &pos);
		if (pos != Nil)
			getindex_error(pos, &value);
		args = ptr->iargs;
		LenSlotVector(list, &count);
		for (i = 0; i < count; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, &root, name, root);
			(void)structure_find_slotslist(name, args, &pos);
			Check(! slotp(pos), "type error");
			cons_local(local, &slots, pos, slots);
			/* location */
			SetLocationSlot(pos, size++);
		}
	}
	/* slots */
	if (ptr->named_p)
		ptr->named_index = value;
	value += ptr->offset;
	for (list = ptr->slots; list != Nil; ) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		if (structure_pushnew_local(local, &root, name, root)) {
			cons_local(local, &slots, pos, slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
		}
	}
	/* array */
	instance = ptr->instance;
	slot_vector_heap(&list, size);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(list, i, pos);
	}
	/* result */
	ptr->size = size;
	ptr->size_value = value;
	ptr->slots = list;
	stdset_structure_slots(instance, list);
	stdset_structure_value(instance, intsizeh(value));
	stdset_structure_named_index(instance,
			ptr->named_p? intsizeh(ptr->named_index): Nil);
	rollback_local(local, stack);
}


/*
 *  accessor
 */
static void structure_slot_callname(struct defstruct *ptr, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (ptr->conc_name == Unbound) {
		stdget_structure_name(ptr->instance, &name);
		GetNameSymbol(name, &name);
		string_concat_hyphen_heap(ret, name, pos);
	}
	else if (ptr->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(ptr->conc_name), "type error");
		string_concat_heap(ret, ptr->conc_name, pos);
	}
}

/* structure_type */
struct structure_type_struct {
	unsigned named : 1;
	unsigned errorp : 1;
	size_t size, size_value, named_index;
};
enum StructureTypeIndex {
	StructureType_instance,
	StructureType_name,
	StructureType_slot,
	StructureType_vector,
	StructureType_size
};
#define PtrStructureType(x)			\
	((struct structure_type_struct *)PtrBodySSa((x), StructureType_size))
#define GetInstanceStructureType(x,y)	GetArraySS((x),StructureType_instance,(y))
#define SetInstanceStructureType(x,y)	SetArraySS((x),StructureType_instance,(y))
#define GetNameStructureType(x,y)	GetArraySS((x),StructureType_name,(y))
#define SetNameStructureType(x,y)	SetArraySS((x),StructureType_name,(y))
#define GetSlotStructureType(x,y)	GetArraySS((x),StructureType_slot,(y))
#define SetSlotStructureType(x,y)	SetArraySS((x),StructureType_slot,(y))
#define GetVectorStructureType(x,y)	GetArraySS((x),StructureType_vector,(y))
#define SetVectorStructureType(x,y)	SetArraySS((x),StructureType_vector,(y))
#define RefNamedStructureType(x)    (PtrStructureType(x)->named)
#define GetNamedStructureType(x,y)  (*(y) = PtrStructureType(x)->named)
#define SetNamedStructureType(x,y)  (PtrStructureType(x)->named = (y))
#define RefErrorpStructureType(x)    (PtrStructureType(x)->errorp)
#define GetErrorpStructureType(x,y)  (*(y) = PtrStructureType(x)->errorp)
#define SetErrorpStructureType(x,y)  (PtrStructureType(x)->errorp = (y))

static void structure_type_heap_unsafe(addr *ret)
{
	heap_smallsize(ret, LISPSYSTEM_STRUCTURE_TYPE,
			StructureType_size, sizeoft(struct structure_type_struct));
}

static void structure_type_heap(addr *ret)
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
	struct structure_type_struct *ptr;

	structure_type_heap_unsafe(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slot);
	SetVectorStructureType(pos, vector);
	ptr = PtrStructureType(pos);
	ptr->size = size;
	ptr->size_value = value;
	ptr->named = named;
	ptr->named_index = named_index;
	ptr->errorp = 0;
	*ret = pos;
}

static void structure_type(struct defstruct *ptr, addr slot, addr *ret)
{
	structure_type_parameter(ret,
			ptr->instance, ptr->name, slot, ptr->type_vector,
			ptr->size, ptr->size_value, ptr->named_p, ptr->named_index);
}

static int structure_type_list_p(addr type, addr var)
{
	struct structure_type_struct *ptr;
	size_t size;

	/* listp */
	ptr = PtrStructureType(type);
	if (length_list_check(var, &size))
		return 0;
	/* length */
	if (size < ptr->size_value)
		return 0;
	/* check */
	if (ptr->named) {
		GetNameStructureType(type, &type);
		getnth(var, ptr->named_index, &var);
		return var == type;
	}

	return 1;
}

static int structure_type_vector_p(addr type, addr var, int *ret)
{
	struct structure_type_struct *ptr;
	addr check;
	size_t size;

	/* vectorp */
	ptr = PtrStructureType(type);
	if (GetType(var) != LISPTYPE_VECTOR) {
		*ret = 0;
		return 0;
	}
	lenarray(var, &size);
	/* length */
	if (size < ptr->size_value) {
		*ret = 0;
		return 0;
	}
	/* check */
	if (ptr->named) {
		GetVectorStructureType(type, &check);
		if (structure_getarray_direct(var, ptr->named_index, check, &var))
			return 1;
		GetNameStructureType(type, &type);
		*ret = (var == type);
		return 0;
	}
	*ret = 1;
	return 0;
}

/* list */
static void function_structure_reader_list(Execute ptr, addr var)
{
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (! structure_type_list_p(type, var))
		fmte("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	getnth_unsafe(var, index, &var);
	setresult_control(ptr, var);
}

static void structure_type_slot_reader_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_reader_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_structure_writer_list(Execute ptr, addr value, addr var)
{
	addr type;
	size_t index;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (! structure_type_list_p(type, var))
		fmte("The argument ~S must be a structure-list.", var, NULL);
	/* access */
	GetSlotStructureType(type, &type);
	GetAccessSlot(type, &index);
	setnth_unsafe(var, index, value);
	setresult_control(ptr, value);
}

static void structure_type_slot_writer_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, List);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_structure_writer_list);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_list(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static int structure_slot_readonly_p(addr slot)
{
	GetReadOnlySlot(slot, &slot);
	return slot != Nil;
}

static void structure_slots_call_list(struct defstruct *ptr)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	getpackage(ptr->ptr, &package);
	slots = ptr->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		structure_slot_callname(ptr, &symbol, pos);
		intern_package(package, symbol, &symbol);
		structure_type(ptr, pos, &type);
		structure_slot_reader_list(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_list(type, symbol);
	}
}

/* vector */
static void function_structure_reader_vector(Execute ptr, addr var)
{
	int check;
	addr type, slot, pos;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (structure_type_vector_p(type, var, &check))
		return;
	if (! check)
		fmte("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &pos);
	if (structure_getarray(var, slot, pos, &var))
		return;
	setresult_control(ptr, var);
}

static void structure_type_slot_reader_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_reader_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_reader_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_structure_writer_vector(Execute ptr, addr value, addr var)
{
	int check;
	addr type, slot;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (structure_type_vector_p(type, var, &check))
		return;
	if (! check)
		fmte("The argument ~S must be a structure-vector.", var, NULL);
	/* access */
	GetSlotStructureType(type, &slot);
	GetVectorStructureType(type, &type);
	if (structure_setarray(var, slot, type, value))
		return;
	setresult_control(ptr, value);
}

static void structure_type_slot_writer_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_structure_writer_vector);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_slot_writer_vector(&type);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static void structure_slots_call_vector(struct defstruct *ptr)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	getpackage(ptr->ptr, &package);
	slots = ptr->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		structure_slot_callname(ptr, &symbol, pos);
		intern_package(package, symbol, &symbol);
		structure_type(ptr, pos, &type);
		structure_slot_reader_vector(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_vector(type, symbol);
	}
}

/* clos */
static void function_structure_reader_clos(Execute ptr, addr var)
{
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	if (! typep_structure(var, pos))
		fmte("The reader don't read ~S structure.", pos, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	GetClosValue(var, index, &var);
	setresult_control(ptr, var);
}

static void structure_type_slot_reader_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &args);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_reader_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_reader_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_reader_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void function_structure_writer_clos(Execute ptr, addr value, addr var)
{
	addr slot, pos;
	size_t index;

	/* closure */
	getdata_control(ptr, &slot);
	/* check-type */
	GetClassSlot(slot, &pos);
	if (! typep_structure(var, pos))
		fmte("The reader don't read ~S structure.", pos, NULL);
	/* result */
	GetLocationSlot(slot, &index);
	GetValueClos(var, &var);
	SetClosValue(var, index, value);
	setresult_control(ptr, value);
}

static void structure_type_slot_writer_clos(addr *ret, addr instance)
{
	addr args, values;

	GetTypeTable(&args, T);
	type_clos_heap(instance, &values);
	typeargs_var2(&args, args, values);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void structure_slot_writer_clos(addr instance, addr slot, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var2(pos, p_defun_structure_writer_clos);
	setsetf_symbol(symbol, pos);
	SetDataFunction(pos, slot);
	/* type */
	structure_type_slot_writer_clos(&type, instance);
	settype_function(pos, type);
	settype_setf_symbol(symbol, type);
}

static void structure_slots_call_clos(struct defstruct *ptr)
{
	addr instance, package, slots, pos, symbol;
	size_t size, i;

	instance = ptr->instance;
	Check(! structure_class_p(instance), "type error");
	getpackage(ptr->ptr, &package);
	slots = ptr->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		structure_slot_callname(ptr, &symbol, pos);
		intern_package(package, symbol, &symbol);
		structure_slot_reader_clos(instance, pos, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_clos(instance, pos, symbol);
	}
}

/* call */
static void structure_slots_call(struct defstruct *ptr)
{
	if (ptr->type_list_p)
		structure_slots_call_list(ptr);
	else if (ptr->type_vector_p)
		structure_slots_call_vector(ptr);
	else
		structure_slots_call_clos(ptr);
}


/*
 *  constructor
 */
static int structure_constructor_find_slots(addr key, addr slots)
{
	addr check;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		GetNameSlot(check, &check);
		GetNameSymbol(check, &check);
		if (string_equal(key, check))
			return 1;
	}

	return 0;
}

static void structure_constructor_dynamic(addr instance,
		addr slots, addr list, unsigned errorp)
{
	addr key;

	while (list != Nil) {
		if (! consp(list))
			fmte("Invalid keyword-argumets ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			fmte("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			fmte("The key ~S must be a symbol type.", key, NULL);
		if (errorp && (! structure_constructor_find_slots(key, slots)))
			fmte("There is no slot ~S in the structure ~S.", key, instance, NULL);
		GetCdr(list, &list);
	}
}

static int function_structure_constructor_find(addr key, addr list, addr *ret)
{
	addr check, value, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		GetCons(list, &check, &list);
		GetCons(list, &value, &list);
		Check(! symbolp(check), "type error");
		if (value == g)
			continue;
		GetNameSymbol(check, &check);
		if (string_equal(key, check)) {
			*ret = value;
			return 1;
		}
	}

	return 0;
}

/* list */
static int structure_constructor_instance_list(Execute ptr,
		addr list, addr slots, addr args)
{
	addr slot, pos;
	size_t size, i, index;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (! function_structure_constructor_find(slot, args, &pos)) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				if (callclang_apply(ptr, &pos, pos, Nil))
					return 1;
			}
		}
		GetAccessSlot(slot, &index);
		setnth_unsafe(list, index, pos);
	}

	return 0;
}

static int make_structure_list(Execute ptr, addr *ret, addr pos, addr args)
{
	addr instance, slots, list, name;
	struct structure_type_struct *str;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	structure_constructor_dynamic(instance, slots, args, str->errorp);
	list_nil_heap(&list, str->size_value);
	if (structure_constructor_instance_list(ptr, list, slots, args))
		return 1;
	if (str->named) {
		GetNameStructureType(pos, &name);
		setnth(list, str->named_index, name);
	}
	*ret = list;

	return 0;
}

static void function_structure_constructor_list(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	if (make_structure_list(ptr, &pos, pos, args))
		return;
	setresult_control(ptr, pos);
}

static void structure_type_constructor_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_list(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_list);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* vector */
static int structure_constructor_instance_vector(Execute ptr,
		addr vector, addr slots, addr check, addr args)
{
	int update;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		update = 0;
		if (function_structure_constructor_find(slot, args, &pos)) {
			update = 1;
		}
		else {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				if (callclang_apply(ptr, &pos, pos, Nil))
					return 1;
				update = 1;
			}
		}
		if (update && structure_setarray(vector, slot, check, pos))
			return 1;
	}

	return 0;
}

static int make_structure_vector(Execute ptr, addr *ret, addr pos, addr args)
{
	addr instance, slots, vector, type, name;
	struct structure_type_struct *str;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	structure_constructor_dynamic(instance, slots, args, str->errorp);
	vector_heap(&vector, str->size_value);
	if (structure_constructor_instance_vector(ptr, vector, slots, type, args))
		return 1;
	if (str->named) {
		GetNameStructureType(pos, &name);
		setarray(vector, str->named_index, name);
	}
	*ret = vector;

	return 0;
}

static void function_structure_constructor_vector(Execute ptr, addr args)
{
	addr pos;

	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	if (make_structure_vector(ptr, &pos, pos, args))
		return;
	setresult_control(ptr, pos);
}

static void structure_type_constructor_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, Vector);
	typeargs_rest(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_vector(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_vector);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static int structure_constructor_instance_clos(Execute ptr, addr clos, addr args)
{
	addr slots, slot, value, pos;
	size_t size, i, location;

	GetSlotClos(clos, &slots);
	GetValueClos(clos, &value);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (! function_structure_constructor_find(slot, args, &pos)) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				if (callclang_apply(ptr, &pos, pos, Nil))
					return 1;
			}
		}
		GetLocationSlot(slot, &location);
		SetClosValue(value, location, pos);
	}

	return 0;
}

static int make_structure_clos(Execute ptr, addr *ret, addr pos, addr args)
{
	unsigned errorp;
	addr instance, slots, clos;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	structure_constructor_dynamic(instance, slots, args, errorp);
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);
	if (structure_constructor_instance_clos(ptr, clos, args))
		return 1;
	*ret = clos;

	return 0;
}

static void function_structure_constructor_clos(Execute ptr, addr args)
{
	addr pos;

	/* closure */
	getdata_control(ptr, &pos);
	SetErrorpStructureType(pos, 1);
	if (make_structure_clos(ptr, &pos, pos, args))
		return;
	setresult_control(ptr, pos);
}

static void structure_type_constructor_clos(addr *ret, addr data)
{
	addr args, values;

	GetTypeTable(&args, T);
	typeargs_rest(&args, args);
	GetInstanceStructureType(data, &data);
	type_clos_heap(data, &values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void structure_constructor_default_clos(addr data, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_structure_constructor_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, data);
	/* type */
	structure_type_constructor_clos(&type, data);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* default */
static void structure_constructor_default(struct defstruct *ptr, addr symbol)
{
	addr pos;

	structure_type(ptr, ptr->slots, &pos);
	if (ptr->type_list_p)
		structure_constructor_default_list(pos, symbol);
	else if (ptr->type_vector_p)
		structure_constructor_default_vector(pos, symbol);
	else
		structure_constructor_default_clos(pos, symbol);
}

/* constructor */
static void structure_constructor_make(struct defstruct *ptr)
{
	addr name;

	/* name */
	stdget_structure_name(ptr->instance, &name);
	GetNameSymbol(name, &name);
	string_concat_char1_heap(&name, "MAKE-", name);
	intern_default_package(ptr->ptr, name, &name);
	/* make */
	structure_constructor_default(ptr, name);
}

static void structure_constructor_lambda(struct defstruct *ptr, addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	list_bind(list, &symbol, &pos, NULL);
	parse_callname_error(&name, symbol);
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

static void structure_constructor(struct defstruct *ptr)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = ptr->constructor; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (pos == g)
			structure_constructor_make(ptr);
		else if (symbolp(pos))
			structure_constructor_default(ptr, pos);
		else if (consp(pos))
			structure_constructor_lambda(ptr, pos);
		else
			fmte("Invalid constructor parameter ~S.", pos, NULL);
	}
}


/*
 *  copier
 */
/* list */
static void function_structure_copier_list(Execute ptr, addr var)
{
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (! structure_type_list_p(type, var))
		fmte("The argument ~S must be a structure-list.", var, NULL);
	/* copy */
	copy_list_heap_unsafe(&var, var);
	setresult_control(ptr, var);
}

static void structure_type_slot_copier_list(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, List);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_list(struct defstruct *ptr, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_copier_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(ptr, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_list(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* vector */
static void function_structure_copier_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	/* closure */
	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	/* type check */
	if (structure_type_vector_p(type, var, &check))
		return;
	if (! check)
		fmte("The argument ~S must be a structure-vector.", var, NULL);
	/* copy */
	copy_vector_heap(&var, var);
	setresult_control(ptr, var);
}

static void structure_type_slot_copier_vector(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Vector);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Vector);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_vector(struct defstruct *ptr, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_copier_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(ptr, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	structure_type_slot_copier_vector(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static void function_structure_copier_clos(Execute ptr, addr var)
{
	copy_structure_common(var, &var);
	setresult_control(ptr, var);
}

static void structure_type_slot_copier_clos(addr *ret, addr instance)
{
	addr args, values;

	type_clos_heap(instance, &values);
	typeargs_var1(&args, values);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void structure_copier_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_copier_clos);
	SetFunctionSymbol(symbol, pos);
	/* type */
	structure_type_slot_copier_clos(&type, instance);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* copier */
static int structure_copier_callname(struct defstruct *ptr, addr *ret)
{
	addr name;

	if (! ptr->copier_p) {
		stdget_structure_name(ptr->instance, &name);
		GetNameSymbol(name, &name);
		string_concat_char1_heap(&name, "COPY-", name);
	}
	else if (ptr->copier == Nil) {
		return 1;
	}
	else {
		Check(! stringp(ptr->copier), "type error");
		name = ptr->copier;
	}
	intern_default_package(ptr->ptr, name, ret);
	return 0;
}

static void structure_copier(struct defstruct *ptr)
{
	addr symbol;

	if (structure_copier_callname(ptr, &symbol))
		return;
	if (ptr->type_list_p)
		structure_copier_list(ptr, symbol);
	else if (ptr->type_vector_p)
		structure_copier_vector(ptr, symbol);
	else
		structure_copier_clos(ptr->instance, symbol);
}


/*
 *  predicate
 */
/* list */
static void function_structure_predicate_list(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	check = structure_type_list_p(type, var);
	setbool_control(ptr, check);
}

static void structure_predicate_list(struct defstruct *ptr, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_list);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(ptr, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* vector */
static void function_structure_predicate_vector(Execute ptr, addr var)
{
	int check;
	addr type;

	getdata_control(ptr, &type);
	CheckType(type, LISPSYSTEM_STRUCTURE_TYPE);
	if (structure_type_vector_p(type, var, &check))
		return;
	setbool_control(ptr, check);
}

static void structure_predicate_vector(struct defstruct *ptr, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_vector);
	SetFunctionSymbol(symbol, pos);
	/* closure */
	structure_type(ptr, Nil, &type);
	SetDataFunction(pos, type);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* clos */
static void function_structure_predicate_clos(Execute ptr, addr var)
{
	int check;
	addr instance;

	getdata_control(ptr, &instance);
	check = typep_structure(var, instance);
	setbool_control(ptr, check);
}

static void structure_predicate_clos(addr instance, addr symbol)
{
	addr pos, type;

	/* function */
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_structure_predicate_clos);
	SetFunctionSymbol(symbol, pos);
	SetDataFunction(pos, instance);
	/* type */
	GetTypeCompiled(&type, Object_Boolean);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}

/* predicate */
static int structure_predicate_callname(struct defstruct *ptr, addr *ret)
{
	addr name;

	if (! ptr->predicate_p) {
		stdget_structure_name(ptr->instance, &name);
		GetNameSymbol(name, &name);
		string_concat_char2_heap(&name, name, "-P");
	}
	else if (ptr->predicate == Nil) {
		return 1;
	}
	else {
		Check(! stringp(ptr->predicate), "type error");
		name = ptr->predicate;
	}
	intern_default_package(ptr->ptr, name, ret);
	return 0;
}

static void structure_predicate(struct defstruct *ptr)
{
	addr symbol;

	if (structure_predicate_callname(ptr, &symbol))
		return;
	if (ptr->type_list_p)
		structure_predicate_list(ptr, symbol);
	else if (ptr->type_vector_p)
		structure_predicate_vector(ptr, symbol);
	else
		structure_predicate_clos(ptr->instance, symbol);
}


/*
 *  printer
 */
static int structure_print_default_p(struct defstruct *ptr)
{
	addr g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (ptr->print_object_p && ptr->print_object == g)
		return 1;
	if (ptr->print_function_p && ptr->print_function == g)
		return 1;

	return 0;
}

static void method_defstruct_default(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	if (! structure_instance_p(var))
		fmte("Invalid structure type ~S.", var, NULL);
	if (print_structure(ptr, stream, var))
		return;
	setresult_control(ptr, var);
}

static void structure_print_default_method(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;
	Execute ptr;

	ptr = str->ptr;
	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_defstruct_default);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	*ret = pos;
}

static void structure_print_add_method(struct defstruct *str, addr name, addr method)
{
	addr generic;
	Execute ptr;

	ptr = str->ptr;
	getfunctioncheck_local(ptr, name, &generic);
	Check(! clos_generic_p(generic), "type error");
	method_add_method(ptr, generic, method);
}

static void structure_print_default(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	structure_print_default_method(str, name, &method);
	structure_print_add_method(str, name, method);
}

static void method_defstruct_object(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	addr call;

	if (! structure_instance_p(var))
		fmte("Invalid structure type ~S.", var, NULL);
	getdata_control(ptr, &call);
	if (callclang_apply(ptr, &call, call, Nil))
		return;
	if (callclang_funcall(ptr, &call, call, var, stream, NULL))
		return;
	setresult_control(ptr, var);
}

static void structure_print_object_method(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;
	Execute ptr;

	ptr = str->ptr;
	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_defstruct_object);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_object);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	*ret = pos;
}

static void structure_print_object(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	structure_print_object_method(str, name, &method);
	structure_print_add_method(str, name, method);
}

static void method_defstruct_function(Execute ptr,
		addr method, addr next, addr var, addr stream)
{
	addr call, pos;

	if (! structure_instance_p(var))
		fmte("Invalid structure type ~S.", var, NULL);
	GetConst(SPECIAL_PRINT_LEVEL, &pos);
	getspecialcheck_local(ptr, pos, &pos);
	getdata_control(ptr, &call);
	if (callclang_apply(ptr, &call, call, Nil))
		return;
	if (callclang_funcall(ptr, &call, call, var, stream, pos, NULL))
		return;
	setresult_control(ptr, var);
}

static void structure_print_function_method(struct defstruct *str, addr name, addr *ret)
{
	addr pos, call, type;
	Execute ptr;

	ptr = str->ptr;
	/* function */
	compiled_heap(&call, name);
	setcompiled_var4(call, p_method_defstruct_function);
	GetTypeCompiled(&type, PrintObject_Method);
	settype_function(call, type);
	SetDataFunction(call, str->print_function);
	/* method */
	mop_argument_method_print_object(&pos, str->instance);
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	*ret = pos;
}

static void structure_print_function(struct defstruct *str)
{
	addr name, method;

	GetConst(COMMON_PRINT_OBJECT, &name);
	structure_print_function_method(str, name, &method);
	structure_print_add_method(str, name, method);
}

static void structure_print(struct defstruct *ptr)
{
	if (ptr->type_p && ptr->print_object_p)
		fmte("Can't make print-object on :TYPE structure.", NULL);
	if (ptr->type_p && ptr->print_function_p)
		fmte("Can't make print-function on :TYPE structure.", NULL);
	if (structure_print_default_p(ptr))
		structure_print_default(ptr);
	else if (ptr->print_object_p)
		structure_print_object(ptr);
	else if (ptr->print_function_p)
		structure_print_function(ptr);
}


/*
 *  ensure-structure
 */
_g int ensure_structure_common(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	ensure_structure_struct(&str, ptr, name, slots, args);

	/* check */
	structure_check_name(&str);
	structure_slots_heap(str.slots, &(str.slots));
	structure_slots_heap(str.iargs, &(str.iargs));
	structure_check_slots(str.slots);
	structure_check_slots(str.iargs);
	structure_check_predicate(&str);
	structure_include(&str);
	structure_include_slots(&str);
	structure_include_arguments(&str);
	structure_print_check(&str);
	structure_slots_value(&str);
	/* make instance */
	structure_instance(&str);
	Check(! structure_class_p(str.instance), "type error");
	clos_define_class(str.name, str.instance);
	/* settings */
	structure_slots_make(&str);
	structure_slots_call(&str);
	structure_copier(&str);
	structure_predicate(&str);
	structure_constructor(&str);
	structure_print(&str);

	return 0;
}


/*
 *  structure-constructor
 */
static int make_structure_common_clos(Execute ptr, addr *ret,
		addr instance, addr rest, unsigned errorp)
{
	addr pos, value;

	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	stdget_structure_slots(instance, &value);
	SetSlotStructureType(pos, value);
	SetErrorpStructureType(pos, errorp);

	return make_structure_clos(ptr, ret, pos, rest);
}

static int make_structure_common_vector(Execute ptr, addr *ret,
		addr instance, addr rest, unsigned errorp)
{
	addr pos, slots, value, type, name, named;
	size_t size;
	struct structure_type_struct *str;

	/* value */
	stdget_structure_slots(instance, &slots);
	stdget_structure_value(instance, &value);
	stdget_structure_vector(instance, &type);
	stdget_structure_name(instance, &name);
	stdget_structure_named(instance, &named);
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	SetVectorStructureType(pos, type);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = errorp;
	str->named = (named != Nil);
	str->size = size;
	getindex_error(value, &size);
	str->size_value = size;

	return make_structure_vector(ptr, ret, pos, rest);
}

static int make_structure_common_list(Execute ptr, addr *ret,
		addr instance, addr rest, unsigned errorp)
{
	addr pos, slots, value, name, named;
	size_t size;
	struct structure_type_struct *str;

	/* value */
	stdget_structure_slots(instance, &slots);
	stdget_structure_value(instance, &value);
	stdget_structure_name(instance, &name);
	stdget_structure_named(instance, &named);
	/* type */
	structure_type_heap(&pos);
	SetInstanceStructureType(pos, instance);
	SetNameStructureType(pos, name);
	SetSlotStructureType(pos, slots);
	LenSlotVector(slots, &size);
	str = PtrStructureType(pos);
	str->errorp = errorp;
	str->named = (named != Nil);
	str->size = size;
	getindex_error(value, &size);
	str->size_value = size;

	return make_structure_list(ptr, ret, pos, rest);
}

static int make_structure_common(Execute ptr, addr *ret,
		addr instance, addr rest, unsigned errorp)
{
	addr type, check;

	/* clos */
	stdget_structure_type(instance, &type);
	GetConst(COMMON_CLASS, &check);
	if (type == check)
		return make_structure_common_clos(ptr, ret, instance, rest, errorp);

	/* vector */
	GetConst(COMMON_VECTOR, &check);
	if (type == check)
		return make_structure_common_vector(ptr, ret, instance, rest, errorp);

	/* list */
	GetConst(COMMON_LIST, &check);
	if (type == check)
		return make_structure_common_list(ptr, ret, instance, rest, errorp);

	/* error */
	fmte("Invalid type value ~S.", type, NULL);
	return 0;
}

_g int structure_constructor_common(Execute ptr, addr symbol, addr rest, addr *ret)
{
	addr instance;

	if (! symbolp(symbol))
		fmte("The first argument ~S must be a symbol type.", symbol, NULL);
	clos_find_class(symbol, &instance);
	if (! structure_class_p(instance))
		fmte("The class ~S don't be a structure-class.", symbol, NULL);

	return make_structure_common(ptr, ret, instance, rest, 0);
}

_g int make_instance_structure(Execute ptr, addr rest, addr *ret)
{
	addr instance;
	getcons(rest, &instance, &rest);
	return make_structure_common(ptr, ret, instance, rest, 1);
}


/*
 *  copy-structure
 */
_g void copy_structure_common(addr inst, addr *ret)
{
	addr class_of, slots, clos, src, dst, pos;
	size_t size, i;

	Check(! structure_instance_p(inst), "type error");
	/* source */
	GetClassOfClos(inst, &class_of);
	GetSlotClos(inst, &slots);
	GetValueClos(inst, &src);
	/* destination */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, class_of);
	GetValueClos(clos, &dst);
	/* value */
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetClosValue(src, i, &pos);
		SetClosValue(dst, i, pos);
	}
	/* result */
	*ret = clos;
}


/*
 *  initialize
 */
_g void init_structure(void)
{
	SetPointerCall(defun, var1, structure_reader_list);
	SetPointerCall(defun, var1, structure_reader_vector);
	SetPointerCall(defun, var1, structure_reader_clos);
	SetPointerCall(defun, var2, structure_writer_list);
	SetPointerCall(defun, var2, structure_writer_vector);
	SetPointerCall(defun, var2, structure_writer_clos);
	SetPointerCall(defun, dynamic, structure_constructor_list);
	SetPointerCall(defun, dynamic, structure_constructor_vector);
	SetPointerCall(defun, dynamic, structure_constructor_clos);
	SetPointerCall(defun, var1, structure_copier_list);
	SetPointerCall(defun, var1, structure_copier_vector);
	SetPointerCall(defun, var1, structure_copier_clos);
	SetPointerCall(defun, var1, structure_predicate_list);
	SetPointerCall(defun, var1, structure_predicate_vector);
	SetPointerCall(defun, var1, structure_predicate_clos);
	SetPointerType(var4, method_defstruct_default);
	SetPointerType(var4, method_defstruct_object);
	SetPointerType(var4, method_defstruct_function);
}

