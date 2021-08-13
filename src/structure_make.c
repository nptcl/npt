#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "constant.h"
#include "cons.h"
#include "cons_list.h"
#include "control_execute.h"
#include "function.h"
#include "integer.h"
#include "package.h"
#include "package_intern.h"
#include "sequence.h"
#include "structure.h"
#include "structure_change.h"
#include "structure_define.h"
#include "structure_make.h"
#include "structure_object.h"
#include "structure_parse.h"
#include "structure_typedef.h"
#include "strtype.h"
#include "symbol.h"
#include "type_table.h"
#include "typedef.h"

/*
 *  make-instance
 */
static int structure_instance_include_(struct defstruct *str, addr instance)
{
	addr list, pos;

	/* include */
	if (str->include_p) {
		Return(stdset_structure_include_(instance, str->iname));
	}

	/* precedence-list */
	list = Nil;
	for (pos = instance; pos != Nil; ) {
		cons_heap(&list, pos, list);
		Return(stdget_structure_include_(pos, &pos));
	}
	GetConst(CLOS_STRUCTURE_OBJECT, &pos);
	cons_heap(&list, pos, list);
	GetConst(CLOS_T, &pos);
	cons_heap(&list, pos, list);
	nreverse(&list, list);
	Return(stdset_structure_precedence_list_(instance, list));

	return 0;
}

int structure_instance_(struct defstruct *str)
{
	addr clos, instance, pos;

	/* structure */
	GetConst(CLOS_STRUCTURE_CLASS, &clos);
	Return(clos_instance_heap_(clos, &instance));
	SetClassOfClos(instance, clos);
	/* name */
	Return(stdset_structure_name_(instance, str->name));
	/* documentation */
	if (str->doc != Nil) {
		Return(stdset_structure_documentation_(instance, str->doc));
	}
	/* include, precedence-list */
	Return(structure_instance_include_(str, instance));
	/* type */
	if (str->type_list_p) {
		GetConst(COMMON_LIST, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	else if (str->type_vector_p) {
		GetConst(COMMON_VECTOR, &pos);
		Return(stdset_structure_type_(instance, pos));
		Return(stdset_structure_vector_(instance, str->type_vector));
	}
	else {
		GetConst(COMMON_CLASS, &pos);
		Return(stdset_structure_type_(instance, pos));
	}
	/* named */
	Return(stdset_structure_named_(instance, str->named_p? T: Nil));
	/* result */
	str->instance = instance;

	return 0;
}


/*
 *  slots-make
 */
static int structure_pushnew_local_(LocalRoot local,
		addr *push, addr value, addr list, int *ret)
{
	int check;
	addr root, x;

	Check(! stringp(value), "type error");
	for (root = list; list != Nil; ) {
		GetCons(list, &x, &list);
		Return(string_equal_(x, value, &check));
		if (check)
			return Result(ret, 0);
	}
	cons_local(local, push, value, root);

	return Result(ret, 1);
}

static int structure_find_slotslist_(addr name, addr list, addr *ret)
{
	int check;
	addr pos, value;

	Check(! stringp(name), "type error");
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(name, value, &check));
		if (check)
			return Result(ret, pos);
	}

	return 0;
}

static int structure_make_slots_getvalue_(struct defstruct *str, size_t *ret)
{
	addr pos;
	size_t value;

	*ret = 0;
	value = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_value_(pos, &pos));
		if (pos != Nil) {
			Return(getindex_integer_(pos, &value));
		}
	}

	/* result */
	if (str->named_p)
		str->named_index = value;
	value += str->offset;

	return Result(ret, value);
}

static int structure_make_slots_setvalue_(struct defstruct *str, size_t value)
{
	addr pos, instance;

	/* value */
	instance = str->instance;
	str->size_value = value;
	make_index_integer_heap(&pos, value);
	Return(stdset_structure_value_(instance, pos));

	/* named_index */
	pos = Nil;
	if (str->named_p)
		make_index_integer_heap(&pos, str->named_index);
	Return(stdset_structure_named_index_(instance, pos));

	return 0;
}

static int structure_make_slots_include_(struct defstruct *str,
		addr *slots, addr *root, size_t *ret)
{
	LocalRoot local;
	addr list, pos, name, args;
	size_t size, i;

	local = str->ptr->local;
	*root = *slots = Nil;
	*ret = 0;
	size = 0;
	if (str->include_p) {
		pos = str->iname;
		Return(stdget_structure_slots_(pos, &list));
		args = str->iargs;
		LenSlotVector(list, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(list, i, &pos);
			slot_copy_heap(&pos, pos);
			GetNameSlot(pos, &name);
			GetNameSymbol(name, &name);
			cons_local(local, root, name, *root);
			Return(structure_find_slotslist_(name, args, &pos));
			Check(! slotp(pos), "type error");
			cons_local(local, slots, pos, *slots);
			/* location */
			SetLocationSlot(pos, i);
		}
	}

	return Result(ret, size);
}

static int structure_make_slots_slots_(struct defstruct *str,
		addr *slots, addr *root, addr *rdirect, size_t *ret)
{
	int check;
	LocalRoot local;
	addr list, pos, name, direct;
	size_t value, size, size_direct;

	local = str->ptr->local;
	Return(structure_make_slots_getvalue_(str, &value));

	/* size */
	*ret = 0;
	size = 0;
	if (str->include_p) {
		Return(stdget_structure_slots_(str->iname, &pos));
		LenSlotVector(pos, &size);
	}

	/* loop */
	direct = Nil;
	size_direct = 0;
	list = str->slots;
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetNameSlot(pos, &name);
		GetNameSymbol(name, &name);
		Return(structure_pushnew_local_(local, root, name, *root, &check));
		if (check) {
			/* slots */
			cons_local(local, slots, pos, *slots);
			SetLocationSlot(pos, size++);
			SetAccessSlot(pos, value++);
			/* direct-slots */
			cons_local(local, &direct, pos, direct);
			size_direct++;
		}
	}

	/* result */
	Return(structure_make_slots_setvalue_(str, value));
	*rdirect = direct;
	return Result(ret, size_direct);
}

static int structure_make_slots_array_(struct defstruct *str, addr slots, size_t size)
{
	addr instance, array, pos;
	size_t i;

	instance = str->instance;
	slot_vector_heap(&array, size);
	while (slots != Nil) {
		GetCons(slots, &pos, &slots);
		GetLocationSlot(pos, &i);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	str->slots = array;
	Return(stdset_structure_slots_(instance, array));

	return 0;
}

static int structure_make_slots_direct_(struct defstruct *str, addr list, size_t size)
{
	addr instance, array, pos;
	size_t i;

	/* direct-slots array */
	instance = str->instance;
	slot_vector_heap(&array, size);
	nreverse(&list, list);
	for (i = 0; list != Nil; i++) {
		GetCons(list, &pos, &list);
		SetClassSlot(pos, instance);
		SetSlotVector(array, i, pos);
	}
	Return(stdset_structure_direct_slots_(instance, array));

	return 0;
}

static int structure_make_slots_call_(struct defstruct *str)
{
	addr slots, root, direct;
	size_t size, size_direct;

	/* make slots */
	Return(structure_make_slots_include_(str, &slots, &root, &size));
	Return(structure_make_slots_slots_(str, &slots, &root, &direct, &size_direct));
	size += size_direct;
	str->size = size;

	/* set slots */
	Return(structure_make_slots_array_(str, slots, size));
	Return(structure_make_slots_direct_(str, direct, size_direct));

	return 0;
}

int structure_make_slots_(struct defstruct *str)
{
	LocalRoot local;
	LocalStack stack;

	local = str->ptr->local;
	push_local(local, &stack);
	Return(structure_make_slots_call_(str));
	rollback_local(local, stack);

	return 0;
}


/*
 *  accessor
 */
static int structure_slot_callname_(struct defstruct *str, addr *ret, addr pos)
{
	addr name;

	Check(! slotp(pos), "type error");
	GetNameSlot(pos, &pos);
	Check(! symbolp(pos), "type error");
	GetNameSymbol(pos, &pos);
	if (str->conc_name == Unbound) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_hyphen_heap_(ret, name, pos));
	}
	else if (str->conc_name == Nil) {
		*ret = pos;
	}
	else {
		Check(! stringp(str->conc_name), "type error");
		Return(string_concat_heap_(ret, str->conc_name, pos));
	}

	return 0;
}

static int structure_slot_readonly_p(addr slot)
{
	GetReadOnlySlot(slot, &slot);
	return slot != Nil;
}

static int structure_make_call_list_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_list(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_list(type, symbol);
	}

	return 0;
}

static int structure_make_call_vector_(struct defstruct *str)
{
	addr package, type, slots, pos, symbol;
	size_t size, i;

	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		structure_type(str, pos, &type);
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_vector(type, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_vector(type, symbol);
	}

	return 0;
}

static int structure_make_call_clos_(struct defstruct *str)
{
	addr instance, package, slots, pos, symbol;
	size_t size, i;

	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	Return(getpackage_(str->ptr, &package));
	slots = str->slots;
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(structure_slot_callname_(str, &symbol, pos));
		Return(intern_package_(package, symbol, &symbol, NULL));
		Return(parse_callname_error_(&symbol, symbol));
		structure_slot_reader_clos(instance, pos, symbol);
		if (! structure_slot_readonly_p(pos))
			structure_slot_writer_clos(instance, pos, symbol);
	}

	return 0;
}

/* call */
int structure_make_call_(struct defstruct *str)
{
	if (str->type_list_p)
		return structure_make_call_list_(str);
	else if (str->type_vector_p)
		return structure_make_call_vector_(str);
	else
		return structure_make_call_clos_(str);
}


/*
 *  constructor
 */
static int structure_constructor_find_slots_(addr key, addr slots, int *ret)
{
	int check;
	addr value;
	size_t size, i;

	Check(! symbolp(key), "type error");
	Check(! slot_vector_p(slots), "type error");
	GetNameSymbol(key, &key);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		GetNameSlot(value, &value);
		GetNameSymbol(value, &value);
		Return(string_equal_(key, value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int structure_constructor_dynamic_p_(
		int errorp, addr key, addr slots, int *ret)
{
	int check;

	if (! errorp)
		return Result(ret, 0);
	Return(structure_constructor_find_slots_(key, slots, &check));
	return Result(ret, ! check);
}

static int structure_constructor_dynamic_(addr instance,
		addr slots, addr list, int errorp)
{
	int check;
	addr key;

	while (list != Nil) {
		if (! consp(list))
			return fmte_("Invalid keyword-argumets ~S.", list, NULL);
		GetCons(list, &key, &list);
		if (! consp(list))
			return fmte_("There is no value in the key ~S arguemnts.", key, NULL);
		if (! symbolp(key))
			return fmte_("The key ~S must be a symbol type.", key, NULL);
		Return(structure_constructor_dynamic_p_(errorp, key, slots, &check));
		if (check) {
			return fmte_("There is no slot ~S "
					"in the structure ~S.", key, instance, NULL);
		}
		GetCdr(list, &list);
	}

	return 0;
}

static int function_structure_constructor_find_(
		addr key, addr list, addr *value, int *ret)
{
	int check;
	addr left, right, g;

	Check(! slotp(key), "type error");
	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	GetNameSlot(key, &key);
	GetNameSymbol(key, &key);
	while (list != Nil) {
		GetCons(list, &left, &list);
		GetCons(list, &right, &list);
		Check(! symbolp(left), "type error");
		if (right == g)
			continue;
		GetNameSymbol(left, &left);
		Return(string_equal_(key, left, &check));
		if (check) {
			*value = right;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}


/* make-structure-list */
static int structure_constructor_instance_list_(Execute ptr,
		addr list, addr slots, addr args)
{
	int check;
	addr slot, pos;
	size_t size, i, index;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetAccessSlot(slot, &index);
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		setnth_unsafe(list, index, pos);
	}

	return 0;
}

static void make_structure_nil(addr *ret, size_t size)
{
	addr list;
	size_t i;

	list = Nil;
	for (i = 0; i < size; i++)
		cons_heap(&list, Nil, list);
	*ret = list;
}

int make_structure_list_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, list, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	make_structure_nil(&list, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, list);
		Return(structure_constructor_instance_list_(ptr, list, slots, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setnth_(list, str->named_index, name));
	}

	return Result(ret, list);
}


/* make-structure-vector */
static int structure_constructor_instance_vector_(Execute ptr,
		addr vector, addr slots, addr value, addr args)
{
	int update, check;
	addr slot, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		update = 0;
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (check) {
			update = 1;
		}
		else {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
				update = 1;
			}
		}
		if (update) {
			Return(structure_setarray_(ptr, vector, slot, value, pos));
		}
	}

	return 0;
}

int make_structure_vector_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	addr instance, slots, vector, type, name;
	struct structure_type_struct *str;
	LocalHold hold;

	/* type */
	str = PtrStructureType(pos);
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetVectorStructureType(pos, &type);
	/* make */
	Return(structure_constructor_dynamic_(instance, slots, args, str->errorp));
	vector_heap(&vector, str->size_value);

	if (initp) {
		hold = LocalHold_local_push(ptr, vector);
		Return(structure_constructor_instance_vector_(ptr, vector, slots, type, args));
		localhold_end(hold);
	}

	if (str->named) {
		GetNameStructureType(pos, &name);
		Return(setelt_sequence_(vector, str->named_index, name));
	}

	return Result(ret, vector);
}


/* make-structure-clos */
static int structure_constructor_instance_clos_(
		Execute ptr, addr clos, addr args, int initp)
{
	int check;
	addr slots, slot, value, pos;
	size_t size, i, location;

	GetSlotClos(clos, &slots);
	GetValueClos(clos, &value);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetLocationSlot(slot, &location);
		if (! initp) {
			SetClosValue(value, location, Nil);
			continue;
		}
		Return(function_structure_constructor_find_(slot, args, &pos, &check));
		if (! check) {
			GetFunctionSlot(slot, &pos);
			if (pos != Nil) {
				Return(callclang_apply(ptr, &pos, pos, Nil));
			}
		}
		SetClosValue(value, location, pos);
	}

	return 0;
}

int make_structure_clos_(Execute ptr, addr *ret, addr pos, addr args, int initp)
{
	int errorp;
	addr instance, slots, clos;
	LocalHold hold;

	/* variables */
	GetInstanceStructureType(pos, &instance);
	GetSlotStructureType(pos, &slots);
	GetErrorpStructureType(pos, &errorp);
	Return(structure_constructor_dynamic_(instance, slots, args, errorp));
	/* make */
	clos_heap(&clos, slots);
	SetClassOfClos(clos, instance);

	hold = LocalHold_local_push(ptr, clos);
	Return(structure_constructor_instance_clos_(ptr, clos, args, initp));
	localhold_end(hold);

	return Result(ret, clos);
}

/* default */
static int structure_constructor_default_(struct defstruct *str, addr symbol)
{
	addr pos;

	Return(parse_callname_error_(&symbol, symbol));
	structure_type(str, str->slots, &pos);
	if (str->type_list_p)
		structure_constructor_default_list(pos, symbol);
	else if (str->type_vector_p)
		structure_constructor_default_vector(pos, symbol);
	else
		structure_constructor_default_clos(pos, symbol);

	return 0;
}

/* constructor */
static int structure_constructor_make_(struct defstruct *str)
{
	addr name;

	/* name */
	Return(stdget_structure_name_(str->instance, &name));
	GetNameSymbol(name, &name);
	Return(string_concat_char1_heap_(&name, "MAKE-", name));
	Return(intern_default_package_(str->ptr, name, &name, NULL));
	/* make */
	return structure_constructor_default_(str, name);
}

static int structure_constructor_lambda_(addr list)
{
	addr symbol, name, pos, type;

	/* (symbol function) */
	Return(list_bind_(list, &symbol, &pos, NULL));
	Return(parse_callname_error_(&name, symbol));
	SetNameFunction(pos, name);
	SetFunctionSymbol(symbol, pos);
	/* type */
	GetTypeTable(&type, Function);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);

	return 0;
}

int structure_make_constructor_(struct defstruct *str)
{
	addr list, pos, g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	for (list = str->constructor; list != Nil; ) {
		GetCons(list, &pos, &list);
		if (pos == g) {
			Return(structure_constructor_make_(str));
		}
		else if (symbolp(pos)) {
			Return(structure_constructor_default_(str, pos));
		}
		else if (consp(pos)) {
			Return(structure_constructor_lambda_(pos));
		}
		else {
			return fmte_("Invalid constructor parameter ~S.", pos, NULL);
		}
	}

	return 0;
}


/*
 *  copier
 */
static int structure_copier_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->copier_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char1_heap_(&name, "COPY-", name));
	}
	else if (str->copier == Nil) {
		return Result(ret, Unbound);
	}
	else {
		Check(! stringp(str->copier), "type error");
		name = str->copier;
	}
	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_make_copier_(struct defstruct *str)
{
	addr symbol;

	Return(structure_copier_callname_(str, &symbol));
	if (symbol == Unbound)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_copier_list(str, symbol);
	else if (str->type_vector_p)
		structure_copier_vector(str, symbol);
	else
		structure_copier_clos(str->instance, symbol);

	return 0;
}


/*
 *  predicate
 */
static int structure_predicate_callname_(struct defstruct *str, addr *ret)
{
	addr name;

	if (! str->predicate_p) {
		Return(stdget_structure_name_(str->instance, &name));
		GetNameSymbol(name, &name);
		Return(string_concat_char2_heap_(&name, name, "-P"));
	}
	else if (str->predicate == Nil) {
		return Result(ret, Nil);
	}
	else {
		Check(! stringp(str->predicate), "type error");
		name = str->predicate;
	}

	return intern_default_package_(str->ptr, name, ret, NULL);
}

int structure_make_predicate_(struct defstruct *str, addr instance)
{
	addr symbol;

	Return(structure_predicate_callname_(str, &symbol));
	Return(stdset_structure_predicate_(instance, symbol));
	if (symbol == Nil)
		return 0;

	Return(parse_callname_error_(&symbol, symbol));
	if (str->type_list_p)
		structure_predicate_list(str, symbol);
	else if (str->type_vector_p)
		structure_predicate_vector(str, symbol);
	else
		structure_predicate_clos(instance, symbol);

	return 0;
}


/*
 *  printer
 */
static int structure_print_default_p(struct defstruct *str)
{
	addr g;

	GetConst(SYSTEM_STRUCTURE_GENSYM, &g);
	if (str->print_object_p && str->print_object == g)
		return 1;
	if (str->print_function_p && str->print_function == g)
		return 1;

	return 0;
}

int structure_make_print_(struct defstruct *str)
{
	if (str->type_p && str->print_object_p)
		return fmte_("Can't make print-object on :TYPE structure.", NULL);
	if (str->type_p && str->print_function_p)
		return fmte_("Can't make print-function on :TYPE structure.", NULL);
	if (structure_print_default_p(str))
		return structure_print_default_(str);
	else if (str->print_object_p)
		return structure_print_object_(str);
	else if (str->print_function_p)
		return structure_print_function_(str);

	return 0;
}


/*
 *  ensure-structure
 */
static int structure_check_exist_p(struct defstruct *str)
{
	addr pos;

	clos_find_class_nil(str->name, &pos);
	str->change = pos;

	return pos != Nil;
}

static int structure_make_(struct defstruct *str)
{
	addr instance;

	/* make instance */
	Return(structure_instance_(str));
	instance = str->instance;
	Check(! structure_class_p_debug(instance), "type error");
	clos_define_class(str->name, instance);

	/* settings */
	Return(structure_make_slots_(str));
	Return(structure_make_call_(str));
	Return(structure_make_copier_(str));
	Return(structure_make_predicate_(str, instance));
	Return(structure_make_constructor_(str));
	Return(structure_make_print_(str));

	return 0;
}

int ensure_structure_common_(Execute ptr, addr name, addr slots, addr args)
{
	struct defstruct str;
	LocalHold hold;

	Check(! symbolp(name), "type error");
	Check(! listp(slots), "type error");
	Return(ensure_structure_struct_(&str, ptr, name, slots, args));

	hold = LocalHold_local(ptr);
	localhold_destruct(hold, &str);
	Return(structure_arguments_(&str));

	if (structure_check_exist_p(&str)) {
		Return(structure_change_(&str));
	}
	else {
		Return(structure_make_(&str));
	}
	localhold_end(hold);

	return 0;
}

