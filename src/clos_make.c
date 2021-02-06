#include "callname.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_make.h"
#include "clos_method.h"
#include "clos_type.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "function.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "pointer.h"
#include "symbol.h"
#include "type_table.h"

/*
 *  ensure-class
 */
int clos_ensure_class_supers_(addr args, addr *ret, int *referp)
{
	int check;
	addr list, pos;

	/* arguments */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SUPERCLASSES, &list)) {
		/* (list (find-class 'standard-object)) */
		GetConst(CLOS_STANDARD_OBJECT, &args);
		Return(clos_find_class_(args, &args));
		list_heap(ret, args, NULL);
		if (referp)
			*referp = 0;
		return 0;
	}

	/* check forward-referenced-class */
	*ret = list;
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check) {
			if (referp) {
				*referp = 1;
				return 0;
			}
			else {
				return fmte_("Cannot have a forward-referenced-class ~S "
						"in the finalized class.", pos, NULL);
			}
		}
	}
	if (referp)
		*referp = 0;

	return 0;
}

static int clos_ensure_class_parse_slots_(addr list, addr *ret)
{
	addr slot, name, readers, writers, alloc, args, form, func, type, doc;

	/* arguments */
	if (GetKeyArgs(list, CLOSKEY_NAME, &name)) {
		*ret = Nil;
		return fmte_("Invalid slot :name ~S.", name, NULL);
	}
	if (GetKeyArgs(list, CLOSKEY_TYPE, &type))
		GetTypeTable(&type, T);
	if (GetKeyArgs(list, CLOSKEY_INITARGS, &args))
		args = Nil;
	if (GetKeyArgs(list, CLOSKEY_INITFORM, &form))
		form = Unbound;
	if (GetKeyArgs(list, CLOSKEY_INITFUNCTION, &func))
		func = Nil;
	if (GetKeyArgs(list, CLOSKEY_READERS, &readers))
		readers = Nil;
	if (GetKeyArgs(list, CLOSKEY_WRITERS, &writers))
		writers = Nil;
	if (GetKeyArgs(list, CLOSKEY_DOCUMENTATION, &doc))
		doc = Nil;
	if (GetKeyArgs(list, CLOSKEY_ALLOCATION, &alloc))
		GetConst(CLOSKEY_INSTANCE, &alloc);

	/* make-slot */
	slot_heap(&slot);
	SetNameSlot(slot, name);
	SetTypeSlot(slot, type);
	SetArgsSlot(slot, args);
	SetFormSlot(slot, form);
	SetFunctionSlot(slot, func);
	SetReadersSlot(slot, readers);
	SetWritersSlot(slot, writers);
	SetDocumentSlot(slot, doc);
	Return(slot_set_allocation_(slot, alloc));

	/* result */
	return Result(ret, slot);
}

int clos_ensure_class_slots_(addr args, addr *ret)
{
	addr slots, pos;
	size_t size, i;

	/* :direct-slot list */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SLOTS, &args))
		args = Nil;

	/* slot-vector */
	Return(length_list_safe_(args, &size));
	slot_vector_heap(&slots, size);
	for (i = 0; args != Nil; i++) {
		GetCons(args, &pos, &args);
		Return(clos_ensure_class_parse_slots_(pos, &pos));
		SetSlotVector(slots, i, pos);
	}

	return Result(ret, slots);
}

int clos_ensure_class_direct_default_initargs_(LocalRoot local,
		addr pos, addr args, addr *ret)
{
	addr check, list, key, a, b;
	LocalStack stack;

	if (GetKeyArgs(args, CLOSKEY_DIRECT_DEFAULT_INITARGS, &args))
		return Result(ret, Nil);
	/* check only */
	push_local(local, &stack);
	*ret = args;
	check = Nil;
	while (args != Nil) {
		Return_getcons(args, &list, &args);
		/* (key initform initfunction) form */
		Return(list_bind_(list, &key, &a, &b, NULL));
		/* check duplicate */
		if (find_list_eq_unsafe(key, check)) {
			*ret = Nil;
			return fmte_(":INITARG ~S is already exist.", key, NULL);
		}
		cons_local(local, &check, key, check);
	}
	rollback_local(local, stack);

	return 0;
}

static int clos_ensure_class_default_initargs_(LocalRoot local, addr pos, addr *ret)
{
	addr root, check, list, args, init, key;
	LocalStack stack;

	Return(stdget_class_precedence_list_(pos, &list));
	root = check = Nil;
	push_local(local, &stack);
	while (list != Nil) {
		Return_getcons(list, &args, &list);
		Return(stdget_class_direct_default_initargs_(args, &args));
		while (args != Nil) {
			Return_getcons(args, &init, &args);
			Return_getcar(init, &key);
			if (! find_list_eq_unsafe(key, check)) {
				cons_local(local, &check, key, check);
				cons_heap(&root, init, root);
			}
		}
	}
	rollback_local(local, stack);
	nreverse(ret, root);

	return 0;
}

/* reader/writer check */
static int clos_ensure_reader_check_(Execute ptr, addr gen)
{
	int check;
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return 0;
	if (functionp(gen))
		return fmte_("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	Return(stdget_generic_method_combination_(gen, &pos));
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		return fmte_("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	Return(stdget_generic_lambda_list_(gen, &pos));
	if (! argumentp(pos)) {
		Return(argument_generic_heap_(ptr->local, &pos, pos));
	}
	if (ArgumentStruct(pos)->var != 1)
		return fmte_("The generic-function ~S must be a 1 specializer.", gen, NULL);

	return 0;
}

static int clos_ensure_writer_method_check_(Execute ptr, addr gen)
{
	int check;
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return 0;
	if (functionp(gen))
		return fmte_("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	Return(stdget_generic_method_combination_(gen, &pos));
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		return fmte_("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	Return(stdget_generic_lambda_list_(gen, &pos));
	if (! argumentp(pos)) {
		Return(argument_generic_heap_(ptr->local, &pos, pos));
	}
	if (ArgumentStruct(pos)->var != 2)
		return fmte_("The generic-function ~S must be a 2 specializers.", gen, NULL);

	return 0;
}

static int clos_ensure_readers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_reader_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_writers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_writer_method_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_class_function_check_(Execute ptr, addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_check_(ptr, list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_check_(ptr, list));
	}

	return 0;
}

/* reader/writer generic */
static int clos_ensure_reader_generic_(addr name)
{
	addr lambda;
	mop_argument_generic_var1(&lambda);
	return generic_empty_(name, lambda, &name);
}

static int clos_ensure_writer_generic_(addr name)
{
	addr lambda;
	mop_argument_generic_var2(&lambda);
	return generic_empty_(name, lambda, &name);
}

static int clos_ensure_readers_generic_(addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		Return(clos_ensure_reader_generic_(name));
	}

	return 0;
}

static int clos_ensure_writers_generic_(addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		Return(clos_ensure_writer_generic_(name));
	}

	return 0;
}

static int clos_ensure_class_function_generic_(addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_generic_(list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_generic_(list));
	}

	return 0;
}

/* make method */
static void method_argument_clos_ensure_reader(addr clos, addr *ret)
{
	addr pos, list;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 1;
	list_heap(&list, Nil, clos, NULL);
	list_heap(&list, list, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int function_clos_ensure_reader(Execute ptr, addr method, addr next, addr inst)
{
	addr call, symbol;

	/* (slot-value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getfunction_global_(call, &call));
	getdata_control(ptr, &symbol);
	return funcall_control(ptr, call, inst, symbol, NULL);
}

static int clos_ensure_reader_method_(Execute ptr,
		addr clos, addr name, addr gen, addr symbol)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	SetDataFunction(call, symbol);
	setcompiled_var3(call, p_defun_clos_ensure_reader);
	GetTypeCompiled(&type, Reader_Method);
	settype_function(call, type);
	/* method */
	method_argument_clos_ensure_reader(clos, &pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return method_add_method_(ptr, gen, pos);
}

static void method_argument_clos_ensure_writer(addr clos, addr *ret)
{
	addr pos, list, type1, type2;
	struct argument_struct *str;

	/* object */
	argument_heap(&pos);
	str = ArgumentStruct(pos);
	str->type = ArgumentType_method;
	/* var */
	str->var = 2;
	ArgumentMethod_var(&type1, T);
	list_heap(&type2, Nil, clos, NULL);
	list_heap(&list, type1, type2, NULL);
	SetArgument(pos, ArgumentIndex_var, list);
	/* result */
	*ret = pos;
}

static int function_clos_ensure_writer_instance(Execute ptr,
		addr method, addr next, addr value, addr inst)
{
	addr call, symbol;

	/* ((setf slot-value) value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getsetf_global_(call, &call));
	getdata_control(ptr, &symbol);
	return funcall_control(ptr, call, value, inst, symbol, NULL);
}

static int clos_ensure_writer_method_(Execute ptr,
		addr clos, addr name, addr gen, addr symbol)
{
	addr pos, call, type;

	/* function */
	compiled_heap(&call, name);
	SetDataFunction(call, symbol);
	setcompiled_var4(call, p_defun_clos_ensure_writer_instance);
	GetTypeCompiled(&type, Writer_Method);
	settype_function(call, type);
	/* method */
	method_argument_clos_ensure_writer(clos, &pos);
	Return(method_instance_lambda_(ptr->local, &pos, Nil, pos));
	Return(stdset_method_function_(pos, call));
	return method_add_method_(ptr, gen, pos);
}

static int clos_ensure_readers_method_(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "generic-function error");
		Return(clos_ensure_reader_method_(ptr, pos, name, gen, symbol));
	}

	return 0;
}

static int clos_ensure_writers_method_(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "generic-function error");
		Return(clos_ensure_writer_method_(ptr, pos, name, gen, symbol));
	}

	return 0;
}

static int clos_ensure_class_method_(Execute ptr, addr pos)
{
	addr slots, slot, symbol, list;
	size_t size, i;

	Return(stdget_class_slots_(pos, &slots));
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &symbol);
		/* reader */
		GetReadersSlot(slot, &list);
		Return(clos_ensure_readers_method_(ptr, pos, symbol, list));
		/* writer */
		GetWritersSlot(slot, &list);
		Return(clos_ensure_writers_method_(ptr, pos, symbol, list));
	}

	return 0;
}

static int clos_ensure_class_function_(Execute ptr, addr pos)
{
	/* check */
	Return(clos_ensure_class_function_check_(ptr, pos));
	/* make generic-function */
	Return(clos_ensure_class_function_generic_(pos));
	/* make method */
	return clos_ensure_class_method_(ptr, pos);
}

static int clos_ensure_class_subclasses_(addr pos)
{
	addr supers, super, list;

	Return(stdget_class_direct_superclasses_(pos, &supers));
	while (supers != Nil) {
		Return_getcons(supers, &super, &supers);
		Return(stdget_class_direct_subclasses_(super, &list));
		pushnew_heap(list, pos, &list);
		Return(stdset_class_direct_subclasses_(super, list));
	}

	return 0;
}

static int clos_built_in_class_check_(addr pos, addr list)
{
	int check;
	addr super, tclass, built;

	GetConst(CLOS_T, &tclass);
	GetConst(CLOS_BUILT_IN_CLASS, &built);
	while (list != Nil) {
		GetCons(list, &super, &list);
		if (super == tclass)
			continue;
		Return(clos_subtype_p_(super, built, &check));
		if (check) {
			Return(stdget_class_name_(pos, &pos));
			return call_type_error_va_(NULL, pos, Nil,
					"Invalid superclass ~S in ~S.", super, pos, NULL);
		}
	}

	return 0;
}

int clos_ensure_class_init_(LocalRoot local, addr pos, int pushp)
{
	addr value;

	/* class-precedence-list */
	Return(clos_precedence_list_(local, pos, &value));
	Return(clos_built_in_class_check_(pos, value));
	Return(stdset_class_precedence_list_(pos, value));
	/* effective-slots */
	Return(clos_compute_slots_(local, pos, &value));
	Return(stdset_class_slots_(pos, value));
	/* default-initargs */
	Return(clos_ensure_class_default_initargs_(local, pos, &value));
	Return(stdset_class_default_initargs_(pos, value));
	/* subclasses */
	if (pushp) {
		Return(clos_ensure_class_subclasses_(pos));
	}

	return 0;
}

static int clos_ensure_class_set_(
		LocalRoot local, addr pos, addr name, addr args, int pushp)
{
	int referp;
	addr supers, slots, value;

	/* arguments */
	Return(clos_ensure_class_supers_(args, &supers, &referp));
	Return(clos_ensure_class_slots_(args, &slots));
	/* set value */
	Return(stdset_class_name_(pos, name));
	Return(clos_stdclass_direct_slots_(pos, slots));
	Return(stdset_class_direct_superclasses_(pos, supers));
	/* direct-default-initargs */
	Return(clos_ensure_class_direct_default_initargs_(local, pos, args, &value));
	Return(stdset_class_direct_default_initargs_(pos, value));
	/* forward-referenced-class */
	if (! referp)
		return clos_ensure_class_init_(local, pos, pushp);

	return 0;
}

int clos_finalize_(Execute ptr, addr pos, int *ret);
static int clos_finalize_forward_(Execute ptr, addr pos, addr *value, int *ret)
{
	int check;
	addr name;

	Return(stdget_class_name_(pos, &name));
	Return(clos_find_class_(name, &pos));
	Return(clos_finalize_(ptr, pos, &check));
	if (check)
		return Result(ret, 1);
	*value = pos;
	return Result(ret, 0);
}

static int clos_finalize_forward_p_(Execute ptr, addr clos, int *refp, int *ret)
{
	int value, check;
	addr list, root, pos;
	LocalRoot local;
	LocalStack stack;

	/* update */
	Return(stdget_class_direct_superclasses_(clos, &list));
	value = 0;
	local = ptr->local;
	push_local(local, &stack);
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check) {
			Return(clos_finalize_forward_(ptr, pos, &pos, &check));
			if (check)
				return Result(ret, 1);
			value = 1;
		}
		cons_local(local, &root, pos, root);
	}
	/* replace */
	if (value) {
		reverse_list_heap_unsafe(&root, root);
		Return(stdset_class_direct_superclasses_(clos, root));
	}
	rollback_local(local, stack);
	*refp = value;

	return Result(ret, 0);
}

int clos_finalize_(Execute ptr, addr pos, int *ret)
{
	int refp, check;
	addr list, value;

	/* finalized check */
	Return(stdget_class_finalized_p_(pos, &value));
	if (value != Nil)
		return Result(ret, 0);

	/* referenced class */
	Return(clos_finalize_forward_p_(ptr, pos, &refp, &check));
	if (check)
		return Result(ret, 1);
	if (refp) {
		/* make class */
		Return(clos_ensure_class_init_(ptr->local, pos, 1));
	}

	/* superclasses */
	Return(stdget_class_direct_superclasses_(pos, &list));
	while (list != Nil) {
		GetCons(list, &value, &list);
		Return(clos_finalize_(ptr, value, &check));
		if (check)
			return Result(ret, 1);
	}

	/* prototype */
	Return(clos_stdclass_prototype_(pos));
	Return(clos_ensure_class_function_(ptr, pos));
	Return(stdset_class_finalized_p_(pos, T));

	return Result(ret, 0);
}

static int clos_ensure_class_object_(Execute ptr, addr name, addr args, addr *ret)
{
	int ignore;
	addr metaclass, pos;
	LocalRoot local;

	/* :metaclass ... */
	if (GetKeyArgs(args, CLOSKEY_METACLASS, &metaclass))
		GetConst(CLOS_STANDARD_CLASS, &metaclass);
	local = ptr->local;
	GetConst(CLOSKEY_METACLASS, &pos);
	Return(remplist_local_(local, args, pos, &args, &ignore));

	/* (apply #'make-instance metaclass args) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	Return(getfunction_global_(pos, &pos));
	return callclang_applya(ptr, ret, pos, metaclass, args, NULL);
}

int clos_ensure_class_(Execute ptr, addr name, addr args, addr *ret)
{
	addr pos;

	/* make-instance */
	Return(clos_ensure_class_object_(ptr, name, args, &pos));

	/* define class */
	Return(clos_ensure_class_set_(ptr->local, pos, name, args, 1));
	clos_define_class(name, pos);

	return Result(ret, pos);
}


/*
 *  allocate-initialize
 */
int allocate_instance_stdclass_(Execute ptr, addr clos, addr *ret)
{
	int check;
	addr instance, slots, slot, name;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* finalized */
	Return(clos_finalize_(ptr, clos, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Cannot finalize class object ~S.", clos, NULL);
	}

	/* allocate */
	Return(stdget_class_slots_(clos, &slots));
	slot_vector_copyheap_alloc(NULL, &slots, slots);
	clos_heap(&instance, slots);

	/* class-of */
	SetClassOfClos(instance, clos);

	/* value */
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* name check */
		GetNameSlot(slot, &name);
		if (! symbolp(name)) {
			*ret = Nil;
			return fmte_("The slot name ~S must be a symbol.", name, NULL);
		}
		/* already exist */
		if (clos_find_slotname(slots, i, name)) {
			*ret = Nil;
			return fmte_("The slot name ~S already exists.", name, NULL);
		}
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i) {
			*ret = Nil;
			return fmte_("The slot location ~A is invalid.", intsizeh(i), NULL);
		}
	}

	return Result(ret, instance);
}


/*
 *  initialize-instance
 *  reinitialize-instance
 */
static int initialize_instance_(Execute ptr, addr pos, addr type, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance type initargs) */
	addr call;

	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	return callclang_applya(ptr, ret, call, pos, type, rest, NULL);
}

int initialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance T initargs) */
	return initialize_instance_(ptr, pos, T, rest, ret);
}

int reinitialize_instance_stdobject_(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance () initargs) */
	return initialize_instance_(ptr, pos, Nil, rest, ret);
}


/*
 *  shared-initialize
 */
static int setf_slot_value_call_(Execute ptr, addr pos, addr key, addr value);
static int shared_initialize_arguments_(Execute ptr,
		addr pos, addr slot, addr rest, int *ret)
{
	addr list, key, value;

	GetArgsSlot(slot, &list);
	while (list != Nil) {
		Return_getcons(list, &key, &list);
		if (getplist_safe(rest, key, &value))
			continue;
		GetNameSlot(slot, &key);
		Return(setf_slot_value_call_(ptr, pos, key, value));
		return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int slot_boundp_call_(Execute ptr, addr pos, addr key, int *ret);
static int shared_initialize_initform_(Execute ptr, addr pos, addr key, addr slot)
{
	int check;
	addr value;

	/* boundp */
	Return(slot_boundp_call_(ptr, pos, key, &check));
	if (check)
		return 0;

	/* initform */
	GetFormSlot(slot, &value);
	if (value == Unbound)
		return 0;

	/* initfunction */
	GetFunctionSlot(slot, &value);
	if (value == Nil) {
		/* :initform */
		GetFormSlot(slot, &value);
	}
	else {
		/* funcall */
		Return(callclang_funcall(ptr, &value, value, NULL));
	}

	/* (setf slot-value) */
	return setf_slot_value_call_(ptr, pos, key, value);
}

static int shared_initialize_stdobject_p_(addr key, addr name, int *ret)
{
	int check;

	if (name == T)
		return Result(ret, 0);
	Return(find_list_eq_safe_(key, name, &check));
	return Result(ret, ! check);
}

int shared_initialize_stdobject_(Execute ptr, addr pos, addr name, addr rest)
{
	int check;
	addr slots, slot, key;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* initialize arguments */
		Return(shared_initialize_arguments_(ptr, pos, slot, rest, &check));
		if (check)
			continue;
		/* initform */
		GetNameSlot(slot, &key);
		Return(shared_initialize_stdobject_p_(key, name, &check));
		if (check)
			continue;
		Return(shared_initialize_initform_(ptr, pos, key, slot));
	}

	return 0;
}


/*
 *  make-instance
 */
static int make_instance_initargs_(Execute ptr, addr clos, addr rest, addr *ret)
{
	addr list, root, keys, key, slots, temp, value, call;
	LocalRoot local;

	local = ptr->local;
	Return(stdget_class_default_initargs_(clos, &list));
	if (list == Nil) {
		cons_local(local, ret, clos, rest);
		return 0;
	}

	/* rest */
	root = keys = Nil;
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcons(rest, &value, &rest);
		if (! find_list_eq_unsafe(key, keys)) {
			cons_local(local, &keys, key, keys);
			cons_local(local, &root, key, root);
			cons_local(local, &root, value, root);
		}
	}

	/* default-initargs */
	Return(stdget_class_slots_(clos, &slots));
	for (root = Nil; list != Nil; ) {
		Return_getcons(list, &temp, &list);
		Return(list_bind_(temp, &key, &value, &call, NULL));
		if (! find_list_eq_unsafe(key, keys)) {
			if (call != Nil) {
				Return(callclang_funcall(ptr, &value, call, NULL));
			}
			cons_local(local, &keys, key, keys);
			cons_local(local, &root, key, root);
			cons_local(local, &root, value, root);
		}
	}

	/* result */
	nreverse(&root, root);
	cons_local(local, ret, clos, root);
	return 0;
}

static int make_instance_check_(Execute ptr, addr clos, addr rest)
{
	int loop, check;
	addr slots, slot, key, value;
	size_t size, i;

	Return(stdget_class_slots_(clos, &slots));
	LenSlotVector(slots, &size);
	Return_getcdr(rest, &rest);
	while (rest != Nil) {
		Return_getcons(rest, &key, &rest);
		Return_getcdr(rest, &rest);
		loop = 0;
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &slot);
			GetArgsSlot(slot, &value);
			Return(find_list_eq_safe_(key, value, &check));
			loop |= check;
		}
		if (! loop) {
			return fmte_("The initialize argument ~S don't exist in ~S slots.",
					key, clos, NULL);
		}
	}

	return 0;
}

int make_instance_stdclass_(Execute ptr, addr rest, addr *ret)
{
	int check;
	addr clos, type, call, instance;

	/* built-in-class */
	GetCons(rest, &clos, &rest);
	GetConst(CLOS_BUILT_IN_CLASS, &type);
	if (clos == type) {
		GetConst(CLOS_STANDARD_CLASS, &type);
		return call_type_error_va_(ptr, clos, type,
				"Cannot make an instance of the built-in-class.", NULL);
	}

	/* finalize */
	Return(clos_finalize_(ptr, clos, &check));
	if (check) {
		*ret = Nil;
		return fmte_("Cannot finalize class object ~S.", clos, NULL);
	}

	/* initargs */
	Return(make_instance_initargs_(ptr, clos, rest, &rest));
	Return(make_instance_check_(ptr, clos, rest));

	/* allocation-instance */
	GetConst(COMMON_ALLOCATE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	Return(callclang_apply(ptr, &instance, call, rest));

	/* initialize-instance */
	GetCdr(rest, &rest);
	cons_local(ptr->local, &rest, instance, rest);
	GetConst(COMMON_INITIALIZE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	Return(callclang_apply(ptr, &call, call, rest));

	/* result */
	return Result(ret, instance);
}


/*
 *  slot-missing
 */
static int clos_slot_missing_(Execute ptr, addr *ret,
		addr clos, addr pos, addr name, addr operation, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_MISSING, &call);
	Return(getfunction_global_(call, &call));
	return callclang_funcall(ptr, ret, call, clos, pos, name, operation, value, NULL);
}

static int clos_slot_unbound_(Execute ptr, addr *ret, addr clos, addr pos, addr name)
{
	addr call;

	GetConst(COMMON_SLOT_UNBOUND, &call);
	Return(getfunction_global_(call, &call));
	return callclang_funcall(ptr, ret, call, clos, pos, name, NULL);
}


/*
 *  slot-boundp
 */
static int slot_boundp_call_(Execute ptr, addr pos, addr key, int *ret)
{
	addr call;

	GetConst(COMMON_SLOT_BOUNDP, &call);
	Return(getfunction_global_(call, &call));
	Return(callclang_funcall(ptr, &pos, call, pos, key, NULL));
	return Result(ret, (pos != Nil));
}

int slot_boundp_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, int *ret)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			GetClosValue(pos, i, &pos);
			return Result(ret, pos != Unbound);
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_boundp_call_(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_BOUNDP, &check);
	Return(clos_slot_missing_(ptr, &pos, clos, pos, key, check, Unbound));
	return Result(ret, (pos != Nil));
}


/*
 *  slot-makunbound
 */
static int slot_makunbound_call_(Execute ptr, addr pos, addr key)
{
	addr call;

	GetConst(COMMON_SLOT_MAKUNBOUND, &call);
	Return(getfunction_global_(call, &call));
	return callclang_funcall(ptr, &pos, call, pos, key, NULL);
}

int slot_makunbound_using_class_(Execute ptr, addr clos, addr pos, addr key)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			SetClosValue(pos, i, Unbound);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_makunbound_call_(ptr, pos, key);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_MAKUNBOUND, &check);
	return clos_slot_missing_(ptr, &pos, clos, pos, key, check, Unbound);
}


/*
 *  slot-value
 */
static int slot_value_call_(Execute ptr, addr pos, addr key, addr *ret)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getfunction_global_(call, &call));
	return callclang_funcall(ptr, ret, call, pos, key, NULL);
}

static int slot_value_using_class_getp_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			GetClosValue(pos, i, ret);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return slot_value_call_(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_VALUE, &check);
	return clos_slot_missing_(ptr, ret, clos, pos, key, check, Unbound);
}

int slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr *ret)
{
	Return(slot_value_using_class_getp_(ptr, clos, pos, key, ret));
	if (*ret == Unbound)
		return clos_slot_unbound_(ptr, ret, clos, pos, key);

	return 0;
}


/*
 *  (setf slot-value)
 */
static int setf_slot_value_call_(Execute ptr, addr pos, addr key, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	Return(getsetf_global_(call, &call));
	return callclang_funcall(ptr, &value, call, value, pos, key, NULL);
}

int setf_slot_value_using_class_common_(Execute ptr,
		addr clos, addr pos, addr key, addr value)
{
	addr slots, slot, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &check);
		if (key != check)
			continue;
		if (slot_instance_p(slot)) {
			/* :instance */
			GetValueClos(pos, &pos);
			SetClosValue(pos, i, value);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			Return(stdget_class_prototype_(pos, &pos));
			return setf_slot_value_call_(ptr, pos, key, value);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SETF, &check);
	return clos_slot_missing_(ptr, &pos, clos, pos, key, check, value);
}


/*
 *  initialize
 */
void init_clos_make(void)
{
	SetPointerCall(defun, var3, clos_ensure_reader);
	SetPointerCall(defun, var4, clos_ensure_writer_instance);
}

