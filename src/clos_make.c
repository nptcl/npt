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
_g void clos_ensure_class_supers(addr args, addr *ret, int *referp)
{
	addr list, pos;

	/* arguments */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SUPERCLASSES, &list)) {
		/* (list (find-class 'standard-object)) */
		GetConst(CLOS_STANDARD_OBJECT, &args);
		clos_find_class(args, &args);
		list_heap(ret, args, NULL);
		if (referp)
			*referp = 0;
		return;
	}

	/* check forward-referenced-class */
	*ret = list;
	while (list != Nil) {
		getcons(list, &pos, &list);
		if (clos_referenced_p(pos)) {
			if (referp) {
				*referp = 1;
			}
			else {
				fmte("Cannot have a forward-referenced-class ~S "
						"in the finalized class.", pos, NULL);
			}
			return;
		}
	}
	if (referp)
		*referp = 0;
}

static void clos_ensure_class_parse_slots(addr list, addr *ret)
{
	addr slot, name, readers, writers, alloc, args, form, func, type, doc;

	/* arguments */
	if (GetKeyArgs(list, CLOSKEY_NAME, &name))
		fmte("Invalid slot :name ~S.", name, NULL);
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
	slot_set_allocation(slot, alloc);

	/* result */
	*ret = slot;
}

_g void clos_ensure_class_slots(addr args, addr *ret)
{
	addr slots, pos;
	size_t size, i;

	/* :direct-slot list */
	if (GetKeyArgs(args, CLOSKEY_DIRECT_SLOTS, &args))
		args = Nil;

	/* slot-vector */
	size = length_list_safe(args);
	slot_vector_heap(&slots, size);
	for (i = 0; args != Nil; i++) {
		GetCons(args, &pos, &args);
		clos_ensure_class_parse_slots(pos, &pos);
		SetSlotVector(slots, i, pos);
	}
	*ret = slots;
}

_g void clos_ensure_class_direct_default_initargs(LocalRoot local,
		addr pos, addr args, addr *ret)
{
	addr check, list, key, a, b;
	LocalStack stack;

	if (GetKeyArgs(args, CLOSKEY_DIRECT_DEFAULT_INITARGS, &args)) {
		*ret = Nil;
		return;
	}
	/* check only */
	push_local(local, &stack);
	*ret = args;
	check = Nil;
	while (args != Nil) {
		getcons(args, &list, &args);
		/* (key initform initfunction) form */
		list_bind(list, &key, &a, &b, NULL);
		/* check duplicate */
		if (find_list_eq_unsafe(key, check))
			fmte(":INITARG ~S is already exist.", key, NULL);
		cons_local(local, &check, key, check);
	}
	rollback_local(local, stack);
}

static void clos_ensure_class_default_initargs(LocalRoot local, addr pos, addr *ret)
{
	addr root, check, list, args, init, key;
	LocalStack stack;

	stdget_class_precedence_list(pos, &list);
	root = check = Nil;
	push_local(local, &stack);
	while (list != Nil) {
		getcons(list, &args, &list);
		stdget_class_direct_default_initargs(args, &args);
		while (args != Nil) {
			getcons(args, &init, &args);
			getcar(init, &key);
			if (! find_list_eq_unsafe(key, check)) {
				cons_local(local, &check, key, check);
				cons_heap(&root, init, root);
			}
		}
	}
	rollback_local(local, stack);
	nreverse(ret, root);
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
		fmte("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	stdget_generic_method_combination(gen, &pos);
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		fmte("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	stdget_generic_lambda_list(gen, &pos);
	if (! argumentp(pos))
		argument_generic_heap(ptr->local, &pos, pos);
	if (ArgumentStruct(pos)->var != 1)
		fmte("The generic-function ~S must be a 1 specializer.", gen, NULL);
	
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
		fmte("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	stdget_generic_method_combination(gen, &pos);
	Return(qualifiers_position_nil_(ptr, Nil, pos, &index, &check));
	if (check)
		fmte("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	stdget_generic_lambda_list(gen, &pos);
	if (! argumentp(pos))
		argument_generic_heap(ptr->local, &pos, pos);
	if (ArgumentStruct(pos)->var != 2)
		fmte("The generic-function ~S must be a 2 specializers.", gen, NULL);
	
	return 0;
}

static int clos_ensure_readers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_reader_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_writers_check_(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getglobal_parse_callname(name, &gen);
		Return(clos_ensure_writer_method_check_(ptr, gen));
	}

	return 0;
}

static int clos_ensure_class_function_check_(Execute ptr, addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	stdget_class_slots(pos, &slots);
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
static void clos_ensure_reader_generic(addr name)
{
	addr lambda;
	mop_argument_generic_var1(&lambda);
	generic_empty(name, lambda, &name);
}

static void clos_ensure_writer_generic(addr name)
{
	addr lambda;
	mop_argument_generic_var2(&lambda);
	generic_empty(name, lambda, &name);
}

static void clos_ensure_readers_generic(addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		clos_ensure_reader_generic(name);
	}
}

static void clos_ensure_writers_generic(addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getglobal_parse_callname(name, &gen);
		if (gen != Unbound)
			continue;
		clos_ensure_writer_generic(name);
	}
}

static void clos_ensure_class_function_generic(addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	stdget_class_slots(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		clos_ensure_readers_generic(list);
		/* writer */
		GetWritersSlot(slot, &list);
		clos_ensure_writers_generic(list);
	}
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
	getfunction_global(call, &call);
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
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
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
	getsetf_global(call, &call);
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
	method_instance_lambda(ptr->local, &pos, Nil, pos);
	stdset_method_function(pos, call);
	return method_add_method_(ptr, gen, pos);
}

static int clos_ensure_readers_method_(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
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
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
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

	stdget_class_slots(pos, &slots);
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
	clos_ensure_class_function_generic(pos);
	/* make method */
	return clos_ensure_class_method_(ptr, pos);
}

static void clos_ensure_class_subclasses(addr pos)
{
	addr supers, super, list;

	stdget_class_direct_superclasses(pos, &supers);
	while (supers != Nil) {
		getcons(supers, &super, &supers);
		stdget_class_direct_subclasses(super, &list);
		pushnew_heap(list, pos, &list);
		stdset_class_direct_subclasses(super, list);
	}
}

_g void clos_ensure_class_init(LocalRoot local, addr pos, int pushp)
{
	addr value;

	/* class-precedence-list */
	clos_precedence_list(local, pos, &value);
	stdset_class_precedence_list(pos, value);
	/* effective-slots */
	clos_compute_slots(local, pos, &value);
	stdset_class_slots(pos, value);
	/* default-initargs */
	clos_ensure_class_default_initargs(local, pos, &value);
	stdset_class_default_initargs(pos, value);
	/* subclasses */
	if (pushp)
		clos_ensure_class_subclasses(pos);
}

static int clos_ensure_class_set(
		LocalRoot local, addr pos, addr name, addr args, int pushp)
{
	int referp;
	addr supers, slots, value;

	/* arguments */
	clos_ensure_class_supers(args, &supers, &referp);
	clos_ensure_class_slots(args, &slots);
	/* set value */
	stdset_class_name(pos, name);
	clos_stdclass_direct_slots(pos, slots);
	stdset_class_direct_superclasses(pos, supers);
	/* direct-default-initargs */
	clos_ensure_class_direct_default_initargs(local, pos, args, &value);
	stdset_class_direct_default_initargs(pos, value);
	/* forward-referenced-class */
	if (! referp) {
		clos_ensure_class_init(local, pos, pushp);
		return 1;
	}

	return 0;
}

_g int clos_finalize(Execute ptr, addr pos);
static int clos_finalize_forward(Execute ptr, addr pos, addr *ret)
{
	addr name;

	stdget_class_name(pos, &name);
	clos_find_class(name, &pos);
	if (clos_finalize(ptr, pos))
		return 1;
	*ret = pos;
	return 0;
}

static int clos_finalize_forward_p(Execute ptr, addr clos, int *ret)
{
	int result;
	addr list, root, pos;
	LocalRoot local;
	LocalStack stack;

	/* update */
	stdget_class_direct_superclasses(clos, &list);
	result = 0;
	local = ptr->local;
	push_local(local, &stack);
	for (root = Nil; list != Nil; ) {
		getcons(list, &pos, &list);
		if (clos_referenced_p(pos)) {
			if (clos_finalize_forward(ptr, pos, &pos))
				return 1;
			result = 1;
		}
		cons_local(local, &root, pos, root);
	}
	/* replace */
	if (result) {
		reverse_list_heap_unsafe(&root, root);
		stdset_class_direct_superclasses(clos, root);
	}
	rollback_local(local, stack);
	*ret = result;

	return 0;
}

_g int clos_finalize(Execute ptr, addr pos)
{
	int check;
	addr list, value;

	/* finalized check */
	stdget_class_finalized_p(pos, &value);
	if (value != Nil)
		return 0;

	/* referenced class */
	if (clos_finalize_forward_p(ptr, pos, &check))
		return 1;
	if (check) {
		/* make class */
		clos_ensure_class_init(ptr->local, pos, 1);
	}

	/* superclasses */
	stdget_class_direct_superclasses(pos, &list);
	while (list != Nil) {
		GetCons(list, &value, &list);
		if (clos_finalize(ptr, value))
			return 1;
	}

	/* prototype */
	clos_stdclass_prototype(pos);
	Return(clos_ensure_class_function_(ptr, pos));
	stdset_class_finalized_p(pos, T);

	return 0;
}

static int clos_ensure_class_object(Execute ptr, addr name, addr args, addr *ret)
{
	addr metaclass, pos;
	LocalRoot local;

	/* :metaclass ... */
	if (GetKeyArgs(args, CLOSKEY_METACLASS, &metaclass))
		GetConst(CLOS_STANDARD_CLASS, &metaclass);
	local = ptr->local;
	GetConst(CLOSKEY_METACLASS, &pos);
	(void)remplist_local(local, args, pos, &args);

	/* (apply #'make-instance metaclass args) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	getfunction_global(pos, &pos);
	return callclang_applya(ptr, ret, pos, metaclass, args, NULL);
}

_g int clos_ensure_class(Execute ptr, addr name, addr args, addr *ret)
{
	addr pos;

	/* make-instance */
	if (clos_ensure_class_object(ptr, name, args, &pos))
		return 1;

	/* define class */
	clos_ensure_class_set(ptr->local, pos, name, args, 1);
	clos_define_class(name, pos);
	*ret = pos;

	return 0;
}


/*
 *  allocate-initialize
 */
_g void allocate_instance_stdclass(Execute ptr, addr clos, addr *ret)
{
	addr instance, slots, slot, name;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* finalized */
	if (clos_finalize(ptr, clos))
		fmte("Cannot finalize class object ~S.", clos, NULL);

	/* allocate */
	stdget_class_slots(clos, &slots);
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
		if (! symbolp(name))
			fmte("The slot name ~S must be a symbol.", name, NULL);
		/* already exist */
		if (clos_find_slotname(slots, i, name))
			fmte("The slot name ~S already exists.", name, NULL);
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i)
			fmte("The slot location ~A is invalid.", intsizeh(i), NULL);
	}
	*ret = instance;
}


/*
 *  initialize-instance
 *  reinitialize-instance
 */
static int initialize_instance(Execute ptr, addr pos, addr type, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance type initargs) */
	addr call;

	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunction_global(call, &call);
	return callclang_applya(ptr, ret, call, pos, type, rest, NULL);
}

_g int initialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance T initargs) */
	return initialize_instance(ptr, pos, T, rest, ret);
}

_g int reinitialize_instance_stdobject(Execute ptr, addr pos, addr rest, addr *ret)
{
	/* (apply #'shared-initialize instance () initargs) */
	return initialize_instance(ptr, pos, Nil, rest, ret);
}


/*
 *  shared-initialize
 */
static int setf_slot_value_call(Execute ptr, addr pos, addr key, addr value);
static int shared_initialize_arguments(Execute ptr,
		addr pos, addr slot, addr rest, int *ret)
{
	addr list, key, value;

	GetArgsSlot(slot, &list);
	while (list != Nil) {
		getcons(list, &key, &list);
		if (getplist_safe(rest, key, &value))
			continue;
		GetNameSlot(slot, &key);
		if (setf_slot_value_call(ptr, pos, key, value))
			return 1;
		*ret = 1;
		return 0;
	}
	*ret = 0;
	return 0;
}

static int slot_boundp_call(Execute ptr, addr pos, addr key, int *ret);
static int shared_initialize_initform(Execute ptr, addr pos, addr key, addr slot)
{
	int check;
	addr value;

	/* boundp */
	if (slot_boundp_call(ptr, pos, key, &check))
		return 1;
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
		if (callclang_funcall(ptr, &value, value, NULL))
			return 1;
	}

	/* (setf slot-value) */
	return setf_slot_value_call(ptr, pos, key, value);
}

_g int shared_initialize_stdobject(Execute ptr, addr pos, addr name, addr rest)
{
	int check;
	addr slots, slot, key;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* initialize arguments */
		if (shared_initialize_arguments(ptr, pos, slot, rest, &check))
			return 1;
		if (check)
			continue;
		/* initform */
		GetNameSlot(slot, &key);
		if ((name != T) && (! find_list_eq_safe(key, name)))
			continue;
		if (shared_initialize_initform(ptr, pos, key, slot))
			return 1;
	}

	return 0;
}


/*
 *  make-instance
 */
static int make_instance_initargs(Execute ptr, addr clos, addr rest, addr *ret)
{
	addr list, root, keys, key, slots, temp, value, call;
	LocalRoot local;

	local = ptr->local;
	stdget_class_default_initargs(clos, &list);
	if (list == Nil) {
		cons_local(local, ret, clos, rest);
		return 0;
	}

	/* rest */
	root = keys = Nil;
	while (rest != Nil) {
		getcons(rest, &key, &rest);
		getcons(rest, &value, &rest);
		if (! find_list_eq_unsafe(key, keys)) {
			cons_local(local, &keys, key, keys);
			cons_local(local, &root, key, root);
			cons_local(local, &root, value, root);
		}
	}

	/* default-initargs */
	stdget_class_slots(clos, &slots);
	for (root = Nil; list != Nil; ) {
		getcons(list, &temp, &list);
		list_bind(temp, &key, &value, &call, NULL);
		if (! find_list_eq_unsafe(key, keys)) {
			if (call != Nil) {
				if (callclang_funcall(ptr, &value, call, NULL))
					return 1;
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

static void make_instance_check(Execute ptr, addr clos, addr rest)
{
	int check;
	addr slots, slot, key, value;
	size_t size, i;

	stdget_class_slots(clos, &slots);
	LenSlotVector(slots, &size);
	getcdr(rest, &rest);
	while (rest != Nil) {
		getcons(rest, &key, &rest);
		getcdr(rest, &rest);
		check = 0;
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &slot);
			GetArgsSlot(slot, &value);
			check |= find_list_eq_safe(key, value);
		}
		if (! check) {
			fmte("The initialize argument ~S don't exist in ~S slots.",
					key, clos, NULL);
		}
	}
}

_g int make_instance_stdclass(Execute ptr, addr rest, addr *ret)
{
	addr clos, call, instance;

	/* finalize */
	GetCons(rest, &clos, &rest);
	if (clos_finalize(ptr, clos))
		fmte("Cannot finalize class object ~S.", clos, NULL);

	/* initargs */
	if (make_instance_initargs(ptr, clos, rest, &rest))
		return 1;
	make_instance_check(ptr, clos, rest);

	/* allocation-instance */
	GetConst(COMMON_ALLOCATE_INSTANCE, &call);
	getfunction_global(call, &call);
	if (callclang_apply(ptr, &instance, call, rest))
		return 1;

	/* initialize-instance */
	GetCdr(rest, &rest);
	cons_local(ptr->local, &rest, instance, rest);
	GetConst(COMMON_INITIALIZE_INSTANCE, &call);
	getfunction_global(call, &call);
	if (callclang_apply(ptr, &call, call, rest))
		return 1;

	/* result */
	*ret = instance;
	return 0;
}


/*
 *  slot-missing
 */
_g int clos_slot_missing(Execute ptr,
		addr clos, addr pos, addr name, addr operation, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_MISSING, &call);
	getfunction_global(call, &call);
	return funcall_control(ptr, call, clos, pos, name, operation, value, NULL);
}

_g int clos_slot_unbound(Execute ptr, addr clos, addr pos, addr name)
{
	addr call;

	GetConst(COMMON_SLOT_UNBOUND, &call);
	getfunction_global(call, &call);
	return funcall_control(ptr, call, clos, pos, name, NULL);
}


/*
 *  slot-boundp
 */
static int slot_boundp_call(Execute ptr, addr pos, addr key, int *ret)
{
	addr call;

	GetConst(COMMON_SLOT_BOUNDP, &call);
	getfunction_global(call, &call);
	if (callclang_funcall(ptr, &pos, call, pos, key, NULL))
		return 1;
	*ret = (pos != Nil);
	return 0;
}

_g int slot_boundp_using_class_common(Execute ptr,
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
			*ret = (pos != Unbound);
			return 0;
		}
		else {
			/* :class */
			GetClassSlot(slot, &pos);
			stdget_class_prototype(pos, &pos);
			return slot_boundp_call(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_BOUNDP, &check);
	return clos_slot_missing(ptr, clos, pos, key, check, Unbound);
}


/*
 *  slot-makunbound
 */
static int slot_makunbound_call(Execute ptr, addr pos, addr key)
{
	addr call;

	GetConst(COMMON_SLOT_MAKUNBOUND, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, &pos, call, pos, key, NULL);
}

_g int slot_makunbound_using_class(Execute ptr, addr clos, addr pos, addr key)
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
			stdget_class_prototype(pos, &pos);
			return slot_makunbound_call(ptr, pos, key);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_MAKUNBOUND, &check);
	return clos_slot_missing(ptr, clos, pos, key, check, Unbound);
}


/*
 *  slot-value
 */
static int slot_value_call(Execute ptr, addr pos, addr key, addr *ret)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, ret, call, pos, key, NULL);
}

static int slot_value_using_class_getp(Execute ptr,
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
			stdget_class_prototype(pos, &pos);
			return slot_value_call(ptr, pos, key, ret);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SLOT_VALUE, &check);
	return clos_slot_missing(ptr, clos, pos, key, check, Unbound);
}

_g int slot_value_using_class_common(Execute ptr,
		addr clos, addr pos, addr key, addr *ret)
{
	if (slot_value_using_class_getp(ptr, clos, pos, key, ret))
		return 1;
	if (*ret == Unbound)
		return clos_slot_unbound(ptr, clos, pos, key);

	return 0;
}


/*
 *  (setf slot-value)
 */
static int setf_slot_value_call(Execute ptr, addr pos, addr key, addr value)
{
	addr call;

	GetConst(COMMON_SLOT_VALUE, &call);
	getsetf_global(call, &call);
	return callclang_funcall(ptr, &value, call, value, pos, key, NULL);
}

_g int setf_slot_value_using_class_common(Execute ptr,
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
			stdget_class_prototype(pos, &pos);
			return setf_slot_value_call(ptr, pos, key, value);
		}
	}

	/* slot-missing */
	GetConst(COMMON_SETF, &check);
	return clos_slot_missing(ptr, clos, pos, key, check, value);
}


/*
 *  initialize
 */
_g void init_clos_make(void)
{
	SetPointerCall(defun, var3, clos_ensure_reader);
	SetPointerCall(defun, var4, clos_ensure_writer_instance);
}

