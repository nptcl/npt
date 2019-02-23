#include "clos_object.h"
#include "clos_standard.h"
#include "clos_type.h"
#include "cons.h"
#include "condition.h"
#include "constant.h"
#include "object.h"
#include "package.h"
#include "sequence.h"
#include <stdarg.h>

typedef addr (*clos_class_call)(Execute, addr);

/*****************************************************************************
 *  class-precedence-list
 *****************************************************************************/
static void call_class_direct_superclasses(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	if (call) {
		*ret = call(ptr, pos);
	}
	else {
		clos_elt(pos, Clos_class_direct_superclasses, ret);
	}
}

static void direct_superclasses_list(Execute ptr,
		clos_class_call call, addr right, addr *ret)
{
	addr left, cons;
	LocalRoot local;

	local = ptr->local;
	conscar_local(local, &cons, right);
	call_class_direct_superclasses(ptr, call, right, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		cons_local(local, &cons, left, cons);
	}
	cons_local(local, &cons, Unbound, cons);
	nreverse_list_unsafe(ret, cons);
}

static void direct_superclasses_chain(Execute ptr,
		clos_class_call call, addr right, addr *ret)
{
	addr child, result, cons, left, temp;
	LocalRoot local;

	local = ptr->local;
	direct_superclasses_list(ptr, call, right, &right);
	child = result = Nil;
	while (right != Nil) {
		GetCons(right, &left, &right);
		if (child == Nil) {
			conscdr_local(local, &child, left);
		}
		else {
			GetCdr(child, &temp);
			SetCar(child, temp);
			SetCdr(child, left);
			GetCons(child, &left, &temp);
			cons_local(local, &cons, left, temp);
			cons_local(local, &result, cons, result);
		}
	}
	*ret = result;
}

static void all_superclass_list(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	addr stack, result, temp, supers, super;
	LocalRoot local;

	local = ptr->local;
	/* first list */
	result = Nil;
	conscar_local(local, &stack, pos);

	/* loop */
	while (stack != Nil) {
		for (temp = Nil; stack != Nil; ) {
			GetCons(stack, &supers, &stack);
			if (! find_list_eq_unsafe(supers, result)) {
				cons_local(local, &result, supers, result);
				call_class_direct_superclasses(ptr, call, supers, &supers);
				while (supers != Nil) {
					GetCons(supers, &super, &supers);
					/* pushnew */
					if (! find_list_eq_unsafe(super, temp))
						cons_local(local, &temp, super, temp);
				}
			}
		}
		nreverse_list_unsafe(&stack, temp);
	}
	nreverse_list_unsafe(ret, result);
}

static int find_chain_cons(addr a1, addr cons)
{
	addr b1, a2, b2;

	Check(GetType(a1) != LISPTYPE_CONS, "type key error");
	GetCons(a1, &a1, &b1);
	while (cons != Nil) {
		GetCons(cons, &a2, &cons);
		GetCons(a2, &a2, &b2);
		if ((a1 == a2) && (b1 == b2)) return 1;
	}

	return 0;
}

static void superclasses_chain(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	addr one, chain, result;
	LocalRoot local;

	local = ptr->local;
	all_superclass_list(ptr, call, pos, &pos);
	for (result = Nil; pos != Nil; ) {
		GetCons(pos, &one, &pos);
		direct_superclasses_chain(ptr, call, one, &one);
		while (one != Nil) {
			GetCons(one, &chain, &one);
			/* pushnew */
			if (! find_chain_cons(chain, result))
				cons_local(local, &result, chain, result);
		}
	}
	*ret = result;
}

static void find_top_superclass(addr cons, addr *ret)
{
	int check;
	addr left, right, car, cdr;

	for (right = cons; right != Nil; ) {
		GetCons(right, &left, &right);
		GetCar(left, &left);
		for (check = 0, cdr = cons; cdr != Nil; ) {
			GetCons(cdr, &car, &cdr);
			GetCdr(car, &car);
			if (car == left) {
				check = 1;
				break;
			}
		}
		if (check == 0) {
			*ret = left;
			return;
		}
	}
	*ret = 0;
	fmte("Cannot make class precedence list. Perhaps, class inherit is loop.", NULL);
}

static void remove_top_superclass(addr key, addr right2, addr *ret)
{
	int check;
	addr left, right1, right3, result;

	check = 0;
	result = right1 = right3 = Nil;
	while (right2 != Nil) {
		GetCons(right2, &left, &right3);
		GetCar(left, &left);
		if (left == key) {
			check = 1;
			if (right1 != Nil) {
				SetCdr(right1, right3);
			}
			right2 = right3;
			continue;
		}
		if (result == Nil)
			result = right2;
		right1 = right2;
		right2 = right3;
	}
	if (check == 0)
			fmte("Cannot make class precedence list. Class key is not found.", NULL);
	*ret = result;
}

static void result_class_precedence_list(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	addr result, key;

	superclasses_chain(ptr, call, pos, &pos);
	result = Nil;
	while (pos != Nil) {
		find_top_superclass(pos, &key);
		cons_heap(&result, key, result);
		remove_top_superclass(key, pos, &pos);
	}
	nreverse_list_unsafe(ret, result);
}

void call_compute_class_precedence_list(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	result_class_precedence_list(ptr, call, pos, ret);
	rollback_local(local, stack);
}

void std_compute_class_precedence_list(Execute ptr, addr pos, addr *ret)
{
	call_compute_class_precedence_list(ptr, NULL, pos, ret);
}


/*****************************************************************************
 *  compute-slots
 *****************************************************************************/
static void call_class_precedence_list(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	if (call) {
		*ret = call(ptr, pos);
	}
	else {
		clos_elt(pos, Clos_class_precedence_list, ret);
	}
}

static void call_class_direct_slots(Execute ptr,
		clos_class_call call, addr pos, addr *ret)
{
	if (call) {
		*ret = call(ptr, pos);
	}
	else {
		clos_elt(pos, Clos_class_direct_slots, ret);
	}
}

static void member_slotname(addr *ret, addr slot, addr right)
{
	addr left, next;

	GetSlot(slot, SLOT_INDEX_NAME, &slot);
	for (; right != Nil; right = next) {
		GetCons(right, &left, &next);
		GetSlot(left, SLOT_INDEX_NAME, &left);
		if (slot == left) {
			*ret = right;
			return;
		}
	}
	*ret = Nil;
}

static void gather_loop(Execute ptr, addr classes,
		clos_class_call call_slots, addr *retcons, size_t *retsize)
{
	addr result, super, slots, slot, cons;
	size_t size, index, slotsize;

	for (result = Nil, size = 0; classes != Nil; ) {
		GetCons(classes, &super, &classes);
		call_class_direct_slots(ptr, call_slots, super, &slots);
		LenArrayA4(slots, &slotsize);
		for (index = 0; index < slotsize; index++) {
			GetArrayA4(slots, index, &slot);
			member_slotname(&cons, slot, result);
			if (cons != Nil) {
				SetCar(cons, slot);
			}
			else {
				cons_heap(&result, slot, result);
				size++;
			}
		}
	}
	nreverse_list_unsafe(retcons, result);
	*retsize = size;
}

static void gather_slots_heap(Execute ptr,
		clos_class_call call_precedence,
		clos_class_call call_slots,
		addr clos,
		addr *retcons,
		size_t *retsize)
{
	addr classes;
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	call_class_precedence_list(ptr, call_precedence, clos, &classes);
	reverse_list_local_unsafe(local, &classes, classes);
	gather_loop(ptr, classes, call_slots, retcons, retsize);
	rollback_local(local, stack);
}

void std_compute_slots_heap(Execute ptr, addr clos, addr *ret)
{
	addr cons, slots, slot, next, pos;
	size_t size, index;

	gather_slots_heap(ptr, NULL, NULL, clos, &cons, &size);
	index = 0;
	for (slots = cons; slots != Nil; slots = next) {
		GetCons(slots, &slot, &next);
		slot_copy_heap(&slot, slot);
		SetCar(slots, slot);
		index_heap(&pos, index);
		SetSlot(slot, SLOT_INDEX_LOCATION, pos);
		index++;
	}
	*ret = cons;
}

void std_compute_slots_vector_heap(Execute ptr, addr clos, addr *ret)
{
	addr cons, slots, slot, next, pos, result;
	size_t size, index;

	gather_slots_heap(ptr, NULL, NULL, clos, &cons, &size);
	vector4_heap(&result, size);
	index = 0;
	for (slots = cons; slots != Nil; slots = next) {
		GetCons(slots, &slot, &next);
		slot_copy_heap(&slot, slot);
		SetArrayA4(result, index, slot);
		index_heap(&pos, index);
		SetSlot(slot, SLOT_INDEX_LOCATION, pos);
		index++;
	}
	*ret = result;
}


/*****************************************************************************
 *  build-standard-class
 *****************************************************************************/
/*
 *  make slots
 */
static void default_slot_nametype(addr pos, int index, enum CONSTANT_INDEX cons)
{
	addr slot, value;

	slot_heap(&slot);
	GetConstant(cons, &value);
	SetSlot(slot, SLOT_INDEX_NAME, value);
	GetConst(COMMON_SYMBOL, &value);
	SetSlot(slot, SLOT_INDEX_TYPE, value);
	SetArrayA4(pos, index, slot);
}

void default_slot_name(addr pos, int index, enum CONSTANT_INDEX cons)
{
	addr slot, value;

	slot_heap(&slot);
	GetConstant(cons, &value);
	SetSlot(slot, SLOT_INDEX_NAME, value);
	SetArrayA4(pos, index, slot);
}

static void default_slot_initform(addr pos,
		int index, enum CONSTANT_INDEX cons, addr initform)
{
	addr slot, value;

	slot_heap(&slot);
	GetConstant(cons, &value);
	SetSlot(slot, SLOT_INDEX_NAME, value);
	SetSlot(slot, SLOT_INDEX_INITFORM, initform);
	SetArrayA4(pos, index, slot);
}

void set_slots_localtion(addr slots)
{
	addr location, slot;
	size_t size, i;

	LenArrayA4(slots, &size);
	for (i = 0; i < size; i++) {
		index_heap(&location, i);
		GetArrayA4(slots, i, &slot);
		SetSlot(slot, SLOT_INDEX_LOCATION, location);
	}
}

static void make_standard_class_slots(addr *ret)
{
	addr slots, pos;

	vector4_heap(&slots, Clos_class_size);
	default_slot_nametype(slots,
			Clos_class_name,
			CONSTANT_CLOSNAME_NAME);
	default_slot_name(slots,
			Clos_class_direct_slots,
			CONSTANT_CLOSNAME_DIRECT_SLOTS);
	default_slot_initform(slots,
			Clos_class_direct_subclasses,
			CONSTANT_CLOSNAME_DIRECT_SUBCLASSES,
			Nil);
	default_slot_name(slots,
			Clos_class_direct_superclasses,
			CONSTANT_CLOSNAME_DIRECT_SUPERCLASSES);
	default_slot_name(slots,
			Clos_class_precedence_list,
			CONSTANT_CLOSNAME_CLASS_PRECEDENCE_LIST);
	default_slot_name(slots,
			Clos_class_slots,
			CONSTANT_CLOSNAME_EFFECTIVE_SLOTS);
	default_slot_initform(slots,
			Clos_class_finalized_p,
			CONSTANT_CLOSNAME_FINALIZED_P,
			Nil);
	default_slot_name(slots,
			Clos_class_prototype,
			CONSTANT_CLOSNAME_PROTOTYPE);
	default_slot_initform(slots,
			Clos_class_direct_methods,
			CONSTANT_CLOSNAME_DIRECT_METHODS,
			Nil);
	default_slot_initform(slots,
			Clos_class_direct_shared,
			CONSTANT_CLOSNAME_DIRECT_SHARED,
			Nil);
	default_slot_initform(slots,
			Clos_class_default_initargs,
			CONSTANT_CLOSNAME_DEFAULT_INITARGS,
			Nil);
	default_slot_initform(slots,
			Clos_class_direct_default_initargs,
			CONSTANT_CLOSNAME_DIRECT_DEFAULT_INITARGS,
			Nil);
	fixnum_heap(&pos, 0);
	default_slot_initform(slots,
			Clos_class_version,
			CONSTANT_CLOSNAME_VERSION,
			pos);
	default_slot_name(slots,
			Clos_class_update_info,
			CONSTANT_CLOSNAME_UPDATE_INFO);
	default_slot_initform(slots,
			Clos_class_document,
			CONSTANT_CLOSNAME_DOCUMENT,
			Nil);

	set_slots_localtion(slots);
	*ret = slots;
}

static void dummy_standard_class(LocalRoot local, addr *ret, addr slots)
{
	addr instance;

	clos_alloc(local, &instance, slots);
	setf_clos_elt(instance, Clos_class_direct_slots, slots);
	setf_clos_elt(instance, Clos_class_slots, slots);
	*ret = instance;
}

static void make_class_slots(addr *ret, addr clos, addr name, addr slots)
{
	addr instance;

	Check(! IsSymbol(name), "type error");
	make_instance_restrict_heap(clos, &instance);
	setf_clos_elt(instance, Clos_class_name, name);
	setf_clos_elt(instance, Clos_class_direct_slots, slots);
	setf_clos_elt(instance, Clos_class_prototype, instance);
	setf_clos_elt(instance, Clos_class_finalized_p, T);
	*ret = instance;
}

static void make_direct_slots_empty(addr *ret, addr clos, addr name)
{
	addr slots;
	vector4_heap(&slots, 0);
	make_class_slots(ret, clos, name, slots);
}

void std_update_class_of(addr instance, addr class_of)
{
	setf_clos_class_of(instance, class_of);
	clos_elt(class_of, Clos_class_version, &class_of);
	setf_clos_version(instance, class_of);
}

static void set_inheritance(Execute ptr, addr instance, addr clos, addr supers)
{
	addr cons, super;

	/* class-of */
	std_update_class_of(instance, clos);
	/* direct-superclasses */
	setf_clos_elt(instance, Clos_class_direct_superclasses, supers);
	/* class-precedence-list */
	std_compute_class_precedence_list(ptr, instance, &cons);
	setf_clos_elt(instance, Clos_class_precedence_list, cons);
	/* effective-slots */
	std_compute_slots_vector_heap(ptr, instance, &cons);
	setf_clos_elt(instance, Clos_class_slots, cons);
	/* direct-subclasses */
	while (supers != Nil) {
		GetCons(supers, &super, &supers);
		clos_elt(super, Clos_class_direct_subclasses, &cons);
		cons_heap(&cons, instance, cons);
		setf_clos_elt(super, Clos_class_direct_subclasses, cons);
	}
	/* setf-find-class */
	clos_elt(instance, Clos_class_name, &cons);
	setf_find_class(cons, instance);
}

static void set_inheritance_single(Execute ptr,
		addr instance, addr clos, addr super)
{
	conscar_heap(&super, super);
	set_inheritance(ptr, instance, clos, super);
}

static void make_standard_class(Execute ptr, addr *ret)
{
	addr slots, dummy, name;
	addr tclass, object, classclass, metaclass, bclass;
	LocalRoot local;
	LocalStack stack;

	/* make dummy metaclass */
	make_standard_class_slots(&slots);
	local = ptr->local;
	push_local(local, &stack);
	dummy_standard_class(NULL, &dummy, slots);
	/* make class class */
	GetConst(COMMON_CLASS, &name);
	make_class_slots(&classclass, dummy, name, slots);
	set_inheritance(ptr, classclass, classclass, Nil);
	rollback_local(local, stack);
	/* make other class */
	make_direct_slots_empty(&tclass, classclass, T);
	set_inheritance(ptr, tclass, classclass, Nil);
	GetConst(COMMON_STANDARD_OBJECT, &name);
	make_direct_slots_empty(&object, classclass, name);
	set_inheritance_single(ptr, object, classclass, tclass);
	set_inheritance_single(ptr, classclass, classclass, object);
	/* make standard-class */
	GetConst(COMMON_STANDARD_CLASS, &name);
	make_direct_slots_empty(&metaclass, classclass, name);
	set_inheritance_single(ptr, metaclass, metaclass, classclass);
	/* make built-in-class */
	GetConst(COMMON_BUILT_IN_CLASS, &name);
	make_direct_slots_empty(&bclass, classclass, name);
	set_inheritance_single(ptr, bclass, metaclass, classclass);

	/* update class-of */
	std_update_class_of(tclass, bclass);
	std_update_class_of(object, metaclass);
	std_update_class_of(classclass, metaclass);
	std_update_class_of(metaclass, metaclass);
	std_update_class_of(bclass, metaclass);
	*ret = metaclass;
}

static void make_type_class_slots(Execute ptr,
		addr *ret, addr metaclass, addr name, addr slots, addr supers)
{
	addr instance;

	make_class_slots(&instance, metaclass, name, slots);
	set_inheritance(ptr, instance, metaclass, supers);
	*ret = instance;
}

void make_type_class(Execute ptr,
		addr *ret, addr metaclass, addr name, addr supers)
{
	addr slots;
	vector4_heap(&slots, 0);
	make_type_class_slots(ptr, ret, metaclass, name, slots, supers);
}

void make_type_class_constant(Execute ptr,
		addr metaclass, enum CONSTANT_INDEX index, ...)
{
	va_list args;
	addr supers, clos, name;
	enum CONSTANT_INDEX arg;

	/* supers */
	supers = Nil;
	va_start(args, index);
	for (;;) {
		arg = va_arg(args, enum CONSTANT_INDEX);
		if (arg == CONSTANT_EMPTY) break;
		GetConstant(arg, &clos);
		clos = find_class(clos);
		cons_heap(&supers, clos, supers);
	}
	va_end(args);
	nreverse_list_unsafe(&supers, supers);

	/* make class */
	GetConstant(index, &name);
	make_type_class(ptr, &clos, metaclass, name, supers);
	setf_find_class(name, clos);
}

void make_type_class_slots_constant(Execute ptr,
		addr metaclass, addr slots,
		enum CONSTANT_INDEX index_name,
		enum CONSTANT_INDEX index_super)
{
	addr name, supers, clos;

	GetConstant(index_name, &name);
	GetConstant(index_super, &supers);
	conscar_heap(&supers, find_class(supers));
	make_type_class_slots(ptr, &clos, metaclass, name, slots, supers);
	setf_find_class(name, clos);
}

static void build_standard_class_call(Execute ptr)
{
	addr metaclass, structure;

	make_standard_class(ptr, &metaclass);
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_STRUCTURE_CLASS,
			CONSTANT_COMMON_CLASS,
			CONSTANT_EMPTY);
	GetConst(COMMON_STRUCTURE_CLASS, &structure);
	structure = find_class(structure);
	make_type_class_constant(ptr, structure,
			CONSTANT_COMMON_STRUCTURE_OBJECT,
			CONSTANT_COMMON_T,
			CONSTANT_EMPTY);
	make_type_class_constant(ptr, metaclass,
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CONSTANT_COMMON_STANDARD_OBJECT,
			CONSTANT_EMPTY);
	make_type_class_constant(ptr, metaclass,
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_CLASS,
			CONSTANT_COMMON_CLASS,
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CONSTANT_EMPTY);
}

static void build_standard_class_execute(Execute ptr)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	build_standard_class_call(ptr);
	rollback_local(local, stack);
}

void find_class_constant(enum CONSTANT_INDEX index, enum CONSTANT_INDEX clos)
{
	addr pos;
	GetConstant(index, &pos);
	pos = find_class(pos);
	SetConstant(clos, pos);
}

void build_standard_class(Execute ptr)
{
	addr pos;

	GetConst(COMMON_STANDARD_CLASS, &pos);
	pos = find_class_nil(pos);
	if (pos != Nil)
		fmte("The standard-class is already exist.", NULL);
	build_standard_class_execute(ptr);
	find_class_constant(CONSTANT_COMMON_STANDARD_CLASS, CONSTANT_CLOS_STANDARD_CLASS);
	find_class_constant(CONSTANT_COMMON_CLASS, CONSTANT_CLOS_CLASS);
}

void getdocument_standard_class(addr instance, addr *ret)
{
	class_elt(instance, Clos_class_document, ret);
}

void setdocument_standard_class(addr instance, addr value)
{
	setf_class_elt(instance, Clos_class_document, value);
}

int classp(addr clos)
{
	addr class_of;

	if (! closp(clos)) return 0;
	GetConst(CLOS_CLASS, &class_of);
	return std_subtype_p(clos, class_of);
}


/*****************************************************************************
 *  standard-generic-function
 *****************************************************************************/
static void make_standard_generic_function_slots(addr *ret)
{
	addr slots;

	vector4_heap(&slots, Clos_generic_size);
	default_slot_nametype(slots,
			Clos_generic_name,
			CONSTANT_CLOSNAME_NAME);
	default_slot_name(slots,
			Clos_generic_lambda_list,
			CONSTANT_CLOSNAME_LAMBDA_LIST);
	default_slot_initform(slots,
			Clos_generic_methods,
			CONSTANT_CLOSNAME_METHODS,
			Nil);
	default_slot_name(slots,
			Clos_generic_method_class,
			CONSTANT_CLOSNAME_METHOD_CLASS);
	default_slot_name(slots,
			Clos_generic_argument_precedence_order,
			CONSTANT_CLOSNAME_ARGUMENT_PRECEDENCE_ORDER);
	default_slot_initform(slots,
			Clos_generic_declarations,
			CONSTANT_CLOSNAME_DECLARATIONS,
			Nil);
	default_slot_initform(slots,
			Clos_generic_method_combination,
			CONSTANT_CLOSNAME_METHOD_COMBINATION,
			Nil);
	default_slot_initform(slots,
			Clos_generic_combination_arguments,
			CONSTANT_CLOSNAME_COMBINATION_ARGUMENTS,
			Nil);
	default_slot_name(slots,
			Clos_generic_eqlcheck,
			CONSTANT_CLOSNAME_EQLCHECK);
	default_slot_name(slots,
			Clos_generic_cache,
			CONSTANT_CLOSNAME_CACHE);
	default_slot_name(slots,
			Clos_generic_call,
			CONSTANT_CLOSNAME_CALL);
	set_slots_localtion(slots);
	*ret = slots;
}

static void build_standard_generic_function_call(Execute ptr)
{
	addr metaclass, built, slots;

	GetConst(COMMON_BUILT_IN_CLASS, &built);
	built = find_class(built);
	GetConst(COMMON_STANDARD_CLASS, &metaclass);
	metaclass = find_class(metaclass);
	make_standard_generic_function_slots(&slots);

	make_type_class_constant(ptr, built,
			CONSTANT_COMMON_FUNCTION,
			CONSTANT_COMMON_T,
			CONSTANT_EMPTY);
	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_GENERIC_FUNCTION,
			CONSTANT_COMMON_FUNCTION,
			CONSTANT_CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CONSTANT_EMPTY);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION,
			CONSTANT_COMMON_GENERIC_FUNCTION);
}

static void build_standard_generic_function_execute(Execute ptr)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	build_standard_generic_function_call(ptr);
	rollback_local(local, stack);
}

void build_standard_generic_function(Execute ptr)
{
	addr pos;

	GetConst(COMMON_STANDARD_GENERIC_FUNCTION, &pos);
	pos = find_class_nil(pos);
	if (pos != Nil)
		fmte("The standard-generic-function is already exist.", NULL);
	build_standard_generic_function_execute(ptr);
	find_class_constant(CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION,
			CONSTANT_CLOS_STANDARD_GENERIC_FUNCTION);
}


/*****************************************************************************
 *  standard-method
 *****************************************************************************/
static void make_standard_method_slots(addr *ret)
{
	addr slots;

	vector4_heap(&slots, Clos_method_size);
	default_slot_name(slots,
			Clos_method_function,
			CONSTANT_CLOSNAME_FUNCTION);
	default_slot_name(slots,
			Clos_method_generic_function,
			CONSTANT_CLOSNAME_GENERIC_FUNCTION);
	default_slot_name(slots,
			Clos_method_lambda_list,
			CONSTANT_CLOSNAME_LAMBDA_LIST);
	default_slot_name(slots,
			Clos_method_lambda_parse,
			CONSTANT_CLOSNAME_LAMBDA_PARSE);
	default_slot_name(slots,
			Clos_method_qualifiers,
			CONSTANT_CLOSNAME_QUALIFIERS);
	default_slot_name(slots,
			Clos_method_specializers,
			CONSTANT_CLOSNAME_SPECIALIZERS);
	set_slots_localtion(slots);
	*ret = slots;
}

static void build_standard_method_call(Execute ptr)
{
	addr metaclass, slots;

	GetConst(COMMON_STANDARD_CLASS, &metaclass);
	metaclass = find_class(metaclass);
	make_standard_method_slots(&slots);

	make_type_class_constant(ptr, metaclass,
			CONSTANT_COMMON_METHOD,
			CONSTANT_COMMON_STANDARD_OBJECT,
			CONSTANT_EMPTY);
	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_STANDARD_METHOD,
			CONSTANT_COMMON_METHOD);
}

static void build_standard_method_execute(Execute ptr)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	build_standard_method_call(ptr);
	rollback_local(local, stack);
}

void build_standard_method(Execute ptr)
{
	addr pos;

	GetConst(COMMON_STANDARD_METHOD, &pos);
	pos = find_class_nil(pos);
	if (pos != Nil)
		fmte("The standard-method is already exist.", NULL);
	build_standard_method_execute(ptr);
}


/*****************************************************************************
 *  standard method-combination
 *****************************************************************************/
static void make_method_combination_slots(addr *ret)
{
	addr slots;

	vector4_heap(&slots, Clos_combination_size);
	default_slot_nametype(slots,
			Clos_combination_name,
			CONSTANT_CLOSNAME_NAME);
	default_slot_name(slots,
			Clos_combination_long_p,
			CONSTANT_CLOSNAME_LONG_P);
	default_slot_name(slots,
			Clos_combination_document,
			CONSTANT_CLOSNAME_DOCUMENT);
	default_slot_name(slots,
			Clos_combination_identity,
			CONSTANT_CLOSNAME_IDENTITY);
	default_slot_name(slots,
			Clos_combination_operator,
			CONSTANT_CLOSNAME_OPERATOR);
	default_slot_name(slots,
			Clos_combination_lambda_list,
			CONSTANT_CLOSNAME_LAMBDA_LIST);
	default_slot_name(slots,
			Clos_combination_qualifiers,
			CONSTANT_CLOSNAME_QUALIFIERS);
	default_slot_name(slots,
			Clos_combination_arguments,
			CONSTANT_CLOSNAME_ARGUMENTS);
	default_slot_name(slots,
			Clos_combination_generic,
			CONSTANT_CLOSNAME_GENERIC);
	default_slot_name(slots,
			Clos_combination_form,
			CONSTANT_CLOSNAME_FORM);
	default_slot_name(slots,
			Clos_combination_function,
			CONSTANT_CLOSNAME_FUNCTION);
	set_slots_localtion(slots);
	*ret = slots;
}

static void build_method_combination_call(Execute ptr)
{
	addr metaclass, slots;

	GetConst(COMMON_STANDARD_CLASS, &metaclass);
	metaclass = find_class(metaclass);
	make_method_combination_slots(&slots);

	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_COMMON_METHOD_COMBINATION,
			CONSTANT_COMMON_STANDARD_OBJECT);
}

static void build_method_combination_execute(Execute ptr)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	build_method_combination_call(ptr);
	rollback_local(local, stack);
}

void build_method_combination(Execute ptr)
{
	addr pos;

	GetConst(COMMON_METHOD_COMBINATION, &pos);
	pos = find_class_nil(pos);
	if (pos != Nil)
		fmte("The method_combination is already exist.", NULL);
	build_method_combination_execute(ptr);
}


/*****************************************************************************
 *  eql-specializer
 *****************************************************************************/
static void make_eql_specializer_slots(addr *ret)
{
	addr slots;

	vector4_heap(&slots, Clos_specializer_size);
	default_slot_name(slots, Clos_specializer_object, CONSTANT_CLOSNAME_OBJECT);
	default_slot_name(slots, Clos_specializer_type, CONSTANT_CLOSNAME_TYPE);
	set_slots_localtion(slots);
	*ret = slots;
}

static void build_eql_specializer_call(Execute ptr)
{
	addr metaclass, slots;

	GetConst(COMMON_STANDARD_CLASS, &metaclass);
	metaclass = find_class(metaclass);
	make_eql_specializer_slots(&slots);

	make_type_class_slots_constant(ptr, metaclass, slots,
			CONSTANT_CLOSNAME_EQL_SPECIALIZER,
			CONSTANT_COMMON_STANDARD_OBJECT);
}

static void build_eql_specializer_execute(Execute ptr)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	push_local(local, &stack);
	build_eql_specializer_call(ptr);
	rollback_local(local, stack);
}

void build_eql_specializer(Execute ptr)
{
	addr pos;

	GetConst(CLOSNAME_EQL_SPECIALIZER, &pos);
	pos = find_class_nil(pos);
	if (pos != Nil)
		fmte("The eql_specializer is already exist.", NULL);
	build_eql_specializer_execute(ptr);
	find_class_constant(CONSTANT_CLOSNAME_EQL_SPECIALIZER,
			CONSTANT_CLOS_EQL_SPECIALIZER);
}

void intern_eql_specializer(addr value, addr *ret)
{
	addr pos, clos, instance;

	pos = find_eql_specializer_nil(value);
	if (pos != Nil) {
		*ret = pos;
		return;
	}

	GetConst(CLOS_EQL_SPECIALIZER, &clos);
	make_instance_restrict_heap(clos, &instance);
	std_update_class_of(instance, clos);
	setf_clos_elt(instance, Clos_specializer_object, value);
	class_of(value, &pos);
	setf_clos_elt(instance, Clos_specializer_type, pos);
	setf_find_eql_specializer(value, instance);
	*ret = instance;
}

int eql_specializer_p(addr clos)
{
	addr class_of;

	GetConst(CLOS_EQL_SPECIALIZER, &class_of);
	return std_subtype_p(clos, class_of);
}


/*****************************************************************************
 *  check function
 *****************************************************************************/
int std_subclass_p(addr clos, addr super)
{
	Check(! closp(clos), "clos type error");
	Check(! closp(super), "super type error");
	clos_elt(clos, Clos_class_precedence_list, &clos);

	return find_list_eq_unsafe(super, clos);
}

int std_subtype_p(addr clos, addr super)
{
	Check(! closp(clos), "clos type error");
	clos_class_of(clos, &clos);

	return std_subclass_p(clos, super);
}

int funcallablep(addr pos)
{
	addr super;

	if (! closp(pos)) return 0;
	GetConst(CLOSNAME_FUNCALLABLE_STANDARD_CLASS, &super);
	return std_subtype_p(pos, super);
}


/*****************************************************************************
 *  build_clos_standard
 *****************************************************************************/
void build_clos_standard(Execute ptr)
{
	build_standard_class(ptr);
	build_standard_generic_function(ptr);
	build_standard_method(ptr);
	build_method_combination(ptr);
	build_eql_specializer(ptr);
}

