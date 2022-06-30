#include "clos.h"
#include "clos_instance.h"
#include "clos_object.h"
#include "clos_slot.h"
#include "clos_type.h"
#include "closget.h"
#include "closget_class.h"
#include "closget_slot.h"
#include "cons.h"
#include "cons_list.h"
#include "condition.h"
#include "integer.h"
#include "symbol.h"
#include "type_memory.h"
#include "typedef.h"

/*
 *  check
 */
int clos_subclass_p_(Execute ptr, addr clos, addr super, int *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	Return(stdget_class_precedence_list_(ptr, clos, &clos));

	return Result(ret, find_list_eq_unsafe(super, clos));
}

int clos_subtype_p_(Execute ptr, addr clos, addr super, int *ret)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	GetClassOfClos(clos, &clos);
	Check(clos == Unbound, "unbound error");

	return clos_subclass_p_(ptr, clos, super, ret);
}

static int clos_constant_p_(Execute ptr, addr clos, constindex index, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConstant(index, &super);
	return clos_subtype_p_(ptr, clos, super, ret);
}

int clos_class_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_CLASS, ret);
}

int clos_funcallable_p_(Execute ptr, addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	if (clos_funcall_p(clos))
		return Result(ret, 1);
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &super);
	return clos_subtype_p_(ptr, clos, super, ret);
}

int clos_generic_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_GENERIC_FUNCTION, ret);
}

int clos_method_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_METHOD, ret);
}

int clos_define_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_DEFINE_METHOD_COMBINATION, ret);
}

int clos_define_long_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos,
			CONSTANT_CLOS_DEFINE_LONG_METHOD_COMBINATION, ret);
}

int clos_define_short_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos,
			CONSTANT_CLOS_DEFINE_SHORT_METHOD_COMBINATION, ret);
}

int clos_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_METHOD_COMBINATION, ret);
}

int clos_long_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_LONG_METHOD_COMBINATION, ret);
}

int clos_short_combination_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_SHORT_METHOD_COMBINATION, ret);
}

int clos_specializer_p_(Execute ptr, addr clos, int *ret)
{
	return clos_constant_p_(ptr, clos, CONSTANT_CLOS_EQL_SPECIALIZER, ret);
}

int clos_referenced_p_(addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConst(CLOS_FORWARD_REFERENCED_CLASS, &super);
	Return(clos_class_of_(clos, &clos));
	return Result(ret, clos == super);
}

int clos_built_p_(Execute ptr, addr clos, int *ret)
{
	addr super;

	if (! closp(clos))
		return Result(ret, 0);
	GetConst(CLOS_BUILT_IN_CLASS, &super);
	return clos_subtype_p_(ptr, clos, super, ret);
}

int funcallp_(Execute ptr, addr pos, int *ret)
{
	switch (GetType(pos)) {
		case LISPTYPE_FUNCTION:
			return Result(ret, 1);

		case LISPTYPE_CLOS:
			if (clos_funcall_p(pos))
				return Result(ret, 1);
			else
				return clos_funcallable_p_(ptr, pos, ret);

		default:
			return Result(ret, 0);
	}
}


/*
 *  make-instance
 */
int clos_find_slotname(addr slots, size_t size, addr name)
{
	addr check;
	size_t i;

	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		getname_slot(check, &check);
		if (check == name)
			return 1;
	}

	return 0;
}

static int clos_instance_unsafe_(LocalRoot local, addr clos, addr slots, addr *ret)
{
	addr instance, value, slot, check;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* allocate */
	slot_vector_copyheap_alloc(local, &slots, slots);
	clos_alloc(local, &instance, slots);

	/* clos-value */
	LenSlotVector(slots, &size);
	GetValueClos(instance, &value);

	/* class-of */
	SetClassOfClos(instance, clos);

	/* value */
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* name check */
		getname_slot(slot, &check);
		if (! symbolp(check)) {
			*ret = Nil;
			return fmte_("The slot name ~S must be a symbol.", check, NULL);
		}
		/* already exist */
		if (clos_find_slotname(slots, i, check)) {
			*ret = Nil;
			return fmte_("The slot name ~S already exists.", check, NULL);
		}
		/* location */
		getlocation_slot(slot, &loc);
		if (loc != i) {
			*ret = Nil;
			make_index_integer_heap(&value, i);
			return fmte_("The slot location ~A is invalid.", value, NULL);
		}
		/* allocation */
		if (slot_class_p(slot)) {
			*ret = Nil;
			return fmte_("The allocation must be an :INSTANCE.", NULL);
		}
		/* value */
		getform_slot(slot, &check);
		SetClosValue(value, i, check);
	}

	return Result(ret, instance);
}
int clos_instance_alloc_(Execute ptr, LocalRoot local, addr clos, addr *ret)
{
	addr pos;

	Check(ptr == NULL, "execute error.");
	/* finalized-p check */
	Return(stdget_class_finalized_p_(ptr, clos, &pos));
	if (pos == Nil) {
		*ret = Nil;
		return fmte_("The class ~S is not finalized.", clos, NULL);
	}

	/* make-instance */
	Return(stdget_class_slots_(ptr, clos, &pos));
	return clos_instance_unsafe_(local, clos, pos, ret);
}
int clos_instance_local_(Execute ptr, addr clos, addr *ret)
{
	return clos_instance_alloc_(ptr, ptr->local, clos, ret);
}
int clos_instance_heap_(Execute ptr, addr clos, addr *ret)
{
	return clos_instance_alloc_(ptr, NULL, clos, ret);
}


/*
 *  class-precedence-list
 */
static int clos_precedence_classes_(Execute ptr, addr right, addr *ret)
{
	addr left, list;
	LocalRoot local;

	local = ptr->local;
	conscar_local(local, &list, right);
	Return(stdget_class_direct_superclasses_(ptr, right, &right));
	while (right != Nil) {
		GetCons(right, &left, &right);
		cons_local(local, &list, left, list);
	}
	cons_local(local, &list, Unbound, list);
	nreverse(ret, list);

	return 0;
}

static int clos_precedence_pair_(Execute ptr, addr right, addr *ret)
{
	addr child, result, cons, left, temp;
	LocalRoot local;

	local = ptr->local;
	Return(clos_precedence_classes_(ptr, right, &right));
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

	return Result(ret, result);
}

static int clos_precedence_super_(Execute ptr, addr pos, addr *ret, addr x, addr list)
{
	addr stack, result, temp, supers, super;
	LocalRoot local;

	local = ptr->local;
	result = Nil;
	conscar_local(local, &stack, pos);
	while (stack != Nil) {
		for (temp = Nil; stack != Nil; ) {
			GetCons(stack, &supers, &stack);
			if (! find_list_eq_unsafe(supers, result)) {
				cons_local(local, &result, supers, result);
				/* redefine */
				if (supers == x) {
					supers = list;
				}
				else {
					Return(stdget_class_direct_superclasses_(ptr, supers, &supers));
				}
				/* supers */
				while (supers != Nil) {
					GetCons(supers, &super, &supers);
					if (pos == super) {
						*ret = Nil;
						return fmte_("Loop detection "
								"in the class-precedence-list.", NULL);
					}
					pushnew_local(local, temp, super, &temp);
				}
			}
		}
		nreverse(&stack, temp);
	}
	nreverse(ret, result);

	return 0;
}

static int clos_precedence_find(addr a1, addr cons)
{
	addr b1, a2, b2;

	Check(GetType(a1) != LISPTYPE_CONS, "type key error");
	GetCons(a1, &a1, &b1);
	while (cons != Nil) {
		GetCons(cons, &a2, &cons);
		GetCons(a2, &a2, &b2);
		if ((a1 == a2) && (b1 == b2))
			return 1;
	}

	return 0;
}

static int clos_precedence_chain_(Execute ptr, addr pos, addr *ret)
{
	addr one, chain, result;
	LocalRoot local;

	local = ptr->local;
	for (result = Nil; pos != Nil; ) {
		GetCons(pos, &one, &pos);
		Return(clos_precedence_pair_(ptr, one, &one));
		while (one != Nil) {
			GetCons(one, &chain, &one);
			/* pushnew */
			if (! clos_precedence_find(chain, result))
				cons_local(local, &result, chain, result);
		}
	}

	return Result(ret, result);
}

static int clos_precedence_top_(addr cons, addr *ret)
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
		if (check == 0)
			return Result(ret, left);
	}
	*ret = 0;
	return fmte_("Cannot make class precedence list. "
			"Perhaps, class inherit is loop.", NULL);
}

static int clos_precedence_remove_(addr key, addr right2, addr *ret)
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
	if (check == 0) {
		*ret = Nil;
		return fmte_("Cannot make class precedence list. "
				"Class key is not found.", NULL);
	}

	return Result(ret, result);
}

static int clos_precedence_result_(Execute ptr, addr pos, addr *ret, addr x, addr list)
{
	addr root, key;

	Return(clos_precedence_super_(ptr, pos, &pos, x, list));
	Return(clos_precedence_chain_(ptr, pos, &pos));
	for (root = Nil; pos != Nil; ) {
		Return(clos_precedence_top_(pos, &key));
		cons_heap(&root, key, root);
		Return(clos_precedence_remove_(key, pos, &pos));
	}
	nreverse(ret, root);

	return 0;
}

int clos_precedence_list_redefine_(Execute ptr, addr pos, addr *ret, addr x, addr list)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	CheckLocal(local);
	CheckType(pos, LISPTYPE_CLOS);
	push_local(local, &stack);
	Return(clos_precedence_result_(ptr, pos, ret, x, list));
	rollback_local(local, stack);

	return 0;
}

int clos_precedence_list_(Execute ptr, addr pos, addr *ret)
{
	return clos_precedence_list_redefine_(ptr, pos, ret, Unbound, Nil);
}


/*
 *  compute-slots
 */
static int clos_slots_name(addr *ret, addr name, addr list)
{
	addr pos, check, next;

	for (; list != Nil; list = next) {
		GetCons(list, &pos, &next);
		getname_slot(pos, &check);
		if (name == check) {
			*ret = pos;
			return 1;
		}
	}

	return 0;
}

static int clos_slots_push_(addr pos, addr check)
{
	addr list, a;

	getargs_slot(pos, &list);
	getargs_slot(check, &check);
	while (check != Nil) {
		Return_getcons(check, &a, &check);
		pushnew_heap(list, a, &list);
	}
	setargs_slot(pos, list);

	return 0;
}

static int clos_slots_loop_(Execute ptr, addr list, addr *ret, size_t *rsize)
{
	addr root, slots, a, b;
	size_t count, i, size;
	LocalRoot local;

	local = ptr->local;
	root = Nil;
	count = 0;
	while (list != Nil) {
		GetCons(list, &slots, &list);
		Return(stdget_class_direct_slots_(ptr, slots, &slots));
		LenSlotVector(slots, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &a);
			getname_slot(a, &b);
			if (clos_slots_name(&b, b, root)) {
				Return(clos_slots_push_(b, a));
			}
			else {
				slot_copy_heap(&a, a);
				cons_local(local, &root, a, root);
				count++;
			}
		}
	}
	nreverse(ret, root);
	*rsize = count;

	return 0;
}

static int clos_slots_gather_(Execute ptr, addr clos, addr *ret)
{
	addr slots, pos;
	size_t i;

	Return(stdget_class_precedence_list_(ptr, clos, &clos));
	Return(clos_slots_loop_(ptr, clos, &clos, &i));
	slot_vector_heap(&slots, i);
	for (i = 0; clos != Nil; i++) {
		GetCons(clos, &pos, &clos);
		SetSlotVector(slots, i, pos);
		setlocation_slot(pos, i);
	}

	return Result(ret, slots);
}

int clos_compute_slots_(Execute ptr, addr clos, addr *ret)
{
	LocalRoot local;
	LocalStack stack;

	local = ptr->local;
	CheckLocal(local);
	CheckType(clos, LISPTYPE_CLOS);
	push_local(local, &stack);
	Return(clos_slots_gather_(ptr, clos, ret));
	rollback_local(local, stack);

	return 0;
}


/*
 *  build-check
 */
static void build_clos_class_init(void)
{
	addr pos;

	/* check */
#ifdef LISP_DEBUG
	GetConst(COMMON_STANDARD_CLASS, &pos);
	clos_find_class_nil(pos, &pos);
	if (pos != Nil)
		Abort("STANDARD-CLASS is already exist.");
#endif

	/* symbol type */
	type0_heap(LISPDECL_SYMBOL, &pos);
	SetStatusReadOnly(pos);
	SetConst(CLOSDATA_SYMBOL_TYPE, pos);
}


/*
 *  standard-class
 */
static void slot_make_name_symbol(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	setname_slot(slot, value);
	/* type */
	GetConst(CLOSDATA_SYMBOL_TYPE, &value);
	CheckType(value, LISPTYPE_TYPE);
	settype_slot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	setargs_slot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeNameSymbol_number(x,y,z) \
	slot_make_name_symbol((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeNameSymbol(x,y,z) \
	slot_make_name_symbol((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_name(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	setname_slot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	setargs_slot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeName_number(x,y,z) \
	slot_make_name((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeName(x,y,z) \
	slot_make_name((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_form(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	setname_slot(slot, value);
	/* initform */
	setform_slot(slot, Nil);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	setargs_slot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeForm_number(x,y,z) \
	slot_make_form((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, (z))
#define SlotMakeForm(x,y,z) \
	slot_make_form((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

static void slot_make_version(addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	slot_heap(&slot);
	/* name */
	GetConstant(n1, &value);
	setname_slot(slot, value);
	/* initform */
	fixnum_heap(&value, 0);
	setform_slot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	setargs_slot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeVersion(x,y,z) \
	slot_make_version((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

void slotvector_set_location(addr slots)
{
	addr pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		setlocation_slot(pos, i);
	}
}

static void clos_stdclass_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_class_size);
	SlotMakeNameSymbol(slots, NAME, class_name);
	SlotMakeForm(slots, DIRECT_SLOTS, class_direct_slots);
	SlotMakeForm(slots, DIRECT_SUBCLASSES, class_direct_subclasses);
	SlotMakeName(slots, DIRECT_SUPERCLASSES, class_direct_superclasses);
	SlotMakeName(slots, CLASS_PRECEDENCE_LIST, class_precedence_list);
	SlotMakeName(slots, EFFECTIVE_SLOTS, class_slots);
	SlotMakeForm(slots, FINALIZED_P, class_finalized_p);
	SlotMakeName(slots, PROTOTYPE, class_prototype);
	SlotMakeForm(slots, DEFAULT_INITARGS, class_default_initargs);
	SlotMakeForm(slots, DIRECT_DEFAULT_INITARGS, class_direct_default_initargs);
	SlotMakeVersion(slots, VERSION, class_version);
	SlotMakeForm(slots, DOCUMENTATION, class_documentation);
	SlotMakeForm(slots, REDEFINED_CLASS, class_redefined_class);
	slotvector_set_location(slots);
	*ret = slots;
}

int clos_stdclass_direct_slots_(Execute ptr, addr instance, addr slots)
{
#ifdef LISP_DEBUG
	addr check;
#endif
	addr slot;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
#ifdef LISP_DEBUG
		getclass_slot(slot, &check);
		Check(check != Nil, "slot class error");
#endif
		setclass_slot(slot, instance);
	}
	return stdset_class_direct_slots_(ptr, instance, slots);
}

static int clos_stdclass_dummy_(Execute ptr, addr *ret, addr slots)
{
	addr instance;

	clos_heap(&instance, slots);
	SetClassOfClos(instance, Nil);
	Return(clos_stdclass_direct_slots_(ptr, instance, slots));
	Return(stdset_class_slots_(ptr, instance, slots));
	Return(stdset_class_finalized_p_(ptr, instance, T));

	return Result(ret, instance);
}

static int clos_stdclass_make_(Execute ptr, addr *ret, addr clos, addr name, addr slots)
{
	addr instance;

	Check(! symbolp(name), "type error");
	Return(clos_instance_heap_(ptr, clos, &instance));
	Return(stdset_class_name_(ptr, instance, name));
	Return(clos_stdclass_direct_slots_(ptr, instance, slots));
	Return(stdset_class_prototype_(ptr, instance, instance));

	return Result(ret, instance);
}

static int clos_stdclass_empty_(Execute ptr, addr *ret, addr clos, addr name)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	return clos_stdclass_make_(ptr, ret, clos, name, slots);
}

static void clos_stdclass_class_of(addr instance, addr class_of)
{
	fixnum version;

	SetClassOfClos(instance, class_of);
	GetVersionClos(class_of, &version);
	SetVersionClos(instance, version);
}

static int list_referenced_check_(addr list, int *ret)
{
	int check;
	addr value;

	while (list != Nil) {
		Return_getcons(list, &value, &list);
		Return(clos_referenced_p_(value, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static void slot_vector_prototype(addr clos, addr slots, addr *ret)
{
	addr pos, slot, check;
	size_t size, i;

	LenSlotVector(slots, &size);
	slot_vector_heap(&pos, size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (slot_class_p(slot)) {
			getclass_slot(slot, &check);
			if (clos == check) {
				slot_copy_heap(&slot, slot);
				slot_set_instance(slot);
			}
		}
		SetSlotVector(pos, i, slot);
	}
	*ret = pos;
}

static void clos_stdclass_prototype_initialize(addr pos, addr slots)
{
	addr slot, value;
	size_t size, i;

	LenSlotVector(slots, &size);
	GetValueClos(pos, &pos);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		if (slot_class_p(slot)) {
			getform_slot(slot, &value);
			SetClosValue(pos, i, value);
		}
	}
}

int clos_stdclass_prototype_(Execute ptr, addr clos)
{
	addr pos, slots, value;

	/* make prototype */
	Return(stdget_class_slots_(ptr, clos, &slots));
	slot_vector_prototype(clos, slots, &value);
	clos_heap(&pos, value);
	SetClassOfClos(pos, clos);
	/* initialize shared slots */
	clos_stdclass_prototype_initialize(pos, slots);
	/* result */
	return stdset_class_prototype_(ptr, clos, pos);
}

static int clos_stdclass_inherit_(Execute ptr, addr pos, addr clos, addr supers)
{
	int check;
	addr list, super;

	/* class-of */
	clos_stdclass_class_of(pos, clos);
	/* direct-superclasses */
	Return(stdset_class_direct_superclasses_(ptr, pos, supers));
	/* forward-referenced-class check */
	Return(list_referenced_check_(supers, &check));
	if (! check) {
		/* class-precedence-list */
		Return(clos_precedence_list_(ptr, pos, &list));
		Return(stdset_class_precedence_list_(ptr, pos, list));
		/* effective-slots */
		Return(clos_compute_slots_(ptr, pos, &list));
		Return(stdset_class_slots_(ptr, pos, list));
		/* finalized-p */
		Return(clos_stdclass_prototype_(ptr, pos));
		Return(stdset_class_finalized_p_(ptr, pos, T));
	}
	/* direct-subclasses */
	while (supers != Nil) {
		GetCons(supers, &super, &supers);
		Return(clos_referenced_p_(super, &check));
		if (! check) {
			Return(stdget_class_direct_subclasses_(ptr, super, &list));
			cons_heap(&list, pos, list);
			Return(stdset_class_direct_subclasses_(ptr, super, list));
		}
	}
	/* setf-find-class */
	Return(stdget_class_name_(ptr, pos, &list));
	clos_define_class(list, pos);

	return 0;
}

static int clos_stdclass_single_(Execute ptr, addr pos, addr clos, addr super)
{
	conscar_heap(&super, super);
	return clos_stdclass_inherit_(ptr, pos, clos, super);
}

static int clos_stdclass_metaclass_(Execute ptr, addr *ret)
{
	addr slots, name;
	addr builtin, tc, stdobject, classc, stdclass, metaobject, specializer;

	/* dummy metaclass */
	clos_stdclass_slots(&slots);
	Return(clos_stdclass_dummy_(ptr, &stdclass, slots));
	/* class */
	slot_vector_clear(slots);
	GetConst(COMMON_CLASS, &name);
	Return(clos_stdclass_make_(ptr, &classc, stdclass, name, slots));
	Return(clos_stdclass_inherit_(ptr, classc, classc, Nil));

	/* object */
	Return(clos_stdclass_empty_(ptr, &tc, classc, T));
	GetConst(COMMON_STANDARD_OBJECT, &name);
	Return(clos_stdclass_empty_(ptr, &stdobject, classc, name));
	GetConst(CLOSNAME_METAOBJECT, &name);
	Return(clos_stdclass_empty_(ptr, &metaobject, classc, name));
	GetConst(CLOSNAME_SPECIALIZER, &name);
	Return(clos_stdclass_empty_(ptr, &specializer, classc, name));
	GetConst(COMMON_STANDARD_CLASS, &name);
	Return(clos_stdclass_empty_(ptr, &stdclass, classc, name));
	GetConst(COMMON_BUILT_IN_CLASS, &name);
	Return(clos_stdclass_empty_(ptr, &builtin, classc, name));

	/* inheritance */
	Return(clos_stdclass_inherit_(ptr, tc, classc, Nil));
	Return(clos_stdclass_single_(ptr, stdobject, classc, tc));
	Return(clos_stdclass_single_(ptr, metaobject, classc, stdobject));
	Return(clos_stdclass_single_(ptr, specializer, classc, metaobject));
	Return(clos_stdclass_single_(ptr, classc, classc, specializer));
	Return(clos_stdclass_single_(ptr, stdclass, stdclass, classc));
	Return(clos_stdclass_single_(ptr, builtin, stdclass, classc));
	Clos_standard_class = stdclass;

	/* update class-of */
	clos_stdclass_class_of(tc, builtin);
	clos_stdclass_class_of(stdobject, stdclass);
	clos_stdclass_class_of(metaobject, stdclass);
	clos_stdclass_class_of(specializer, stdclass);
	clos_stdclass_class_of(classc, stdclass);
	clos_stdclass_class_of(stdclass, stdclass);
	clos_stdclass_class_of(builtin, stdclass);
	/* constant */
	SetConst(CLOS_T, tc);
	SetConst(CLOS_STANDARD_OBJECT, stdobject);
	SetConst(CLOS_METAOBJECT, metaobject);
	SetConst(CLOS_SPECIALIZER, specializer);
	SetConst(CLOS_CLASS, classc);
	SetConst(CLOS_STANDARD_CLASS, stdclass);
	SetConst(CLOS_BUILT_IN_CLASS, builtin);
	/* constant */
	SetStatusReadOnly(builtin);
	/* result */
	return Result(ret, stdclass);
}

int clos_stdclass_supers_(Execute ptr,
		addr *ret, addr metaclass, addr name, addr slots, addr supers)
{
	addr instance;

	Return(clos_stdclass_make_(ptr, &instance, metaclass, name, slots));
	Return(clos_stdclass_inherit_(ptr, instance, metaclass, supers));
	return Result(ret, instance);
}

static int clos_stdclass_type_(Execute ptr,
		addr *ret, addr metaclass, addr name, addr supers)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	return clos_stdclass_supers_(ptr, ret, metaclass, name, slots, supers);
}

static void clos_stdclass_va_list(addr *ret, va_list args)
{
	addr list, pos;
	constindex arg;

	list = Nil;
	for (;;) {
		arg = va_arg(args, constindex);
		if (arg == CONSTANT_EMPTY)
			break;
		GetConstant(arg, &pos);
		CheckType(pos, LISPTYPE_CLOS);
		cons_heap(&list, pos, list);
	}
	nreverse(ret, list);
}

static int clos_stdclass_va_(Execute ptr, addr m, constindex n, constindex c, ...)
{
	va_list args;
	addr list, clos, name;

	/* args */
	va_start(args, c);
	clos_stdclass_va_list(&list, args);
	va_end(args);
	/* make class */
	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	Return(clos_stdclass_type_(ptr, &clos, m, name, list));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}

#define ClosMakeClass1_(p,m,a,b,c) { \
	Return(clos_stdclass_va_((p),(m), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_EMPTY)); \
}
#define ClosMakeClass2_(p,m,a,b,c,d) { \
	Return(clos_stdclass_va_((p),(m), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_##d,CONSTANT_EMPTY)); \
}

static int clos_stdclass_slotsconstant_(Execute ptr, addr metaclass, addr slots,
		constindex n, constindex c, constindex s)
{
	addr name, supers, clos;

	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	GetConstant(s, &supers);
	CheckType(supers, LISPTYPE_CLOS);
	conscar_heap(&supers, supers);
	Return(clos_stdclass_supers_(ptr, &clos, metaclass, name, slots, supers));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}
#define ClosMakeClassSlot_(p,m,s,a,b,c) { \
	Return(clos_stdclass_slotsconstant_((p),(m),(s), \
				CONSTANT_##a,CONSTANT_##b,CONSTANT_##c)); \
}

static int clos_stdclass2_slotsconstant_(Execute ptr,
		addr metaclass, addr slots,
		constindex n, constindex c,
		constindex x, constindex y)
{
	addr name, supers, c1, c2, clos;

	GetConstant(n, &name);
	Check(! symbolp(name), "type error");

	GetConstant(x, &c1);
	GetConstant(y, &c2);
	CheckType(c1, LISPTYPE_CLOS);
	CheckType(c2, LISPTYPE_CLOS);
	list_heap(&supers, c1, c2, NULL);
	Return(clos_stdclass_supers_(ptr, &clos, metaclass, name, slots, supers));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}
#define ClosMakeClass2Slot_(p,m,s,a,b,x,y) { \
	Return(clos_stdclass2_slotsconstant_((p),(m),(s), \
				CONSTANT_##a, CONSTANT_##b, \
				CONSTANT_##x, CONSTANT_##y)); \
}

static void clos_structure_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_structure_size);
	SlotMakeNameSymbol(slots, NAME, structure_name);
	SlotMakeForm(slots, DIRECT_SLOTS, structure_direct_slots);
	SlotMakeForm(slots, SLOTS, structure_slots);
	SlotMakeForm(slots, DOCUMENTATION, structure_documentation);
	SlotMakeForm(slots, INCLUDE, structure_include);
	SlotMakeName(slots, CLASS_PRECEDENCE_LIST, structure_precedence_list);
	SlotMakeName(slots, VALUE, structure_value);
	SlotMakeName(slots, PREDICATE, structure_predicate);
	SlotMakeForm(slots, ACCESS, structure_access);
	SlotMakeForm(slots, COPIER, structure_copier);
	SlotMakeForm(slots, CONSTRUCTOR, structure_constructor);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_standard_(Execute ptr)
{
	addr metaclass, structure, slots;

	/* standard-class, others */
	Return(clos_stdclass_metaclass_(ptr, &metaclass));
	/* structure-class */
	clos_structure_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STRUCTURE_CLASS,
			CLOS_STRUCTURE_CLASS,
			CLOS_CLASS);
	/* structure-object */
	GetConst(CLOS_STRUCTURE_CLASS, &structure);
	ClosMakeClass1_(ptr, structure,
			COMMON_STRUCTURE_OBJECT,
			CLOS_STRUCTURE_OBJECT,
			CLOS_T);
	/* forward-referenced-class */
	clos_stdclass_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_FORWARD_REFERENCED_CLASS,
			CLOS_FORWARD_REFERENCED_CLASS,
			CLOS_CLASS);

	return 0;
}


/*
 *  standard-generic-function
 */
static void clos_stdgeneric_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_generic_size);
	SlotMakeNameSymbol(slots, NAME, generic_name);
	SlotMakeForm(slots, METHODS, generic_methods);
	SlotMakeName(slots, LAMBDA_LIST, generic_lambda_list);
	SlotMakeName(slots, ARGUMENT_PRECEDENCE_ORDER, generic_argument_precedence_order);
	SlotMakeForm(slots, DECLARATIONS, generic_declarations);
	SlotMakeName(slots, METHOD_CLASS, generic_method_class);
	SlotMakeForm(slots, METHOD_COMBINATION, generic_method_combination);
	SlotMakeForm(slots, VECTOR, generic_vector);
	SlotMakeForm(slots, REMOVE, generic_remove);
	SlotMakeForm(slots, ARGUMENT, generic_argument);
	SlotMakeForm(slots, DOCUMENTATION, generic_documentation);
	SlotMakeName(slots, EQLCHECK, generic_eqlcheck);
	SlotMakeName(slots, CACHE, generic_cache);
	SlotMakeName(slots, CALL, generic_call);
	SlotMakeName(slots, PRECEDENCE_INDEX, generic_precedence_index);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_generic_(Execute ptr)
{
	addr metaclass, builtin, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	GetConst(CLOS_BUILT_IN_CLASS, &builtin);
	/* function */
	ClosMakeClass1_(ptr, builtin,
			COMMON_FUNCTION,
			CLOS_FUNCTION,
			CLOS_T);
	/* funcallable-standard-object */
	ClosMakeClass2_(ptr, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_FUNCTION,
			CLOS_STANDARD_OBJECT);
	/* funcallable-standard-class */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_CLASS,
			CLOS_FUNCALLABLE_STANDARD_CLASS,
			CLOS_CLASS);
	/* generic-function */
	ClosMakeClass2_(ptr, metaclass,
			COMMON_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION,
			CLOS_METAOBJECT,
			CLOS_FUNCALLABLE_STANDARD_OBJECT);
	/* standard-generic-function */
	clos_stdgeneric_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STANDARD_GENERIC_FUNCTION,
			CLOS_STANDARD_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION);

	return 0;
}


/*
 *  standard-method
 */
static void clos_stdmethod_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_method_size);
	SlotMakeName(slots, FUNCTION, method_function);
	SlotMakeForm(slots, GENERIC_FUNCTION, method_generic_function);
	SlotMakeName(slots, LAMBDA_LIST, method_lambda_list);
	SlotMakeName(slots, QUALIFIERS, method_qualifiers);
	SlotMakeName(slots, SPECIALIZERS, method_specializers);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_method_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_METHOD,
			CLOS_METHOD,
			CLOS_METAOBJECT);
	/* standard-method */
	clos_stdmethod_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STANDARD_METHOD,
			CLOS_STANDARD_METHOD,
			CLOS_METHOD);
	/* standard-accessor-method */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_STANDARD_ACCESSOR_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD,
			CLOS_STANDARD_METHOD);
	/* standard-reader-method */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_STANDARD_READER_METHOD,
			CLOS_STANDARD_READER_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD);
	/* standard-writer-method */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_STANDARD_WRITER_METHOD,
			CLOS_STANDARD_WRITER_METHOD,
			CLOS_STANDARD_ACCESSOR_METHOD);

	return 0;
}


/*
 *  method-combination
 */
static void clos_stdlongcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longcomb_size);
	SlotMakeNameSymbol(slots, NAME, longcomb_name);
	SlotMakeName(slots, DOCUMENTATION, longcomb_documentation);
	SlotMakeName(slots, LAMBDA_LIST, longcomb_lambda_list);
	SlotMakeName(slots, BINDING, longcomb_binding);
	SlotMakeName(slots, QUALIFIERS, longcomb_qualifiers);
	SlotMakeName(slots, ARGUMENTS, longcomb_arguments);
	SlotMakeName(slots, GENERIC, longcomb_generic);
	SlotMakeName(slots, FORM, longcomb_form);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdshortcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortcomb_size);
	SlotMakeNameSymbol(slots, NAME, shortcomb_name);
	SlotMakeName(slots, DOCUMENTATION, shortcomb_documentation);
	SlotMakeName(slots, IDENTITY, shortcomb_identity);
	SlotMakeName(slots, OPERATOR, shortcomb_operator);
	SlotMakeName(slots, ORDER, shortcomb_order);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdlongdef_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longdef_size);
	SlotMakeNameSymbol(slots, NAME, longdef_name);
	SlotMakeName(slots, DOCUMENTATION, longdef_documentation);
	SlotMakeName(slots, LAMBDA_LIST, longdef_lambda_list);
	SlotMakeName(slots, QUALIFIERS, longdef_qualifiers);
	SlotMakeName(slots, ARGUMENTS, longdef_arguments);
	SlotMakeName(slots, GENERIC, longdef_generic);
	SlotMakeName(slots, FORM, longdef_form);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdshortdef_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortdef_size);
	SlotMakeNameSymbol(slots, NAME, shortdef_name);
	SlotMakeName(slots, DOCUMENTATION, shortdef_documentation);
	SlotMakeName(slots, IDENTITY, shortdef_identity);
	SlotMakeName(slots, OPERATOR, shortdef_operator);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_combination_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method-combination */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION,
			CLOS_METAOBJECT);
	/* long-method-combination */
	clos_stdlongcomb_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_LONG_METHOD_COMBINATION,
			CLOS_LONG_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* short-method-combination */
	clos_stdshortcomb_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_SHORT_METHOD_COMBINATION,
			CLOS_SHORT_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* define-method-combination */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_DEFINE_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION,
			CLOS_STANDARD_OBJECT);
	/* define-long-method-combination */
	clos_stdlongdef_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);
	/* define-short-method-combination */
	clos_stdshortdef_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);

	return 0;
}


/*
 *  eql-specializer
 */
static void clos_stdspecializer_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_specializer_size);
	SlotMakeName(slots, OBJECT, specializer_object);
	SlotMakeName(slots, TYPE, specializer_type);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_specializer_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* eql-specializer */
	clos_stdspecializer_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_EQL_SPECIALIZER,
			CLOS_EQL_SPECIALIZER,
			CLOS_SPECIALIZER);

	return 0;
}


/*
 *  condition
 */
static void clos_stdcondition_slot1(addr *ret, constindex n1, constindex n2)
{
	addr slots;

	slot_vector_heap(&slots, 1);
	slot_make_name(slots, n1, n2, 0);
	slotvector_set_location(slots);
	*ret = slots;
}
#define SlotMakeCondition1(p,a) \
	clos_stdcondition_slot1((p), CONSTANT_CLOSNAME_##a, CONSTANT_KEYWORD_##a)

static void clos_stdcondition_slot2(addr *ret,
		constindex a1, constindex a2,
		constindex b1, constindex b2)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	slot_make_name(slots, a1, a2, 0);
	slot_make_name(slots, b1, b2, 1);
	slotvector_set_location(slots);
	*ret = slots;
}
#define SlotMakeCondition2(p,a,b) { \
	clos_stdcondition_slot2((p), \
			CONSTANT_CLOSNAME_##a, CONSTANT_KEYWORD_##a, \
			CONSTANT_CLOSNAME_##b, CONSTANT_KEYWORD_##b); \
}

static void clos_simple_condition_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	/* :format-control */
	slot_make_name(slots,
			CONSTANT_CLOSNAME_FORMAT_CONTROL,
			CONSTANT_KEYWORD_FORMAT_CONTROL,
			0);
	/* :format-arguments */
	slot_make_form(slots,
			CONSTANT_CLOSNAME_FORMAT_ARGUMENTS,
			CONSTANT_KEYWORD_FORMAT_ARGUMENTS,
			1);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_condition_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* condition */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_CONDITION,
			CLOS_CONDITION,
			CLOS_STANDARD_OBJECT);
	/* serious-condition (condition) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_SERIOUS_CONDITION,
			CONDITION_SERIOUS_CONDITION,
			CLOS_CONDITION);
	/* simple-condition (condition) :format-control :format-arguments*/
	clos_simple_condition_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_SIMPLE_CONDITION,
			CONDITION_SIMPLE_CONDITION,
			CLOS_CONDITION);
	/* warning (condition) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_WARNING,
			CONDITION_WARNING,
			CLOS_CONDITION);
	/* error (serious-condition) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_ERROR,
			CONDITION_ERROR,
			CONDITION_SERIOUS_CONDITION);
	/* storage-condition (serious-condition) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_STORAGE_CONDITION,
			CONDITION_STORAGE_CONDITION,
			CONDITION_SERIOUS_CONDITION);
	/* arithmetic-error (error) :operation :operands */
	SlotMakeCondition2(&slots, OPERATION, OPERANDS);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_ARITHMETIC_ERROR,
			CONDITION_ARITHMETIC_ERROR,
			CONDITION_ERROR);
	/* cell-error (error) :name */
	SlotMakeCondition1(&slots, NAME);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_CELL_ERROR,
			CONDITION_CELL_ERROR,
			CONDITION_ERROR);
	/* control-error (error) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_CONTROL_ERROR,
			CONDITION_CONTROL_ERROR,
			CONDITION_ERROR);
	/* file-error (error) :pathname */
	SlotMakeCondition1(&slots, PATHNAME);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_FILE_ERROR,
			CONDITION_FILE_ERROR,
			CONDITION_ERROR);
	/* package-error (error) :package */
	SlotMakeCondition1(&slots, PACKAGE);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_PACKAGE_ERROR,
			CONDITION_PACKAGE_ERROR,
			CONDITION_ERROR);
	/* parse-error (error) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_PARSE_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_ERROR);
	/* print-not-readable (error) :object */
	SlotMakeCondition1(&slots, OBJECT);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_PRINT_NOT_READABLE,
			CONDITION_PRINT_NOT_READABLE,
			CONDITION_ERROR);
	/* program-error (error) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_PROGRAM_ERROR,
			CONDITION_PROGRAM_ERROR,
			CONDITION_ERROR);
	/* stream-error (error) :stream */
	SlotMakeCondition1(&slots, STREAM);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STREAM_ERROR,
			CONDITION_STREAM_ERROR,
			CONDITION_ERROR);
	/* type-error (error) :datum :expected-type */
	SlotMakeCondition2(&slots, DATUM, EXPECTED_TYPE);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_TYPE_ERROR,
			CONDITION_TYPE_ERROR,
			CONDITION_ERROR);
	/* unbound-slot (cell-error) :instance :name */
	SlotMakeCondition1(&slots, INSTANCE);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_UNBOUND_SLOT,
			CONDITION_UNBOUND_SLOT,
			CONDITION_CELL_ERROR);
	/* unbound-variable (cell-error) :name */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_UNBOUND_VARIABLE,
			CONDITION_UNBOUND_VARIABLE,
			CONDITION_CELL_ERROR);
	/* undefined-function (cell-error) :name */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_UNDEFINED_FUNCTION,
			CONDITION_UNDEFINED_FUNCTION,
			CONDITION_CELL_ERROR);
	/* style-warning (warning) */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_STYLE_WARNING,
			CONDITION_STYLE_WARNING,
			CONDITION_WARNING);
	/* simple-error (simple-condition error) :format-control :format-arguments */
	ClosMakeClass2_(ptr, metaclass,
			COMMON_SIMPLE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_ERROR);
	/* simple_type_error (simple-condition type_error)
	 *   :format-control :format-arguments :datum :expected-type */
	ClosMakeClass2_(ptr, metaclass,
			COMMON_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_TYPE_ERROR);
	/* simple-warning (simple-condition warning) :format-control :format-arguments */
	ClosMakeClass2_(ptr, metaclass,
			COMMON_SIMPLE_WARNING,
			CONDITION_SIMPLE_WARNING,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_WARNING);
	/* division-by-zero (arithmetic-error) :operation :operands */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_DIVISION_BY_ZERO,
			CONDITION_DIVISION_BY_ZERO,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-inexact (arithmetic-error) :operation :operands */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_FLOATING_POINT_INEXACT,
			CONDITION_FLOATING_POINT_INEXACT,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-invalid-operation (arithmetic-error) :operation :operands */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-overflow (arithmetic-error) :operation :operands */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_FLOATING_POINT_OVERFLOW,
			CONDITION_FLOATING_POINT_OVERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-underflow (arithmetic-error) :operation :operands */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_FLOATING_POINT_UNDERFLOW,
			CONDITION_FLOATING_POINT_UNDERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* end-of-file (stream-error) :stream */
	ClosMakeClass1_(ptr, metaclass,
			COMMON_END_OF_FILE,
			CONDITION_END_OF_FILE,
			CONDITION_STREAM_ERROR);
	/* reader-error (parse-error stream-error) :stream */
	ClosMakeClass2_(ptr, metaclass,
			COMMON_READER_ERROR,
			CONDITION_READER_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_STREAM_ERROR);
	/* lisp-system::simple-control-error (simple-error control-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_CONTROL_ERROR,
			CONDITION_SIMPLE_CONTROL_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_CONTROL_ERROR);
	/* lisp-system::simple-file-error (simple-error file-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_FILE_ERROR);
	/* lisp-system::simple-package-error (simple-error package-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_PACKAGE_ERROR,
			CONDITION_SIMPLE_PACKAGE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PACKAGE_ERROR);
	/* lisp-system::simple-parse-error (simple-error parse-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_PARSE_ERROR,
			CONDITION_SIMPLE_PARSE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PARSE_ERROR);
	/* lisp-system::simple-program-error (simple-error program-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_PROGRAM_ERROR,
			CONDITION_SIMPLE_PROGRAM_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_PROGRAM_ERROR);
	/* lisp-system::simple-reader-error (simple-error reader-error) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_READER_ERROR,
			CONDITION_SIMPLE_READER_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_READER_ERROR);
	/* lisp-system::simple-style-warning (simple-warning style-warning) */
	ClosMakeClass2_(ptr, metaclass,
			SYSTEM_SIMPLE_STYLE_WARNING,
			CONDITION_SIMPLE_STYLE_WARNING,
			CONDITION_SIMPLE_WARNING,
			CONDITION_STYLE_WARNING);
	/* lisp-system::delay-warning (warning) */
	ClosMakeClass1_(ptr, metaclass,
			SYSTEM_DELAY_WARNING,
			CONDITION_DELAY_WARNING,
			CONDITION_WARNING);

	return 0;
}

static int build_clos_class_system_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* savecore (serious-condition) */
	ClosMakeClass1_(ptr, metaclass,
			SYSTEM_SAVECORE,
			CONDITION_SAVECORE,
			CONDITION_SERIOUS_CONDITION);
	/* exit (serious-condition) */
	SlotMakeCondition1(&slots, VALUE);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			SYSTEM_EXIT,
			CONDITION_EXIT,
			CONDITION_SERIOUS_CONDITION);

	return 0;
}


/*
 *  lisp type
 */
static int clos_stdtype_buildin_(Execute ptr, constindex n, constindex c, addr list)
{
	addr metaclass, name, clos;

	GetConst(CLOS_BUILT_IN_CLASS, &metaclass);
	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	Return(clos_stdclass_type_(ptr, &clos, metaclass, name, list));
	clos_define_class(name, clos);
	SetConstant(c, clos);

	return 0;
}

static int clos_stdtype_buildin0_(Execute ptr,
		constindex n, constindex c)
{
	addr list;

	GetConst(CLOS_T, &list);
	list_heap(&list, list, NULL);
	return clos_stdtype_buildin_(ptr, n, c, list);
}

static int clos_stdtype_buildin1_(Execute ptr,
		constindex n, constindex c, constindex s1)
{
	addr list, clos1, clos2;

	GetConstant(s1, &clos1);
	GetConst(CLOS_T, &clos2);
	CheckType(clos1, LISPTYPE_CLOS);
	CheckType(clos2, LISPTYPE_CLOS);
	list_heap(&list, clos1, clos2, NULL);
	return clos_stdtype_buildin_(ptr, n, c, list);
}

static int clos_stdtype_buildin2_(Execute ptr,
		constindex n, constindex c, constindex s1, constindex s2)
{
	addr list, clos1, clos2, clos3;

	GetConstant(s1, &clos1);
	GetConstant(s2, &clos2);
	GetConst(CLOS_T, &clos3);
	CheckType(clos1, LISPTYPE_CLOS);
	CheckType(clos2, LISPTYPE_CLOS);
	CheckType(clos3, LISPTYPE_CLOS);
	list_heap(&list, clos1, clos2, clos3, NULL);
	return clos_stdtype_buildin_(ptr, n, c, list);
}

#define ClosMakeType0_(p,x) { \
	Return(clos_stdtype_buildin0_(p, CONSTANT_COMMON_##x, CONSTANT_CLOS_##x)); \
}
#define ClosMakeType1_(p,x,y) { \
	Return(clos_stdtype_buildin1_(p, \
				CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, CONSTANT_CLOS_##y)); \
}
#define ClosMakeType2_(p,x,y,z) { \
	Return(clos_stdtype_buildin2_(p, \
				CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, \
				CONSTANT_CLOS_##y, CONSTANT_CLOS_##z)); \
}

static int build_clos_class_type_(Execute ptr)
{
	ClosMakeType0_(ptr, ARRAY);
	ClosMakeType0_(ptr, CHARACTER);
	ClosMakeType0_(ptr, HASH_TABLE);
	ClosMakeType0_(ptr, NUMBER);
	ClosMakeType0_(ptr, PACKAGE);
	ClosMakeType0_(ptr, PATHNAME);
	ClosMakeType0_(ptr, RANDOM_STATE);
	ClosMakeType0_(ptr, READTABLE);
	ClosMakeType0_(ptr, RESTART);
	ClosMakeType0_(ptr, SEQUENCE);
	ClosMakeType0_(ptr, STREAM);
	ClosMakeType0_(ptr, SYMBOL);
	ClosMakeType1_(ptr, LOGICAL_PATHNAME, PATHNAME);

	ClosMakeType1_(ptr, LIST, SEQUENCE);
	ClosMakeType1_(ptr, CONS, LIST);
	ClosMakeType2_(ptr, VECTOR, ARRAY, SEQUENCE);
	ClosMakeType1_(ptr, BIT_VECTOR, VECTOR);
	ClosMakeType2_(ptr, NULL, SYMBOL, LIST);
	ClosMakeType1_(ptr, STRING, VECTOR);

	ClosMakeType1_(ptr, COMPLEX, NUMBER);
	ClosMakeType1_(ptr, REAL, NUMBER);
	ClosMakeType1_(ptr, FLOAT, REAL);
	ClosMakeType1_(ptr, RATIONAL, REAL);
	ClosMakeType1_(ptr, INTEGER, RATIONAL);
	ClosMakeType1_(ptr, RATIO, RATIONAL);

	ClosMakeType1_(ptr, BROADCAST_STREAM, STREAM);
	ClosMakeType1_(ptr, CONCATENATED_STREAM, STREAM);
	ClosMakeType1_(ptr, ECHO_STREAM, STREAM);
	ClosMakeType1_(ptr, FILE_STREAM, STREAM);
	ClosMakeType1_(ptr, STRING_STREAM, STREAM);
	ClosMakeType1_(ptr, SYNONYM_STREAM, STREAM);
	ClosMakeType1_(ptr, TWO_WAY_STREAM, STREAM);

	ClosMakeType1_(ptr, BASE_CHAR, CHARACTER);
	ClosMakeType1_(ptr, EXTENDED_CHAR, CHARACTER);
	ClosMakeType1_(ptr, STANDARD_CHAR, BASE_CHAR);
	ClosMakeType1_(ptr, SIMPLE_ARRAY, ARRAY);
	ClosMakeType2_(ptr, SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY);

	ClosMakeType1_(ptr, BASE_STRING, STRING);
	ClosMakeType2_(ptr, SIMPLE_STRING, STRING, SIMPLE_ARRAY);
	ClosMakeType2_(ptr, SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING);
	ClosMakeType2_(ptr, SIMPLE_BIT_VECTOR, BIT_VECTOR, SIMPLE_ARRAY);
	ClosMakeType1_(ptr, BIGNUM, INTEGER);
	ClosMakeType1_(ptr, FIXNUM, INTEGER);
	ClosMakeType1_(ptr, SHORT_FLOAT, FLOAT);
	ClosMakeType1_(ptr, SINGLE_FLOAT, FLOAT);
	ClosMakeType1_(ptr, DOUBLE_FLOAT, FLOAT);
	ClosMakeType1_(ptr, LONG_FLOAT, FLOAT);
	ClosMakeType1_(ptr, SIGNED_BYTE, INTEGER);
	ClosMakeType1_(ptr, UNSIGNED_BYTE, SIGNED_BYTE);
	ClosMakeType1_(ptr, BIT, UNSIGNED_BYTE);
	ClosMakeType1_(ptr, COMPILED_FUNCTION, FUNCTION);
	ClosMakeType1_(ptr, KEYWORD, SYMBOL);

	Return(clos_stdtype_buildin0_(ptr, CONSTANT_SYSTEM_PAPER, CONSTANT_CLOS_PAPER));

	return 0;
}


/*
 *  Metaobject Protocol
 */
static void clos_mop_slot_definition_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 9);
	SlotMakeNameSymbol_number(slots, NAME, 0);
	SlotMakeName_number(slots, TYPE, 1);
	SlotMakeName_number(slots, ALLOCATION, 2);
	SlotMakeName_number(slots, INITARGS, 3);
	SlotMakeName_number(slots, INITFORM, 4);
	SlotMakeForm_number(slots, INITFUNCTION, 5);
	SlotMakeForm_number(slots, DOCUMENTATION, 6);
	SlotMakeForm_number(slots, READERS, 7);
	SlotMakeForm_number(slots, WRITERS, 8);
	slotvector_set_location(slots);
	*ret = slots;
}

static int build_clos_class_mop_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* slot-definition */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION,
			CLOS_METAOBJECT);
	/* direct-slot-definition */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_DIRECT_SLOT_DEFINITION,
			CLOS_DIRECT_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* effective-slot-definition */
	ClosMakeClass1_(ptr, metaclass,
			CLOSNAME_EFFECTIVE_SLOT_DEFINITION,
			CLOS_EFFECTIVE_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* standard-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_STANDARD_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* standard-direct-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClass2Slot_(ptr, metaclass, slots,
			CLOSNAME_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_DIRECT_SLOT_DEFINITION);
	/* standard-effective-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClass2Slot_(ptr, metaclass, slots,
			CLOSNAME_STANDARD_EFFECTIVE_SLOT_DEFINITION,
			CLOS_STANDARD_EFFECTIVE_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_EFFECTIVE_SLOT_DEFINITION);

	return 0;
}


/*
 *  build-clos-class
 */
static void build_clos_class_variable(void)
{
	addr pos;

	/* standard-class */
#ifdef LISP_DEBUG
	GetConst(CLOS_STANDARD_CLASS, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_standard_class != pos, "error.");
#endif

	/* standard-generic-function */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_generic = pos;

	/* standard-method */
	GetConst(CLOS_STANDARD_METHOD, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_method = pos;

	/* method-combination */
	GetConst(CLOS_METHOD_COMBINATION, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_combination = pos;

	/* eql-specializer */
	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	CheckType(pos, LISPTYPE_CLOS);
	Clos_standard_specializer = pos;
}

static int build_clos_class_call_(Execute ptr)
{
	build_clos_class_init();
	Return(build_clos_class_standard_(ptr));
	Return(build_clos_class_generic_(ptr));
	Return(build_clos_class_method_(ptr));
	Return(build_clos_class_combination_(ptr));
	Return(build_clos_class_specializer_(ptr));
	Return(build_clos_class_condition_(ptr));
	Return(build_clos_class_system_(ptr));
	Return(build_clos_class_type_(ptr));
	Return(build_clos_class_mop_(ptr));
	build_clos_class_variable();

	return 0;
}

void build_clos_class(Execute ptr)
{
	Error(build_clos_class_call_(ptr));
}


/*
 *  debug
 */
int clos_subclass_p_debug(addr clos, addr super)
{
	int check;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	Error(clos_subclass_p_(ptr, clos, super, &check));

	return check;
}

int clos_subtype_p_debug(addr clos, addr super)
{
	int check;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	Error(clos_subtype_p_(ptr, clos, super, &check));

	return check;
}

int clos_generic_p_debug(addr clos)
{
	int check;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	Error(clos_generic_p_(ptr, clos, &check));

	return check;
}

int clos_method_p_debug(addr clos)
{
	int check;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	Error(clos_method_p_(ptr, clos, &check));

	return check;
}

int clos_define_combination_p_debug(addr clos)
{
	int check;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	Error(clos_define_combination_p_(ptr, clos, &check));

	return check;
}

