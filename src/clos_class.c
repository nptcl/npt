#include "clos.h"
#include "clos_class.h"
#include "clos_combination.h"
#include "clos_generic.h"
#include "clos_method.h"
#include "clos_type.h"
#include "condition.h"
#include "cons.h"
#include "control.h"
#include "equal.h"
#include "function.h"
#include "hashtable.h"
#include "integer.h"
#include "lambda.h"
#include "mop.h"
#include "pointer.h"
#include "sequence.h"
#include "symbol.h"
#include "type.h"
#include "type_table.h"

/*
 *  access
 */
static void stdget_class_constant(addr pos, addr *ret,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_checkelt(pos, (size_t)index1, ret);
	}
	else {
		GetConstant(index2, &check);
		clos_check(pos, check, ret);
	}
}

static void stdset_class_constant(addr pos, addr value,
		enum Clos_class_Index index1, constindex index2)
{
	addr clos, check;

	CheckType(pos, LISPTYPE_CLOS);
	Check(Clos_class_size <= index1, "index error");
	GetClassOfClos(pos, &clos);
	Check(clos == Unbound, "unbound error");
	GetConst(CLOS_STANDARD_CLASS, &check);
	if (clos == check) {
		Check(clos_errorp(pos, (size_t)index1, index2), "index error");
		clos_setelt(pos, (size_t)index1, value);
	}
	else {
		GetConstant(index2, &check);
		clos_set(pos, check, value);
	}
}
#define StdGetClass(p,r,a,b) \
	stdget_class_constant((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)
#define StdSetClass(p,r,a,b) \
	stdset_class_constant((p), (r), Clos_class_##a, CONSTANT_CLOSNAME_##b)

_g void stdget_class_name(addr pos, addr *ret)
{
	StdGetClass(pos, ret, name, NAME);
}
_g void stdset_class_name(addr pos, addr value)
{
	StdSetClass(pos, value, name, NAME);
}

_g void stdget_class_direct_slots(addr pos, addr *ret)
{
	StdGetClass(pos, ret, direct_slots, DIRECT_SLOTS);
}
_g void stdset_class_direct_slots(addr pos, addr value)
{
	StdSetClass(pos, value, direct_slots, DIRECT_SLOTS);
}

_g void stdget_class_direct_subclasses(addr pos, addr *ret)
{
	StdGetClass(pos, ret, direct_subclasses, DIRECT_SUBCLASSES);
}
_g void stdset_class_direct_subclasses(addr pos, addr value)
{
	StdSetClass(pos, value, direct_subclasses, DIRECT_SUBCLASSES);
}

_g void stdget_class_direct_superclasses(addr pos, addr *ret)
{
	StdGetClass(pos, ret, direct_superclasses, DIRECT_SUPERCLASSES);
}
_g void stdset_class_direct_superclasses(addr pos, addr value)
{
	StdSetClass(pos, value, direct_superclasses, DIRECT_SUPERCLASSES);
}

_g void stdget_class_precedence_list(addr pos, addr *ret)
{
	StdGetClass(pos, ret, precedence_list, CLASS_PRECEDENCE_LIST);
}
_g void stdset_class_precedence_list(addr pos, addr value)
{
	StdSetClass(pos, value, precedence_list, CLASS_PRECEDENCE_LIST);
}

_g void stdget_class_slots(addr pos, addr *ret)
{
	StdGetClass(pos, ret, slots, EFFECTIVE_SLOTS);
}
_g void stdset_class_slots(addr pos, addr value)
{
	StdSetClass(pos, value, slots, EFFECTIVE_SLOTS);
}

_g void stdget_class_finalized_p(addr pos, addr *ret)
{
	StdGetClass(pos, ret, finalized_p, FINALIZED_P);
}
_g void stdset_class_finalized_p(addr pos, addr value)
{
	StdSetClass(pos, value, finalized_p, FINALIZED_P);
}

_g void stdget_class_prototype(addr pos, addr *ret)
{
	StdGetClass(pos, ret, prototype, PROTOTYPE);
}
_g void stdset_class_prototype(addr pos, addr value)
{
	StdSetClass(pos, value, prototype, PROTOTYPE);
}

_g void stdget_class_direct_methods(addr pos, addr *ret)
{
	StdGetClass(pos, ret, direct_methods, DIRECT_METHODS);
}
_g void stdset_class_direct_methods(addr pos, addr value)
{
	StdSetClass(pos, value, direct_methods, DIRECT_METHODS);
}

_g void stdget_class_default_initargs(addr pos, addr *ret)
{
	StdGetClass(pos, ret, default_initargs, DEFAULT_INITARGS);
}
_g void stdset_class_default_initargs(addr pos, addr value)
{
	StdSetClass(pos, value, default_initargs, DEFAULT_INITARGS);
}

_g void stdget_class_direct_default_initargs(addr pos, addr *ret)
{
	StdGetClass(pos, ret, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}
_g void stdset_class_direct_default_initargs(addr pos, addr value)
{
	StdSetClass(pos, value, direct_default_initargs, DIRECT_DEFAULT_INITARGS);
}

_g void stdget_class_version(addr pos, addr *ret)
{
	StdGetClass(pos, ret, version, VERSION);
}
_g void stdset_class_version(addr pos, addr value)
{
	StdSetClass(pos, value, version, VERSION);
}

_g void stdget_class_document(addr pos, addr *ret)
{
	StdGetClass(pos, ret, document, DOCUMENTATION);
}
_g void stdset_class_document(addr pos, addr value)
{
	StdSetClass(pos, value, document, DOCUMENTATION);
}

_g void stdget_class_redefined_class(addr pos, addr *ret)
{
	StdGetClass(pos, ret, redefined_class, REDEFINED_CLASS);
}
_g void stdset_class_redefined_class(addr pos, addr value)
{
	StdSetClass(pos, value, redefined_class, REDEFINED_CLASS);
}


/*
 *  check
 */
_g int clos_subclass_p(addr clos, addr super)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	stdget_class_precedence_list(clos, &clos);

	return find_list_eq_unsafe(super, clos);
}

_g int clos_subtype_p(addr clos, addr super)
{
	CheckType(clos, LISPTYPE_CLOS);
	CheckType(super, LISPTYPE_CLOS);
	GetClassOfClos(clos, &clos);
	Check(clos == Unbound, "unbound error");

	return clos_subclass_p(clos, super);
}

static int clos_constant_p(addr clos, constindex index)
{
	addr super;

	if (! closp(clos)) return 0;
	GetConstant(index, &super);
	return clos_subtype_p(clos, super);
}

_g int clos_class_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_CLASS);
}

_g int clos_funcallable_p(addr clos)
{
	addr super;

	if (! closp(clos)) return 0;
	if (clos_funcall_p(clos)) return 1;
	GetConst(CLOS_FUNCALLABLE_STANDARD_OBJECT, &super);
	return clos_subtype_p(clos, super);
}

_g int clos_generic_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_GENERIC_FUNCTION);
}

_g int clos_method_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_METHOD);
}

_g int clos_define_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_DEFINE_METHOD_COMBINATION);
}

_g int clos_define_long_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_DEFINE_LONG_METHOD_COMBINATION);
}

_g int clos_define_short_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_DEFINE_SHORT_METHOD_COMBINATION);
}

_g int clos_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_METHOD_COMBINATION);
}

_g int clos_long_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_LONG_METHOD_COMBINATION);
}

_g int clos_short_combination_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_SHORT_METHOD_COMBINATION);
}

_g int clos_specializer_p(addr clos)
{
	return clos_constant_p(clos, CONSTANT_CLOS_EQL_SPECIALIZER);
}

_g int clos_referenced_p(addr clos)
{
	addr super;

	if (! closp(clos)) return 0;
	GetConst(CLOS_FORWARD_REFERENCED_CLASS, &super);
	clos_class_of(clos, &clos);
	return clos == super;
}

_g int clos_built_p(addr clos)
{
	addr super;

	if (! closp(clos)) return 0;
	GetConst(CLOS_BUILT_IN_CLASS, &super);
	return clos_subtype_p(clos, super);
}

_g int funcallp(addr pos)
{
	switch (GetType(pos)) {
		case LISPTYPE_FUNCTION:
			return 1;

		case LISPTYPE_CLOS:
			return clos_funcall_p(pos) || clos_funcallable_p(pos);

		default:
			return 0;
	}
}


/*
 *  make-instance
 */
static int clos_find_slotname(addr slots, size_t size, addr name)
{
	addr check;
	size_t i;

	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		GetNameSlot(check, &check);
		if (check == name)
			return 1;
	}

	return 0;
}

static void clos_instance_unsafe(LocalRoot local, addr clos, addr slots, addr *ret)
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
		GetNameSlot(slot, &check);
		if (! symbolp(check))
			fmte("The slot name ~S must be a symbol.", check, NULL);
		/* already exist */
		if (clos_find_slotname(slots, i, check))
			fmte("The slot name ~S already exists.", check, NULL);
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i)
			fmte("The slot location ~A is invalid.", intsizeh(i), NULL);
		/* allocation */
		if (slot_class_p(slot))
			fmte("The allocation must be an :INSTANCE.", NULL);
		/* value */
		GetFormSlot(slot, &check);
		SetClosValue(value, i, check);
	}
	*ret = instance;
}
_g void clos_instance_alloc(LocalRoot local, addr clos, addr *ret)
{
	addr pos;

	/* finalized-p check */
	stdget_class_finalized_p(clos, &pos);
	if (pos == Nil)
		fmte("The class ~S is not finalized.", clos, NULL);

	/* make-instance */
	stdget_class_slots(clos, &pos);
	clos_instance_unsafe(local, clos, pos, ret);
}
_g void clos_instance_local(LocalRoot local, addr clos, addr *ret)
{
	CheckLocal(local);
	clos_instance_alloc(local, clos, ret);
}
_g void clos_instance_heap(addr clos, addr *ret)
{
	clos_instance_alloc(NULL, clos, ret);
}


/*
 *  class-precedence-list
 */
static void clos_precedence_classes(LocalRoot local, addr right, addr *ret)
{
	addr left, list;

	conscar_local(local, &list, right);
	stdget_class_direct_superclasses(right, &right);
	while (right != Nil) {
		GetCons(right, &left, &right);
		cons_local(local, &list, left, list);
	}
	cons_local(local, &list, Unbound, list);
	nreverse_list_unsafe(ret, list);
}

static void clos_precedence_pair(LocalRoot local, addr right, addr *ret)
{
	addr child, result, cons, left, temp;

	clos_precedence_classes(local, right, &right);
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

static void clos_precedence_super(LocalRoot local, addr pos, addr *ret)
{
	addr stack, result, temp, supers, super;

	/* first list */
	result = Nil;
	conscar_local(local, &stack, pos);

	/* loop */
	while (stack != Nil) {
		for (temp = Nil; stack != Nil; ) {
			GetCons(stack, &supers, &stack);
			if (! find_list_eq_unsafe(supers, result)) {
				cons_local(local, &result, supers, result);
				stdget_class_direct_superclasses(supers, &supers);
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

static int clos_precedence_find(addr a1, addr cons)
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

static void clos_precedence_chain(LocalRoot local, addr pos, addr *ret)
{
	addr one, chain, result;

	clos_precedence_super(local, pos, &pos);
	for (result = Nil; pos != Nil; ) {
		GetCons(pos, &one, &pos);
		clos_precedence_pair(local, one, &one);
		while (one != Nil) {
			GetCons(one, &chain, &one);
			/* pushnew */
			if (! clos_precedence_find(chain, result))
				cons_local(local, &result, chain, result);
		}
	}
	*ret = result;
}

static void clos_precedence_top(addr cons, addr *ret)
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

static void clos_precedence_remove(addr key, addr right2, addr *ret)
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

static void clos_precedence_result(LocalRoot local, addr pos, addr *ret)
{
	addr list, key;

	clos_precedence_chain(local, pos, &pos);
	for (list = Nil; pos != Nil; ) {
		clos_precedence_top(pos, &key);
		cons_heap(&list, key, list);
		clos_precedence_remove(key, pos, &pos);
	}
	nreverse_list_unsafe(ret, list);
}

static void clos_precedence_list(LocalRoot local, addr clos, addr *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(clos, LISPTYPE_CLOS);
	push_local(local, &stack);
	clos_precedence_result(local, clos, ret);
	rollback_local(local, stack);
}


/*
 *  compute-slots
 */
static int clos_slots_name(addr *ret, addr name, addr list)
{
	addr pos, check, next;

	for (; list != Nil; list = next) {
		GetCons(list, &pos, &next);
		GetNameSlot(pos, &check);
		if (name == check) {
			*ret = pos;
			return 1;
		}
	}

	return 0;
}

static void clos_slots_push(addr pos, addr check)
{
	addr list, a;

	GetArgsSlot(pos, &list);
	GetArgsSlot(check, &check);
	while (check != Nil) {
		getcons(check, &a, &check);
		pushnew_heap(list, a, &list);
	}
	SetArgsSlot(pos, list);
}

static void clos_slots_loop(LocalRoot local, addr list, addr *ret, size_t *rsize)
{
	addr root, slots, a, b;
	size_t count, i, size;

	root = Nil;
	count = 0;
	while (list != Nil) {
		GetCons(list, &slots, &list);
		stdget_class_direct_slots(slots, &slots);
		LenSlotVector(slots, &size);
		for (i = 0; i < size; i++) {
			GetSlotVector(slots, i, &a);
			GetNameSlot(a, &b);
			if (clos_slots_name(&b, b, root)) {
				clos_slots_push(b, a);
			}
			else {
				slot_copy_heap(&a, a);
				cons_local(local, &root, a, root);
				count++;
			}
		}
	}
	nreverse_list_unsafe(ret, root);
	*rsize = count;
}

static void clos_slots_gather(LocalRoot local, addr clos, addr *ret)
{
	addr slots, pos;
	size_t i;

	stdget_class_precedence_list(clos, &clos);
	clos_slots_loop(local, clos, &clos, &i);
	slot_vector_heap(&slots, i);
	for (i = 0; clos != Nil; i++) {
		GetCons(clos, &pos, &clos);
		SetSlotVector(slots, i, pos);
		SetLocationSlot(pos, i);
	}
	*ret = slots;
}

static void clos_compute_slots(LocalRoot local, addr clos, addr *ret)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(clos, LISPTYPE_CLOS);
	push_local(local, &stack);
	clos_slots_gather(local, clos, ret);
	rollback_local(local, stack);
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
		fmte("STANDARD-CLASS is already exist.", NULL);
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
	SetNameSlot(slot, value);
	/* type */
	GetConst(CLOSDATA_SYMBOL_TYPE, &value);
	CheckType(value, LISPTYPE_TYPE);
	SetTypeSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
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
	SetNameSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
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
	SetNameSlot(slot, value);
	/* initform */
	SetFormSlot(slot, Nil);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
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
	SetNameSlot(slot, value);
	/* initform */
	fixnum_heap(&value, 0);
	SetFormSlot(slot, value);
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	SetArgsSlot(slot, value);
	/* result */
	SetSlotVector(pos, n3, slot);
}
#define SlotMakeVersion(x,y,z) \
	slot_make_version((x), CONSTANT_CLOSNAME_##y, CONSTANT_CLOSKEY_##y, Clos_##z)

_g void slotvector_set_location(addr slots)
{
	addr pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		SetLocationSlot(pos, i);
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
	SlotMakeForm(slots, DIRECT_METHODS, class_direct_methods);
	SlotMakeForm(slots, DEFAULT_INITARGS, class_default_initargs);
	SlotMakeForm(slots, DIRECT_DEFAULT_INITARGS, class_direct_default_initargs);
	SlotMakeVersion(slots, VERSION, class_version);
	SlotMakeForm(slots, DOCUMENTATION, class_document);
	SlotMakeForm(slots, REDEFINED_CLASS, class_redefined_class);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdclass_direct_slots(addr instance, addr slots)
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
		GetClassSlot(slot, &check);
		Check(check != Nil, "slot class error");
#endif
		SetClassSlot(slot, instance);
	}
	stdset_class_direct_slots(instance, slots);
}

static void clos_stdclass_dummy(addr *ret, addr slots)
{
	addr instance;

	clos_heap(&instance, slots);
	SetClassOfClos(instance, Nil);
	clos_stdclass_direct_slots(instance, slots);
	stdset_class_slots(instance, slots);
	stdset_class_finalized_p(instance, T);
	*ret = instance;
}

static void clos_stdclass_make(addr *ret, addr clos, addr name, addr slots)
{
	addr instance;

	Check(! symbolp(name), "type error");
	clos_instance_heap(clos, &instance);
	stdset_class_name(instance, name);
	clos_stdclass_direct_slots(instance, slots);
	stdset_class_prototype(instance, instance);
	*ret = instance;
}

static void clos_stdclass_empty(addr *ret, addr clos, addr name)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	clos_stdclass_make(ret, clos, name, slots);
}

static void clos_stdclass_class_of(addr instance, addr class_of)
{
	fixnum version;

	SetClassOfClos(instance, class_of);
	GetVersionClos(class_of, &version);
	SetVersionClos(instance, version);
}

static int list_referenced_check(addr list)
{
	addr check;

	while (list != Nil) {
		getcons(list, &check, &list);
		if (clos_referenced_p(check))
			return 1;
	}

	return 0;
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
			GetClassSlot(slot, &check);
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
			GetFormSlot(slot, &value);
			SetClosValue(pos, i, value);
		}
	}
}

static void clos_stdclass_prototype(addr clos)
{
	addr pos, slots, value;

	/* make prototype */
	stdget_class_slots(clos, &slots);
	slot_vector_prototype(clos, slots, &value);
	clos_heap(&pos, value);
	SetClassOfClos(pos, clos);
	/* initialize shared slots */
	clos_stdclass_prototype_initialize(pos, slots);
	/* result */
	stdset_class_prototype(clos, pos);
}

static void clos_stdclass_inherit(LocalRoot local, addr pos, addr clos, addr supers)
{
	addr list, super;

	/* class-of */
	clos_stdclass_class_of(pos, clos);
	/* direct-superclasses */
	stdset_class_direct_superclasses(pos, supers);
	/* forward-referenced-class check */
	if (! list_referenced_check(supers)) {
		/* class-precedence-list */
		clos_precedence_list(local, pos, &list);
		stdset_class_precedence_list(pos, list);
		/* effective-slots */
		clos_compute_slots(local, pos, &list);
		stdset_class_slots(pos, list);
		/* finalized-p */
		clos_stdclass_prototype(pos);
		stdset_class_finalized_p(pos, T);
	}
	/* direct-subclasses */
	while (supers != Nil) {
		GetCons(supers, &super, &supers);
		if (! clos_referenced_p(super)) {
			stdget_class_direct_subclasses(super, &list);
			cons_heap(&list, pos, list);
			stdset_class_direct_subclasses(super, list);
		}
	}
	/* setf-find-class */
	stdget_class_name(pos, &list);
	clos_define_class(list, pos);
}

static void clos_stdclass_single(LocalRoot local, addr pos, addr clos, addr super)
{
	conscar_heap(&super, super);
	clos_stdclass_inherit(local, pos, clos, super);
}

static void clos_stdclass_metaclass(LocalRoot local, addr *ret)
{
	addr slots, name;
	addr builtin, tc, stdobject, classc, stdclass;

	/* make dummy metaclass */
	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&stdclass, slots);
	/* make class class */
	GetConst(COMMON_CLASS, &name);
	slot_vector_clear(slots);
	clos_stdclass_make(&classc, stdclass, name, slots);
	clos_stdclass_inherit(local, classc, classc, Nil);
	/* make t class */
	clos_stdclass_empty(&tc, classc, T);
	clos_stdclass_inherit(local, tc, classc, Nil);
	GetConst(COMMON_STANDARD_OBJECT, &name);
	/* make standard-object */
	clos_stdclass_empty(&stdobject, classc, name);
	clos_stdclass_single(local, stdobject, classc, tc);
	clos_stdclass_single(local, classc, classc, stdobject);
	/* make standard-class */
	GetConst(COMMON_STANDARD_CLASS, &name);
	clos_stdclass_empty(&stdclass, classc, name);
	clos_stdclass_single(local, stdclass, stdclass, classc);
	Clos_standard_class = stdclass;
	/* make built-in-class */
	GetConst(COMMON_BUILT_IN_CLASS, &name);
	clos_stdclass_empty(&builtin, classc, name);
	clos_stdclass_single(local, builtin, stdclass, classc);
	/* update class-of */
	clos_stdclass_class_of(tc, builtin);
	clos_stdclass_class_of(classc, stdclass);
	clos_stdclass_class_of(stdobject, stdclass);
	clos_stdclass_class_of(stdclass, stdclass);
	clos_stdclass_class_of(builtin, stdclass);
	/* constant */
	SetConst(CLOS_T, tc);
	SetConst(CLOS_CLASS, classc);
	SetConst(CLOS_STANDARD_OBJECT, stdobject);
	SetConst(CLOS_STANDARD_CLASS, stdclass);
	SetConst(CLOS_BUILT_IN_CLASS, builtin);
	/* result */
	*ret = stdclass;
}

static void clos_stdclass_supers(LocalRoot local,
		addr *ret, addr metaclass, addr name, addr slots, addr supers)
{
	addr instance;

	clos_stdclass_make(&instance, metaclass, name, slots);
	clos_stdclass_inherit(local, instance, metaclass, supers);
	*ret = instance;
}

static void clos_stdclass_type(LocalRoot local,
		addr *ret, addr metaclass, addr name, addr supers)
{
	addr slots;
	slot_vector_heap(&slots, 0);
	clos_stdclass_supers(local, ret, metaclass, name, slots, supers);
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
	nreverse_list_unsafe(ret, list);
}

static void clos_stdclass_va(LocalRoot local, addr m, constindex n, constindex c, ...)
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
	clos_stdclass_type(local, &clos, m, name, list);
	clos_define_class(name, clos);
	SetConstant(c, clos);
}

#define ClosMakeClass1(p,m,a,b,c) \
	clos_stdclass_va((p),(m),CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_EMPTY)
#define ClosMakeClass2(p,m,a,b,c,d) { \
	clos_stdclass_va((p),(m), \
			CONSTANT_##a,CONSTANT_##b,CONSTANT_##c,CONSTANT_##d,CONSTANT_EMPTY); \
}

_g void clos_stdclass_slotsconstant(LocalRoot local, addr metaclass, addr slots,
		constindex n, constindex c, constindex s)
{
	addr name, supers, clos;

	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	GetConstant(s, &supers);
	CheckType(supers, LISPTYPE_CLOS);
	conscar_heap(&supers, supers);
	clos_stdclass_supers(local, &clos, metaclass, name, slots, supers);
	clos_define_class(name, clos);
	SetConstant(c, clos);
}
#define ClosMakeClassSlot(p,m,s,a,b,c) \
	clos_stdclass_slotsconstant((p),(m),(s),CONSTANT_##a,CONSTANT_##b,CONSTANT_##c);

static void clos_structure_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_structure_size);
	SlotMakeNameSymbol(slots, NAME, structure_name);
	SlotMakeForm(slots, SLOTS, structure_slots);
	SlotMakeForm(slots, DOCUMENTATION, structure_documentation);
	SlotMakeForm(slots, INCLUDE, structure_include);
	SlotMakeName(slots, CLASS_PRECEDENCE_LIST, structure_precedence_list);
	SlotMakeName(slots, TYPE, structure_type);
	SlotMakeName(slots, VECTOR, structure_vector);
	SlotMakeName(slots, NAMED, structure_named);
	SlotMakeName(slots, NAMED_INDEX, structure_named_index);
	SlotMakeName(slots, VALUE, structure_value);
	slotvector_set_location(slots);
	*ret = slots;
}

static void build_clos_class_standard(LocalRoot local)
{
	addr metaclass, structure, slots;

	/* standard-class, others */
	clos_stdclass_metaclass(local, &metaclass);
	/* structure-class */
	clos_structure_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_STRUCTURE_CLASS,
			CLOS_STRUCTURE_CLASS,
			CLOS_CLASS);
	/* structure-object */
	GetConst(CLOS_STRUCTURE_CLASS, &structure);
	ClosMakeClass1(local, structure,
			COMMON_STRUCTURE_OBJECT,
			CLOS_STRUCTURE_OBJECT,
			CLOS_T);
	/* forward-referenced-class */
	clos_stdclass_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_FORWARD_REFERENCED_CLASS,
			CLOS_FORWARD_REFERENCED_CLASS,
			CLOS_CLASS);
}


/*
 *  standard-generic-function
 */
static void clos_stdgeneric_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_generic_size);
	SlotMakeNameSymbol(slots, NAME, generic_name);
	SlotMakeName(slots, LAMBDA_LIST, generic_lambda_list);
	SlotMakeForm(slots, METHODS, generic_methods);
	SlotMakeName(slots, METHOD_CLASS, generic_method_class);
	SlotMakeName(slots, ARGUMENT_PRECEDENCE_ORDER, generic_argument_precedence_order);
	SlotMakeForm(slots, DECLARATIONS, generic_declarations);
	SlotMakeForm(slots, METHOD_COMBINATION, generic_method_combination);
	SlotMakeName(slots, EQLCHECK, generic_eqlcheck);
	SlotMakeName(slots, CACHE, generic_cache);
	SlotMakeName(slots, CALL, generic_call);
	slotvector_set_location(slots);
	*ret = slots;
}

static void build_clos_class_generic(LocalRoot local)
{
	addr metaclass, builtin, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	GetConst(CLOS_BUILT_IN_CLASS, &builtin);
	/* funcallable-standard-object */
	ClosMakeClass1(local, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_FUNCALLABLE_STANDARD_OBJECT,
			CLOS_STANDARD_OBJECT);
	/* funcallable-standard-class */
	ClosMakeClass2(local, metaclass,
			CLOSNAME_FUNCALLABLE_STANDARD_CLASS,
			CLOS_FUNCALLABLE_STANDARD_CLASS,
			CLOS_CLASS,
			CLOS_FUNCALLABLE_STANDARD_OBJECT);
	/* function */
	ClosMakeClass1(local, builtin,
			COMMON_FUNCTION,
			CLOS_FUNCTION,
			CLOS_T);
	/* generic-function */
	ClosMakeClass2(local, metaclass,
			COMMON_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION,
			CLOS_FUNCTION,
			CLOS_FUNCALLABLE_STANDARD_OBJECT);
	/* standard-generic-function */
	clos_stdgeneric_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_STANDARD_GENERIC_FUNCTION,
			CLOS_STANDARD_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION);
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

static void build_clos_class_method(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method */
	ClosMakeClass1(local, metaclass,
			COMMON_METHOD,
			CLOS_METHOD,
			CLOS_STANDARD_OBJECT);
	/* standard-method */
	clos_stdmethod_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_STANDARD_METHOD,
			CLOS_STANDARD_METHOD,
			CLOS_METHOD);
}


/*
 *  method-combination
 */
static void clos_stdlongcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longcomb_size);
	SlotMakeNameSymbol(slots, NAME, longcomb_name);
	SlotMakeName(slots, DOCUMENTATION, longcomb_document);
	SlotMakeName(slots, LAMBDA_LIST, longcomb_lambda_list);
	SlotMakeName(slots, BINDING, longcomb_binding);
	SlotMakeName(slots, QUALIFIERS, longcomb_qualifiers);
	SlotMakeName(slots, ARGUMENTS, longcomb_arguments);
	SlotMakeName(slots, GENERIC, longcomb_generic);
	SlotMakeName(slots, FORM, longcomb_form);
	SlotMakeName(slots, FUNCTION, longcomb_function);
	slotvector_set_location(slots);
	*ret = slots;
}

static void clos_stdshortcomb_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortcomb_size);
	SlotMakeNameSymbol(slots, NAME, shortcomb_name);
	SlotMakeName(slots, DOCUMENTATION, shortcomb_document);
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
	SlotMakeName(slots, DOCUMENTATION, longdef_document);
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
	SlotMakeName(slots, DOCUMENTATION, shortdef_document);
	SlotMakeName(slots, IDENTITY, shortdef_identity);
	SlotMakeName(slots, OPERATOR, shortdef_operator);
	slotvector_set_location(slots);
	*ret = slots;
}

static void build_clos_class_combination(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* method-combination */
	ClosMakeClass1(local, metaclass,
			COMMON_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION,
			CLOS_STANDARD_OBJECT);
	/* long-method-combination */
	clos_stdlongcomb_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_LONG_METHOD_COMBINATION,
			CLOS_LONG_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* short-method-combination */
	clos_stdshortcomb_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_SHORT_METHOD_COMBINATION,
			CLOS_SHORT_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* define-method-combination */
	ClosMakeClass1(local, metaclass,
			COMMON_DEFINE_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION,
			CLOS_STANDARD_OBJECT);
	/* define-long-method-combination */
	clos_stdlongdef_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);
	/* define-short-method-combination */
	clos_stdshortdef_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);
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

static void build_clos_class_specializer(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* specializer */
	clos_stdspecializer_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_EQL_SPECIALIZER,
			CLOS_EQL_SPECIALIZER,
			CLOS_STANDARD_OBJECT);
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

static void build_clos_class_condition(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* condition */
	ClosMakeClass1(local, metaclass,
			COMMON_CONDITION,
			CLOS_CONDITION,
			CLOS_STANDARD_OBJECT);
	/* serious-condition (condition) */
	ClosMakeClass1(local, metaclass,
			COMMON_SERIOUS_CONDITION,
			CONDITION_SERIOUS_CONDITION,
			CLOS_CONDITION);
	/* simple-condition (condition) :format-control :format-arguments*/
	SlotMakeCondition2(&slots, FORMAT_CONTROL, FORMAT_ARGUMENTS);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_SIMPLE_CONDITION,
			CONDITION_SIMPLE_CONDITION,
			CLOS_CONDITION);
	/* warning (condition) */
	ClosMakeClass1(local, metaclass,
			COMMON_WARNING,
			CONDITION_WARNING,
			CLOS_CONDITION);
	/* error (serious-condition) */
	ClosMakeClass1(local, metaclass,
			COMMON_ERROR,
			CONDITION_ERROR,
			CONDITION_SERIOUS_CONDITION);
	/* storage-condition (serious-condition) */
	ClosMakeClass1(local, metaclass,
			COMMON_STORAGE_CONDITION,
			CONDITION_STORAGE_CONDITION,
			CONDITION_SERIOUS_CONDITION);
	/* arithmetic-error (error) :operation :operands */
	SlotMakeCondition2(&slots, OPERATION, OPERANDS);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_ARITHMETIC_ERROR,
			CONDITION_ARITHMETIC_ERROR,
			CONDITION_ERROR);
	/* cell-error (error) :name */
	SlotMakeCondition1(&slots, NAME);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_CELL_ERROR,
			CONDITION_CELL_ERROR,
			CONDITION_ERROR);
	/* control-error (error) */
	ClosMakeClass1(local, metaclass,
			COMMON_CONTROL_ERROR,
			CONDITION_CONTROL_ERROR,
			CONDITION_ERROR);
	/* file-error (error) :pathname */
	SlotMakeCondition1(&slots, PATHNAME);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_FILE_ERROR,
			CONDITION_FILE_ERROR,
			CONDITION_ERROR);
	/* package-error (error) :package */
	SlotMakeCondition1(&slots, PACKAGE);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_PACKAGE_ERROR,
			CONDITION_PACKAGE_ERROR,
			CONDITION_ERROR);
	/* parse-error (error) */
	ClosMakeClass1(local, metaclass,
			COMMON_PARSE_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_ERROR);
	/* print-not-readable (error) :object */
	SlotMakeCondition1(&slots, OBJECT);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_PRINT_NOT_READABLE,
			CONDITION_PRINT_NOT_READABLE,
			CONDITION_ERROR);
	/* program-error (error) */
	ClosMakeClass1(local, metaclass,
			COMMON_PROGRAM_ERROR,
			CONDITION_PROGRAM_ERROR,
			CONDITION_ERROR);
	/* stream-error (error) :stream */
	SlotMakeCondition1(&slots, STREAM);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_STREAM_ERROR,
			CONDITION_STREAM_ERROR,
			CONDITION_ERROR);
	/* type-error (error) :datum :expected-type */
	SlotMakeCondition2(&slots, DATUM, EXPECTED_TYPE);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_TYPE_ERROR,
			CONDITION_TYPE_ERROR,
			CONDITION_ERROR);
	/* unbound-slot (cell-error) :instance :name */
	SlotMakeCondition1(&slots, INSTANCE);
	ClosMakeClassSlot(local, metaclass, slots,
			COMMON_UNBOUND_SLOT,
			CONDITION_UNBOUND_SLOT,
			CONDITION_CELL_ERROR);
	/* unbound-variable (cell-error) :name */
	ClosMakeClass1(local, metaclass,
			COMMON_UNBOUND_VARIABLE,
			CONDITION_UNBOUND_VARIABLE,
			CONDITION_CELL_ERROR);
	/* undefined-function (cell-error) :name */
	ClosMakeClass1(local, metaclass,
			COMMON_UNDEFINED_FUNCTION,
			CONDITION_UNDEFINED_FUNCTION,
			CONDITION_CELL_ERROR);
	/* style-warning (warning) */
	ClosMakeClass1(local, metaclass,
			COMMON_STYLE_WARNING,
			CONDITION_STYLE_WARNING,
			CONDITION_WARNING);
	/* simple-error (simple-condition error) :format-control :format-arguments */
	ClosMakeClass2(local, metaclass,
			COMMON_SIMPLE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_ERROR);
	/* simple_type_error (simple_condition type_error)
	 *   :format-control :format-arguments :datum :expected-type */
	ClosMakeClass2(local, metaclass,
			COMMON_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_TYPE_ERROR,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_TYPE_ERROR);
	/* simple-warning (simple-condition warning) :format-control :format-arguments */
	ClosMakeClass2(local, metaclass,
			COMMON_SIMPLE_WARNING,
			CONDITION_SIMPLE_WARNING,
			CONDITION_SIMPLE_CONDITION,
			CONDITION_WARNING);
	/* division-by-zero (arithmetic-error) :operation :operands */
	ClosMakeClass1(local, metaclass,
			COMMON_DIVISION_BY_ZERO,
			CONDITION_DIVISION_BY_ZERO,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-inexact (arithmetic-error) :operation :operands */
	ClosMakeClass1(local, metaclass,
			COMMON_FLOATING_POINT_INEXACT,
			CONDITION_FLOATING_POINT_INEXACT,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-invalid-operation (arithmetic-error) :operation :operands */
	ClosMakeClass1(local, metaclass,
			COMMON_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_FLOATING_POINT_INVALID_OPERATION,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-overflow (arithmetic-error) :operation :operands */
	ClosMakeClass1(local, metaclass,
			COMMON_FLOATING_POINT_OVERFLOW,
			CONDITION_FLOATING_POINT_OVERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* floating-point-underflow (arithmetic-error) :operation :operands */
	ClosMakeClass1(local, metaclass,
			COMMON_FLOATING_POINT_UNDERFLOW,
			CONDITION_FLOATING_POINT_UNDERFLOW,
			CONDITION_ARITHMETIC_ERROR);
	/* end-of-file (stream-error) :stream */
	ClosMakeClass1(local, metaclass,
			COMMON_END_OF_FILE,
			CONDITION_END_OF_FILE,
			CONDITION_STREAM_ERROR);
	/* reader-error (parse-error stream-error) :stream */
	ClosMakeClass2(local, metaclass,
			COMMON_READER_ERROR,
			CONDITION_READER_ERROR,
			CONDITION_PARSE_ERROR,
			CONDITION_STREAM_ERROR);
	/* lisp-system::simple-file-error (simple-error file-error) */
	ClosMakeClass2(local, metaclass,
			SYSTEM_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_FILE_ERROR,
			CONDITION_SIMPLE_ERROR,
			CONDITION_FILE_ERROR);
}

static void build_clos_class_system(LocalRoot local)
{
	addr metaclass;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* savecore (serious-condition) */
	ClosMakeClass1(local, metaclass,
			SYSTEM_SAVECORE,
			CONDITION_SAVECORE,
			CONDITION_SERIOUS_CONDITION);
}


/*
 *  lisp type
 */
static void clos_stdtype_buildin(LocalRoot local, constindex n, constindex c, addr list)
{
	addr metaclass, name, clos;

	GetConst(CLOS_BUILT_IN_CLASS, &metaclass);
	GetConstant(n, &name);
	Check(! symbolp(name), "type error");
	clos_stdclass_type(local, &clos, metaclass, name, list);
	clos_define_class(name, clos);
	SetConstant(c, clos);
}

static void clos_stdtype_buildin0(LocalRoot local,
		constindex n, constindex c)
{
	addr list;

	GetConst(CLOS_T, &list);
	list_heap(&list, list, NULL);
	clos_stdtype_buildin(local, n, c, list);
}

static void clos_stdtype_buildin1(LocalRoot local,
		constindex n, constindex c, constindex s1)
{
	addr list, clos1, clos2;

	GetConstant(s1, &clos1);
	GetConst(CLOS_T, &clos2);
	CheckType(clos1, LISPTYPE_CLOS);
	CheckType(clos2, LISPTYPE_CLOS);
	list_heap(&list, clos1, clos2, NULL);
	clos_stdtype_buildin(local, n, c, list);
}

static void clos_stdtype_buildin2(LocalRoot local,
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
	clos_stdtype_buildin(local, n, c, list);
}

#define ClosMakeType0(p,x) clos_stdtype_buildin0(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x)
#define ClosMakeType1(p,x,y) clos_stdtype_buildin1(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, CONSTANT_CLOS_##y)
#define ClosMakeType2(p,x,y,z) clos_stdtype_buildin2(p, \
		CONSTANT_COMMON_##x, CONSTANT_CLOS_##x, CONSTANT_CLOS_##y, CONSTANT_CLOS_##z)

static void build_clos_class_type(LocalRoot local)
{
	ClosMakeType0(local, ARRAY);
	ClosMakeType0(local, CHARACTER);
	ClosMakeType0(local, HASH_TABLE);
	ClosMakeType0(local, NUMBER);
	ClosMakeType0(local, PACKAGE);
	ClosMakeType0(local, PATHNAME);
	ClosMakeType0(local, RANDOM_STATE);
	ClosMakeType0(local, READTABLE);
	ClosMakeType0(local, RESTART);
	ClosMakeType0(local, SEQUENCE);
	ClosMakeType0(local, STREAM);
	ClosMakeType0(local, SYMBOL);
	ClosMakeType1(local, LOGICAL_PATHNAME, PATHNAME);

	ClosMakeType1(local, LIST, SEQUENCE);
	ClosMakeType1(local, CONS, LIST);
	ClosMakeType1(local, VECTOR, SEQUENCE);
	ClosMakeType1(local, BIT_VECTOR, VECTOR);
	ClosMakeType2(local, NULL, SYMBOL, LIST);
	ClosMakeType1(local, STRING, VECTOR);

	ClosMakeType1(local, COMPLEX, NUMBER);
	ClosMakeType1(local, REAL, NUMBER);
	ClosMakeType1(local, FLOAT, REAL);
	ClosMakeType1(local, RATIONAL, REAL);
	ClosMakeType1(local, INTEGER, RATIONAL);
	ClosMakeType1(local, RATIO, RATIONAL);

	ClosMakeType1(local, BROADCAST_STREAM, STREAM);
	ClosMakeType1(local, CONCATENATED_STREAM, STREAM);
	ClosMakeType1(local, ECHO_STREAM, STREAM);
	ClosMakeType1(local, FILE_STREAM, STREAM);
	ClosMakeType1(local, STRING_STREAM, STREAM);
	ClosMakeType1(local, SYNONYM_STREAM, STREAM);
	ClosMakeType1(local, TWO_WAY_STREAM, STREAM);

	ClosMakeType1(local, BASE_CHAR, CHARACTER);
	ClosMakeType1(local, EXTENDED_CHAR, CHARACTER);
	ClosMakeType1(local, STANDARD_CHAR, CHARACTER);
	ClosMakeType1(local, SIMPLE_ARRAY, ARRAY);
	ClosMakeType2(local, SIMPLE_VECTOR, VECTOR, SIMPLE_ARRAY);

	ClosMakeType1(local, BASE_STRING, STRING);
	ClosMakeType2(local, SIMPLE_STRING, STRING, SIMPLE_ARRAY);
	ClosMakeType2(local, SIMPLE_BASE_STRING, BASE_STRING, SIMPLE_STRING);
	ClosMakeType2(local, SIMPLE_BIT_VECTOR, BIT_VECTOR, SIMPLE_ARRAY);
	ClosMakeType1(local, BIGNUM, INTEGER);
	ClosMakeType1(local, FIXNUM, INTEGER);
	ClosMakeType1(local, SHORT_FLOAT, FLOAT);
	ClosMakeType1(local, SINGLE_FLOAT, FLOAT);
	ClosMakeType1(local, DOUBLE_FLOAT, FLOAT);
	ClosMakeType1(local, LONG_FLOAT, FLOAT);
	ClosMakeType1(local, SIGNED_BYTE, INTEGER);
	ClosMakeType1(local, UNSIGNED_BYTE, SIGNED_BYTE);
	ClosMakeType1(local, BIT, UNSIGNED_BYTE);
	ClosMakeType1(local, COMPILED_FUNCTION, FUNCTION);
	ClosMakeType1(local, KEYWORD, SYMBOL);
}


/*
 *  Metaobject Protocol
 */
static void clos_mop_slot_definition_slots(addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 6);
	SlotMakeNameSymbol_number(slots, NAME, 0);
	SlotMakeName_number(slots, TYPE, 1);
	SlotMakeName_number(slots, ALLOCATION, 2);
	SlotMakeName_number(slots, INITARGS, 3);
	SlotMakeName_number(slots, INITFORM, 4);
	SlotMakeForm_number(slots, INITFUNCTION, 5);
	slotvector_set_location(slots);
	*ret = slots;
}

static void build_clos_class_mop(LocalRoot local)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* slot-definition */
	ClosMakeClass1(local, metaclass,
			CLOSNAME_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION,
			CLOS_STANDARD_OBJECT);
	/* standard-slot-definition */
	clos_mop_slot_definition_slots(&slots);
	ClosMakeClassSlot(local, metaclass, slots,
			CLOSNAME_STANDARD_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
}


/*
 *  build-clos-class
 */
_g void build_clos_class_variable(void)
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

_g void build_clos_class(LocalRoot local)
{
	build_clos_class_init();
	build_clos_class_standard(local);
	build_clos_class_generic(local);
	build_clos_class_method(local);
	build_clos_class_combination(local);
	build_clos_class_specializer(local);
	build_clos_class_condition(local);
	build_clos_class_system(local);
	build_clos_class_type(local);
	build_clos_class_mop(local);
	build_clos_class_variable();
}


/*
 *  ensure-class
 */
static void clos_ensure_class_supers(addr args, addr *ret, int *referp)
{
	addr list, pos;

	/* arguments */
	if (getkeyargs(args, CLOSKEY_DIRECT_SUPERCLASSES, &list)) {
		/* (list (find-class 'standard-object)) */
		GetConst(CLOS_STANDARD_OBJECT, &args);
		clos_find_class(args, &args);
		list_heap(ret, args, NULL);
		*referp = 0;
		return;
	}

	/* check forward-referenced-class */
	*ret = list;
	while (list != Nil) {
		getcons(list, &pos, &list);
		if (clos_referenced_p(pos)) {
			*referp = 1;
			return;
		}
	}
	*referp = 0;
}

static void clos_ensure_class_parse_slots(addr list, addr *ret)
{
	addr slot, name, readers, writers, alloc, args, form, func, type, doc;

	/* arguments */
	if (getkeyargs(list, CLOSKEY_NAME, &name))
		fmte("Invalid slot :name ~S.", name, NULL);
	if (getkeyargs(list, CLOSKEY_TYPE, &type))
		GetTypeTable(&type, T);
	if (getkeyargs(list, CLOSKEY_INITARGS, &args))
		args = Nil;
	if (getkeyargs(list, CLOSKEY_INITFORM, &form))
		form = Unbound;
	if (getkeyargs(list, CLOSKEY_INITFUNCTION, &func))
		func = Nil;
	if (getkeyargs(list, CLOSKEY_READERS, &readers))
		readers = Nil;
	if (getkeyargs(list, CLOSKEY_WRITERS, &writers))
		writers = Nil;
	if (getkeyargs(list, CLOSKEY_DOCUMENTATION, &doc))
		doc = Nil;
	if (getkeyargs(list, CLOSKEY_ALLOCATION, &alloc))
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

static void clos_ensure_class_slots(addr args, addr *ret)
{
	addr slots, pos;
	size_t size, i;

	/* :direct-slot list */
	if (getkeyargs(args, CLOSKEY_DIRECT_SLOTS, &args))
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

static void clos_ensure_class_direct_default_initargs(LocalRoot local,
		addr pos, addr args, addr *ret)
{
	addr check, list, key, a, b;
	LocalStack stack;

	if (getkeyargs(args, CLOSKEY_DIRECT_DEFAULT_INITARGS, &args)) {
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
	nreverse_list_unsafe(ret, root);
}

/* reader/writer check */
static void clos_ensure_reader_check(Execute ptr, addr gen)
{
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return;
	if (functionp(gen))
		fmte("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	stdget_generic_method_combination(gen, &pos);
	if (qualifiers_position_nil(ptr, Nil, pos, &index))
		fmte("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	stdget_generic_lambda_list(gen, &pos);
	if (! argumentp(pos))
		argument_generic_heap(ptr->local, &pos, pos);
	if (ArgumentStruct(pos)->var != 1)
		fmte("The generic-function ~S must be a 1 specializer.", gen, NULL);
}

static void clos_ensure_writer_method_check(Execute ptr, addr gen)
{
	addr pos;
	size_t index;

	/* generic-function */
	if (gen == Unbound)
		return;
	if (functionp(gen))
		fmte("The function ~S must be a generic-function.", gen, NULL);

	/* qualifiers */
	stdget_generic_method_combination(gen, &pos);
	if (qualifiers_position_nil(ptr, Nil, pos, &index))
		fmte("The generic-function ~S don't have a NIL qualifier.", gen, NULL);

	/* specializer */
	stdget_generic_lambda_list(gen, &pos);
	if (! argumentp(pos))
		argument_generic_heap(ptr->local, &pos, pos);
	if (ArgumentStruct(pos)->var != 2)
		fmte("The generic-function ~S must be a 2 specializers.", gen, NULL);
}

static void clos_ensure_readers_check(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		clos_ensure_reader_check(ptr, gen);
	}
}

static void clos_ensure_writers_check(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		clos_ensure_writer_method_check(ptr, gen);
	}
}

static void clos_ensure_class_function_check(Execute ptr, addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	stdget_class_slots(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		clos_ensure_readers_check(ptr, list);
		/* writer */
		GetWritersSlot(slot, &list);
		clos_ensure_writers_check(ptr, list);
	}
}

/* reader/writer generic */
static void clos_ensure_reader_generic(Execute ptr, addr name)
{
	addr lambda;
	mop_argument_generic_var1(&lambda);
	generic_empty(name, lambda, &name);
}

static void clos_ensure_writer_generic(Execute ptr, addr name)
{
	addr lambda;
	mop_argument_generic_var2(&lambda);
	generic_empty(name, lambda, &name);
}

static void clos_ensure_readers_generic(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		if (gen != Unbound)
			continue;
		clos_ensure_reader_generic(ptr, name);
	}
}

static void clos_ensure_writers_generic(Execute ptr, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		if (gen != Unbound)
			continue;
		clos_ensure_writer_generic(ptr, name);
	}
}

static void clos_ensure_class_function_generic(Execute ptr, addr pos)
{
	addr slots, slot, list;
	size_t size, i;

	stdget_class_slots(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		/* reader */
		GetReadersSlot(slot, &list);
		clos_ensure_readers_generic(ptr, list);
		/* writer */
		GetWritersSlot(slot, &list);
		clos_ensure_writers_generic(ptr, list);
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

static void function_clos_ensure_reader(Execute ptr, addr method, addr next, addr inst)
{
	addr call, symbol;

	/* (slot-value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	getfunctioncheck_local(ptr, call, &call);
	getdata_control(ptr, &symbol);
	funcall_control(ptr, call, inst, symbol, NULL);
}

static void clos_ensure_reader_method(Execute ptr,
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
	method_add_method(ptr, gen, pos);
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

static void function_clos_ensure_writer_instance(Execute ptr,
		addr method, addr next, addr value, addr inst)
{
	addr call, symbol;

	/* ((setf slot-value) value inst symbol) */
	GetConst(COMMON_SLOT_VALUE, &call);
	getsetfcheck_local(ptr, call, &call);
	getdata_control(ptr, &symbol);
	funcall_control(ptr, call, value, inst, symbol, NULL);
}

static void clos_ensure_writer_method(Execute ptr,
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
	method_add_method(ptr, gen, pos);
}

static void clos_ensure_readers_method(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		Check(gen == Unbound, "generic-function error");
		clos_ensure_reader_method(ptr, pos, name, gen, symbol);
	}
}

static void clos_ensure_writers_method(Execute ptr, addr pos, addr symbol, addr list)
{
	addr name, gen;

	while (list != Nil) {
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
		getcallname_global(name, &gen);
		Check(gen == Unbound, "generic-function error");
		clos_ensure_writer_method(ptr, pos, name, gen, symbol);
	}
}

static void clos_ensure_class_method(Execute ptr, addr pos)
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
		clos_ensure_readers_method(ptr, pos, symbol, list);
		/* writer */
		GetWritersSlot(slot, &list);
		clos_ensure_writers_method(ptr, pos, symbol, list);
	}
}

static void clos_ensure_class_function(Execute ptr, addr pos)
{
	/* check */
	clos_ensure_class_function_check(ptr, pos);
	/* make generic-function */
	clos_ensure_class_function_generic(ptr, pos);
	/* make method */
	clos_ensure_class_method(ptr, pos);
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

static void clos_ensure_class_init(Execute ptr, addr pos)
{
	addr value;
	LocalRoot local;

	local = ptr->local;
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
	clos_ensure_class_subclasses(pos);
}

static void clos_ensure_class_set(Execute ptr, addr pos, addr name, addr args)
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
	clos_ensure_class_direct_default_initargs(ptr->local, pos, args, &value);
	stdset_class_direct_default_initargs(pos, value);
	/* forward-referenced-class */
	if (! referp)
		clos_ensure_class_init(ptr, pos);
}

static int clos_finalize(Execute ptr, addr pos);
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

static int clos_finalize(Execute ptr, addr pos)
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
		clos_ensure_class_init(ptr, pos);
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
	clos_ensure_class_function(ptr, pos);
	stdset_class_finalized_p(pos, T);

	return 0;
}

static int clos_ensure_class_object(Execute ptr, addr name, addr args, addr *ret)
{
	addr metaclass, pos, list;
	LocalRoot local;

	/* :metaclass ... */
	if (getkeyargs(args, CLOSKEY_METACLASS, &metaclass))
		GetConst(CLOS_STANDARD_CLASS, &metaclass);
	local = ptr->local;
	GetConst(CLOSKEY_METACLASS, &pos);
	(void)remplist_local(local, args, pos, &args);

	/* (apply #'make-instance metaclass args) */
	GetConst(COMMON_MAKE_INSTANCE, &pos);
	getfunctioncheck_local(ptr, pos, &pos);
	lista_local(local, &list, metaclass, args, NULL);

	return callclang_apply(ptr, ret, pos, list);
}

_g int clos_ensure_class(Execute ptr, addr name, addr args, addr *ret)
{
	addr pos;

	/* make-instance */
	if (clos_ensure_class_object(ptr, name, args, &pos))
		return 1;

	/* define class */
	clos_ensure_class_set(ptr, pos, name, args);
	clos_define_class(name, pos);
	*ret = pos;

	return 0;
}


/*
 *  redefine
 */
static int clos_ensure_class_redefine_direct(Execute ptr,
		addr clos, addr name, addr rest)
{
	fmte("TODO", NULL);
	return 0;
}

static void clos_ensure_class_delete(addr clos, addr temp)
{
	addr list, a, b;

	/* remove superclasses */
	stdget_class_direct_superclasses(temp, &list);
	while (list != Nil) {
		getcons(list, &a, &list);
		stdget_class_direct_subclasses(a, &b);
		(void)delete_cons_eq_unsafe(clos, b, &b);
		stdset_class_direct_subclasses(a, b);
	}
	stdset_class_direct_superclasses(temp, Nil);

	/* copy class */

	/* remove class table */
}

_g int clos_ensure_class_redefine(Execute ptr, addr clos, addr name, addr rest)
{
	addr temp;

	/* make-instance */
	Check(! clos_standard_class_p(clos), "type error");
	stdget_class_finalized_p(clos, &temp);
	if (temp == Nil)
		return clos_ensure_class_redefine_direct(ptr, clos, name, rest);
	if (clos_ensure_class_object(ptr, name, rest, &temp))
		return 1;

	/* redefined */
	clos_ensure_class_delete(clos, temp);
	clos_ensure_class_set(ptr, clos, name, rest);
	clos_define_class(name, clos);

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
	clos_finalize(ptr, clos);

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
	LocalRoot local;
	LocalStack stack;

	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunctioncheck_local(ptr, call, &call);
	local = ptr->local;
	push_local(local, &stack);
	lista_local(local, &rest, pos, type, rest, NULL);
	if (callclang_apply(ptr, ret, call, rest)) return 1;
	rollback_local(local, stack);

	return 0;
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
	nreverse_list_unsafe(&root, root);
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
	clos_finalize(ptr, clos);

	/* initargs */
	if (make_instance_initargs(ptr, clos, rest, &rest))
		return 1;
	make_instance_check(ptr, clos, rest);

	/* allocation-instance */
	GetConst(COMMON_ALLOCATE_INSTANCE, &call);
	getfunctioncheck_local(ptr, call, &call);
	if (callclang_apply(ptr, &instance, call, rest))
		return 1;

	/* initialize-instance */
	GetCdr(rest, &rest);
	cons_local(ptr->local, &rest, instance, rest);
	GetConst(COMMON_INITIALIZE_INSTANCE, &call);
	getfunctioncheck_local(ptr, call, &call);
	if (callclang_apply(ptr, &call, call, rest))
		return 1;

	/* result */
	*ret = instance;
	return 0;
}


/*
 *  redefined
 */
static int clos_redefined_class(Execute ptr, addr pos, addr clos)
{
	addr call;
	LocalRoot local;
	LocalStack stack;

	fmte("TODO7", NULL);
	local = ptr->local;
	push_local(local, &stack);
	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &call);
	getfunctioncheck_local(ptr, call, &call);
	list_local(local, &pos, clos, pos, NULL);
	if (apply_control(ptr, call, pos)) return 1;
	rollback_local(local, stack);

	return 0;
}

_g int clos_version_diff_p(addr pos)
{
	addr clos, check;
	fixnum a, b;

	clos_class_of(pos, &clos);
	GetVersionClos(pos, &a);
	stdget_class_version(clos, &check);
	GetFixnum(check, &b);

	return a != b;
}

_g int clos_version_check(Execute ptr, addr pos, addr clos)
{
	addr check;
	fixnum a, b;

	GetVersionClos(pos, &a);
	stdget_class_version(clos, &check);
	GetFixnum(check, &b);
	if (a == b)
		return 0;
	if (clos_redefined_class(ptr, pos, clos))
		return 1;
	SetVersionClos(pos, b);

	return 0;
}


/*
 *  change-class
 */
_g int clos_change_class(Execute ptr, addr pos, addr clos, addr rest, addr *ret)
{
	fmte("TODO8", NULL);
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
	getfunctioncheck_local(ptr, call, &call);
	return funcall_control(ptr, call, clos, pos, name, operation, value, NULL);
}

_g int clos_slot_unbound(Execute ptr, addr clos, addr pos, addr name)
{
	addr call;

	GetConst(COMMON_SLOT_UNBOUND, &call);
	getfunctioncheck_local(ptr, call, &call);
	return funcall_control(ptr, call, clos, pos, name, NULL);
}


/*
 *  slot-boundp
 */
static int slot_boundp_call(Execute ptr, addr pos, addr key, int *ret)
{
	addr call;

	GetConst(COMMON_SLOT_BOUNDP, &call);
	getfunctioncheck_local(ptr, call, &call);
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
	getfunctioncheck_local(ptr, call, &call);
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
	getfunctioncheck_local(ptr, call, &call);
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
	getsetfcheck_local(ptr, call, &call);
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
_g void init_clos_class(void)
{
	SetPointerCall(defun, var3, clos_ensure_reader);
	SetPointerCall(defun, var4, clos_ensure_writer_instance);
}

