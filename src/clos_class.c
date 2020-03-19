#include "clos.h"
#include "clos_class.h"
#include "clos_type.h"
#include "cons.h"
#include "cons_list.h"
#include "condition.h"
#include "control.h"
#include "equal.h"
#include "integer.h"
#include "symbol.h"
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
_g int clos_find_slotname(addr slots, size_t size, addr name)
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
			_fmte("The slot name ~S must be a symbol.", check, NULL);
		/* already exist */
		if (clos_find_slotname(slots, i, check))
			_fmte("The slot name ~S already exists.", check, NULL);
		/* location */
		GetLocationSlot(slot, &loc);
		if (loc != i)
			_fmte("The slot location ~A is invalid.", intsizeh(i), NULL);
		/* allocation */
		if (slot_class_p(slot))
			_fmte("The allocation must be an :INSTANCE.", NULL);
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
		_fmte("The class ~S is not finalized.", clos, NULL);

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

static void clos_precedence_super(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	addr stack, result, temp, supers, super;

	result = Nil;
	conscar_local(local, &stack, pos);

	while (stack != Nil) {
		for (temp = Nil; stack != Nil; ) {
			GetCons(stack, &supers, &stack);
			if (! find_list_eq_unsafe(supers, result)) {
				cons_local(local, &result, supers, result);
				/* redefine */
				if (supers == x)
					supers = list;
				else
					stdget_class_direct_superclasses(supers, &supers);
				/* supers */
				while (supers != Nil) {
					GetCons(supers, &super, &supers);
					if (pos == super)
						_fmte("Loop detection in the class-precedence-list.", NULL);
					pushnew_local(local, temp, super, &temp);
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
	_fmte("Cannot make class precedence list. Perhaps, class inherit is loop.", NULL);
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
		_fmte("Cannot make class precedence list. Class key is not found.", NULL);
	*ret = result;
}

static void clos_precedence_result(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	addr root, key;

	clos_precedence_super(local, pos, &pos, x, list);
	clos_precedence_chain(local, pos, &pos);
	for (root = Nil; pos != Nil; ) {
		clos_precedence_top(pos, &key);
		cons_heap(&root, key, root);
		clos_precedence_remove(key, pos, &pos);
	}
	nreverse_list_unsafe(ret, root);
}

_g void clos_precedence_list_redefine(
		LocalRoot local, addr pos, addr *ret, addr x, addr list)
{
	LocalStack stack;

	CheckLocal(local);
	CheckType(pos, LISPTYPE_CLOS);
	push_local(local, &stack);
	clos_precedence_result(local, pos, ret, x, list);
	rollback_local(local, stack);
}

_g void clos_precedence_list(LocalRoot local, addr pos, addr *ret)
{
	clos_precedence_list_redefine(local, pos, ret, Unbound, Nil);
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

_g void clos_compute_slots(LocalRoot local, addr clos, addr *ret)
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
		_fmte("STANDARD-CLASS is already exist.", NULL);
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

_g void clos_stdclass_direct_slots(addr instance, addr slots)
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

_g void clos_stdclass_prototype(addr clos)
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
	SlotMakeName(slots, PRECEDENCE_INDEX, generic_precedence_index);
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
	/* lisp-system::delay-warning (warning) */
	ClosMakeClass1(local, metaclass,
			SYSTEM_DELAY_WARNING,
			CONDITION_DELAY_WARNING,
			CONDITION_WARNING);
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

