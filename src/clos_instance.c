#include "clos_instance.h"
#include "clos_object.h"
#include "clos_slot.h"
#include "clos_type.h"
#include "closget_class.h"
#include "closget_slot.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "integer.h"
#include "memory.h"
#include "symbol.h"
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
int clos_find_slotname_(Execute ptr, addr slots, size_t size, addr name, int *ret)
{
	addr check;
	size_t i;

	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		Return(getname_slot_(ptr, check, &check));
		if (check == name)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int clos_instance_unsafe_(Execute ptr, LocalRoot local,
		addr clos, addr slots, addr *ret)
{
	int check;
	addr instance, value, slot, pos;
	size_t size, i, loc;

	CheckType(clos, LISPTYPE_CLOS);
	/* allocate */
	Return(slot_vector_copyheap_alloc_(ptr, local, &slots, slots));
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
		Return(getname_slot_(ptr, slot, &pos));
		if (! symbolp(pos)) {
			*ret = Nil;
			return fmte_("The slot name ~S must be a symbol.", pos, NULL);
		}
		/* already exist */
		Return(clos_find_slotname_(ptr, slots, i, pos, &check));
		if (check) {
			*ret = Nil;
			return fmte_("The slot name ~S already exists.", pos, NULL);
		}
		/* location */
		Return(getlocation_slot_(ptr, slot, &loc));
		if (loc != i) {
			*ret = Nil;
			make_index_integer_heap(&value, i);
			return fmte_("The slot location ~A is invalid.", value, NULL);
		}
		/* allocation */
		Return(slot_class_p_(ptr, slot, &check));
		if (check) {
			*ret = Nil;
			return fmte_("The allocation must be an :INSTANCE.", NULL);
		}
		/* value */
		Return(getform_slot_(ptr, slot, &pos));
		SetClosValue(value, i, pos);
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
	return clos_instance_unsafe_(ptr, local, clos, pos, ret);
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
static int clos_slots_name_(Execute ptr, addr name, addr list, addr *value, int *ret)
{
	addr pos, check, next;

	for (; list != Nil; list = next) {
		GetCons(list, &pos, &next);
		Return(getname_slot_(ptr, pos, &check));
		if (name == check) {
			*value = pos;
			return Result(ret, 1);
		}
	}

	return Result(ret, 0);
}

static int clos_slots_push_(Execute ptr, addr pos, addr check)
{
	addr list, a;

	Return(getargs_slot_(ptr, pos, &list));
	Return(getargs_slot_(ptr, check, &check));
	while (check != Nil) {
		Return_getcons(check, &a, &check);
		pushnew_heap(list, a, &list);
	}
	Return(setargs_slot_(ptr, pos, list));

	return 0;
}

static int clos_slots_loop_(Execute ptr, addr list, addr *ret, size_t *rsize)
{
	int check;
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
			Return(getname_slot_(ptr, a, &b));
			Return(clos_slots_name_(ptr, b, root, &b, &check));
			if (check) {
				Return(clos_slots_push_(ptr, b, a));
			}
			else {
				Return(slot_copy_heap_(ptr, &a, a));
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
		Return(setlocation_slot_(ptr, pos, i));
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

