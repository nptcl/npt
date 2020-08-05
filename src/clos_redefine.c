#include "callname.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "clos.h"
#include "clos_class.h"
#include "clos_generic.h"
#include "clos_make.h"
#include "clos_method.h"
#include "clos_redefine.h"
#include "clos_type.h"
#include "condition.h"
#include "control_execute.h"
#include "function.h"
#include "heap.h"
#include "integer.h"
#include "local.h"
#include "symbol.h"

enum ClosRedefine_Index {
	ClosRedefine_Add,
	ClosRedefine_Discard,
	ClosRedefine_Size
};

#define SetRedefine(x,i,y)		SetArrayA2((x),(i),(y))
#define GetRedefine(x,i,y)		GetArrayA2((x),(i),(y))

static void redefine_heap(addr *ret)
{
	heap_array2(ret, LISPSYSTEM_REDEFINE, ClosRedefine_Size);
}

static void getadd_redefine(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	GetRedefine(pos, ClosRedefine_Add, ret);
}
static void setadd_redefine(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	SetRedefine(pos, ClosRedefine_Add, value);
}

static void getdiscard_redefine(addr pos, addr *ret)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	GetRedefine(pos, ClosRedefine_Discard, ret);
}
static void setdiscard_redefine(addr pos, addr value)
{
	CheckType(pos, LISPSYSTEM_REDEFINE);
	SetRedefine(pos, ClosRedefine_Discard, value);
}


/*
 *  redefine check
 */
static int clos_finalized_p_(addr pos, int *ret)
{
	Return(stdget_class_finalized_p_(pos, &pos));
	return Result(ret, pos != Nil);
}

static int clos_superclasses_referenced_p_(addr pos, int *ret)
{
	int check;
	addr list;

	Return(stdget_class_direct_superclasses_(pos, &list));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int clos_redefine_check_subclasses_(
		LocalRoot local, addr pos, addr x, addr list)
{
	addr root;

	/* class-precedence-list check */
	Return(clos_precedence_list_redefine_(local, pos, &root, x, list));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(pos, &root));
	while (root != Nil) {
		Return_getcons(root, &pos, &root);
		Return(clos_redefine_check_subclasses_(local, pos, x, list));
	}

	return 0;
}

static int clos_redefine_slots_(addr pos, addr clos, addr slots1)
{
	/* slots1 -> slots2 */
	addr slots2, slot, name, list;
	size_t size1, size2, i;

	Return(stdget_class_slots_(clos, &slots2));
	LenSlotVector(slots1, &size1);
	LenSlotVector(slots2, &size2);

	/* added-slots */
	list = Nil;
	for (i = 0; i < size2; i++) {
		GetSlotVector(slots2, i, &slot);
		GetNameSlot(slot, &name);
		if (! clos_find_slotname(slots1, size1, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setadd_redefine(pos, list);

	/* discarded-slots */
	list = Nil;
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &slot);
		GetNameSlot(slot, &name);
		if (! clos_find_slotname(slots2, size2, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setdiscard_redefine(pos, list);

	return 0;
}

static int clos_redefine_information_(LocalRoot local, addr clos, addr slots)
{
	addr pos, value;

	/* redefined */
	redefine_heap(&pos);
	Return(clos_redefine_slots_(pos, clos, slots));
	Return(stdset_class_redefined_class_(clos, pos));
	/* increment */
	Return(stdget_class_version_(clos, &value));
	oneplus_integer_common(local, value, &value);
	Return(stdset_class_version_(clos, value));
	/* finalized_p */
	Return(stdset_class_finalized_p_(clos, Nil));

	return 0;
}

static int clos_redefine_delete_reader_(Execute ptr, addr clos, addr gen)
{
	addr spec, method;

	list_heap(&spec, clos, NULL);
	Return(generic_find_method_(ptr, gen, Nil, spec, Nil, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}

static int clos_redefine_delete_readers_(Execute ptr, addr clos, addr list)
{
	addr gen, name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "unbound error");
		if (gen != Unbound) {
			Return(clos_redefine_delete_reader_(ptr, clos, gen));
		}
	}

	return 0;
}

static int clos_redefine_delete_writer_(Execute ptr, addr clos, addr gen)
{
	addr spec, method;

	GetConst(CLOS_T, &spec);
	list_heap(&spec, spec, clos, NULL);
	Return(generic_find_method_(ptr, gen, Nil, spec, Nil, &method));
	if (method != Nil) {
		Return(method_remove_method_(ptr, gen, method));
	}

	return 0;
}

static int clos_redefine_delete_writers_(Execute ptr, addr clos, addr list)
{
	addr gen, name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		Return(parse_callname_error_(&name, name));
		getglobal_parse_callname(name, &gen);
		Check(gen == Unbound, "unbound error");
		if (gen != Unbound) {
			Return(clos_redefine_delete_writer_(ptr, clos, gen));
		}
	}

	return 0;
}

static int clos_redefine_delete_accessor_(Execute ptr, addr clos, addr slots)
{
	addr list, pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetReadersSlot(pos, &list);
		Return(clos_redefine_delete_readers_(ptr, clos, list));
		GetWritersSlot(pos, &list);
		Return(clos_redefine_delete_writers_(ptr, clos, list));
	}

	return 0;
}

static int clos_redefine_superclasses_(addr clos, addr supers)
{
	addr list, x, y;

	/* delete subclases */
	Return(stdget_class_direct_superclasses_(clos, &list));
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(stdget_class_direct_subclasses_(x, &y));
		delete_list_eq_unsafe(clos, y, &y);
		Return(stdset_class_direct_subclasses_(x, y));
	}

	/* push subclasses */
	Return(stdset_class_direct_superclasses_(clos, supers));
	while (supers != Nil) {
		Return_getcons(supers, &x, &supers);
		Return(stdget_class_direct_subclasses_(x, &y));
		pushnew_heap(y, clos, &y);
		Return(stdset_class_direct_subclasses_(x, y));
	}

	return 0;
}

static int clos_redefine_update_subclasses_(
		LocalRoot local, addr clos, addr x, addr list)
{
	addr root, slots;

	/* update */
	Return(stdget_class_slots_(clos, &slots));
	Return(clos_ensure_class_init_(local, clos, 0));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(clos, &root));
	while (root != Nil) {
		Return_getcons(root, &clos, &root);
		Return(clos_redefine_update_subclasses_(local, clos, x, list));
	}
	/* version increment */
	return clos_redefine_information_(local, clos, slots);
}

static int clos_redefine_finalized_(Execute ptr, addr clos, addr name, addr rest)
{
	addr supers, prev_slots, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	Return(stdget_class_slots_(clos, &prev_slots));
	Return(clos_ensure_class_supers_(rest, &supers, NULL));
	Return(clos_ensure_class_slots_(rest, &slots));
	Return(clos_ensure_class_direct_default_initargs_(local, clos, rest, &value));
	Return(clos_redefine_check_subclasses_(local, clos, clos, supers));
	/* update redefine */
	Return(clos_stdclass_direct_slots_(clos, slots));
	Return(clos_redefine_delete_accessor_(ptr, clos, prev_slots));
	Return(clos_redefine_superclasses_(clos, supers));
	Return(stdset_class_direct_default_initargs_(clos, value));
	Return(clos_redefine_update_subclasses_(local, clos, clos, supers));

	return 0;
}

static int clos_redefine_reference_(Execute ptr, addr clos, addr name, addr rest)
{
	int referp;
	addr supers, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	Return(clos_ensure_class_supers_(rest, &supers, &referp));
	Return(clos_ensure_class_slots_(rest, &slots));
	Return(clos_ensure_class_direct_default_initargs_(local, clos, rest, &value));
	/* update redefine */
	Return(clos_stdclass_direct_slots_(clos, slots));
	Return(clos_redefine_superclasses_(clos, supers));
	Return(stdset_class_direct_default_initargs_(clos, value));

	return 0;
}

static int clos_redefine_make_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	Return(clos_superclasses_referenced_p_(clos, &check));
	if (check)
		return clos_redefine_reference_(ptr, clos, name, rest);
	else
		return clos_redefine_finalized_(ptr, clos, name, rest);
}

static int clos_redefine_make_instances_obsolete_(Execute ptr, addr clos)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, &call, call, clos, NULL);
}

static int clos_redefine_finalize_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	/* finalize check */
	Return(clos_finalized_p_(clos, &check));
	Return(clos_redefine_make_(ptr, clos, name, rest));

	/* finalize */
	if (check) {
		Return(clos_finalize_(ptr, clos, &check));
		if (check)
			return fmte_("Cannot finalize class object ~S.", clos, NULL);
		return clos_redefine_make_instances_obsolete_(ptr, clos);
	}

	return 0;
}

_g int clos_ensure_class_redefine_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;
	addr metaclass, pos;

	/* metaclass check */
	Return(clos_class_of_(clos, &metaclass));
	if (! GetKeyArgs(rest, KEYWORD_METACLASS, &pos)) {
		Return(clos_find_class_(pos, &pos));
		if (metaclass != pos)
			return fmte_("Cannot change the metaclass in class ~S.", clos, NULL);
	}

	/* standard-class only */
	check = clos_standard_class_p(metaclass);
	if (! check)
		return fmte_("This implementation can only redefine a STANDARD-CLASS.", NULL);

	/* make-instance */
	return clos_redefine_finalize_(ptr, clos, name, rest);
}


/*
 *  version
 */
static int getproperty_redefine_(addr pos, addr *ret)
{
	addr slots, root, slot, name, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &name);
		Return(clos_get_(pos, name, &check));
		if (check != Unbound) {
			cons_heap(&root, name, root);
			cons_heap(&root, check, root);
		}
	}
	nreverse(ret, root);

	return 0;
}

static void clos_redefined_set_value(addr slots, addr values, addr x, addr v)
{
	addr pos, check;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		GetNameSlot(pos, &check);
		if (check == x) {
			SetClosValue(values, i, v);
			break;
		}
	}
}

static int clos_redefined_instance_(addr pos, addr clos)
{
	addr pslots, pvalues, slots, values, x, v;
	size_t size, i;
	fixnum version;

	GetSlotClos(pos, &pslots);
	GetValueClos(pos, &pvalues);

	/* version */
	Return(stdget_class_version_(clos, &x));
	GetFixnum(x, &version);
	SetVersionClos(pos, version);

	/* update */
	Return(stdget_class_slots_(clos, &slots));
	LenSlotVector(slots, &size);
	clos_value_heap(&values, size);
	SetSlotClos(pos, slots);
	SetValueClos(pos, values);

	/* set values */
	LenSlotVector(pslots, &size);
	for (i = 0; i < size; i++) {
		GetClosValue(pvalues, i, &v);
		if (v != Unbound) {
			GetSlotVector(pslots, i, &x);
			GetNameSlot(x, &x);
			clos_redefined_set_value(slots, values, x, v);
		}
	}

	return 0;
}

static int clos_redefined_class_(Execute ptr, addr pos, addr clos)
{
	/* (update-instance-for-redefined-class
	 *     instance added-slots discarded-slots property-list
	 *     &rest initargs &key &allow-other-keys)
	 */
	addr call, add, del, prop;

	/* update instance */
	Return(getproperty_redefine_(pos, &prop));
	Return(clos_redefined_instance_(pos, clos));

	/* argument */
	Return(stdget_class_redefined_class_(clos, &clos));
	getadd_redefine(clos, &add);
	getdiscard_redefine(clos, &del);

	/* call update-instance-for-redefined-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, &call, call, pos, add, del, prop, NULL);
}

_g int clos_version_diff_p_(addr pos, int *ret)
{
	addr clos, check;
	fixnum a, b;

	Return(clos_class_of_(pos, &clos));
	GetVersionClos(pos, &a);
	Return(stdget_class_version_(clos, &check));
	GetFixnum(check, &b);

	return Result(ret, a != b);
}

_g int clos_version_check_(Execute ptr, addr pos, addr clos)
{
	addr check;
	fixnum a, b;

	GetVersionClos(pos, &a);
	Return(stdget_class_version_(clos, &check));
	GetFixnum(check, &b);
	if (a == b)
		return 0;
	Return(clos_redefined_class_(ptr, pos, clos));
	SetVersionClos(pos, b);

	return 0;
}


/*
 *  update-instance-for-redefined-class
 */
_g int clos_redefine_method_(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest)
{
	addr call;

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunction_global(call, &call);
	return applya_control(ptr, call, pos, add, rest, NULL);
}


/*
 *  change-class
 */
_g int clos_change_class_(Execute ptr, addr pos, addr clos, addr rest)
{
	addr copy, call;

	/* copy */
	Return(allocate_instance_stdclass_(ptr, clos, &copy));
	clos_swap(copy, pos);

	/* call update-instance-for-different-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, &call);
	getfunction_global(call, &call);
	Return(callclang_applya(ptr, &call, call, copy, pos, rest, NULL));

	/* destroy copy instance */
	clos_destroy(copy);
	return 0;
}


/*
 *  update-instance-for-different-class
 */
_g int clos_change_method_(Execute ptr, addr prev, addr inst, addr rest)
{
	addr call;

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunction_global(call, &call);
	return applya_control(ptr, call, inst, T, rest, NULL);
}

