#include "callname.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "clos.h"
#include "clos_generic.h"
#include "clos_instance.h"
#include "clos_make.h"
#include "clos_method.h"
#include "clos_redefine.h"
#include "clos_object.h"
#include "clos_type.h"
#include "closget.h"
#include "closget_class.h"
#include "closget_slot.h"
#include "condition.h"
#include "control_execute.h"
#include "function.h"
#include "heap.h"
#include "hold.h"
#include "integer_calc.h"
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
static int clos_finalized_p_(Execute ptr, addr pos, int *ret)
{
	Return(stdget_class_finalized_p_(ptr, pos, &pos));
	return Result(ret, pos != Nil);
}

static int clos_superclasses_referenced_p_(Execute ptr, addr pos, int *ret)
{
	int check;
	addr list;

	Return(stdget_class_direct_superclasses_(ptr, pos, &list));
	while (list != Nil) {
		Return_getcons(list, &pos, &list);
		Return(clos_referenced_p_(pos, &check));
		if (check)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int clos_redefine_check_subclasses_(Execute ptr, addr pos, addr x, addr list)
{
	addr root;

	/* class-precedence-list check */
	Return(clos_precedence_list_redefine_(ptr, pos, &root, x, list));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(ptr, pos, &root));
	while (root != Nil) {
		Return_getcons(root, &pos, &root);
		Return(clos_redefine_check_subclasses_(ptr, pos, x, list));
	}

	return 0;
}

static int clos_redefine_slots_(Execute ptr, addr pos, addr clos, addr slots1)
{
	/* slots1 -> slots2 */
	addr slots2, slot, name, list;
	size_t size1, size2, i;

	Return(stdget_class_slots_(ptr, clos, &slots2));
	LenSlotVector(slots1, &size1);
	LenSlotVector(slots2, &size2);

	/* added-slots */
	list = Nil;
	for (i = 0; i < size2; i++) {
		GetSlotVector(slots2, i, &slot);
		getname_slot(slot, &name);
		if (! clos_find_slotname(slots1, size1, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setadd_redefine(pos, list);

	/* discarded-slots */
	list = Nil;
	for (i = 0; i < size1; i++) {
		GetSlotVector(slots1, i, &slot);
		getname_slot(slot, &name);
		if (! clos_find_slotname(slots2, size2, name))
			cons_heap(&list, name, list);
	}
	nreverse(&list, list);
	setdiscard_redefine(pos, list);

	return 0;
}

static int clos_redefine_information_(Execute ptr, addr clos, addr slots)
{
	addr pos, value;

	/* redefined */
	redefine_heap(&pos);
	Return(clos_redefine_slots_(ptr, pos, clos, slots));
	Return(stdset_class_redefined_class_(ptr, clos, pos));
	/* increment */
	Return(stdget_class_version_(ptr, clos, &value));
	Return(oneplus_integer_common_(ptr->local, value, &value));
	Return(stdset_class_version_(ptr, clos, value));
	/* finalized_p */
	Return(stdset_class_finalized_p_(ptr, clos, Nil));

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
		getreaders_slot(pos, &list);
		Return(clos_redefine_delete_readers_(ptr, clos, list));
		getwriters_slot(pos, &list);
		Return(clos_redefine_delete_writers_(ptr, clos, list));
	}

	return 0;
}

static int clos_redefine_superclasses_(Execute ptr, addr clos, addr supers)
{
	addr list, x, y;

	/* delete subclases */
	Return(stdget_class_direct_superclasses_(ptr, clos, &list));
	while (list != Nil) {
		Return_getcons(list, &x, &list);
		Return(stdget_class_direct_subclasses_(ptr, x, &y));
		delete_list_eq_unsafe(clos, y, &y);
		Return(stdset_class_direct_subclasses_(ptr, x, y));
	}

	/* push subclasses */
	Return(stdset_class_direct_superclasses_(ptr, clos, supers));
	while (supers != Nil) {
		Return_getcons(supers, &x, &supers);
		Return(stdget_class_direct_subclasses_(ptr, x, &y));
		pushnew_heap(y, clos, &y);
		Return(stdset_class_direct_subclasses_(ptr, x, y));
	}

	return 0;
}

static int clos_redefine_update_subclasses_(Execute ptr, addr clos, addr x, addr list)
{
	addr root, slots;

	/* update */
	Return(stdget_class_slots_(ptr, clos, &slots));
	Return(clos_ensure_class_init_(ptr, clos, 0));
	/* subclasses */
	Return(stdget_class_direct_subclasses_(ptr, clos, &root));
	while (root != Nil) {
		Return_getcons(root, &clos, &root);
		Return(clos_redefine_update_subclasses_(ptr, clos, x, list));
	}
	/* version increment */
	return clos_redefine_information_(ptr, clos, slots);
}

static int clos_redefine_finalized_(Execute ptr, addr clos, addr name, addr rest)
{
	addr supers, prev_slots, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	Return(stdget_class_slots_(ptr, clos, &prev_slots));
	Return(clos_ensure_class_supers_(rest, &supers, NULL));
	Return(clos_ensure_class_slots_(rest, &slots));
	Return(clos_ensure_class_direct_default_initargs_(local, clos, rest, &value));
	Return(clos_redefine_check_subclasses_(ptr, clos, clos, supers));
	/* update redefine */
	Return(clos_stdclass_direct_slots_(ptr, clos, slots));
	Return(clos_redefine_delete_accessor_(ptr, clos, prev_slots));
	Return(clos_redefine_superclasses_(ptr, clos, supers));
	Return(stdset_class_direct_default_initargs_(ptr, clos, value));
	Return(clos_redefine_update_subclasses_(ptr, clos, clos, supers));

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
	Return(clos_stdclass_direct_slots_(ptr, clos, slots));
	Return(clos_redefine_superclasses_(ptr, clos, supers));
	Return(stdset_class_direct_default_initargs_(ptr, clos, value));

	return 0;
}

static int clos_redefine_make_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	Return(clos_superclasses_referenced_p_(ptr, clos, &check));
	if (check)
		return clos_redefine_reference_(ptr, clos, name, rest);
	else
		return clos_redefine_finalized_(ptr, clos, name, rest);
}

static int clos_redefine_make_instances_obsolete_(Execute ptr, addr clos)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, &call, call, clos, NULL);
}

static int clos_redefine_finalize_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;

	/* finalize check */
	Return(clos_finalized_p_(ptr, clos, &check));
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

int clos_ensure_class_redefine_(Execute ptr, addr clos, addr name, addr rest)
{
	int check;
	addr metaclass, pos;

	/* readonly check */
	if (GetStatusReadOnly(clos)) {
		return call_type_error_va_(ptr, clos, Nil,
				"Cannot redefine the object ~S that is a constant.", clos, NULL);
	}

	/* metaclass check */
	Return(clos_class_of_(clos, &metaclass));
	if (! GetKeyArgs(rest, KEYWORD_METACLASS, &pos)) {
		Return(clos_find_class_(pos, &pos));
		if (metaclass != pos)
			return fmte_("Cannot change the metaclass in class ~S.", clos, NULL);
	}

	/* standard-class only */
	check = clos_standard_class_p(metaclass);
	if (! check) {
		return fmte_("This implementation can only redefine a STANDARD-CLASS. "
				"(~S, ~S)", clos, name, NULL);
	}

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
		getname_slot(slot, &name);
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
		getname_slot(pos, &check);
		if (check == x) {
			SetClosValue(values, i, v);
			break;
		}
	}
}

static int clos_redefined_instance_(Execute ptr, addr pos, addr clos)
{
	addr pslots, pvalues, slots, values, x, v;
	size_t size, i;
	fixnum version;

	GetSlotClos(pos, &pslots);
	GetValueClos(pos, &pvalues);

	/* version */
	Return(stdget_class_version_(ptr, clos, &x));
	GetFixnum(x, &version);
	SetVersionClos(pos, version);

	/* update */
	Return(stdget_class_slots_(ptr, clos, &slots));
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
			getname_slot(x, &x);
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
	Return(clos_redefined_instance_(ptr, pos, clos));

	/* argument */
	Return(stdget_class_redefined_class_(ptr, clos, &clos));
	getadd_redefine(clos, &add);
	getdiscard_redefine(clos, &del);

	/* call update-instance-for-redefined-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return funcall1_control_(ptr, &call, call, pos, add, del, prop, NULL);
}

int clos_version_diff_p_(Execute ptr, addr pos, int *ret)
{
	addr clos, check;
	fixnum a, b;

	Return(clos_class_of_(pos, &clos));
	GetVersionClos(pos, &a);
	Return(stdget_class_version_(ptr, clos, &check));
	GetFixnum(check, &b);

	return Result(ret, a != b);
}

int clos_version_check_(Execute ptr, addr pos, addr clos)
{
	addr check;
	fixnum a, b;

	GetVersionClos(pos, &a);
	Return(stdget_class_version_(ptr, clos, &check));
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
static int clos_redefine_method_find(addr pos, addr key)
{
	addr list, check;
	size_t size, i;

	GetSlotClos(pos, &pos);
	LenSlotVector(pos, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(pos, i, &list);
		getargs_slot(list, &list);
		while (list != Nil) {
			GetCons(list, &check, &list);
			if (check == key)
				return 1;
		}
	}

	return 0;
}

static int clos_redefine_method_initargs_(Execute ptr, addr pos, addr rest)
{
	addr key;

	while (rest != Nil) {
		if (! consp_getcons(rest, &key, &rest))
			goto error;
		if (! consp_getcdr(rest, &rest))
			goto error;
		if (! clos_redefine_method_find(pos, key))
			return fmte_("There is no name ~S in the initargs.", key, NULL);
	}
	return 0;

error:
	return fmte_("Invalid &key arguments, ~S.", rest, NULL);
}

int clos_redefine_method_(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest)
{
	addr call;

	/* initargs */
	Return(clos_redefine_method_initargs_(ptr, pos, rest));

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	return applya_control_(ptr, call, pos, add, rest, NULL);
}


/*
 *  change-class
 */
static int clos_change_class_find(addr copy, addr name, addr *ret)
{
	addr slots, array, value;
	size_t size, i;

	GetValueClos(copy, &array);
	GetSlotClos(copy, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		getname_slot(value, &value);
		if (name == value) {
			GetClosValue(array, i, ret);
			return 1;
		}
	}

	return 0;
}

static int clos_change_class_update_(Execute ptr, addr copy, addr pos, addr rest)
{
	addr call, slots, array, value;
	size_t size, i;

	/* new slots */
	GetValueClos(pos, &array);
	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &value);
		getname_slot(value, &value);
		if (clos_change_class_find(copy, value, &value)) {
			SetClosValue(array, i, value);
		}
	}

	/* update-instance-for-different-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_DIFFERENT_CLASS, &call);
	Return(getfunction_global_(call, &call));
	return applya1_control_(ptr, &call, call, copy, pos, rest, NULL);
}

int clos_change_class_(Execute ptr, addr pos, addr clos, addr rest)
{
	addr copy, type, rollback;
	LocalHold hold;

	/* readonly check */
	GetConst(CLOS_BUILT_IN_CLASS, &type);
	if (clos == type) {
		return call_type_error_va_(ptr, clos, Nil,
				"Cannot change the object ~S to the built-in-class.", pos, NULL);
	}

	/* copy */
	hold = LocalHold_array(ptr, 1);
	clos_copy_alloc(ptr->local, pos, &rollback);
	Return(allocate_instance_standard_(ptr, clos, &copy));
	clos_swap(copy, pos);
	localhold_set(hold, 0, copy);

	/* update */
	if (clos_change_class_update_(ptr, copy, pos, rest)) {
		clos_swap(pos, rollback);
		return 1;
	}

	/* destroy copy instance */
	clos_destroy(copy);
	localhold_end(hold);

	return 0;
}


/*
 *  update-instance-for-different-class
 */
static void clos_change_method_slots(Execute ptr, addr copy, addr pos, addr *ret)
{
	addr slots, check, ignore, list;
	size_t size, i;

	/* new slots */
	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	list = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &check);
		getname_slot(check, &check);
		if (! clos_change_class_find(copy, check, &ignore))
			cons_heap(&list, check, list);
	}
	nreverse(ret, list);
}

int clos_change_method_(Execute ptr, addr copy, addr pos, addr rest)
{
	addr list, call;
	LocalHold hold;

	/* slots */
	Return(clos_redefine_method_initargs_(ptr, pos, rest));
	clos_change_method_slots(ptr, copy, pos, &list);

	/* (shared-initialize ...) */
	hold = LocalHold_local_push(ptr, list);
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	Return(getfunction_global_(call, &call));
	Return(applya_control_(ptr, call, pos, list, rest, NULL));
	localhold_end(hold);

	return 0;
}

