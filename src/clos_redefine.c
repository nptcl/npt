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
static int clos_finalized_p(addr pos)
{
	stdget_class_finalized_p(pos, &pos);
	return pos != Nil;
}

static int clos_superclasses_referenced_p(addr pos)
{
	addr list;

	stdget_class_direct_superclasses(pos, &list);
	while (list != Nil) {
		getcons(list, &pos, &list);
		if (clos_referenced_p(pos))
			return 1;
	}

	return 0;
}

static void clos_redefine_check_subclasses(
		LocalRoot local, addr pos, addr x, addr list)
{
	addr root;

	/* class-precedence-list check */
	clos_precedence_list_redefine(local, pos, &root, x, list);
	/* subclasses */
	stdget_class_direct_subclasses(pos, &root);
	while (root != Nil) {
		getcons(root, &pos, &root);
		clos_redefine_check_subclasses(local, pos, x, list);
	}
}

static void clos_redefine_slots(addr pos, addr clos, addr slots1)
{
	/* slots1 -> slots2 */
	addr slots2, slot, name, list;
	size_t size1, size2, i;

	stdget_class_slots(clos, &slots2);
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
}

static void clos_redefine_information(LocalRoot local, addr clos, addr slots)
{
	addr pos, value;

	/* redefined */
	redefine_heap(&pos);
	clos_redefine_slots(pos, clos, slots);
	stdset_class_redefined_class(clos, pos);
	/* increment */
	stdget_class_version(clos, &value);
	oneplus_integer_common(local, value, &value);
	stdset_class_version(clos, value);
	/* finalized_p */
	stdset_class_finalized_p(clos, Nil);
}

static int clos_redefine_delete_reader_(Execute ptr, addr clos, addr gen)
{
	addr spec, method;

	list_local(ptr->local, &spec, clos, NULL);
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
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
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
	list_local(ptr->local, &spec, spec, clos, NULL);
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
		getcons(list, &name, &list);
		parse_callname_error(&name, name);
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

static void clos_redefine_superclasses(addr clos, addr supers)
{
	addr list, x, y;

	/* delete subclases */
	stdget_class_direct_superclasses(clos, &list);
	while (list != Nil) {
		getcons(list, &x, &list);
		stdget_class_direct_subclasses(x, &y);
		delete_list_eq_unsafe(clos, y, &y);
		stdset_class_direct_subclasses(x, y);
	}

	/* push subclasses */
	stdset_class_direct_superclasses(clos, supers);
	while (supers != Nil) {
		getcons(supers, &x, &supers);
		stdget_class_direct_subclasses(x, &y);
		pushnew_heap(y, clos, &y);
		stdset_class_direct_subclasses(x, y);
	}
}

static void clos_redefine_update_subclasses(
		LocalRoot local, addr clos, addr x, addr list)
{
	addr root, slots;

	/* update */
	stdget_class_slots(clos, &slots);
	clos_ensure_class_init(local, clos, 0);
	/* subclasses */
	stdget_class_direct_subclasses(clos, &root);
	while (root != Nil) {
		getcons(root, &clos, &root);
		clos_redefine_update_subclasses(local, clos, x, list);
	}
	/* version increment */
	clos_redefine_information(local, clos, slots);
}

static int clos_redefine_finalized_(Execute ptr, addr clos, addr name, addr rest)
{
	addr supers, prev_slots, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	stdget_class_slots(clos, &prev_slots);
	clos_ensure_class_supers(rest, &supers, NULL);
	clos_ensure_class_slots(rest, &slots);
	clos_ensure_class_direct_default_initargs(local, clos, rest, &value);
	clos_redefine_check_subclasses(local, clos, clos, supers);
	/* update redefine */
	clos_stdclass_direct_slots(clos, slots);
	Return(clos_redefine_delete_accessor_(ptr, clos, prev_slots));
	clos_redefine_superclasses(clos, supers);
	stdset_class_direct_default_initargs(clos, value);
	clos_redefine_update_subclasses(local, clos, clos, supers);

	return 0;
}

static void clos_redefine_reference(Execute ptr, addr clos, addr name, addr rest)
{
	int referp;
	addr supers, slots, value;
	LocalRoot local;

	/* class-precedence-list check */
	local = ptr->local;
	clos_ensure_class_supers(rest, &supers, &referp);
	clos_ensure_class_slots(rest, &slots);
	clos_ensure_class_direct_default_initargs(local, clos, rest, &value);
	/* update redefine */
	clos_stdclass_direct_slots(clos, slots);
	clos_redefine_superclasses(clos, supers);
	stdset_class_direct_default_initargs(clos, value);
}

static int clos_redefine_make_(Execute ptr, addr clos, addr name, addr rest)
{
	if (! clos_superclasses_referenced_p(clos))
		return clos_redefine_finalized_(ptr, clos, name, rest);
	else {
		clos_redefine_reference(ptr, clos, name, rest);
		return 0;
	}
}

static int clos_redefine_make_instances_obsolete(Execute ptr, addr clos)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCES_OBSOLETE, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, &call, call, clos, NULL);
}

static int clos_redefine_finalize(Execute ptr, addr clos, addr name, addr rest)
{
	int final;

	/* finalize check */
	final = clos_finalized_p(clos);
	Return(clos_redefine_make_(ptr, clos, name, rest));

	/* finalize */
	if (final) {
		if (clos_finalize(ptr, clos))
			fmte("Cannot finalize class object ~S.", clos, NULL);
		return clos_redefine_make_instances_obsolete(ptr, clos);
	}

	return 0;
}

_g int clos_ensure_class_redefine(Execute ptr, addr clos, addr name, addr rest)
{
	addr metaclass, pos;

	/* metaclass check */
	clos_class_of(clos, &metaclass);
	if (! GetKeyArgs(rest, KEYWORD_METACLASS, &pos)) {
		clos_find_class(pos, &pos);
		if (metaclass != pos)
			fmte("Cannot change the metaclass in class ~S.", clos, NULL);
	}

	/* standard-class only */
	if (! clos_standard_class_p(metaclass))
		fmte("This implementation can only redefine a STANDARD-CLASS.", NULL);

	/* make-instance */
	return clos_redefine_finalize(ptr, clos, name, rest);
}


/*
 *  version
 */
static void getproperty_redefine(LocalRoot local, addr pos, addr *ret)
{
	addr slots, root, slot, name, check;
	size_t size, i;

	GetSlotClos(pos, &slots);
	LenSlotVector(slots, &size);
	root = Nil;
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		GetNameSlot(slot, &name);
		clos_get(pos, name, &check);
		if (check != Unbound) {
			cons_local(local, &root, name, root);
			cons_local(local, &root, check, root);
		}
	}
	nreverse(ret, root);
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

static void clos_redefined_instance(addr pos, addr clos)
{
	addr pslots, pvalues, slots, values, x, v;
	size_t size, i;
	fixnum version;

	GetSlotClos(pos, &pslots);
	GetValueClos(pos, &pvalues);

	/* version */
	stdget_class_version(clos, &x);
	GetFixnum(x, &version);
	SetVersionClos(pos, version);

	/* update */
	stdget_class_slots(clos, &slots);
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
}

static int clos_redefined_class(Execute ptr, addr pos, addr clos)
{
	/* (update-instance-for-redefined-class
	 *     instance added-slots discarded-slots property-list
	 *     &rest initargs &key &allow-other-keys)
	 */
	addr call, add, del, prop;

	/* update instance */
	getproperty_redefine(ptr->local, pos, &prop);
	clos_redefined_instance(pos, clos);

	/* argument */
	stdget_class_redefined_class(clos, &clos);
	getadd_redefine(clos, &add);
	getdiscard_redefine(clos, &del);

	/* call update-instance-for-redefined-class */
	GetConst(COMMON_UPDATE_INSTANCE_FOR_REDEFINED_CLASS, &call);
	getfunction_global(call, &call);
	return callclang_funcall(ptr, &call, call, pos, add, del, prop, NULL);
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
 *  update-instance-for-redefined-class
 */
_g void clos_redefine_method(Execute ptr,
		addr pos, addr add, addr del, addr prop, addr rest)
{
	addr call;

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunction_global(call, &call);
	applya_control(ptr, call, pos, add, rest, NULL);
}


/*
 *  change-class
 */
_g int clos_change_class(Execute ptr, addr pos, addr clos, addr rest)
{
	addr copy, call;

	/* copy */
	allocate_instance_stdclass(ptr, clos, &copy);
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
_g void clos_change_method(Execute ptr, addr prev, addr inst, addr rest)
{
	addr call;

	/* (shared-initialize ...) */
	GetConst(COMMON_SHARED_INITIALIZE, &call);
	getfunction_global(call, &call);
	applya_control(ptr, call, inst, T, rest, NULL);
}

