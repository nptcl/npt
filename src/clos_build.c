#include "clos.h"
#include "clos_build.h"
#include "clos_instance.h"
#include "clos_object.h"
#include "clos_slot.h"
#include "closget.h"
#include "closget_class.h"
#include "closget_slot.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "symbol.h"
#include "type_memory.h"
#include "typedef.h"

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
static int slot_make_name_symbol_(Execute ptr,
		addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	Return(slot_heap_(ptr, &slot));
	/* name */
	GetConstant(n1, &value);
	Return(setname_slot_(ptr, slot, value));
	/* type */
	GetConst(CLOSDATA_SYMBOL_TYPE, &value);
	CheckType(value, LISPTYPE_TYPE);
	Return(settype_slot_(ptr, slot, value));
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	Return(setargs_slot_(ptr, slot, value));
	/* result */
	SetSlotVector(pos, n3, slot);

	return 0;
}
#define Return_SlotMakeNameSymbol_number(p,x,y,z) { \
	Return(slot_make_name_symbol_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				(z))); \
}
#define Return_SlotMakeNameSymbol(p,x,y,z) { \
	Return(slot_make_name_symbol_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				Clos_##z)); \
}

static int slot_make_name_(Execute ptr,
		addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	Return(slot_heap_(ptr, &slot));
	/* name */
	GetConstant(n1, &value);
	Return(setname_slot_(ptr, slot, value));
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	Return(setargs_slot_(ptr, slot, value));
	/* result */
	SetSlotVector(pos, n3, slot);

	return 0;
}
#define Return_SlotMakeName_number(p,x,y,z) { \
	Return(slot_make_name_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				(z))); \
}
#define Return_SlotMakeName(p,x,y,z) { \
	Return(slot_make_name_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				Clos_##z)); \
}

static int slot_make_form_(Execute ptr,
		addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	Return(slot_heap_(ptr, &slot));
	/* name */
	GetConstant(n1, &value);
	Return(setname_slot_(ptr, slot, value));
	/* initform */
	Return(setform_slot_(ptr, slot, Nil));
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	Return(setargs_slot_(ptr, slot, value));
	/* result */
	SetSlotVector(pos, n3, slot);

	return 0;
}
#define Return_SlotMakeForm_number(p,x,y,z) { \
	Return(slot_make_form_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				(z))); \
}
#define Return_SlotMakeForm(p,x,y,z) { \
	Return(slot_make_form_((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				Clos_##z)); \
}

static int slot_make_version(Execute ptr,
		addr pos, constindex n1, constindex n2, size_t n3)
{
	addr slot, value;

	Return(slot_heap_(ptr, &slot));
	/* name */
	GetConstant(n1, &value);
	Return(setname_slot_(ptr, slot, value));
	/* initform */
	fixnum_heap(&value, 0);
	Return(setform_slot_(ptr, slot, value));
	/* initargs */
	GetConstant(n2, &value);
	conscar_heap(&value, value);
	Return(setargs_slot_(ptr, slot, value));
	/* result */
	SetSlotVector(pos, n3, slot);

	return 0;
}
#define Return_SlotMakeVersion(p,x,y,z) { \
	Return(slot_make_version((p), (x), \
				CONSTANT_CLOSNAME_##y, \
				CONSTANT_CLOSKEY_##y, \
				Clos_##z)); \
}

int slotvector_set_location_(Execute ptr, addr slots)
{
	addr pos;
	size_t size, i;

	LenSlotVector(slots, &size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &pos);
		Return(setlocation_slot_(ptr, pos, i));
	}

	return 0;
}

int clos_stdclass_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_class_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, class_name);
	Return_SlotMakeForm(ptr, slots, DIRECT_SLOTS, class_direct_slots);
	Return_SlotMakeForm(ptr, slots, DIRECT_SUBCLASSES, class_direct_subclasses);
	Return_SlotMakeName(ptr, slots, DIRECT_SUPERCLASSES, class_direct_superclasses);
	Return_SlotMakeName(ptr, slots, CLASS_PRECEDENCE_LIST, class_precedence_list);
	Return_SlotMakeName(ptr, slots, EFFECTIVE_SLOTS, class_slots);
	Return_SlotMakeForm(ptr, slots, FINALIZED_P, class_finalized_p);
	Return_SlotMakeName(ptr, slots, PROTOTYPE, class_prototype);
	Return_SlotMakeForm(ptr, slots, DEFAULT_INITARGS, class_default_initargs);
	Return_SlotMakeForm(ptr, slots, DIRECT_DEFAULT_INITARGS, class_direct_default_initargs);
	Return_SlotMakeVersion(ptr, slots, VERSION, class_version);
	Return_SlotMakeForm(ptr, slots, DOCUMENTATION, class_documentation);
	Return_SlotMakeForm(ptr, slots, REDEFINED_CLASS, class_redefined_class);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
		Return(getclass_slot_(ptr, slot, &check));
		Check(check != Nil, "slot class error");
#endif
		Return(setclass_slot_(ptr, slot, instance));
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

static int slot_vector_prototype_(Execute ptr, addr clos, addr slots, addr *ret)
{
	int check;
	addr pos, slot, value;
	size_t size, i;

	LenSlotVector(slots, &size);
	slot_vector_heap(&pos, size);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(slot_class_p_(ptr, slot, &check));
		if (check) {
			Return(getclass_slot_(ptr, slot, &value));
			if (clos == value) {
				Return(slot_copy_heap_(ptr, &slot, slot));
				Return(slot_set_instance_(ptr, slot));
			}
		}
		SetSlotVector(pos, i, slot);
	}

	return Result(ret, pos);
}

static int clos_stdclass_prototype_initialize_(Execute ptr, addr pos, addr slots)
{
	int check;
	addr slot, value;
	size_t size, i;

	LenSlotVector(slots, &size);
	GetValueClos(pos, &pos);
	for (i = 0; i < size; i++) {
		GetSlotVector(slots, i, &slot);
		Return(slot_class_p_(ptr, slot, &check));
		if (check) {
			Return(getform_slot_(ptr, slot, &value));
			SetClosValue(pos, i, value);
		}
	}

	return 0;
}

int clos_stdclass_prototype_(Execute ptr, addr clos)
{
	addr pos, slots, value;

	/* make prototype */
	Return(stdget_class_slots_(ptr, clos, &slots));
	Return(slot_vector_prototype_(ptr, clos, slots, &value));
	clos_heap(&pos, value);
	SetClassOfClos(pos, clos);
	/* initialize shared slots */
	Return(clos_stdclass_prototype_initialize_(ptr, pos, slots));
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
	Return(clos_stdclass_slots_(ptr, &slots));
	Return(clos_stdclass_dummy_(ptr, &stdclass, slots));
	/* class */
	Return(slot_vector_clear_(ptr, slots));
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

int clos_stdclass_type_(Execute ptr, addr *ret, addr metaclass, addr name, addr supers)
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

static int clos_structure_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_structure_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, structure_name);
	Return_SlotMakeForm(ptr, slots, DIRECT_SLOTS, structure_direct_slots);
	Return_SlotMakeForm(ptr, slots, SLOTS, structure_slots);
	Return_SlotMakeForm(ptr, slots, DOCUMENTATION, structure_documentation);
	Return_SlotMakeForm(ptr, slots, INCLUDE, structure_include);
	Return_SlotMakeName(ptr, slots, CLASS_PRECEDENCE_LIST, structure_precedence_list);
	Return_SlotMakeName(ptr, slots, VALUE, structure_value);
	Return_SlotMakeName(ptr, slots, PREDICATE, structure_predicate);
	Return_SlotMakeForm(ptr, slots, ACCESS, structure_access);
	Return_SlotMakeForm(ptr, slots, COPIER, structure_copier);
	Return_SlotMakeForm(ptr, slots, CONSTRUCTOR, structure_constructor);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}

static int build_clos_class_standard_(Execute ptr)
{
	addr metaclass, structure, slots;

	/* standard-class, others */
	Return(clos_stdclass_metaclass_(ptr, &metaclass));
	/* structure-class */
	Return(clos_structure_slots_(ptr, &slots));
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
	Return(clos_stdclass_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_FORWARD_REFERENCED_CLASS,
			CLOS_FORWARD_REFERENCED_CLASS,
			CLOS_CLASS);

	return 0;
}


/*
 *  standard-generic-function
 */
static int clos_stdgeneric_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_generic_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, generic_name);
	Return_SlotMakeForm(ptr, slots, METHODS, generic_methods);
	Return_SlotMakeName(ptr, slots, LAMBDA_LIST, generic_lambda_list);
	Return_SlotMakeName(ptr, slots,
			ARGUMENT_PRECEDENCE_ORDER, generic_argument_precedence_order);
	Return_SlotMakeForm(ptr, slots, DECLARATIONS, generic_declarations);
	Return_SlotMakeName(ptr, slots, METHOD_CLASS, generic_method_class);
	Return_SlotMakeForm(ptr, slots, METHOD_COMBINATION, generic_method_combination);
	Return_SlotMakeForm(ptr, slots, VECTOR, generic_vector);
	Return_SlotMakeForm(ptr, slots, REMOVE, generic_remove);
	Return_SlotMakeForm(ptr, slots, ARGUMENT, generic_argument);
	Return_SlotMakeForm(ptr, slots, DOCUMENTATION, generic_documentation);
	Return_SlotMakeName(ptr, slots, EQLCHECK, generic_eqlcheck);
	Return_SlotMakeName(ptr, slots, CACHE, generic_cache);
	Return_SlotMakeName(ptr, slots, CALL, generic_call);
	Return_SlotMakeName(ptr, slots, PRECEDENCE_INDEX, generic_precedence_index);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
	Return(clos_stdgeneric_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STANDARD_GENERIC_FUNCTION,
			CLOS_STANDARD_GENERIC_FUNCTION,
			CLOS_GENERIC_FUNCTION);

	return 0;
}


/*
 *  standard-method
 */
static int clos_stdmethod_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_method_size);
	Return_SlotMakeName(ptr, slots, FUNCTION, method_function);
	Return_SlotMakeForm(ptr, slots, GENERIC_FUNCTION, method_generic_function);
	Return_SlotMakeName(ptr, slots, LAMBDA_LIST, method_lambda_list);
	Return_SlotMakeName(ptr, slots, QUALIFIERS, method_qualifiers);
	Return_SlotMakeName(ptr, slots, SPECIALIZERS, method_specializers);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
	Return(clos_stdmethod_slots_(ptr, &slots));
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
static int clos_stdlongcomb_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longcomb_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, longcomb_name);
	Return_SlotMakeName(ptr, slots, DOCUMENTATION, longcomb_documentation);
	Return_SlotMakeName(ptr, slots, LAMBDA_LIST, longcomb_lambda_list);
	Return_SlotMakeName(ptr, slots, BINDING, longcomb_binding);
	Return_SlotMakeName(ptr, slots, QUALIFIERS, longcomb_qualifiers);
	Return_SlotMakeName(ptr, slots, ARGUMENTS, longcomb_arguments);
	Return_SlotMakeName(ptr, slots, GENERIC, longcomb_generic);
	Return_SlotMakeName(ptr, slots, FORM, longcomb_form);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}

static int clos_stdshortcomb_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortcomb_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, shortcomb_name);
	Return_SlotMakeName(ptr, slots, DOCUMENTATION, shortcomb_documentation);
	Return_SlotMakeName(ptr, slots, IDENTITY, shortcomb_identity);
	Return_SlotMakeName(ptr, slots, OPERATOR, shortcomb_operator);
	Return_SlotMakeName(ptr, slots, ORDER, shortcomb_order);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}

static int clos_stdlongdef_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_longdef_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, longdef_name);
	Return_SlotMakeName(ptr, slots, DOCUMENTATION, longdef_documentation);
	Return_SlotMakeName(ptr, slots, LAMBDA_LIST, longdef_lambda_list);
	Return_SlotMakeName(ptr, slots, QUALIFIERS, longdef_qualifiers);
	Return_SlotMakeName(ptr, slots, ARGUMENTS, longdef_arguments);
	Return_SlotMakeName(ptr, slots, GENERIC, longdef_generic);
	Return_SlotMakeName(ptr, slots, FORM, longdef_form);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}

static int clos_stdshortdef_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_shortdef_size);
	Return_SlotMakeNameSymbol(ptr, slots, NAME, shortdef_name);
	Return_SlotMakeName(ptr, slots, DOCUMENTATION, shortdef_documentation);
	Return_SlotMakeName(ptr, slots, IDENTITY, shortdef_identity);
	Return_SlotMakeName(ptr, slots, OPERATOR, shortdef_operator);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
	Return(clos_stdlongcomb_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_LONG_METHOD_COMBINATION,
			CLOS_LONG_METHOD_COMBINATION,
			CLOS_METHOD_COMBINATION);
	/* short-method-combination */
	Return(clos_stdshortcomb_slots_(ptr, &slots));
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
	Return(clos_stdlongdef_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_LONG_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);
	/* define-short-method-combination */
	Return(clos_stdshortdef_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_SHORT_METHOD_COMBINATION,
			CLOS_DEFINE_METHOD_COMBINATION);

	return 0;
}


/*
 *  eql-specializer
 */
static int clos_stdspecializer_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, Clos_specializer_size);
	Return_SlotMakeName(ptr, slots, OBJECT, specializer_object);
	Return_SlotMakeName(ptr, slots, TYPE, specializer_type);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}

static int build_clos_class_specializer_(Execute ptr)
{
	addr metaclass, slots;

	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	/* eql-specializer */
	Return(clos_stdspecializer_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_EQL_SPECIALIZER,
			CLOS_EQL_SPECIALIZER,
			CLOS_SPECIALIZER);

	return 0;
}


/*
 *  condition
 */
static int clos_stdcondition_slot1_(Execute ptr,
		addr *ret, constindex n1, constindex n2)
{
	addr slots;

	slot_vector_heap(&slots, 1);
	Return(slot_make_name_(ptr, slots, n1, n2, 0));
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}
#define Return_SlotMakeCondition1(p,r,a) { \
	Return(clos_stdcondition_slot1_((p), (r), \
				CONSTANT_CLOSNAME_##a, \
				CONSTANT_KEYWORD_##a)); \
}

static int clos_stdcondition_slot2_(Execute ptr, addr *ret,
		constindex a1, constindex a2,
		constindex b1, constindex b2)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	Return(slot_make_name_(ptr, slots, a1, a2, 0));
	Return(slot_make_name_(ptr, slots, b1, b2, 1));
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
}
#define Return_SlotMakeCondition2(p,r,a,b) { \
	Return(clos_stdcondition_slot2_((p), (r), \
				CONSTANT_CLOSNAME_##a, CONSTANT_KEYWORD_##a, \
				CONSTANT_CLOSNAME_##b, CONSTANT_KEYWORD_##b)); \
}

static int clos_simple_condition_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 2);
	/* :format-control */
	Return(slot_make_name_(ptr, slots,
				CONSTANT_CLOSNAME_FORMAT_CONTROL,
				CONSTANT_KEYWORD_FORMAT_CONTROL,
				0));
	/* :format-arguments */
	Return(slot_make_form_(ptr, slots,
				CONSTANT_CLOSNAME_FORMAT_ARGUMENTS,
				CONSTANT_KEYWORD_FORMAT_ARGUMENTS,
				1));
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
	Return(clos_simple_condition_slots_(ptr, &slots));
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
	Return_SlotMakeCondition2(ptr, &slots, OPERATION, OPERANDS);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_ARITHMETIC_ERROR,
			CONDITION_ARITHMETIC_ERROR,
			CONDITION_ERROR);
	/* cell-error (error) :name */
	Return_SlotMakeCondition1(ptr, &slots, NAME);
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
	Return_SlotMakeCondition1(ptr, &slots, PATHNAME);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_FILE_ERROR,
			CONDITION_FILE_ERROR,
			CONDITION_ERROR);
	/* package-error (error) :package */
	Return_SlotMakeCondition1(ptr, &slots, PACKAGE);
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
	Return_SlotMakeCondition1(ptr, &slots, OBJECT);
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
	Return_SlotMakeCondition1(ptr, &slots, STREAM);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_STREAM_ERROR,
			CONDITION_STREAM_ERROR,
			CONDITION_ERROR);
	/* type-error (error) :datum :expected-type */
	Return_SlotMakeCondition2(ptr, &slots, DATUM, EXPECTED_TYPE);
	ClosMakeClassSlot_(ptr, metaclass, slots,
			COMMON_TYPE_ERROR,
			CONDITION_TYPE_ERROR,
			CONDITION_ERROR);
	/* unbound-slot (cell-error) :instance :name */
	Return_SlotMakeCondition1(ptr, &slots, INSTANCE);
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
	Return_SlotMakeCondition1(ptr, &slots, VALUE);
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
static int clos_mop_slot_definition_slots_(Execute ptr, addr *ret)
{
	addr slots;

	slot_vector_heap(&slots, 9);
	Return_SlotMakeNameSymbol_number(ptr, slots, NAME, 0);
	Return_SlotMakeName_number(ptr, slots, TYPE, 1);
	Return_SlotMakeName_number(ptr, slots, ALLOCATION, 2);
	Return_SlotMakeName_number(ptr, slots, INITARGS, 3);
	Return_SlotMakeName_number(ptr, slots, INITFORM, 4);
	Return_SlotMakeForm_number(ptr, slots, INITFUNCTION, 5);
	Return_SlotMakeForm_number(ptr, slots, DOCUMENTATION, 6);
	Return_SlotMakeForm_number(ptr, slots, READERS, 7);
	Return_SlotMakeForm_number(ptr, slots, WRITERS, 8);
	Return(slotvector_set_location_(ptr, slots));

	return Result(ret, slots);
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
	Return(clos_mop_slot_definition_slots_(ptr, &slots));
	ClosMakeClassSlot_(ptr, metaclass, slots,
			CLOSNAME_STANDARD_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_SLOT_DEFINITION);
	/* standard-direct-slot-definition */
	Return(clos_mop_slot_definition_slots_(ptr, &slots));
	ClosMakeClass2Slot_(ptr, metaclass, slots,
			CLOSNAME_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_DIRECT_SLOT_DEFINITION,
			CLOS_STANDARD_SLOT_DEFINITION,
			CLOS_DIRECT_SLOT_DEFINITION);
	/* standard-effective-slot-definition */
	Return(clos_mop_slot_definition_slots_(ptr, &slots));
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

