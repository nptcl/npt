#include "clos_build.c"
#include "clos_type.h"
#include "character.h"
#include "common.h"
#include "condition.h"
#include "control_execute.h"
#include "control_object.h"
#include "degrade.h"
#include "equal.h"
#include "package.h"
#include "package_intern.h"
#include "reader.h"
#include "real.h"
#include "stream.h"
#include "strtype.h"
#include "syscall.h"
#include "type.h"

static void test_forget_class(constindex index)
{
	addr symbol;
	GetConstant(index, &symbol);
	remclass_symbol(symbol);
}

static void test_forget_all_classes(void)
{
	test_forget_class(CONSTANT_COMMON_T);
	test_forget_class(CONSTANT_COMMON_CLASS);
	test_forget_class(CONSTANT_COMMON_STANDARD_CLASS);
	test_forget_class(CONSTANT_COMMON_FUNCTION);
	test_forget_class(CONSTANT_COMMON_GENERIC_FUNCTION);
	test_forget_class(CONSTANT_COMMON_STANDARD_GENERIC_FUNCTION);
	test_forget_class(CONSTANT_COMMON_METHOD);
	test_forget_class(CONSTANT_COMMON_STANDARD_METHOD);
}


/*
 *  standard-class
 */
static int test_slot_make_name_symbol(void)
{
	addr pos, check, type;
	Execute ptr;

	ptr = Execute_Thread;
	slot_vector_heap(&pos, Clos_class_size);
	slot_make_name_symbol_(ptr, pos,
			CONSTANT_CLOSNAME_NAME,
			CONSTANT_CLOSKEY_NAME,
			Clos_class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	gettype_slot_(ptr, pos, &type);
	getname_slot_(ptr, pos, &pos);
	GetConst(CLOSNAME_NAME, &check);
	test(pos == check, "slot_make_name_symbol1");
	test(LowLispDecl(type) == LISPDECL_SYMBOL, "slot_make_name_symbol2");

	RETURN;
}

static int test_slot_make_name(void)
{
	addr pos, check;
	Execute ptr;

	ptr = Execute_Thread;
	slot_vector_heap(&pos, Clos_class_size);
	slot_make_name_(ptr, pos,
			CONSTANT_CLOSNAME_EFFECTIVE_SLOTS,
			CONSTANT_CLOSKEY_EFFECTIVE_SLOTS,
			Clos_class_slots);
	GetSlotVector(pos, Clos_class_slots, &pos);
	getname_slot_(ptr, pos, &pos);
	GetConst(CLOSNAME_EFFECTIVE_SLOTS, &check);
	test(pos == check, "slot_make_name1");

	RETURN;
}

static int test_slot_make_form(void)
{
	addr pos, check, value;
	Execute ptr;

	ptr = Execute_Thread;
	slot_vector_heap(&pos, Clos_class_size);
	slot_make_form_(ptr, pos,
			CONSTANT_CLOSNAME_NAME,
			CONSTANT_CLOSKEY_NAME,
			Clos_class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	getform_slot_(ptr, pos, &value);
	getname_slot_(ptr, pos, &pos);
	GetConst(CLOSNAME_NAME, &check);
	test(pos == check, "slot_make_form1");
	test(value == Nil, "slot_make_form2");

	RETURN;
}

static int test_slot_make_version(void)
{
	addr pos, check, value;
	Execute ptr;

	ptr = Execute_Thread;
	slot_vector_heap(&pos, Clos_class_size);
	slot_make_version(ptr, pos,
			CONSTANT_CLOSNAME_NAME,
			CONSTANT_CLOSKEY_NAME,
			Clos_class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	getform_slot_(ptr, pos, &value);
	getname_slot_(ptr, pos, &pos);
	GetConst(CLOSNAME_NAME, &check);
	test(pos == check, "slot_make_version1");
	test(RefFixnum(value) == 0, "slot_make_version2");

	RETURN;
}

static int test_slotvector_set_location(void)
{
	Execute ptr;
	addr slots, slot;
	size_t check;

	ptr = Execute_Thread;
	slot_vector_heap(&slots, 3);
	slot_heap_(ptr, &slot);
	SetSlotVector(slots, 0, slot);
	slot_heap_(ptr, &slot);
	SetSlotVector(slots, 1, slot);
	slot_heap_(ptr, &slot);
	SetSlotVector(slots, 2, slot);
	slotvector_set_location_(ptr, slots);

	GetSlotVector(slots, 0, &slot);
	getlocation_slot_(ptr, slot, &check);
	test(check == 0, "slotvector_set_location1");
	GetSlotVector(slots, 1, &slot);
	getlocation_slot_(ptr, slot, &check);
	test(check == 1, "slotvector_set_location2");
	GetSlotVector(slots, 2, &slot);
	getlocation_slot_(ptr, slot, &check);
	test(check == 2, "slotvector_set_location3");

	RETURN;
}

static int test_slotname(addr slots, int index, const char *name)
{
	Execute ptr;
	addr pos, check;

	ptr = Execute_Thread;
	GetSlotVector(slots, index, &pos);
	getname_slot_(ptr, pos, &pos);
	internchar_debug(LISP_CLOS, name, &check);

	return check == pos;
}

static int test_clos_stdclass_slots(void)
{
	int check;
	Execute ptr;
	addr slots, pos;
	size_t i, size;

	ptr = Execute_Thread;
	check = 0;

	clos_stdclass_slots_(ptr, &slots);
	LenSlotVector(slots, &size);
	test(size == Clos_class_size, "clos_stdclass_slots1");

	for (check = 1, i = 0; i < Clos_class_size; i++) {
		GetSlotVector(slots, i, &pos);
		getlocation_slot_(ptr, pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "clos_stdclass_slots2");
	test(test_slotname(slots, Clos_class_name, "NAME"),
			"clos_stdclass_slots3");
	test(test_slotname(slots, Clos_class_direct_slots, "DIRECT-SLOTS"),
			"clos_stdclass_slots4");
	test(test_slotname(slots, Clos_class_direct_subclasses, "DIRECT-SUBCLASSES"),
			"clos_stdclass_slots5");
	test(test_slotname(slots, Clos_class_direct_superclasses, "DIRECT-SUPERCLASSES"),
			"clos_stdclass_slots6");
	test(test_slotname(slots, Clos_class_precedence_list, "CLASS-PRECEDENCE-LIST"),
			"clos_stdclass_slots7");
	test(test_slotname(slots, Clos_class_slots, "EFFECTIVE-SLOTS"),
			"clos_stdclass_slots8");
	test(test_slotname(slots, Clos_class_finalized_p, "FINALIZED-P"),
			"clos_stdclass_slots9");
	test(test_slotname(slots, Clos_class_prototype, "PROTOTYPE"),
			"clos_stdclass_slots10");
	test(test_slotname(slots, Clos_class_default_initargs, "DEFAULT-INITARGS"),
			"clos_stdclass_slots11");
	test(test_slotname(slots, Clos_class_direct_default_initargs,
				"DIRECT-DEFAULT-INITARGS"),
			"clos_stdclass_slots12");
	test(test_slotname(slots, Clos_class_version, "VERSION"),
			"clos_stdclass_slots13");

	RETURN;
}

static int test_clos_stdclass_dummy(void)
{
	addr slots, clos, check;
	Execute ptr;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &clos, slots);
	test(closp(clos), "clos_stdclass_dummy1");
	stdget_class_direct_slots_(ptr, clos, &check);
	test(check == slots, "clos_stdclass_dummy2");
	stdget_class_slots_(ptr, clos, &check);
	test(check == slots, "clos_stdclass_dummy3");

	RETURN;
}

static void test_slot_vector_heap(addr *ret, size_t size)
{
	Execute ptr;
	addr slots, pos;
	size_t i;

	ptr = Execute_Thread;
	slot_vector_heap(&slots, size);
	for (i = 0; i < size; i++) {
		slot_heap_(ptr, &pos);
		SetSlotVector(slots, i, pos);
	}
	*ret = slots;
}

static int test_clos_stdclass_make(void)
{
	Execute ptr;
	addr slots, clos, name, check;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &clos, slots);
	test_slot_vector_heap(&slots, 4);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	clos_stdclass_make_(ptr, &clos, clos, name, slots);
	stdget_class_name_(ptr, clos, &check);
	test(check == name, "clos_stdclass_make1");
	stdget_class_prototype_(ptr, clos, &check);
	test(check == clos, "clos_stdclass_make2");
	stdget_class_finalized_p_(ptr, clos, &check);
	test(check == Nil, "clos_stdclass_make3");
	stdget_class_direct_slots_(ptr, clos, &check);
	test(check == slots, "clos_stdclass_make4");

	RETURN;
}

static int test_clos_stdclass_empty(void)
{
	addr slots, clos, name, check;
	size_t size;
	Execute ptr;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &clos, slots);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	clos_stdclass_empty_(ptr, &clos, clos, name);
	stdget_class_direct_slots_(ptr, clos, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_empty1");

	RETURN;
}

static int test_clos_stdclass_class_of(void)
{
	addr slots, clos, instance, name, check;
	fixnum version;
	Execute ptr;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &clos, slots);
	internchar_debug(LISP_COMMON_USER, "AAA", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &clos, clos, name, slots);

	SetVersionClos(clos, 100);
	clos_heap(&instance, slots);
	clos_stdclass_class_of(instance, clos);
	clos_class_of_(instance, &check);
	test(check == clos, "clos_stdclass_class_of1");
	GetVersionClos(instance, &version);
	test(version == 100, "clos_stdclass_class_of2");

	RETURN;
}

static int test_clos_stdclass_inherit(void)
{
	addr slots, metaclass, clos, name, super, supers, check, left;
	Execute ptr;
	fixnum version;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &metaclass, slots);
	internchar_debug(LISP_COMMON_USER, "METACLASS", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &metaclass, metaclass, name, slots);
	clos_stdclass_inherit_(ptr, metaclass, metaclass, Nil);

	internchar_debug(LISP_COMMON_USER, "SUPER", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &super, metaclass, name, slots);
	clos_stdclass_inherit_(ptr, super, metaclass, Nil);

	internchar_debug(LISP_COMMON_USER, "AAA", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &clos, metaclass, name, slots);
	list_heap(&supers, super, NULL);
	clos_stdclass_inherit_(ptr, clos, metaclass, supers);

	GetVersionClos(clos, &version);
	test(version == 0, "clos_stdclass_inherit1");
	clos_class_of_(clos, &check);
	test(check == metaclass, "clos_stdclass_inherit2");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCons(check, &left, &check);
	test(left == super, "clos_stdclass_inherit3");
	test(check == Nil, "clos_stdclass_inherit4");
	stdget_class_precedence_list_(ptr, clos, &check);
	test(GetType(check) == LISPTYPE_CONS, "clos_stdclass_inherit5");
	stdget_class_slots_(ptr, clos, &check);
	test(GetType(check) == LISPSYSTEM_SLOT_VECTOR, "clos_stdclass_inherit6");
	stdget_class_direct_subclasses_(ptr, super, &check);
	GetCons(check, &left, &check);
	test(left == clos, "clos_stdclass_inherit7");
	test(check == Nil, "clos_stdclass_inherit8");
	stdget_class_name_(ptr, clos, &check);
	test(check == name, "clos_stdclass_inherit9");
	clos_find_class_(name, &check);
	test(check == clos, "clos_stdclass_inherit10");

	RETURN;
}

static int test_clos_stdclass_single(void)
{
	addr slots, metaclass, clos, name, super, check, left;
	Execute ptr;
	fixnum version;

	ptr = Execute_Thread;
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_dummy_(ptr, &metaclass, slots);
	internchar_debug(LISP_COMMON_USER, "METACLASS", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &metaclass, metaclass, name, slots);
	clos_stdclass_inherit_(ptr, metaclass, metaclass, Nil);

	internchar_debug(LISP_COMMON_USER, "SUPER", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &super, metaclass, name, slots);
	clos_stdclass_inherit_(ptr, super, metaclass, Nil);

	internchar_debug(LISP_COMMON_USER, "AAA", &name);
	clos_stdclass_slots_(ptr, &slots);
	clos_stdclass_make_(ptr, &clos, metaclass, name, slots);
	clos_stdclass_single_(ptr, clos, metaclass, super);

	GetVersionClos(clos, &version);
	test(version == 0, "clos_stdclass_single1");
	clos_class_of_(clos, &check);
	test(check == metaclass, "clos_stdclass_single2");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCons(check, &left, &check);
	test(left == super, "clos_stdclass_single3");
	test(check == Nil, "clos_stdclass_single4");
	stdget_class_precedence_list_(ptr, clos, &check);
	test(GetType(check) == LISPTYPE_CONS, "clos_stdclass_single5");
	stdget_class_slots_(ptr, clos, &check);
	test(GetType(check) == LISPSYSTEM_SLOT_VECTOR, "clos_stdclass_single6");
	stdget_class_direct_subclasses_(ptr, super, &check);
	GetCons(check, &left, &check);
	test(left == clos, "clos_stdclass_single7");
	test(check == Nil, "clos_stdclass_single8");
	stdget_class_name_(ptr, clos, &check);
	test(check == name, "clos_stdclass_single9");
	clos_find_class_(name, &check);
	test(check == clos, "clos_stdclass_single10");

	RETURN;
}

static int test_clos_stdclass_metaclass(void)
{
	addr metaclass, tclass, object, classclass, check, left;
	Execute ptr;

	ptr = Execute_Thread;
	clos_stdclass_metaclass_(ptr, &metaclass);

	/* t */
	clos_find_class_(T, &tclass);
	clos_class_of_(tclass, &check);
	GetConst(COMMON_BUILT_IN_CLASS, &left);
	clos_find_class_(left, &left);
	test(check == left, "clos_stdclass_metaclass1");
	stdget_class_direct_superclasses_(ptr, tclass, &check);
	test(check == Nil, "clos_stdclass_metaclass2");

	/* object */
	GetConst(COMMON_STANDARD_OBJECT, &object);
	clos_find_class_(object, &object);
	GetConst(CLOS_STANDARD_OBJECT, &left);
	test(object == left, "clos_stdclass_metaclass3");
	clos_class_of_(object, &check);
	test(check == metaclass, "clos_stdclass_metaclass4");
	stdget_class_direct_superclasses_(ptr, object, &check);
	GetCons(check, &left, &check);
	test(left == tclass, "clos_stdclass_metaclass5");
	test(check == Nil, "clos_stdclass_metaclass6");

	/* class */
	GetConst(COMMON_CLASS, &classclass);
	clos_find_class_(classclass, &classclass);
	GetConst(CLOS_CLASS, &left);
	test(classclass == left, "clos_stdclass_metaclass7");
	clos_class_of_(classclass, &check);
	test(check == metaclass, "clos_stdclass_metaclass8");
	stdget_class_direct_superclasses_(ptr, classclass, &check);
#if 0
	GetCons(check, &left, &check);
	test(left == object, "clos_stdclass_metaclass9");
	test(check == Nil, "clos_stdclass_metaclass10");
#endif

	/* standard-class */
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	GetConst(CLOS_STANDARD_CLASS, &left);
	test(check == left, "clos_stdclass_metaclass11");
	test(check == metaclass, "clos_stdclass_metaclass12");
	clos_class_of_(check, &check);
	test(check == metaclass, "clos_stdclass_metaclass13");
	stdget_class_direct_superclasses_(ptr, metaclass, &check);
	GetCons(check, &left, &check);
	test(left == classclass, "clos_stdclass_metaclass14");
	test(check == Nil, "clos_stdclass_metaclass15");

	RETURN;
}

static int test_clos_stdclass_supers(void)
{
	addr instance, metaclass, name, slots, slot, symbol, supers, check;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	clos_stdclass_metaclass_(ptr, &metaclass);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	clos_find_class_(T, &check);
	list_heap(&supers, check, NULL);
	test_slot_vector_heap(&slots, 2);
	slot_heap_(ptr, &slot);
	internchar_debug(LISP_COMMON_USER, "AAA", &symbol);
	setname_slot_(ptr, slot, symbol);
	SetSlotVector(slots, 0, slot);
	internchar_debug(LISP_COMMON_USER, "BBB", &symbol);
	slot_heap_(ptr, &slot);
	setname_slot_(ptr, slot, symbol);
	SetSlotVector(slots, 1, slot);
	instance = Nil;
	clos_stdclass_supers_(ptr, &instance, metaclass, name, slots, supers);
	clos_class_of_(instance, &check);
	test(check == metaclass, "clos_stdclass_supers1");
	stdget_class_direct_superclasses_(ptr, instance, &supers);
	GetCons(supers, &check, &supers);
	clos_find_class_(T, &name);
	test(check == name, "clos_stdclass_supers2");
	test(supers == Nil, "clos_stdclass_supers3");
	stdget_class_direct_slots_(ptr, instance, &check);
	LenSlotVector(check, &size);
	test(size == 2, "clos_stdclass_supers4");

	RETURN;
}

static int test_clos_stdclass_type(void)
{
	addr instance, metaclass, name, supers, check, tclass;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	clos_stdclass_metaclass_(ptr, &metaclass);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	clos_find_class_(T, &tclass);
	list_heap(&supers, tclass, NULL);
	instance = Nil;
	clos_stdclass_type_(ptr, &instance, metaclass, name, supers);
	clos_class_of_(instance, &check);
	test(check == metaclass, "clos_stdclass_type1");
	stdget_class_direct_superclasses_(ptr, instance, &supers);
	GetCons(supers, &check, &supers);
	test(check == tclass, "clos_stdclass_type2");
	test(supers == Nil, "clos_stdclass_type3");
	stdget_class_direct_slots_(ptr, instance, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_type4");

	RETURN;
}

static int test_clos_stdclass_va(void)
{
	addr metaclass, clos, check, name;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	//test_forget_all_classes();

	SetConst(DEBUG1, readr_debug("debug1"));
	clos_stdclass_metaclass_(ptr, &metaclass);
	clos_stdclass_va_(ptr, metaclass,
			CONSTANT_DEBUG1,
			CONSTANT_DEBUG2,
			CONSTANT_CLOS_CLASS,
			CONSTANT_EMPTY);
	GetConst(DEBUG1, &clos);
	clos_find_class_(clos, &clos);
	clos_class_of_(clos, &check);
	test(check == metaclass, "clos_stdclass_va1");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCar(check, &check);
	GetConst(COMMON_CLASS, &name);
	clos_find_class_(name, &name);
	test(check == name, "clos_stdclass_va2");
	stdget_class_direct_slots_(ptr, clos, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_va3");

	GetConst(DEBUG1, &clos);
	clos_find_class_(clos, &clos);
	GetConst(DEBUG2, &check);
	test(clos == check, "clos_stdclass_va4");

	RETURN;
}

static int test_clos_stdclass_slotsconstant(void)
{
	addr metaclass, clos, check, slot, slots, name;
	Execute ptr;
	size_t size;

	ptr = Execute_Thread;
	test_slot_vector_heap(&slots, 2);
	slot_heap_(ptr, &slot);
	internchar_debug(LISP_COMMON_USER, "AAA", &name);
	setname_slot_(ptr, slot, name);
	SetSlotVector(slots, 0, slot);
	slot_heap_(ptr, &slot);
	internchar_debug(LISP_COMMON_USER, "BBB", &name);
	setname_slot_(ptr, slot, name);
	SetSlotVector(slots, 1, slot);

	SetConst(DEBUG2, readr_debug("debug2"));
	clos_stdclass_metaclass_(ptr, &metaclass);
	clos_stdclass_slotsconstant_(ptr, metaclass, slots,
			CONSTANT_DEBUG2,
			CONSTANT_DEBUG3,
			CONSTANT_CLOS_CLASS);
	GetConst(DEBUG2, &clos);
	clos_find_class_(clos, &clos);
	clos_class_of_(clos, &check);
	test(check == metaclass, "clos_stdclass_slotsconstant1");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCar(check, &check);
	GetConst(COMMON_CLASS, &name);
	clos_find_class_(name, &name);
	test(check == name, "clos_stdclass_slotsconstant2");
	stdget_class_direct_slots_(ptr, clos, &check);
	LenSlotVector(check, &size);
	test(size == 2, "clos_stdclass_slotsconstant3");

	GetConst(DEBUG2, &clos);
	clos_find_class_(clos, &clos);
	GetConst(DEBUG3, &check);
	test(clos == check, "clos_stdclass_slotsconstant4");

	RETURN;
}

static int test_build_standard_class(void)
{
	addr check;
	addr trueclass, classclass, builtinclass, metaclass, objectclass;
	addr classname, builtinname, metaname, objectname;
	Execute ptr;

	ptr = Execute_Thread;
	interncommon_debug("CLASS", &classname);
	interncommon_debug("BUILT-IN-CLASS", &builtinname);
	interncommon_debug("STANDARD-CLASS", &metaname);
	interncommon_debug("STANDARD-OBJECT", &objectname);
	clos_find_class_(T, &trueclass);
	clos_find_class_(classname, &classclass);
	clos_find_class_(builtinname, &builtinclass);
	clos_find_class_(metaname, &metaclass);
	clos_find_class_(objectname, &objectclass);
	test(closp(trueclass), "build_standard_class1");
	test(closp(classclass), "build_standard_class2");
	test(closp(builtinclass), "build_standard_class3");
	test(closp(metaclass), "build_standard_class4");
	test(closp(objectclass), "build_standard_class5");
	stdget_class_name_(ptr, trueclass, &check);
	test(check == T, "build_standard_class6");
	stdget_class_name_(ptr, classclass, &check);
	test(check == classname, "build_standard_class7");
	stdget_class_name_(ptr, builtinclass, &check);
	test(check == builtinname, "build_standard_class8");
	stdget_class_name_(ptr, metaclass, &check);
	test(check == metaname, "build_standard_class9");
	stdget_class_name_(ptr, objectclass, &check);
	test(check == objectname, "build_standard_class10");

	/* t */
	clos_class_of_(trueclass, &check);
	test(check == builtinclass, "build_standard_class11");
	stdget_class_direct_superclasses_(ptr, trueclass, &check);
	test(check == Nil, "build_standard_class12");
	stdget_class_direct_subclasses_(ptr, trueclass, &check);
	test(find_list_eq_unsafe(objectclass, check), "build_standard_class13");

	/* standard-object */
	clos_class_of_(objectclass, &check);
	test(metaclass == check, "build_standard_class14");
	test(clos_subclass_p_debug(objectclass, trueclass), "build_standard_class15");
#if 0
	stdget_class_direct_subclasses_(ptr, objectclass, &check);
	test(find_list_eq_unsafe(classclass, check), "build_standard_class16");
#endif

	/* class */
	clos_class_of_(classclass, &check);
	test(metaclass == check, "build_standard_class17");
	test(clos_subclass_p_debug(classclass, objectclass), "build_standard_class18");
	test(clos_subclass_p_debug(classclass, trueclass), "build_standard_class19");
	stdget_class_direct_subclasses_(ptr, classclass, &check);
	test(find_list_eq_unsafe(metaclass, check), "build_standard_class20");
	test(find_list_eq_unsafe(builtinclass, check), "build_standard_class21");

	/* standard-class */
	clos_class_of_(metaclass, &check);
	test(metaclass == check, "build_standard_class22");
	test(clos_subclass_p_debug(metaclass, classclass), "build_standard_class23");
	test(clos_subclass_p_debug(metaclass, objectclass), "build_standard_class24");
	test(clos_subclass_p_debug(metaclass, trueclass), "build_standard_class25");
	stdget_class_direct_subclasses_(ptr, metaclass, &check);
	test(check == Nil, "build_standard_class26");

	/* built-in-class */
	clos_class_of_(builtinclass, &check);
	test(metaclass == check, "build_standard_class27");
	test(clos_subclass_p_debug(builtinclass, objectclass), "build_standard_class28");
	test(clos_subclass_p_debug(builtinclass, classclass), "build_standard_class29");
	test(clos_subclass_p_debug(builtinclass, trueclass), "build_standard_class30");
	stdget_class_direct_subclasses_(ptr, builtinclass, &check);
	test(check == Nil, "build_standard_class31");

	/* constant */
	GetConst(CLOS_CLASS, &check);
	test(classclass == check, "build_standard_class32");
	GetConst(CLOS_STANDARD_CLASS, &check);
	test(metaclass == check, "build_standard_class33");

	RETURN;
}


/*
 *  standard-generic-function
 */
static int test_clos_stdgeneric_slots(void)
{
	int check;
	Execute ptr;
	addr slots, pos;
	size_t i, size;

	ptr = Execute_Thread;
	check = 0;

	clos_stdgeneric_slots_(ptr, &slots);
	LenSlotVector(slots, &size);
	test(size == Clos_generic_size, "clos_stdgeneric_slots1");

	for (check = 1, i = 0; i < Clos_generic_size; i++) {
		GetSlotVector(slots, i, &pos);
		getlocation_slot_(ptr, pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "clos_stdgeneric_slots2");
	test(test_slotname(slots, Clos_generic_name, "NAME"),
			"clos_stdgeneric_slots3");
	test(test_slotname(slots, Clos_generic_lambda_list, "LAMBDA-LIST"),
			"clos_stdgeneric_slots4");
	test(test_slotname(slots, Clos_generic_methods, "METHODS"),
			"clos_stdgeneric_slots5");
	test(test_slotname(slots, Clos_generic_method_class, "METHOD-CLASS"),
			"clos_stdgeneric_slots6");
	test(test_slotname(slots, Clos_generic_argument_precedence_order,
				"ARGUMENT-PRECEDENCE-ORDER"),
			"clos_stdgeneric_slots7");
	test(test_slotname(slots, Clos_generic_declarations, "DECLARATIONS"),
			"clos_stdgeneric_slots8");
	test(test_slotname(slots, Clos_generic_method_combination, "METHOD-COMBINATION"),
			"clos_stdgeneric_slots9");
	test(test_slotname(slots, Clos_generic_eqlcheck, "EQLCHECK"),
			"clos_stdgeneric_slots10");
	test(test_slotname(slots, Clos_generic_cache, "CACHE"),
			"clos_stdgeneric_slots11");
	test(test_slotname(slots, Clos_generic_call, "CALL"),
			"clos_stdgeneric_slots12");

	RETURN;
}

static int test_build_clos_class_generic(void)
{
	addr pos, fclass, gclass, sgclass, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	test_forget_all_classes();
	build_clos_class(ptr);

	/* function */
	GetConst(COMMON_FUNCTION, &fclass);
	clos_find_class_(fclass, &fclass);
	test(closp(fclass), "build_clos_class_generic1");
	clos_class_of_(fclass, &pos);
	GetConst(COMMON_BUILT_IN_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_generic2");
	stdget_class_direct_superclasses_(ptr, fclass, &check);
	GetCons(check, &left, &check);
	clos_find_class_(T, &pos);
	test(left == pos, "build_clos_class_generic3");
	test(check == Nil, "build_clos_class_generic4");

	/* generic-function */
	GetConst(COMMON_GENERIC_FUNCTION, &gclass);
	clos_find_class_(gclass, &gclass);
	test(closp(gclass), "build_clos_class_generic5");
	clos_class_of_(gclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_generic6");
#if 0
	stdget_class_direct_superclasses_(ptr, gclass, &check);
	GetCons(check, &left, &check);
	test(left == fclass, "build_clos_class_generic7");
	test(check != Nil, "build_clos_class_generic8");
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_generic9");
	GetConst(CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, &check);
	clos_find_class_(check, &check);
	test(left == check, "build_clos_class_generic10");
#endif

	/* standard-generic-function */
	GetConst(COMMON_STANDARD_GENERIC_FUNCTION, &sgclass);
	clos_find_class_(sgclass, &sgclass);
	test(closp(sgclass), "build_clos_class_generic11");
	clos_class_of_(sgclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_generic12");
	stdget_class_direct_superclasses_(ptr, sgclass, &check);
	GetCons(check, &left, &check);
	test(left == gclass, "build_clos_class_generic13");
	test(check == Nil, "build_clos_class_generic14");

	/* constant */
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	test(pos == sgclass, "build_clos_class_generic15");

	RETURN;
}

static int test_generic_function_instance(void)
{
	addr pos, name;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	GetConst(CLOSNAME_EQLCHECK, &name);
	test(clos_slot_exists_p(pos, name), "generic_function_instance1");

	RETURN;
}


/*
 *  standard-method
 */
static int test_clos_stdmethod_slots(void)
{
	int check;
	Execute ptr;
	addr slots, pos;
	size_t i, size;

	ptr = Execute_Thread;
	check = 0;

	clos_stdmethod_slots_(ptr, &slots);
	LenSlotVector(slots, &size);
	test(size == Clos_method_size, "clos_stdmethod_slots1");

	for (check = 1, i = 0; i < Clos_method_size; i++) {
		GetSlotVector(slots, i, &pos);
		getlocation_slot_(ptr, pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "clos_stdmethod_slots2");
	test(test_slotname(slots, Clos_method_function, "FUNCTION"),
			"clos_stdmethod_slots3");
	test(test_slotname(slots, Clos_method_generic_function, "GENERIC-FUNCTION"),
			"clos_stdmethod_slots4");
	test(test_slotname(slots, Clos_method_lambda_list, "LAMBDA-LIST"),
			"clos_stdmethod_slots5");
	test(test_slotname(slots, Clos_method_qualifiers, "QUALIFIERS"),
			"clos_stdmethod_slots6");
	test(test_slotname(slots, Clos_method_specializers, "SPECIALIZERS"),
			"clos_stdmethod_slots7");

	RETURN;
}

static int test_build_clos_class_method(void)
{
	addr pos, mclass, smclass, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	test_forget_all_classes();
	build_clos_class(ptr);

	/* method */
	GetConst(COMMON_METHOD, &mclass);
	clos_find_class_(mclass, &mclass);
	test(closp(mclass), "build_clos_class_method1");
	clos_class_of_(mclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_method2");
	stdget_class_direct_superclasses_(ptr, mclass, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_method3");
#if 0
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class_(check, &check);
	test(left == check, "build_clos_class_method4");
#endif

	/* standard-method */
	GetConst(COMMON_STANDARD_METHOD, &smclass);
	clos_find_class_(smclass, &smclass);
	test(closp(smclass), "build_clos_class_method5");
	clos_class_of_(smclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_method6");
	stdget_class_direct_superclasses_(ptr, smclass, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_method7");
	test(left == mclass, "build_clos_class_method8");

	RETURN;
}


#if 0
/*
 *  method-combination
 */
static int test_clos_stdcombination_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	check = 0;

	clos_stdcombination_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_combination_size, "clos_stdcombination_slots1");

	for (check = 1, i = 0; i < Clos_combination_size; i++) {
		GetSlotVector(slots, i, &pos);
		getlocation_slot_(ptr, pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "clos_stdcombination_slots2");
	test(test_slotname(slots, Clos_combination_name, "NAME"),
			"clos_stdcombination_slots3");
	test(test_slotname(slots, Clos_combination_long_p, "LONG-P"),
			"clos_stdcombination_slots4");
	test(test_slotname(slots, Clos_combination_document, "DOCUMENTATION"),
			"clos_stdcombination_slots5");
	test(test_slotname(slots, Clos_combination_identity, "IDENTITY"),
			"clos_stdcombination_slots6");
	test(test_slotname(slots, Clos_combination_operator, "OPERATOR"),
			"clos_stdcombination_slots7");
	test(test_slotname(slots, Clos_combination_lambda_list, "LAMBDA-LIST"),
			"clos_stdcombination_slots8");
	test(test_slotname(slots, Clos_combination_qualifiers, "QUALIFIERS"),
			"clos_stdcombination_slots9");
	test(test_slotname(slots, Clos_combination_arguments, "ARGUMENTS"),
			"clos_stdcombination_slots10");
	test(test_slotname(slots, Clos_combination_generic, "GENERIC"),
			"clos_stdcombination_slots11");
	test(test_slotname(slots, Clos_combination_form, "FORM"),
			"clos_stdcombination_slots12");
	test(test_slotname(slots, Clos_combination_function, "FUNCTION"),
			"clos_stdcombination_slots13");

	RETURN;
}

static int test_build_clos_method_combination(void)
{
	addr pos, clos, left, check;

	test_forget_all_classes();
	build_clos_class(Execute_Thread);

	/* method_combination */
	GetConst(COMMON_METHOD_COMBINATION, &clos);
	clos_find_class_(clos, &clos);
	test(closp(clos), "build_clos_method_combination1");
	clos_class_of_(clos, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_method_combination2");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_method_combination3");
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class_(check, &check);
	test(left == check, "build_clos_method_combination4");

	RETURN;
}

static int test_build_clos_class_combination(void)
{
	addr pos, clos;

	GetConst(CLOS_METHOD_COMBINATION, &clos);
	GetConst(COMMON_STANDARD, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination1");
	GetConst(COMMON_PLUS, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination2");
	GetConst(COMMON_AND, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination3");
	GetConst(COMMON_APPEND, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination4");
	GetConst(COMMON_LIST, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination5");
	GetConst(COMMON_MAX, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination6");
	GetConst(COMMON_MIN, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination7");
	GetConst(COMMON_NCONC, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination8");
	GetConst(COMMON_PROGN, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p_debug(pos, clos), "build_clos_class_combination9");

	RETURN;
}
#endif


/*
 *  eql-specializer
 */
static int test_clos_stdspecializer_slots(void)
{
	int check;
	Execute ptr;
	addr slots, pos;
	size_t i, size;

	ptr = Execute_Thread;
	check = 0;

	clos_stdspecializer_slots_(ptr, &slots);
	LenSlotVector(slots, &size);
	test(size == Clos_specializer_size, "clos_stdspecializer_slots1");

	for (check = 1, i = 0; i < Clos_specializer_size; i++) {
		GetSlotVector(slots, i, &pos);
		getlocation_slot_(ptr, pos, &size);
		if (i != size) {
			check = 0;
			break;
		}
	}
	test(check, "clos_stdspecializer_slots2");
	test(test_slotname(slots, Clos_specializer_object, "OBJECT"),
			"clos_stdspecializer_slots3");
	test(test_slotname(slots, Clos_specializer_type, "TYPE"),
			"clos_stdspecializer_slots4");

	RETURN;
}

static int test_build_clos_class_specializer(void)
{
	addr pos, clos, left, check;
	Execute ptr;

	ptr = Execute_Thread;
	/* eql_specializer */
	GetConst(CLOSNAME_EQL_SPECIALIZER, &clos);
	clos_find_class_(clos, &clos);
	test(closp(clos), "build_clos_class_specializer1");
	clos_class_of_(clos, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class_(check, &check);
	test(check == pos, "build_clos_class_specializer2");
	stdget_class_direct_superclasses_(ptr, clos, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_specializer3");
#if 0
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class_(check, &check);
	test(left == check, "build_clos_class_specializer4");
#endif

	RETURN;
}


/*
 *  class-check
 */
static int checkclass_name(addr a, addr b)
{
	return symbolp(a) && symbolp(b) && a == b;
}

static int checkclass_va(constindex index, const char *name, va_list args)
{
	addr x, y;
	addr a, b;
	const char *str;
	Execute ptr;

	ptr = Execute_Thread;
	/* name, clos */
	a = readr_debug(name);
	clos_find_class_(a, &x);
	if (! closp(x)) {
		degrade_printf("type1 %s error.\n", name);
		return 0;
	}
	stdget_class_name_(ptr, x, &b);
	if (! checkclass_name(a, b)) {
		degrade_printf("name1 %s error\n", name);
		return 0;
	}

	/* class-of */
	GetClassOfClos(x, &a);
	GetConstant(index, &b);
	if (a != b) {
		degrade_printf("class-of %s error\n", name);
		return 0;
	}

	/* precedence-list */
	for (;;) {
		str = va_arg(args, const char *);
		if (str == NULL)
			break;
		a = readr_debug(str);
		clos_find_class_(a, &y);
		if (! closp(y)) {
			degrade_printf("type2 %s, %s error.\n", name, str);
			return 0;
		}
		stdget_class_name_(ptr, y, &b);
		if (! checkclass_name(a, b)) {
			degrade_printf("name2 %s, %s error\n", name, str);
			return 0;
		}
		if (! clos_subclass_p_debug(x, y)) {
			degrade_printf("subclass_p %s, %s error\n", name, str);
			return 0;
		}
	}

	return 1;
}

static int checkclass(const char *name, ...)
{
	int check;
	char data[256];
	va_list args;

	check = 0;

	snprintf(data, 256, "supers-%s", name);
	va_start(args, name);
	check = checkclass_va(CONSTANT_CLOS_STANDARD_CLASS, name, args);
	va_end(args);

	return degrade_test(check, data);
}

#define CheckClass2(a,b) \
	if (checkclass((a),(b),NULL)) goto error;
#define CheckClass3(a,b,c) \
	if (checkclass((a),(b),(c),NULL)) goto error;
#define CheckClass4(a,b,c,d) \
	if (checkclass((a),(b),(c),NULL)) goto error;
#define CheckClass5(a,b,c,d,e) \
	if (checkclass((a),(b),(c),NULL)) goto error;
#define CheckClass6(a,b,c,d,e,f) \
	if (checkclass((a),(b),(c),(d),(e),(f),NULL)) goto error;
#define CheckClass7(a,b,c,d,e,f,g) \
	if (checkclass((a),(b),(c),(d),(e),(f),(g),NULL)) goto error;

static int checkbuilt(const char *name, ...)
{
	int check;
	char data[256];
	va_list args;

	check = 0;

	snprintf(data, 256, "supers-%s", name);
	va_start(args, name);
	check = checkclass_va(CONSTANT_CLOS_BUILT_IN_CLASS, name, args);
	va_end(args);

	return degrade_test(check, data);
}

#define CheckBuilt1(a) \
	if (checkbuilt((a),NULL)) goto error;
#define CheckBuilt2(a,b) \
	if (checkbuilt((a),(b),NULL)) goto error;
#define CheckBuilt3(a,b,c) \
	if (checkbuilt((a),(b),(c),NULL)) goto error;
#define CheckBuilt4(a,b,c,d) \
	if (checkbuilt((a),(b),(c),(d),NULL)) goto error;
#define CheckBuilt5(a,b,c,d,e) \
	if (checkbuilt((a),(b),(c),(d),(e),NULL)) goto error;
#define CheckBuilt6(a,b,c,d,e,f) \
	if (checkbuilt((a),(b),(c),(d),(e),(f),NULL)) goto error;
#define CheckBuilt7(a,b,c,d,e,f,g) \
	if (checkbuilt((a),(b),(c),(d),(e),(f),(g),NULL)) goto error;
#define CheckBuilt8(a,b,c,d,e,f,g,h) \
	if (checkbuilt((a),(b),(c),(d),(e),(f),(g),(h),NULL)) goto error;
#define CheckBuilt9(a,b,c,d,e,f,g,h,i) \
	if (checkbuilt((a),(b),(c),(d),(e),(f),(g),(h),(i),NULL)) goto error;

static int checkstructure(const char *name, ...)
{
	int check;
	char data[256];
	va_list args;

	check = 0;

	snprintf(data, 256, "supers-%s", name);
	va_start(args, name);
	check = checkclass_va(CONSTANT_CLOS_STRUCTURE_CLASS, name, args);
	va_end(args);

	return degrade_test(check, data);
}

#define CheckStruct2(a,b) \
	if (checkstructure((a),(b),NULL)) goto error;

static addr readr_clos(const char *str)
{
	addr control, ret, symbol, value;
	Execute ptr;

	ptr = Execute_Thread;
	push_control(ptr, &control);
	GetConst(SPECIAL_PACKAGE, &symbol);
	GetConst(PACKAGE_CLOS, &value);
	pushspecial_control(ptr, symbol, value);
	ret = readr_debug(str);
	pop_control_(ptr, control);

	return ret;
}

static int checkslots_va(const char *name, va_list args)
{
	addr x, y;
	const char *str;
	size_t size, check;
	Execute ptr;

	ptr = Execute_Thread;
	clos_find_class_(readr_debug(name), &x);
	clos_instance_heap_(ptr, x, &x);
	for (size = 0; ; size++) {
		str = va_arg(args, const char *);
		if (str == NULL)
			break;
		clos_get_(x, readr_clos(str), &y);
	}
	GetSlotClos(x, &x);
	LenSlotVector(x, &check);

	return size == check;
}

static int checkslots(const char *name, ...)
{
	int check;
	char data[256];
	va_list args;

	check = 0;

	snprintf(data, 256, "slots-%s", name);
	va_start(args, name);
	check = checkslots_va(name, args);
	va_end(args);

	return degrade_test(check, data);
}

#define CheckSlots0(a) \
	if (checkslots((a),NULL)) goto error;
#define CheckSlots1(a,b) \
	if (checkslots((a),(b),NULL)) goto error;
#define CheckSlots2(a,b,c) \
	if (checkslots((a),(b),(c),NULL)) goto error;
#define CheckSlots4(a,b,c,d,e) \
	if (checkslots((a),(b),(c),(d),(e),NULL)) goto error;

static int test_class_check_standard(void)
{
	CheckBuilt1("t");
	CheckBuilt2("function", "t");
	CheckStruct2("structure-object", "t");
	CheckClass2("method", "t");
	CheckClass2("method-combination", "t");
	CheckClass2("standard-object", "t");
	CheckClass3("class", "standard-object", "t");
	CheckClass3("generic-function", "function", "t");
	CheckClass4("standard-class", "class", "standard-object", "t");
	CheckClass4("structure-class", "class", "standard-object", "t");
	CheckClass4("standard-generic-function", "generic-function", "function", "t");
	CheckClass4("standard-method", "method", "standard-object", "t");
	CheckClass4("built-in-class", "class", "standard-object", "t");

	RETURN;
}

static int test_class_check_condition(void)
{
	/* precedence-list */
	CheckClass5("arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass5("cell-error", "error", "serious-condition", "condition", "t");
	CheckClass2("condition", /*"standard-object",*/ "t");
	CheckClass5("control-error", "error", "serious-condition", "condition", "t");
	CheckClass6("division-by-zero",
			"arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass6("end-of-file",
			"stream-error", "error", "serious-condition", "condition", "t");
	CheckClass4("error", "serious-condition", "condition", "t");
	CheckClass5("file-error", "error", "serious-condition", "condition", "t");
	CheckClass6("floating-point-inexact",
			"arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass6("floating-point-invalid-operation",
			"arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass6("floating-point-overflow",
			"arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass6("floating-point-underflow",
			"arithmetic-error", "error", "serious-condition", "condition", "t");
	CheckClass5("package-error", "error", "serious-condition", "condition", "t");
	CheckClass5("parse-error", "error", "serious-condition", "condition", "t");
	CheckClass5("print-not-readable", "error", "serious-condition", "condition", "t");
	CheckClass5("program-error", "error", "serious-condition", "condition", "t");
	CheckClass6("reader-error",
			"parse-error", "error", "serious-condition", "condition", "t");
	CheckClass3("serious-condition", "condition", "t");
	CheckClass3("simple-condition", "condition", "t");
	CheckClass6("simple-error",
			"simple-condition", "error", "serious-condition", "condition", "t");
	CheckClass7("simple-type-error", "simple-condition",
			"type-error", "error", "serious-condition", "condition", "t");
	CheckClass5("simple-warning", "simple-condition", "warning", "condition", "t");
	CheckClass4("storage-condition", "serious-condition", "condition", "t");
	CheckClass5("stream-error", "error", "serious-condition", "condition", "t");
	CheckClass4("style-warning", "warning", "condition", "t");
	CheckClass5("type-error", "error", "serious-condition", "condition", "t");
	CheckClass6("unbound-slot",
			"cell-error", "error", "serious-condition", "condition", "t");
	CheckClass6("unbound-variable",
			"cell-error", "error", "serious-condition", "condition", "t");
	CheckClass6("undefined-function",
			"cell-error", "error", "serious-condition", "condition", "t");
	CheckClass3("warning", "condition", "t");

	/* slots */
	CheckSlots2("arithmetic-error", "operation", "operands");
	CheckSlots1("cell-error", "name");
	CheckSlots0("condition");
	CheckSlots0("control-error");
	CheckSlots2("division-by-zero", "operation", "operands");
	CheckSlots1("end-of-file", "stream");
	CheckSlots0("error");
	CheckSlots1("file-error", "pathname");
	CheckSlots2("floating-point-inexact", "operation", "operands");
	CheckSlots2("floating-point-invalid-operation", "operation", "operands");
	CheckSlots2("floating-point-overflow", "operation", "operands");
	CheckSlots2("floating-point-underflow", "operation", "operands");
	CheckSlots1("package-error", "package");
	CheckSlots0("parse-error");
	CheckSlots1("print-not-readable", "object");
	CheckSlots0("program-error");
	CheckSlots1("reader-error", "stream");
	CheckSlots0("serious-condition");
	CheckSlots2("simple-condition", "format-control", "format-arguments");
	CheckSlots2("simple-error", "format-control", "format-arguments");
	CheckSlots4("simple-type-error",
			"format-control", "format-arguments", "datum", "expected-type");
	CheckSlots2("simple-warning", "format-control", "format-arguments");
	CheckSlots0("storage-condition");
	CheckSlots1("stream-error", "stream");
	CheckSlots0("style-warning");
	CheckSlots2("type-error", "datum", "expected-type");
	CheckSlots1("unbound-variable", "name");
	CheckSlots1("undefined-function", "name");
	CheckSlots0("warning");

	RETURN;
}

static int test_class_check_builtin(void)
{
	CheckBuilt1("t");
	CheckBuilt2("array", "t");
	CheckBuilt2("character", "t");
	CheckBuilt2("hash-table", "t");
	CheckBuilt2("number", "t");
	CheckBuilt2("package", "t");
	CheckBuilt2("pathname", "t");
	CheckBuilt2("random-state", "t");
	CheckBuilt2("restart", "t");
	CheckBuilt2("sequence", "t");
	CheckBuilt2("stream", "t");
	CheckBuilt2("symbol", "t");
	CheckBuilt3("logical-pathname", "pathname", "t");
	CheckBuilt3("list", "sequence", "t");
	CheckBuilt4("cons", "list", "sequence", "t");
	CheckBuilt3("vector", "sequence", "t");
	CheckBuilt4("bit-vector", "vector", "sequence", "t");
	CheckBuilt5("null", "symbol", "list", "sequence", "t");
	CheckBuilt4("string", "vector", "sequence", "t");
	CheckBuilt3("complex", "number", "t");
	CheckBuilt3("real", "number", "t");
	CheckBuilt4("float", "real", "number", "t");
	CheckBuilt4("rational", "real", "number", "t");
	CheckBuilt4("integer", "real", "number", "t");
	CheckBuilt5("ratio", "rational", "real", "number", "t");
	CheckBuilt3("broadcast-stream", "stream", "t");
	CheckBuilt3("concatenated-stream", "stream", "t");
	CheckBuilt3("echo-stream", "stream", "t");
	CheckBuilt3("file-stream", "stream", "t");
	CheckBuilt3("string-stream", "stream", "t");
	CheckBuilt3("synonym-stream", "stream", "t");
	CheckBuilt3("two-way-stream", "stream", "t");
	CheckBuilt3("base-char", "character", "t");
	CheckBuilt3("extended-char", "character", "t");
	CheckBuilt3("standard-char", "character", "t");
	CheckBuilt3("simple-array", "array", "t");
	CheckBuilt6("simple-vector", "vector", "sequence", "simple-array", "array", "t");
	CheckBuilt5("base-string", "string", "vector", "sequence", "t");
	CheckBuilt7("simple-string",
			"string", "vector", "sequence", "simple-array", "array", "t");
	CheckBuilt9("simple-base-string", "simple-string", "base-string",
			"string", "vector", "sequence", "simple-array", "array", "t");
	CheckBuilt7("simple-bit-vector",
			"bit-vector", "vector", "sequence", "simple-array", "array", "t");
	CheckBuilt5("bignum", "integer", "real", "number", "t");
	CheckBuilt5("fixnum", "integer", "real", "number", "t");
	CheckBuilt5("short-float", "float", "real", "number", "t");
	CheckBuilt5("single-float", "float", "real", "number", "t");
	CheckBuilt5("double-float", "float", "real", "number", "t");
	CheckBuilt5("long-float", "float", "real", "number", "t");
	CheckBuilt5("signed-byte", "integer", "real", "number", "t");
	CheckBuilt6("unsigned-byte", "signed-byte", "integer", "real", "number", "t");
	CheckBuilt7("bit",
			"unsigned-byte", "signed-byte", "integer", "real", "number", "t");
	CheckBuilt2("function", "t");
	CheckBuilt3("compiled-function", "function", "t");
	CheckBuilt3("keyword", "symbol", "t");

	RETURN;
}


/*
 *  main
 */
static int testcase_clos_build(void)
{
	/* standard-class */
	TestBreak(test_slot_make_name_symbol);
	TestBreak(test_slot_make_name);
	TestBreak(test_slot_make_form);
	TestBreak(test_slot_make_version);
	TestBreak(test_slotvector_set_location);
	TestBreak(test_clos_stdclass_slots);
	TestBreak(test_clos_stdclass_dummy);
	TestBreak(test_clos_stdclass_make);
	TestBreak(test_clos_stdclass_empty);
	TestBreak(test_clos_stdclass_class_of);
	TestBreak(test_clos_stdclass_inherit);
	TestBreak(test_clos_stdclass_single);
	TestBreak(test_clos_stdclass_metaclass);
	TestBreak(test_clos_stdclass_supers);
	TestBreak(test_clos_stdclass_type);
	TestBreak(test_clos_stdclass_va);
	TestBreak(test_clos_stdclass_slotsconstant);
	TestBreak(test_build_standard_class);
	/* standard-generic-function */
	TestBreak(test_clos_stdgeneric_slots);
	TestBreak(test_build_clos_class_generic);
	TestBreak(test_generic_function_instance);
	/* standard-method */
	TestBreak(test_clos_stdmethod_slots);
	TestBreak(test_build_clos_class_method);
	/* method-combination */
#if 0
	TestBreak(test_clos_stdcombination_slots);
	TestBreak(test_build_clos_method_combination);
	TestBreak(test_build_clos_class_combination);
#endif
	/* eql-specializer */
	TestBreak(test_clos_stdspecializer_slots);
	TestBreak(test_build_clos_class_specializer);
	/* class-check */
	TestBreak(test_class_check_standard);
	TestBreak(test_class_check_condition);
	TestBreak(test_class_check_builtin);

	return 0;
}

static void testinit_clos_build(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_real();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_common();
	build_reader();
}

int test_clos_build(void)
{
	DegradeTitle;
	return DegradeCode(clos_build);
}

