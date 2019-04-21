#include "clos_class.c"
#include "clos_type.h"
#include "character.h"
#include "common.h"
#include "real.h"
#include "degrade.h"
#include "package.h"
#include "readtable.h"
#include "stream.h"
#include "strtype.h"
#include "syscall.h"
#include "type.h"

static void clos_supers_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr super, cons, clos, slots;

	/* make cons */
	cons = Nil;
	for (;;) {
		super = va_arg(args, addr);
		if (super == NULL) break;
		cons_alloc(local, &cons, super, cons);
	}

	/* make clos */
	clos_stdclass_slots(&slots);
	clos_alloc(local, &clos, slots);
	SetClassOfClos(clos, Nil);
	nreverse_list_unsafe(&cons, cons);
	stdset_class_direct_superclasses(clos, cons);
	*ret = clos;
}
static void clos_supers_local(LocalRoot local, addr *ret, ...)
{
	va_list args;

	Check(local == NULL, "local error");
	va_start(args, ret);
	clos_supers_alloc(local, ret, args);
	va_end(args);
}
static void clos_supers_heap(addr *ret, ...)
{
	va_list args;

	va_start(args, ret);
	clos_supers_alloc(NULL, ret, args);
	va_end(args);
}


/*
 *  access
 */
static int test_stdget_class_name(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(KEYWORD_NAME, &k);
	clos_set(pos, k, v);
	stdget_class_name(pos, &check);
	test(check == v, "stdget_class_name1");

	stdset_class_name(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_name1");

	RETURN;
}

static int test_stdget_class_direct_slots(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DIRECT_SLOTS, &k);
	clos_set(pos, k, v);
	stdget_class_direct_slots(pos, &check);
	test(check == v, "stdget_class_direct_slots1");

	stdset_class_direct_slots(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_direct_slots1");

	RETURN;
}

static int test_stdget_class_direct_subclasses(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DIRECT_SUBCLASSES, &k);
	clos_set(pos, k, v);
	stdget_class_direct_subclasses(pos, &check);
	test(check == v, "stdget_class_direct_subclasses1");

	stdset_class_direct_subclasses(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_direct_subclasses1");

	RETURN;
}

static int test_stdget_class_direct_superclasses(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DIRECT_SUPERCLASSES, &k);
	clos_set(pos, k, v);
	stdget_class_direct_superclasses(pos, &check);
	test(check == v, "stdget_class_direct_superclasses1");

	stdset_class_direct_superclasses(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_direct_superclasses1");

	RETURN;
}

static int test_stdget_class_precedence_list(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_CLASS_PRECEDENCE_LIST, &k);
	clos_set(pos, k, v);
	stdget_class_precedence_list(pos, &check);
	test(check == v, "stdget_class_precedence_list1");

	stdset_class_precedence_list(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_precedence_list2");

	RETURN;
}

static int test_stdget_class_slots(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_EFFECTIVE_SLOTS, &k);
	clos_set(pos, k, v);
	stdget_class_slots(pos, &check);
	test(check == v, "stdget_class_slots1");

	stdset_class_slots(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_slots2");

	RETURN;
}

static int test_stdget_class_finalized_p(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_FINALIZED_P, &k);
	clos_set(pos, k, v);
	stdget_class_finalized_p(pos, &check);
	test(check == v, "stdget_class_finalized_p1");

	stdset_class_finalized_p(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_finalized_p2");

	RETURN;
}

static int test_stdget_class_prototype(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_PROTOTYPE, &k);
	clos_set(pos, k, v);
	stdget_class_prototype(pos, &check);
	test(check == v, "stdget_class_prototype1");

	stdset_class_prototype(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_prototype2");

	RETURN;
}

static int test_stdget_class_direct_methods(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DIRECT_METHODS, &k);
	clos_set(pos, k, v);
	stdget_class_direct_methods(pos, &check);
	test(check == v, "stdget_class_direct_methods1");

	stdset_class_direct_methods(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_direct_methods2");

	RETURN;
}

static int test_stdget_class_default_initargs(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DEFAULT_INITARGS, &k);
	clos_set(pos, k, v);
	stdget_class_default_initargs(pos, &check);
	test(check == v, "stdget_class_default_initargs1");

	stdset_class_default_initargs(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_default_initargs2");

	RETURN;
}

static int test_stdget_class_direct_default_initargs(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DIRECT_DEFAULT_INITARGS, &k);
	clos_set(pos, k, v);
	stdget_class_direct_default_initargs(pos, &check);
	test(check == v, "stdget_class_direct_default_initargs1");

	stdset_class_direct_default_initargs(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_direct_default_initargs2");

	RETURN;
}

static int test_stdget_class_version(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_VERSION, &k);
	clos_set(pos, k, v);
	stdget_class_version(pos, &check);
	test(check == v, "stdget_class_version1");

	stdset_class_version(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_version2");

	RETURN;
}

static int test_stdget_class_update_info(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_UPDATE_INFO, &k);
	clos_set(pos, k, v);
	stdget_class_update_info(pos, &check);
	test(check == v, "stdget_class_update_info1");

	stdset_class_update_info(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_update_info2");

	RETURN;
}

static int test_stdget_class_document(void)
{
	addr pos, check, k, v;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	v = readr("aaa");
	GetConst(CLOSKEY_DOCUMENT, &k);
	clos_set(pos, k, v);
	stdget_class_document(pos, &check);
	test(check == v, "stdget_class_document1");

	stdset_class_document(pos, T);
	clos_get(pos, k, &check);
	test(check == T, "stdset_class_document2");

	RETURN;
}


/*
 *  check
 */
static int test_clos_subclass_p(void)
{
	addr pos, clos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	GetConst(CLOS_CLASS, &clos);
	test(clos_subclass_p(pos, clos), "clos_subclass_p1");
	test(! clos_subclass_p(clos, pos), "clos_subclass_p2");

	GetConst(CLOS_STANDARD_CLASS, &clos);
	clos_instance_heap(clos, &pos);
	test(! clos_subclass_p(clos, pos), "clos_subclass_p3");

	RETURN;
}

static int test_clos_subtype_p(void)
{
	addr pos, clos;

	GetConst(CLOS_METHOD, &pos);
	GetConst(CLOS_STANDARD_METHOD, &clos);
	test(! clos_subtype_p(pos, clos), "clos_subtype_p1");
	test(! clos_subtype_p(clos, pos), "clos_subtype_p2");

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	GetConst(CLOS_CLASS, &clos);
	test(clos_subtype_p(pos, clos), "clos_subtype_p3");

	RETURN;
}

static int test_clos_subclass_p2(void)
{
	addr t, a, b, cons;
	LocalRoot local;

	local = Local_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, NULL);
	clos_supers_heap(&b, t, NULL);
	clos_precedence_list(local, t, &cons);
	stdset_class_precedence_list(t, cons);
	clos_precedence_list(local, a, &cons);
	stdset_class_precedence_list(a, cons);
	clos_precedence_list(local, b, &cons);
	stdset_class_precedence_list(b, cons);

	test(clos_subclass_p(t, t), "clos_subclass_p1");
	test(! clos_subclass_p(t, a), "clos_subclass_p2");
	test(! clos_subclass_p(a, t), "clos_subclass_p3");
	test(clos_subclass_p(b, t), "clos_subclass_p4");
	test(! clos_subclass_p(t, b), "clos_subclass_p5");
	test(! clos_subclass_p(t, b), "clos_subclass_p6");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, t, NULL);
	clos_supers_heap(&b, a, NULL);
	clos_precedence_list(local, t, &cons);
	stdset_class_precedence_list(t, cons);
	clos_precedence_list(local, b, &cons);
	stdset_class_precedence_list(b, cons);
	test(clos_subclass_p(b, t), "clos_subclass_p7");
	test(! clos_subclass_p(t, b), "clos_subclass_p8");

	RETURN;
}

static int test_clos_subtype_p2(void)
{
	addr metaclass, clos, name, instance;
	LocalRoot local;

	local = Local_Thread;
	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_stdclass_type(local, &clos, metaclass, name, Nil);
	clos_instance_heap(clos, &instance);

	test(clos_subtype_p(instance, clos), "clos_subtype_p1");
	test(! clos_subtype_p(clos, instance), "clos_subtype_p2");

	RETURN;
}

static int test_clos_class_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(clos_class_p(pos), "clos_class_p1");
	GetConst(CLOS_STANDARD_OBJECT, &pos);
	test(clos_class_p(pos), "clos_class_p2");
	GetConst(CLOS_COMBINATION_STANDARD, &pos);
	test(! clos_class_p(pos), "clos_class_p3");
	GetConst(CLOS_CLASS, &pos);
	test(clos_class_p(pos), "clos_class_p4");
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap(pos, &pos);
	test(clos_class_p(pos), "clos_class_p5");

	RETURN;
}

static int test_clos_funcallable_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_funcallable_p(pos), "clos_funcallable_p1");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	test(! clos_funcallable_p(pos), "clos_funcallable_p2");
	clos_instance_heap(pos, &pos);
	test(clos_funcallable_p(pos), "clos_funcallable_p3");
	GetConst(CLOS_STANDARD_OBJECT, &pos);
	test(! clos_funcallable_p(pos), "clos_funcallable_p4");
	clos_set_funcall(pos);
	test(clos_funcallable_p(pos), "clos_funcallable_p5");

	RETURN;
}

static int test_clos_generic_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_generic_p(pos), "clos_generic_p1");

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	test(! clos_generic_p(pos), "clos_generic_p2");
	clos_instance_heap(pos, &pos);
	test(clos_generic_p(pos), "clos_generic_p3");

	GetConst(CLOS_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	test(clos_generic_p(pos), "clos_generic_p4");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	test(! clos_generic_p(pos), "clos_generic_p5");
	clos_set_funcall(pos);
	test(! clos_generic_p(pos), "clos_generic_p6");

	RETURN;
}

static int test_clos_method_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_method_p(pos), "clos_method_p1");

	GetConst(CLOS_STANDARD_METHOD, &pos);
	test(! clos_method_p(pos), "clos_method_p2");
	clos_instance_heap(pos, &pos);
	test(clos_method_p(pos), "clos_method_p3");

	GetConst(CLOS_METHOD, &pos);
	clos_instance_heap(pos, &pos);
	test(clos_method_p(pos), "clos_method_p4");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	test(! clos_method_p(pos), "clos_method_p5");

	RETURN;
}

static int test_clos_define_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p2");
	clos_instance_heap(pos, &pos);
	test(clos_define_combination_p(pos), "clos_define_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p4");
	clos_instance_heap(pos, &pos);
	test(clos_define_combination_p(pos), "clos_define_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p6");
	clos_instance_heap(pos, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p8");
	clos_instance_heap(pos, &pos);
	test(! clos_define_combination_p(pos), "clos_define_combination_p9");

	RETURN;
}

static int test_clos_define_long_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p2");
	clos_instance_heap(pos, &pos);
	test(clos_define_long_combination_p(pos), "clos_define_long_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p4");
	clos_instance_heap(pos, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p6");
	clos_instance_heap(pos, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p8");
	clos_instance_heap(pos, &pos);
	test(! clos_define_long_combination_p(pos), "clos_define_long_combination_p9");

	RETURN;
}

static int test_clos_define_short_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p2");
	clos_instance_heap(pos, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p4");
	clos_instance_heap(pos, &pos);
	test(clos_define_short_combination_p(pos), "clos_define_short_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p6");
	clos_instance_heap(pos, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p8");
	clos_instance_heap(pos, &pos);
	test(! clos_define_short_combination_p(pos), "clos_define_short_combination_p9");

	RETURN;
}

static int test_clos_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_combination_p(pos), "clos_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_combination_p(pos), "clos_combination_p2");
	clos_instance_heap(pos, &pos);
	test(! clos_combination_p(pos), "clos_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_combination_p(pos), "clos_combination_p4");
	clos_instance_heap(pos, &pos);
	test(! clos_combination_p(pos), "clos_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_combination_p(pos), "clos_combination_p6");
	clos_instance_heap(pos, &pos);
	test(clos_combination_p(pos), "clos_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_combination_p(pos), "clos_combination_p8");
	clos_instance_heap(pos, &pos);
	test(clos_combination_p(pos), "clos_combination_p9");

	RETURN;
}

static int test_clos_long_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p2");
	clos_instance_heap(pos, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p4");
	clos_instance_heap(pos, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p6");
	clos_instance_heap(pos, &pos);
	test(clos_long_combination_p(pos), "clos_long_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p8");
	clos_instance_heap(pos, &pos);
	test(! clos_long_combination_p(pos), "clos_long_combination_p9");

	RETURN;
}

static int test_clos_short_combination_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p2");
	clos_instance_heap(pos, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p4");
	clos_instance_heap(pos, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p6");
	clos_instance_heap(pos, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	test(! clos_short_combination_p(pos), "clos_short_combination_p8");
	clos_instance_heap(pos, &pos);
	test(clos_short_combination_p(pos), "clos_short_combination_p9");

	RETURN;
}

static int test_clos_specializer_p(void)
{
	addr pos;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	test(! clos_specializer_p(pos), "clos_specializer_p1");

	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	test(! clos_specializer_p(pos), "clos_specializer_p2");
	clos_instance_heap(pos, &pos);
	test(clos_specializer_p(pos), "clos_specializer_p3");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	test(! clos_specializer_p(pos), "clos_specializer_p4");

	RETURN;
}

static int test_funcallp(void)
{
	addr pos;

	test(! funcallp(T), "funcallp1");
	GetConst(COMMON_CAR, &pos);
	test(! funcallp(pos), "funcallp2");
	GetFunctionSymbol(pos, &pos);
	test(funcallp(pos), "funcallp3");

	GetConst(CLOS_METHOD, &pos);
	clos_instance_heap(pos, &pos);
	test(! funcallp(pos), "funcallp4");
	clos_set_funcall(pos);
	test(funcallp(pos), "funcallp5");

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	test(funcallp(pos), "funcallp6");

	RETURN;
}


/*
 *  make-instance
 */
static int test_clos_instance_alloc(void)
{
	addr slots, pos, name, one, clos;
	fixnum value, version;

	/* direct-slots */
	slot_vector_heap(&slots, 3);

	slot_heap(&pos);
	internchar_keyword("HELLO", &name);
	SetNameSlot(pos, name);
	fixnum_heap(&one, 111);
	SetFormSlot(pos, one);
	SetLocationSlot(pos, 0);
	SetSlotVector(slots, 0, pos);

	slot_heap(&pos);
	internchar_keyword("AAA", &name);
	SetNameSlot(pos, name);
	fixnum_heap(&one, 222);
	SetFormSlot(pos, one);
	SetLocationSlot(pos, 1);
	SetSlotVector(slots, 1, pos);

	slot_heap(&pos);
	internchar_keyword("BBB", &name);
	SetNameSlot(pos, name);
	fixnum_heap(&one, 333);
	SetFormSlot(pos, one);
	SetLocationSlot(pos, 2);
	SetSlotVector(slots, 2, pos);

	/* clos */
	clos_stdclass_slots(&pos);
	clos_heap(&clos, pos);
	SetClassOfClos(clos, Nil);
	stdset_class_slots(clos, slots);
	stdset_class_finalized_p(clos, T);

	/* test */
	clos_instance_heap(clos, &clos);
	clos_getelt(clos, 0, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "clos_instance_heap1");
	clos_getelt(clos, 1, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "clos_instance_heap2");
	clos_getelt(clos, 2, &pos);
	GetFixnum(pos, &value);
	test(value == 333, "clos_instance_heap3");

	internchar_keyword("HELLO", &name);
	clos_check(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "clos_instance_heap4");
	internchar_keyword("AAA", &name);
	clos_check(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "clos_instance_heap5");
	internchar_keyword("BBB", &name);
	clos_check(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 333, "clos_instance_heap6");

	GetVersionClos(clos, &version);
	test(version == 0, "clos_instance_heap7");

	RETURN;
}


/*
 *  class-precedence-list
 */
static int test_clos_precedence_classes(void)
{
	addr clos, left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* test */
	clos_supers_local(local, &clos, NULL);
	clos_precedence_classes(local, clos, &right);
	test(GetType(right) == LISPTYPE_CONS, "clos_precedence_classes1");
	GetCons(right, &left, &right);
	test(left == clos, "clos_precedence_classes2");
	test(right != Nil, "clos_precedence_classes3");
	GetCons(right, &left, &right);
	test(left == Unbound, "clos_precedence_classes4");
	test(right == Nil, "clos_precedence_classes5");

	clos_supers_local(local, &clos, Nil, T, NULL);
	clos_precedence_classes(local, clos, &right);
	test(GetType(right) == LISPTYPE_CONS, "clos_precedence_classes6");
	GetCons(right, &left, &right);
	test(left == clos, "clos_precedence_classes7");
	test(right != Nil, "clos_precedence_classes8");
	GetCons(right, &left, &right);
	test(left == Nil, "clos_precedence_classes9");
	test(right != Nil, "clos_precedence_classes10");
	GetCons(right, &left, &right);
	test(left == T, "clos_precedence_classes11");
	test(right != Nil, "clos_precedence_classes12");
	GetCons(right, &left, &right);
	test(left == Unbound, "clos_precedence_classes13");
	test(right == Nil, "clos_precedence_classes14");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_pair(void)
{
	addr clos, left, right, cons, a, b;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* (clos unbound) -> ((clos . unbound)) */
	clos_supers_local(local, &clos, NULL);
	clos_precedence_pair(local, clos, &right);

	test(GetType(right) == LISPTYPE_CONS, "clos_precedence_pair1");
	GetCons(right, &left, &right);
	test(GetType(left) == LISPTYPE_CONS, "clos_precedence_pair2");
	test(right == Nil, "clos_precedence_pair3");
	GetCons(left, &left, &right);
	test(left == clos, "clos_precedence_pair4");
	test(right == Unbound, "clos_precedence_pair5");

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair(local, clos, &cons);
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == b, "clos_precedence_pair6");
	test(right == Unbound, "clos_precedence_pair7");
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == a, "clos_precedence_pair8");
	test(right == b, "clos_precedence_pair9");
	GetCons(cons, &left, &cons);
	GetCons(left, &left, &right);
	test(left == clos, "clos_precedence_pair10");
	test(right == a, "clos_precedence_pair11");
	test(cons == Nil, "clos_precedence_pair12");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_super(void)
{
	addr clos, left, right, a, b, c, d, e;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	clos_supers_local(local, &clos, NULL);
	clos_precedence_super(local, clos, &right);
	test(right != Nil, "clos_precedence_super1");
	GetCons(right, &left, &right);
	test(left == clos, "clos_precedence_super2");
	test(right == Nil, "clos_precedence_super3");

	clos_supers_local(local, &a, NULL);
	clos_supers_local(local, &b, a, NULL);
	clos_supers_local(local, &c, b, NULL);
	clos_supers_local(local, &d, NULL);
	clos_supers_local(local, &e, d, NULL);
	clos_supers_local(local, &clos, c, d, e, NULL);
	clos_precedence_super(local, clos, &right);
	test(length_list_unsafe(right) == 6, "clos_precedence_super4");
	test(find_list_eq_unsafe(a, right) &&
			find_list_eq_unsafe(b, right) &&
			find_list_eq_unsafe(c, right) &&
			find_list_eq_unsafe(d, right) &&
			find_list_eq_unsafe(e, right) &&
			find_list_eq_unsafe(clos, right), "clos_precedence_super5");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_find(void)
{
	addr clos, left, right, a, b;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair(local, clos, &right);

	cons_local(local, &left, a, b);
	test(clos_precedence_find(left, right), "clos_precedence_find1");
	cons_local(local, &left, a, Nil);
	test(! clos_precedence_find(left, right), "clos_precedence_find2");
	cons_local(local, &left, Nil, b);
	test(! clos_precedence_find(left, right), "clos_precedence_find3");
	cons_local(local, &left, b, a);
	test(! clos_precedence_find(left, right), "clos_precedence_find4");
	cons_local(local, &left, b, Unbound);
	test(clos_precedence_find(left, right), "clos_precedence_find5");
	cons_local(local, &left, clos, a);
	test(clos_precedence_find(left, right), "clos_precedence_find6");

	rollback_local(local, stack);

	RETURN;
}

static int test_find_cons_chain_check(addr car, addr cdr, addr cons)
{
	cons_local(Local_Thread, &cdr, car, cdr);
	return clos_precedence_find(cdr, cons);
}

static int test_clos_precedence_chain(void)
{
	addr clos, cons, a, b, c, d, e, t;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* (t . Unbound) */
	clos_supers_local(local, &t, NULL);
	clos_precedence_chain(local, t, &cons);
	test(length_list_unsafe(cons) == 1, "clos_precedence_chain1");
	test(test_find_cons_chain_check(t, Unbound, cons), "clos_precedence_chain2");

	clos_supers_local(local, &t, NULL);
	clos_supers_local(local, &a, t, NULL);
	clos_supers_local(local, &b, t, NULL);
	clos_supers_local(local, &c, a, b, t, NULL);
	clos_supers_local(local, &d, t, NULL);
	clos_supers_local(local, &e, t, NULL);
	clos_supers_local(local, &clos, d, c, e, t, NULL);
	clos_precedence_chain(local, clos, &cons);
	/* (t . Unbound) (a . t) (b . t) (c . a) (a . b)
	   (d . t) (e . t) (clos . d) (d . c) (c . e) */
	test(length_list_unsafe(cons) == 10, "clos_precedence_chain3");
	test(test_find_cons_chain_check(t, Unbound, cons), "clos_precedence_chain4");
	test(test_find_cons_chain_check(a, t, cons), "clos_precedence_chain5");
	test(test_find_cons_chain_check(b, t, cons), "clos_precedence_chain6");
	test(test_find_cons_chain_check(c, a, cons), "clos_precedence_chain7");
	test(test_find_cons_chain_check(a, b, cons), "clos_precedence_chain8");
	test(test_find_cons_chain_check(d, t, cons), "clos_precedence_chain9");
	test(test_find_cons_chain_check(e, t, cons), "clos_precedence_chain10");
	test(test_find_cons_chain_check(clos, d, cons), "clos_precedence_chain11");
	test(test_find_cons_chain_check(d, c, cons), "clos_precedence_chain12");
	test(test_find_cons_chain_check(c, e, cons), "clos_precedence_chain13");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_top(void)
{
	addr clos, left, right, a, b;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair(local, clos, &right);
	clos_precedence_top(right, &left);
	test(left == clos, "clos_precedence_top1");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_remove(void)
{
	addr clos, left, right, a, b, c, check;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair(local, clos, &right);
	clos_precedence_remove(b, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == a, "clos_precedence_remove1");
	test(check == b, "clos_precedence_remove2");
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == clos, "clos_precedence_remove3");
	test(check == a, "clos_precedence_remove4");
	test(right == Nil, "clos_precedence_remove5");

	/* ((A . b)) */
	cons_local(local, &right, a, b);
	conscar_local(local, &right, right);
	clos_precedence_remove(a, right, &right);
	test(right == Nil, "clos_precedence_remove6");

	/* ((A . b) (b . a)) */
	cons_local(local, &left, b, a);
	conscar_local(local, &right, left);
	cons_local(local, &left, a, b);
	cons_local(local, &right, left, right);
	clos_precedence_remove(a, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == b, "clos_precedence_remove7");
	test(check == a, "clos_precedence_remove8");
	test(right == Nil, "clos_precedence_remove9");

	/* ((b . a) (A . b)) */
	cons_local(local, &left, a, b);
	conscar_local(local, &right, left);
	cons_local(local, &left, b, a);
	cons_local(local, &right, left, right);
	clos_precedence_remove(a, right, &right);
	GetCons(right, &left, &right);
	GetCons(left, &left, &check);
	test(left == b, "clos_precedence_remove10");
	test(check == a, "clos_precedence_remove11");
	test(right == Nil, "clos_precedence_remove12");

	/* ((A . b) (b . c) (c . unbound)) */
	/* ((b . c) (A . b) (c . unbound)) */
	/* ((b . c) (c . a) (A . b)) */

	rollback_local(local, stack);

	RETURN;
}

static int no_dynamic_check(addr right)
{
	while (right != Nil) {
		if (GetStatusDynamic(right)) return 0;
		GetCdr(right, &right);
	}

	return 1;
}

static int test_clos_precedence_result(void)
{
	addr clos, left, right;
	LocalRoot local;
	LocalStack stack;

	local = Local_Thread;
	push_local(local, &stack);

	clos_supers_heap(&clos, NULL);
	clos_precedence_result(local, clos, &right);
	test(right != Nil, "clos_precedence_result1");
	test(no_dynamic_check(right), "clos_precedence_result2");
	GetCons(right, &left, &right);
	test(left == clos, "clos_precedence_result3");
	test(right == Nil, "clos_precedence_result4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Common Lisp the Language, 2nd Edition
 *    28. Common Lisp Object System
 *    28.1.5.2. Examples
 *      (defclass pie (apple cinnamon) ())
 *      (defclass apple (fruit) ())
 *      (defclass cinnamon (spice) ())
 *      (defclass fruit (food) ())
 *      (defclass spice (food) ())
 *      (defclass food () ())
 *         -> (pie apple fruit cinnamon spice food standard-object t)
 *
 *      (defclass pie (apple cinnamon) ())
 *      (defclass pastry (cinnamon apple) ())
 *      (defclass apple () ())
 *      (defclass cinnamon () ())
 *         -> (pie apple cinnamon standard-object t)
 *         -> (pastry cinnamon apple standard-object t)
 */
static int cons_check(addr right, ...)
{
	addr left, check;
	va_list args;

	va_start(args, right);
	while (right != Nil) {
		check = va_arg(args, addr);
		if (check == NULL) return 0;
		GetCons(right, &left, &right);
		if (check != left) return 0;
	}
	va_end(args);

	return 1;
}

static int test_clos_precedence_list(void)
{
	addr pie, apple, cinnamon, fruit, spice, food, pastry, t, object;
	addr cons;
	LocalRoot local;

	local = Local_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&food, object, NULL);
	clos_supers_heap(&spice, food, object, NULL);
	clos_supers_heap(&fruit, food, object, NULL);
	clos_supers_heap(&cinnamon, spice, object, NULL);
	clos_supers_heap(&apple, fruit, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	clos_precedence_list(local, pie, &cons);
	test(no_dynamic_check(cons), "clos_precedence_list1");
	test(cons_check(cons, pie, apple, fruit, cinnamon, spice, food, object, t, NULL),
			"clos_precedence_list2");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&cinnamon, object, NULL);
	clos_supers_heap(&apple, object, NULL);
	clos_supers_heap(&pastry, cinnamon, apple, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	clos_precedence_list(local, pie, &cons);
	test(no_dynamic_check(cons), "clos_precedence_list3");
	test(cons_check(cons, pie, apple, cinnamon, object, t, NULL),
			"clos_precedence_list4");

	clos_precedence_list(local, pastry, &cons);
	test(no_dynamic_check(cons), "clos_precedence_list5");
	test(cons_check(cons, pastry, cinnamon, apple, object, t, NULL),
			"clos_precedence_list6");

	RETURN;
}


/*
 *  compute-slots
 */
static int test_clos_slots_name(void)
{
	addr slot1, slot2, slot3, pos, cons;

	slot_heap(&slot1);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	SetNameSlot(slot1, pos);

	clos_slots_name(&cons, slot1, Nil);
	test(cons == Nil, "clos_slots_name1");

	slot_heap(&slot2);
	internchar(LISP_PACKAGE, "AAA", &pos);
	SetNameSlot(slot2, pos);

	slot_heap(&slot3);
	internchar(LISP_PACKAGE, "BBB", &pos);
	SetNameSlot(slot3, pos);

	list_heap(&cons, slot2, slot3, NULL);
	clos_slots_name(&cons, slot1, cons);
	test(cons == Nil, "clos_slots_name2");

	slot_heap(&slot3);
	internchar(LISP_PACKAGE, "HELLO", &pos);
	SetNameSlot(slot3, pos);

	list_heap(&cons, slot2, slot3, NULL);
	clos_slots_name(&cons, slot1, cons);
	test(cons != Nil, "clos_slots_name3");

	RETURN;
}

static void slotname_heap(addr *ret, const char *name)
{
	addr symbol;
	slot_heap(ret);
	internchar(LISP_PACKAGE, name, &symbol);
	SetNameSlot(*ret, symbol);
}

static void test_makeclos_heap(addr *ret, ...)
{
	const char *name;
	addr cons, slot, slots, clos, temp;
	size_t i;
	va_list args;

	/* make list */
	cons = Nil;
	va_start(args, ret);
	for (i = 0; ; i++) {
		name = va_arg(args, const char *);
		if (name == NULL) break;
		slotname_heap(&slot, name);
		cons_heap(&cons, slot, cons);
	}
	va_end(args);
	nreverse_list_unsafe(&cons, cons);

	/* make clos */
	slot_vector_heap(&slots, i);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &slot, &cons);
		SetSlotVector(slots, i, slot);
	}
	clos_stdclass_slots(&temp);
	clos_heap(&clos, temp);
	SetClassOfClos(clos, Nil);
	stdset_class_direct_slots(clos, slots);
	list_heap(&temp, clos, NULL);
	stdset_class_precedence_list(clos, temp);
	*ret = clos;
}

static int slotnamecheck(addr slot, const char *name)
{
	addr check;
	GetNameSlot(slot, &slot);
	internchar(LISP_PACKAGE, name, &check);
	return check == slot;
}

static int test_clos_slots_gather(void)
{
	addr clos1, clos2, cons, check, aaa;
	LocalRoot local;
	size_t size;

	local = Local_Thread;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	clos_slots_gather(local, clos1, &cons, &size);
	test(cons != Nil, "clos_slots_gather1");
	test(size == 3, "clos_slots_gather2");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "clos_slots_gather3");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "AAA"), "clos_slots_gather4");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "clos_slots_gather5");
	test(cons == Nil, "clos_slots_gather6");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	stdset_class_precedence_list(clos2, cons);
	clos_slots_gather(local, clos2, &cons, &size);
	test(size == 4, "clos_slots_gather7");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "clos_slots_gather8");
	GetCons(cons, &check, &cons);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "clos_slots_gather9");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "clos_slots_gather10");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "CCC"), "clos_slots_gather11");
	test(cons == Nil, "clos_slots_gather12");

	stdget_class_direct_slots(clos2, &cons);
	GetSlotVector(cons, 0, &cons);
	test(cons == aaa, "clos_slots_gather13");

	RETURN;
}

static int test_clos_compute_slots(void)
{
	LocalRoot local;
	addr clos1, clos2, cons, check, aaa;
	size_t size;

	local = Local_Thread;;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	clos_compute_slots(local, clos1, &cons);
	test(cons != Nil, "clos_compute_slots1");
	GetSlotVector(cons, 0, &check);
	test(slotnamecheck(check, "HELLO"), "clos_compute_slots2");
	GetSlotVector(cons, 1, &check);
	test(slotnamecheck(check, "AAA"), "clos_compute_slots3");
	GetSlotVector(cons, 2, &check);
	test(slotnamecheck(check, "BBB"), "clos_compute_slots4");
	LenSlotVector(cons, &size);
	test(size == 3, "clos_compute_slots5");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	stdset_class_precedence_list(clos2, cons);
	clos_compute_slots(local, clos2, &cons);
	GetSlotVector(cons, 0, &check);
	test(slotnamecheck(check, "HELLO"), "clos_compute_slots6");
	GetSlotVector(cons, 1, &check);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "clos_compute_slots7");
	GetSlotVector(cons, 2, &check);
	test(slotnamecheck(check, "BBB"), "clos_compute_slots8");
	GetSlotVector(cons, 3, &check);
	test(slotnamecheck(check, "CCC"), "clos_compute_slots9");
	LenSlotVector(cons, &size);
	test(size == 4, "clos_compute_slots10");

	stdget_class_direct_slots(clos2, &cons);
	GetSlotVector(cons, 0, &cons);
	test(cons != aaa, "clos_compute_slots11");

	RETURN;
}


/*
 *  standard-class
 */
static int test_slot_make_name_symbol(void)
{
	addr pos, check, type;

	slot_vector_heap(&pos, Clos_class_size);
	SlotMakeNameSymbol(pos, NAME, class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	GetTypeSlot(pos, &type);
	GetNameSlot(pos, &pos);
	GetConst(CLOSKEY_NAME, &check);
	test(pos == check, "slot_make_name_symbol1");
	test(LispDecl(type) == LISPDECL_SYMBOL, "slot_make_name_symbol2");

	RETURN;
}

static int test_slot_make_name(void)
{
	addr pos, check;

	slot_vector_heap(&pos, Clos_class_size);
	SlotMakeName(pos, EFFECTIVE_SLOTS, class_slots);
	GetSlotVector(pos, Clos_class_slots, &pos);
	GetNameSlot(pos, &pos);
	GetConst(CLOSKEY_EFFECTIVE_SLOTS, &check);
	test(pos == check, "slot_make_name1");

	RETURN;
}

static int test_slot_make_form(void)
{
	addr pos, check, value;

	slot_vector_heap(&pos, Clos_class_size);
	SlotMakeForm(pos, NAME, class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	GetFormSlot(pos, &value);
	GetNameSlot(pos, &pos);
	GetConst(CLOSKEY_NAME, &check);
	test(pos == check, "slot_make_form1");
	test(value == Nil, "slot_make_form2");

	RETURN;
}

static int test_slot_make_version(void)
{
	addr pos, check, value;

	slot_vector_heap(&pos, Clos_class_size);
	SlotMakeVersion(pos, NAME, class_name);
	GetSlotVector(pos, Clos_class_name, &pos);
	GetFormSlot(pos, &value);
	GetNameSlot(pos, &pos);
	GetConst(CLOSKEY_NAME, &check);
	test(pos == check, "slot_make_version1");
	test(RefFixnum(value) == 0, "slot_make_version2");

	RETURN;
}

static int test_slotvector_set_location(void)
{
	addr slots, slot;
	size_t check;

	slot_vector_heap(&slots, 3);
	slot_heap(&slot);
	SetSlotVector(slots, 0, slot);
	slot_heap(&slot);
	SetSlotVector(slots, 1, slot);
	slot_heap(&slot);
	SetSlotVector(slots, 2, slot);
	slotvector_set_location(slots);

	GetSlotVector(slots, 0, &slot);
	GetLocationSlot(slot, &check);
	test(check == 0, "slotvector_set_location1");
	GetSlotVector(slots, 1, &slot);
	GetLocationSlot(slot, &check);
	test(check == 1, "slotvector_set_location2");
	GetSlotVector(slots, 2, &slot);
	GetLocationSlot(slot, &check);
	test(check == 2, "slotvector_set_location3");

	RETURN;
}

static int test_slotname(addr slots, int index, const char *name)
{
	addr pos, check;

	GetSlotVector(slots, index, &pos);
	GetNameSlot(pos, &pos);
	internchar_keyword(name, &check);

	return check == pos;
}

static int test_clos_stdclass_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	clos_stdclass_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_class_size, "clos_stdclass_slots1");

	for (check = 1, i = 0; i < Clos_class_size; i++) {
		GetSlotVector(slots, i, &pos);
		GetLocationSlot(pos, &size);
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
	test(test_slotname(slots, Clos_class_direct_methods, "DIRECT-METHODS"),
			"clos_stdclass_slots11");
	test(test_slotname(slots, Clos_class_default_initargs, "DEFAULT-INITARGS"),
			"clos_stdclass_slots12");
	test(test_slotname(slots, Clos_class_direct_default_initargs,
				"DIRECT-DEFAULT-INITARGS"),
			"clos_stdclass_slots13");
	test(test_slotname(slots, Clos_class_version, "VERSION"),
			"clos_stdclass_slots14");
	test(test_slotname(slots, Clos_class_update_info, "UPDATE-INFO"),
			"clos_stdclass_slots15");

	RETURN;
}

static int test_clos_stdclass_dummy(void)
{
	addr slots, clos, check;

	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&clos, slots);
	test(closp(clos), "clos_stdclass_dummy1");
	stdget_class_direct_slots(clos, &check);
	test(check == slots, "clos_stdclass_dummy2");
	stdget_class_slots(clos, &check);
	test(check == slots, "clos_stdclass_dummy3");

	RETURN;
}

static void test_slot_vector_heap(addr *ret, size_t size)
{
	addr slots, pos;
	size_t i;

	slot_vector_heap(&slots, size);
	for (i = 0; i < size; i++) {
		slot_heap(&pos);
		SetSlotVector(slots, i, pos);
	}
	*ret = slots;
}

static int test_clos_stdclass_make(void)
{
	addr slots, clos, name, check;

	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&clos, slots);
	test_slot_vector_heap(&slots, 4);
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_stdclass_make(&clos, clos, name, slots);
	stdget_class_name(clos, &check);
	test(check == name, "clos_stdclass_make1");
	stdget_class_prototype(clos, &check);
	test(check == clos, "clos_stdclass_make2");
	stdget_class_finalized_p(clos, &check);
	test(check == Nil, "clos_stdclass_make3");
	stdget_class_direct_slots(clos, &check);
	test(check == slots, "clos_stdclass_make4");

	RETURN;
}

static int test_clos_stdclass_empty(void)
{
	addr slots, clos, name, check;
	size_t size;

	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&clos, slots);
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_stdclass_empty(&clos, clos, name);
	stdget_class_direct_slots(clos, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_empty1");

	RETURN;
}

static int test_clos_stdclass_class_of(void)
{
	addr slots, clos, instance, name, check;
	fixnum version;

	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&clos, slots);
	internchar(LISP_PACKAGE, "AAA", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&clos, clos, name, slots);

	SetVersionClos(clos, 100);
	clos_heap(&instance, slots);
	clos_stdclass_class_of(instance, clos);
	clos_class_of(instance, &check);
	test(check == clos, "clos_stdclass_class_of1");
	GetVersionClos(instance, &version);
	test(version == 100, "clos_stdclass_class_of2");

	RETURN;
}

static int test_clos_stdclass_inherit(void)
{
	addr slots, metaclass, clos, name, super, supers, check, left;
	LocalRoot local;
	fixnum version;

	local = Local_Thread;
	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&metaclass, slots);
	internchar(LISP_PACKAGE, "METACLASS", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&metaclass, metaclass, name, slots);
	clos_stdclass_inherit(local, metaclass, metaclass, Nil);

	internchar(LISP_PACKAGE, "SUPER", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&super, metaclass, name, slots);
	clos_stdclass_inherit(local, super, metaclass, Nil);

	internchar(LISP_PACKAGE, "AAA", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&clos, metaclass, name, slots);
	list_heap(&supers, super, NULL);
	clos_stdclass_inherit(local, clos, metaclass, supers);

	GetVersionClos(clos, &version);
	test(version == 0, "clos_stdclass_inherit1");
	clos_class_of(clos, &check);
	test(check == metaclass, "clos_stdclass_inherit2");
	stdget_class_direct_superclasses(clos, &check);
	GetCons(check, &left, &check);
	test(left == super, "clos_stdclass_inherit3");
	test(check == Nil, "clos_stdclass_inherit4");
	stdget_class_precedence_list(clos, &check);
	test(GetType(check) == LISPTYPE_CONS, "clos_stdclass_inherit5");
	stdget_class_slots(clos, &check);
	test(GetType(check) == LISPSYSTEM_SLOT_VECTOR, "clos_stdclass_inherit6");
	stdget_class_direct_subclasses(super, &check);
	GetCons(check, &left, &check);
	test(left == clos, "clos_stdclass_inherit7");
	test(check == Nil, "clos_stdclass_inherit8");
	stdget_class_name(clos, &check);
	test(check == name, "clos_stdclass_inherit9");
	clos_find_class(name, &check);
	test(check == clos, "clos_stdclass_inherit10");

	RETURN;
}

static int test_clos_stdclass_single(void)
{
	addr slots, metaclass, clos, name, super, check, left;
	LocalRoot local;
	fixnum version;

	local = Local_Thread;
	clos_stdclass_slots(&slots);
	clos_stdclass_dummy(&metaclass, slots);
	internchar(LISP_PACKAGE, "METACLASS", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&metaclass, metaclass, name, slots);
	clos_stdclass_inherit(local, metaclass, metaclass, Nil);

	internchar(LISP_PACKAGE, "SUPER", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&super, metaclass, name, slots);
	clos_stdclass_inherit(local, super, metaclass, Nil);

	internchar(LISP_PACKAGE, "AAA", &name);
	clos_stdclass_slots(&slots);
	clos_stdclass_make(&clos, metaclass, name, slots);
	clos_stdclass_single(local, clos, metaclass, super);

	GetVersionClos(clos, &version);
	test(version == 0, "clos_stdclass_single1");
	clos_class_of(clos, &check);
	test(check == metaclass, "clos_stdclass_single2");
	stdget_class_direct_superclasses(clos, &check);
	GetCons(check, &left, &check);
	test(left == super, "clos_stdclass_single3");
	test(check == Nil, "clos_stdclass_single4");
	stdget_class_precedence_list(clos, &check);
	test(GetType(check) == LISPTYPE_CONS, "clos_stdclass_single5");
	stdget_class_slots(clos, &check);
	test(GetType(check) == LISPSYSTEM_SLOT_VECTOR, "clos_stdclass_single6");
	stdget_class_direct_subclasses(super, &check);
	GetCons(check, &left, &check);
	test(left == clos, "clos_stdclass_single7");
	test(check == Nil, "clos_stdclass_single8");
	stdget_class_name(clos, &check);
	test(check == name, "clos_stdclass_single9");
	clos_find_class(name, &check);
	test(check == clos, "clos_stdclass_single10");

	RETURN;
}

static int test_clos_stdclass_metaclass(void)
{
	addr metaclass, tclass, object, classclass, check, left;
	LocalRoot local;

	local = Local_Thread;;
	clos_stdclass_metaclass(local, &metaclass);

	/* t */
	clos_find_class(T, &tclass);
	clos_class_of(tclass, &check);
	GetConst(COMMON_BUILT_IN_CLASS, &left);
	clos_find_class(left, &left);
	test(check == left, "clos_stdclass_metaclass1");
	stdget_class_direct_superclasses(tclass, &check);
	test(check == Nil, "clos_stdclass_metaclass2");

	/* object */
	GetConst(COMMON_STANDARD_OBJECT, &object);
	clos_find_class(object, &object);
	GetConst(CLOS_STANDARD_OBJECT, &left);
	test(object == left, "clos_stdclass_metaclass3");
	clos_class_of(object, &check);
	test(check == metaclass, "clos_stdclass_metaclass4");
	stdget_class_direct_superclasses(object, &check);
	GetCons(check, &left, &check);
	test(left == tclass, "clos_stdclass_metaclass5");
	test(check == Nil, "clos_stdclass_metaclass6");

	/* class */
	GetConst(COMMON_CLASS, &classclass);
	clos_find_class(classclass, &classclass);
	GetConst(CLOS_CLASS, &left);
	test(classclass == left, "clos_stdclass_metaclass7");
	clos_class_of(classclass, &check);
	test(check == metaclass, "clos_stdclass_metaclass8");
	stdget_class_direct_superclasses(classclass, &check);
	GetCons(check, &left, &check);
	test(left == object, "clos_stdclass_metaclass9");
	test(check == Nil, "clos_stdclass_metaclass10");

	/* standard-class */
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	GetConst(CLOS_STANDARD_CLASS, &left);
	test(check == left, "clos_stdclass_metaclass11");
	test(check == metaclass, "clos_stdclass_metaclass12");
	clos_class_of(check, &check);
	test(check == metaclass, "clos_stdclass_metaclass13");
	stdget_class_direct_superclasses(metaclass, &check);
	GetCons(check, &left, &check);
	test(left == classclass, "clos_stdclass_metaclass14");
	test(check == Nil, "clos_stdclass_metaclass15");

	RETURN;
}

static int test_clos_stdclass_supers(void)
{
	addr instance, metaclass, name, slots, slot, symbol, supers, check;
	LocalRoot local;
	size_t size;

	local = Local_Thread;
	clos_stdclass_metaclass(local, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_find_class(T, &check);
	list_heap(&supers, check, NULL);
	test_slot_vector_heap(&slots, 2);
	slot_heap(&slot);
	internchar(LISP_PACKAGE, "AAA", &symbol);
	SetNameSlot(slot, symbol);
	SetSlotVector(slots, 0, slot);
	internchar(LISP_PACKAGE, "BBB", &symbol);
	slot_heap(&slot);
	SetNameSlot(slot, symbol);
	SetSlotVector(slots, 1, slot);
	clos_stdclass_supers(local, &instance, metaclass, name, slots, supers);
	clos_class_of(instance, &check);
	test(check == metaclass, "clos_stdclass_supers1");
	stdget_class_direct_superclasses(instance, &supers);
	GetCons(supers, &check, &supers);
	clos_find_class(T, &name);
	test(check == name, "clos_stdclass_supers2");
	test(supers == Nil, "clos_stdclass_supers3");
	stdget_class_direct_slots(instance, &check);
	LenSlotVector(check, &size);
	test(size == 2, "clos_stdclass_supers4");

	RETURN;
}

static int test_clos_stdclass_type(void)
{
	addr instance, metaclass, name, supers, check, tclass;
	LocalRoot local;
	size_t size;

	local = Local_Thread;
	clos_stdclass_metaclass(local, &metaclass);
	internchar(LISP_PACKAGE, "HELLO", &name);
	clos_find_class(T, &tclass);
	list_heap(&supers, tclass, NULL);
	clos_stdclass_type(local, &instance, metaclass, name, supers);
	clos_class_of(instance, &check);
	test(check == metaclass, "clos_stdclass_type1");
	stdget_class_direct_superclasses(instance, &supers);
	GetCons(supers, &check, &supers);
	test(check == tclass, "clos_stdclass_type2");
	test(supers == Nil, "clos_stdclass_type3");
	stdget_class_direct_slots(instance, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_type4");

	RETURN;
}

static int test_clos_stdclass_va(void)
{
	addr metaclass, clos, check, name;
	LocalRoot local;
	size_t size;

	local = Local_Thread;
	//clos_forget_all_classes_unsafe();
	SetConst(DEBUG1, readr("debug1"));
	clos_stdclass_metaclass(local, &metaclass);
	clos_stdclass_va(local, metaclass,
			CONSTANT_DEBUG1,
			CONSTANT_DEBUG2,
			CONSTANT_CLOS_CLASS,
			CONSTANT_EMPTY);
	GetConst(DEBUG1, &clos);
	clos_find_class(clos, &clos);
	clos_class_of(clos, &check);
	test(check == metaclass, "clos_stdclass_va1");
	stdget_class_direct_superclasses(clos, &check);
	GetCar(check, &check);
	GetConst(COMMON_CLASS, &name);
	clos_find_class(name, &name);
	test(check == name, "clos_stdclass_va2");
	stdget_class_direct_slots(clos, &check);
	LenSlotVector(check, &size);
	test(size == 0, "clos_stdclass_va3");

	GetConst(DEBUG1, &clos);
	clos_find_class(clos, &clos);
	GetConst(DEBUG2, &check);
	test(clos == check, "clos_stdclass_va4");

	RETURN;
}

static int test_clos_stdclass_slotsconstant(void)
{
	addr metaclass, clos, check, slot, slots, name;
	LocalRoot local;
	size_t size;

	local = Local_Thread;
	test_slot_vector_heap(&slots, 2);
	slot_heap(&slot);
	internchar(LISP_PACKAGE, "AAA", &name);
	SetNameSlot(slot, name);
	SetSlotVector(slots, 0, slot);
	slot_heap(&slot);
	internchar(LISP_PACKAGE, "BBB", &name);
	SetNameSlot(slot, name);
	SetSlotVector(slots, 1, slot);

	SetConst(DEBUG2, readr("debug2"));
	clos_stdclass_metaclass(local, &metaclass);
	clos_stdclass_slotsconstant(local, metaclass, slots,
			CONSTANT_DEBUG2,
			CONSTANT_DEBUG3,
			CONSTANT_CLOS_CLASS);
	GetConst(DEBUG2, &clos);
	clos_find_class(clos, &clos);
	clos_class_of(clos, &check);
	test(check == metaclass, "clos_stdclass_slotsconstant1");
	stdget_class_direct_superclasses(clos, &check);
	GetCar(check, &check);
	GetConst(COMMON_CLASS, &name);
	clos_find_class(name, &name);
	test(check == name, "clos_stdclass_slotsconstant2");
	stdget_class_direct_slots(clos, &check);
	LenSlotVector(check, &size);
	test(size == 2, "clos_stdclass_slotsconstant3");

	GetConst(DEBUG2, &clos);
	clos_find_class(clos, &clos);
	GetConst(DEBUG3, &check);
	test(clos == check, "clos_stdclass_slotsconstant4");

	RETURN;
}

static int test_build_standard_class(void)
{
	addr check;
	addr trueclass, classclass, builtinclass, metaclass, objectclass;
	addr classname, builtinname, metaname, objectname;

	interncommon("CLASS", &classname);
	interncommon("BUILT-IN-CLASS", &builtinname);
	interncommon("STANDARD-CLASS", &metaname);
	interncommon("STANDARD-OBJECT", &objectname);
	clos_find_class(T, &trueclass);
	clos_find_class(classname, &classclass);
	clos_find_class(builtinname, &builtinclass);
	clos_find_class(metaname, &metaclass);
	clos_find_class(objectname, &objectclass);
	test(closp(trueclass), "build_standard_class1");
	test(closp(classclass), "build_standard_class2");
	test(closp(builtinclass), "build_standard_class3");
	test(closp(metaclass), "build_standard_class4");
	test(closp(objectclass), "build_standard_class5");
	stdget_class_name(trueclass, &check);
	test(check == T, "build_standard_class6");
	stdget_class_name(classclass, &check);
	test(check == classname, "build_standard_class7");
	stdget_class_name(builtinclass, &check);
	test(check == builtinname, "build_standard_class8");
	stdget_class_name(metaclass, &check);
	test(check == metaname, "build_standard_class9");
	stdget_class_name(objectclass, &check);
	test(check == objectname, "build_standard_class10");

	/* t */
	clos_class_of(trueclass, &check);
	test(check == builtinclass, "build_standard_class11");
	stdget_class_direct_superclasses(trueclass, &check);
	test(check == Nil, "build_standard_class12");
	stdget_class_direct_subclasses(trueclass, &check);
	test(find_list_eq_unsafe(objectclass, check), "build_standard_class13");

	/* standard-object */
	clos_class_of(objectclass, &check);
	test(metaclass == check, "build_standard_class14");
	test(clos_subclass_p(objectclass, trueclass), "build_standard_class15");
	stdget_class_direct_subclasses(objectclass, &check);
	test(find_list_eq_unsafe(classclass, check), "build_standard_class16");

	/* class */
	clos_class_of(classclass, &check);
	test(metaclass == check, "build_standard_class17");
	test(clos_subclass_p(classclass, objectclass), "build_standard_class18");
	test(clos_subclass_p(classclass, trueclass), "build_standard_class19");
	stdget_class_direct_subclasses(classclass, &check);
	test(find_list_eq_unsafe(metaclass, check), "build_standard_class20");
	test(find_list_eq_unsafe(builtinclass, check), "build_standard_class21");

	/* standard-class */
	clos_class_of(metaclass, &check);
	test(metaclass == check, "build_standard_class22");
	test(clos_subclass_p(metaclass, classclass), "build_standard_class23");
	test(clos_subclass_p(metaclass, objectclass), "build_standard_class24");
	test(clos_subclass_p(metaclass, trueclass), "build_standard_class25");
	stdget_class_direct_subclasses(metaclass, &check);
	test(check == Nil, "build_standard_class26");

	/* built-in-class */
	clos_class_of(builtinclass, &check);
	test(metaclass == check, "build_standard_class27");
	test(clos_subclass_p(builtinclass, objectclass), "build_standard_class28");
	test(clos_subclass_p(builtinclass, classclass), "build_standard_class29");
	test(clos_subclass_p(builtinclass, trueclass), "build_standard_class30");
	stdget_class_direct_subclasses(builtinclass, &check);
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
	addr slots, pos;
	size_t i, size;

	clos_stdgeneric_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_generic_size, "clos_stdgeneric_slots1");

	for (check = 1, i = 0; i < Clos_generic_size; i++) {
		GetSlotVector(slots, i, &pos);
		GetLocationSlot(pos, &size);
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

	clos_forget_all_classes_unsafe();
	build_clos_class(Local_Thread);
	/* function */
	GetConst(COMMON_FUNCTION, &fclass);
	clos_find_class(fclass, &fclass);
	test(closp(fclass), "build_clos_class_generic1");
	clos_class_of(fclass, &pos);
	GetConst(COMMON_BUILT_IN_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_generic2");
	stdget_class_direct_superclasses(fclass, &check);
	GetCons(check, &left, &check);
	clos_find_class(T, &pos);
	test(left == pos, "build_clos_class_generic3");
	test(check == Nil, "build_clos_class_generic4");

	/* generic-function */
	GetConst(COMMON_GENERIC_FUNCTION, &gclass);
	clos_find_class(gclass, &gclass);
	test(closp(gclass), "build_clos_class_generic5");
	clos_class_of(gclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_generic6");
	stdget_class_direct_superclasses(gclass, &check);
	GetCons(check, &left, &check);
	test(left == fclass, "build_clos_class_generic7");
	test(check != Nil, "build_clos_class_generic8");
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_generic9");
	GetConst(CLOSNAME_FUNCALLABLE_STANDARD_OBJECT, &check);
	clos_find_class(check, &check);
	test(left == check, "build_clos_class_generic10");

	/* standard-generic-function */
	GetConst(COMMON_STANDARD_GENERIC_FUNCTION, &sgclass);
	clos_find_class(sgclass, &sgclass);
	test(closp(sgclass), "build_clos_class_generic11");
	clos_class_of(sgclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_generic12");
	stdget_class_direct_superclasses(sgclass, &check);
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

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap(pos, &pos);
	GetConst(CLOSKEY_EQLCHECK, &name);
	test(clos_slot_exists_p(pos, name), "generic_function_instance1");

	RETURN;
}


/*
 *  standard-method
 */
static int test_clos_stdmethod_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	clos_stdmethod_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_method_size, "clos_stdmethod_slots1");

	for (check = 1, i = 0; i < Clos_method_size; i++) {
		GetSlotVector(slots, i, &pos);
		GetLocationSlot(pos, &size);
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

	clos_forget_all_classes_unsafe();
	build_clos_class(Local_Thread);

	/* method */
	GetConst(COMMON_METHOD, &mclass);
	clos_find_class(mclass, &mclass);
	test(closp(mclass), "build_clos_class_method1");
	clos_class_of(mclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_method2");
	stdget_class_direct_superclasses(mclass, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_method3");
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class(check, &check);
	test(left == check, "build_clos_class_method4");

	/* standard-method */
	GetConst(COMMON_STANDARD_METHOD, &smclass);
	clos_find_class(smclass, &smclass);
	test(closp(smclass), "build_clos_class_method5");
	clos_class_of(smclass, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_method6");
	stdget_class_direct_superclasses(smclass, &check);
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

	clos_stdcombination_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_combination_size, "clos_stdcombination_slots1");

	for (check = 1, i = 0; i < Clos_combination_size; i++) {
		GetSlotVector(slots, i, &pos);
		GetLocationSlot(pos, &size);
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
	test(test_slotname(slots, Clos_combination_document, "DOCUMENT"),
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

	clos_forget_all_classes_unsafe();
	build_clos_class(Local_Thread);

	/* method_combination */
	GetConst(COMMON_METHOD_COMBINATION, &clos);
	clos_find_class(clos, &clos);
	test(closp(clos), "build_clos_method_combination1");
	clos_class_of(clos, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_method_combination2");
	stdget_class_direct_superclasses(clos, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_method_combination3");
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class(check, &check);
	test(left == check, "build_clos_method_combination4");

	RETURN;
}

static int test_build_clos_class_combination(void)
{
	addr pos, clos;

	GetConst(CLOS_METHOD_COMBINATION, &clos);
	GetConst(COMMON_STANDARD, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination1");
	GetConst(COMMON_PLUS, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination2");
	GetConst(COMMON_AND, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination3");
	GetConst(COMMON_APPEND, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination4");
	GetConst(COMMON_LIST, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination5");
	GetConst(COMMON_MAX, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination6");
	GetConst(COMMON_MIN, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination7");
	GetConst(COMMON_NCONC, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination8");
	GetConst(COMMON_PROGN, &pos);
	clos_find_combination(pos, &pos);
	test(clos_subtype_p(pos, clos), "build_clos_class_combination9");

	RETURN;
}
#endif


/*
 *  eql-specializer
 */
static int test_clos_stdspecializer_slots(void)
{
	int check;
	addr slots, pos;
	size_t i, size;

	clos_stdspecializer_slots(&slots);
	LenSlotVector(slots, &size);
	test(size == Clos_specializer_size, "clos_stdspecializer_slots1");

	for (check = 1, i = 0; i < Clos_specializer_size; i++) {
		GetSlotVector(slots, i, &pos);
		GetLocationSlot(pos, &size);
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

	/* eql_specializer */
	GetConst(CLOSNAME_EQL_SPECIALIZER, &clos);
	clos_find_class(clos, &clos);
	test(closp(clos), "build_clos_class_specializer1");
	clos_class_of(clos, &pos);
	GetConst(COMMON_STANDARD_CLASS, &check);
	clos_find_class(check, &check);
	test(check == pos, "build_clos_class_specializer2");
	stdget_class_direct_superclasses(clos, &check);
	GetCons(check, &left, &check);
	test(check == Nil, "build_clos_class_specializer3");
	GetConst(COMMON_STANDARD_OBJECT, &check);
	clos_find_class(check, &check);
	test(left == check, "build_clos_class_specializer4");

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
	const char *ptr;

	/* name, clos */
	a = readr(name);
	clos_find_class(a, &x);
	if (! closp(x)) {
		degrade_printf("type1 %s error.\n", name);
		return 0;
	}
	stdget_class_name(x, &b);
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
		ptr = va_arg(args, const char *);
		if (ptr == NULL)
			break;
		a = readr(ptr);
		clos_find_class(a, &y);
		if (! closp(y)) {
			degrade_printf("type2 %s, %s error.\n", name, ptr);
			return 0;
		}
		stdget_class_name(y, &b);
		if (! checkclass_name(a, b)) {
			degrade_printf("name2 %s, %s error\n", name, ptr);
			return 0;
		}
		if (! clos_subclass_p(x, y)) {
			degrade_printf("subclass_p %s, %s error\n", name, ptr);
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

	snprintf(data, 256, "supers-%s", name);
	va_start(args, name);
	check = checkclass_va(CONSTANT_CLOS_STRUCTURE_CLASS, name, args);
	va_end(args);

	return degrade_test(check, data);
}

#define CheckStruct2(a,b) \
	if (checkstructure((a),(b),NULL)) goto error;

static int checkslots_va(const char *name, va_list args)
{
	addr x, y;
	const char *ptr;
	size_t size, check;

	clos_find_class(readr(name), &x);
	clos_instance_heap(x, &x);
	for (size = 0; ; size++) {
		ptr = va_arg(args, const char *);
		if (ptr == NULL)
			break;
		clos_get(x, readr(ptr), &y);
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
	CheckSlots2("arithmetic-error", ":operation", ":operands");
	CheckSlots1("cell-error", ":name");
	CheckSlots0("condition");
	CheckSlots0("control-error");
	CheckSlots2("division-by-zero", ":operation", ":operands");
	CheckSlots1("end-of-file", ":stream");
	CheckSlots0("error");
	CheckSlots1("file-error", ":pathname");
	CheckSlots2("floating-point-inexact", ":operation", ":operands");
	CheckSlots2("floating-point-invalid-operation", ":operation", ":operands");
	CheckSlots2("floating-point-overflow", ":operation", ":operands");
	CheckSlots2("floating-point-underflow", ":operation", ":operands");
	CheckSlots1("package-error", ":package");
	CheckSlots0("parse-error");
	CheckSlots1("print-not-readable", ":object");
	CheckSlots0("program-error");
	CheckSlots1("reader-error", ":stream");
	CheckSlots0("serious-condition");
	CheckSlots2("simple-condition", ":format-control", ":format-arguments");
	CheckSlots2("simple-error", ":format-control", ":format-arguments");
	CheckSlots4("simple-type-error",
			":format-control", ":format-arguments", ":datum", ":expected-type");
	CheckSlots2("simple-warning", ":format-control", ":format-arguments");
	CheckSlots0("storage-condition");
	CheckSlots1("stream-error", ":stream");
	CheckSlots0("style-warning");
	CheckSlots2("type-error", ":datum", ":expected-type");
	CheckSlots1("unbound-variable", ":name");
	CheckSlots1("undefined-function", ":name");
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
static int testbreak_clos_class(void)
{
	/* access */
	TestBreak(test_stdget_class_name);
	TestBreak(test_stdget_class_direct_slots);
	TestBreak(test_stdget_class_direct_subclasses);
	TestBreak(test_stdget_class_direct_superclasses);
	TestBreak(test_stdget_class_precedence_list);
	TestBreak(test_stdget_class_slots);
	TestBreak(test_stdget_class_finalized_p);
	TestBreak(test_stdget_class_prototype);
	TestBreak(test_stdget_class_direct_methods);
	TestBreak(test_stdget_class_default_initargs);
	TestBreak(test_stdget_class_direct_default_initargs);
	TestBreak(test_stdget_class_version);
	TestBreak(test_stdget_class_update_info);
	TestBreak(test_stdget_class_document);
	/* check */
	TestBreak(test_clos_subclass_p);
	TestBreak(test_clos_subtype_p);
	TestBreak(test_clos_subclass_p2);
	TestBreak(test_clos_subtype_p2);
	TestBreak(test_clos_class_p);
	TestBreak(test_clos_funcallable_p);
	TestBreak(test_clos_generic_p);
	TestBreak(test_clos_method_p);
	TestBreak(test_clos_define_combination_p);
	TestBreak(test_clos_define_long_combination_p);
	TestBreak(test_clos_define_short_combination_p);
	TestBreak(test_clos_combination_p);
	TestBreak(test_clos_long_combination_p);
	TestBreak(test_clos_short_combination_p);
	TestBreak(test_clos_specializer_p);
	TestBreak(test_funcallp);
	/* make-instance */
	TestBreak(test_clos_instance_alloc);
	/* class-precedence-list */
	TestBreak(test_clos_precedence_classes);
	TestBreak(test_clos_precedence_pair);
	TestBreak(test_clos_precedence_super);
	TestBreak(test_clos_precedence_find);
	TestBreak(test_clos_precedence_chain);
	TestBreak(test_clos_precedence_top);
	TestBreak(test_clos_precedence_remove);
	TestBreak(test_clos_precedence_result);
	TestBreak(test_clos_precedence_list);
	/* compute-slots */
	TestBreak(test_clos_slots_name);
	TestBreak(test_clos_slots_gather);
	TestBreak(test_clos_compute_slots);
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

int test_clos_class(void)
{
	int result;
	lispcode code;
	Execute ptr;

	TITLE;

	freelisp();
	alloclisp(0, 0);
	lisp_info_enable = 1;
	ptr = Execute_Thread;
	begin_code(ptr, &code);
	if (code_run_p(code)) {
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
		build_readtable();
		lisp_init = 1;
		result = testbreak_clos_class();
	}
	end_code(ptr);
	freelisp();
	TestCheck(code_error_p(code));
	lisp_info_enable = 1;

	return result;
}

