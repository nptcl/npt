#include "clos_instance.c"
#include "clos.h"
#include "clos_build.h"
#include "clos_type.h"
#include "closget.h"
#include "character.h"
#include "common.h"
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

static void clos_supers_alloc(LocalRoot local, addr *ret, va_list args)
{
	addr super, cons, clos, slots;
	Execute ptr;

	ptr = Execute_Thread;
	/* make cons */
	cons = Nil;
	for (;;) {
		super = va_arg(args, addr);
		if (super == NULL) break;
		cons_alloc(local, &cons, super, cons);
	}

	/* make clos */
	clos_stdclass_slots_(ptr, &slots);
	clos_alloc(local, &clos, slots);
	SetClassOfClos(clos, Nil);
	nreverse(&cons, cons);
	stdset_class_direct_superclasses_(ptr, clos, cons);
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
	Execute ptr;
	addr pos, check, k, v;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_NAME, &k);
	clos_set_(pos, k, v);
	stdget_class_name_(ptr, pos, &check);
	test(check == v, "stdget_class_name1");

	stdset_class_name_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_name1");

	RETURN;
}

static int test_stdget_class_direct_slots(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DIRECT_SLOTS, &k);
	clos_set_(pos, k, v);
	stdget_class_direct_slots_(ptr, pos, &check);
	test(check == v, "stdget_class_direct_slots1");

	stdset_class_direct_slots_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_direct_slots1");

	RETURN;
}

static int test_stdget_class_direct_subclasses(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DIRECT_SUBCLASSES, &k);
	clos_set_(pos, k, v);
	stdget_class_direct_subclasses_(ptr, pos, &check);
	test(check == v, "stdget_class_direct_subclasses1");

	stdset_class_direct_subclasses_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_direct_subclasses1");

	RETURN;
}

static int test_stdget_class_direct_superclasses(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DIRECT_SUPERCLASSES, &k);
	clos_set_(pos, k, v);
	stdget_class_direct_superclasses_(ptr, pos, &check);
	test(check == v, "stdget_class_direct_superclasses1");

	stdset_class_direct_superclasses_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_direct_superclasses1");

	RETURN;
}

static int test_stdget_class_precedence_list(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_CLASS_PRECEDENCE_LIST, &k);
	clos_set_(pos, k, v);
	stdget_class_precedence_list_(ptr, pos, &check);
	test(check == v, "stdget_class_precedence_list1");

	stdset_class_precedence_list_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_precedence_list2");

	RETURN;
}

static int test_stdget_class_slots(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_EFFECTIVE_SLOTS, &k);
	clos_set_(pos, k, v);
	stdget_class_slots_(ptr, pos, &check);
	test(check == v, "stdget_class_slots1");

	stdset_class_slots_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_slots2");

	RETURN;
}

static int test_stdget_class_finalized_p(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_FINALIZED_P, &k);
	clos_set_(pos, k, v);
	stdget_class_finalized_p_(ptr, pos, &check);
	test(check == v, "stdget_class_finalized_p1");

	stdset_class_finalized_p_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_finalized_p2");

	RETURN;
}

static int test_stdget_class_prototype(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_PROTOTYPE, &k);
	clos_set_(pos, k, v);
	stdget_class_prototype_(ptr, pos, &check);
	test(check == v, "stdget_class_prototype1");

	stdset_class_prototype_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_prototype2");

	RETURN;
}

static int test_stdget_class_default_initargs(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DEFAULT_INITARGS, &k);
	clos_set_(pos, k, v);
	stdget_class_default_initargs_(ptr, pos, &check);
	test(check == v, "stdget_class_default_initargs1");

	stdset_class_default_initargs_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_default_initargs2");

	RETURN;
}

static int test_stdget_class_direct_default_initargs(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DIRECT_DEFAULT_INITARGS, &k);
	clos_set_(pos, k, v);
	stdget_class_direct_default_initargs_(ptr, pos, &check);
	test(check == v, "stdget_class_direct_default_initargs1");

	stdset_class_direct_default_initargs_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_direct_default_initargs2");

	RETURN;
}

static int test_stdget_class_version(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_VERSION, &k);
	clos_set_(pos, k, v);
	stdget_class_version_(ptr, pos, &check);
	test(check == v, "stdget_class_version1");

	stdset_class_version_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_version2");

	RETURN;
}

static int test_stdget_class_documentation(void)
{
	addr pos, check, k, v;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	v = readr_debug("aaa");
	GetConst(CLOSNAME_DOCUMENTATION, &k);
	clos_set_(pos, k, v);
	stdget_class_documentation_(ptr, pos, &check);
	test(check == v, "stdget_class_documentation.1");

	stdset_class_documentation_(ptr, pos, T);
	clos_get_(pos, k, &check);
	test(check == T, "stdset_class_documentation.2");

	RETURN;
}


/*
 *  check
 */
static int test_clos_subclass_p(void)
{
	addr pos, clos;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &pos);
	GetConst(CLOS_CLASS, &clos);
	test(clos_subclass_p_debug(pos, clos), "clos_subclass_p1");
	test(! clos_subclass_p_debug(clos, pos), "clos_subclass_p2");

	GetConst(CLOS_STANDARD_CLASS, &clos);
	clos_instance_heap_(ptr, clos, &pos);
	test(! clos_subclass_p_debug(clos, pos), "clos_subclass_p3");

	RETURN;
}

static int test_clos_subtype_p(void)
{
	addr pos, clos;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_METHOD, &pos);
	GetConst(CLOS_STANDARD_METHOD, &clos);
	test(! clos_subtype_p_debug(pos, clos), "clos_subtype_p1");
	test(! clos_subtype_p_debug(clos, pos), "clos_subtype_p2");

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	GetConst(CLOS_CLASS, &clos);
	test(clos_subtype_p_debug(pos, clos), "clos_subtype_p3");

	RETURN;
}

static int test_clos_subclass_p2(void)
{
	addr t, a, b, cons;
	Execute ptr;

	ptr = Execute_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, NULL);
	clos_supers_heap(&b, t, NULL);
	clos_precedence_list_(ptr, t, &cons);
	stdset_class_precedence_list_(ptr, t, cons);
	clos_precedence_list_(ptr, a, &cons);
	stdset_class_precedence_list_(ptr, a, cons);
	clos_precedence_list_(ptr, b, &cons);
	stdset_class_precedence_list_(ptr, b, cons);

	test(clos_subclass_p_debug(t, t), "clos_subclass_p1");
	test(! clos_subclass_p_debug(t, a), "clos_subclass_p2");
	test(! clos_subclass_p_debug(a, t), "clos_subclass_p3");
	test(clos_subclass_p_debug(b, t), "clos_subclass_p4");
	test(! clos_subclass_p_debug(t, b), "clos_subclass_p5");
	test(! clos_subclass_p_debug(t, b), "clos_subclass_p6");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&a, t, NULL);
	clos_supers_heap(&b, a, NULL);
	clos_precedence_list_(ptr, t, &cons);
	stdset_class_precedence_list_(ptr, t, cons);
	clos_precedence_list_(ptr, b, &cons);
	stdset_class_precedence_list_(ptr, b, cons);
	test(clos_subclass_p_debug(b, t), "clos_subclass_p7");
	test(! clos_subclass_p_debug(t, b), "clos_subclass_p8");

	RETURN;
}

static int test_clos_subtype_p2(void)
{
	addr metaclass, clos, name, instance;
	Execute ptr;

	ptr = Execute_Thread;
	GetConst(CLOS_STANDARD_CLASS, &metaclass);
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);
	clos = Nil;
	clos_stdclass_type_(ptr, &clos, metaclass, name, Nil);
	clos_instance_heap_(ptr, clos, &instance);

	test(clos_subtype_p_debug(instance, clos), "clos_subtype_p1");
	test(! clos_subtype_p_debug(clos, instance), "clos_subtype_p2");

	RETURN;
}

static int test_clos_class_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_class_p_(ptr, pos, &check);
	test(check, "clos_class_p1");
	GetConst(CLOS_STANDARD_OBJECT, &pos);
	clos_class_p_(ptr, pos, &check);
	test(check, "clos_class_p2");
	GetConst(CLOS_COMBINATION_STANDARD, &pos);
	clos_class_p_(ptr, pos, &check);
	test(! check, "clos_class_p3");
	GetConst(CLOS_CLASS, &pos);
	clos_class_p_(ptr, pos, &check);
	test(check, "clos_class_p4");
	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_class_p_(ptr, pos, &check);
	test(check, "clos_class_p5");

	RETURN;
}

static int test_clos_funcallable_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_funcallable_p_(ptr, pos, &check);
	test(! check, "clos_funcallable_p1");
	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_funcallable_p_(ptr, pos, &check);
	test(! check, "clos_funcallable_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_funcallable_p_(ptr, pos, &check);
	test(check, "clos_funcallable_p3");
	GetConst(CLOS_STANDARD_OBJECT, &pos);
	clos_funcallable_p_(ptr, pos, &check);
	test(! check, "clos_funcallable_p4");
	clos_set_funcall(pos);
	clos_funcallable_p_(ptr, pos, &check);
	test(check, "clos_funcallable_p5");

	RETURN;
}

static int test_clos_generic_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_generic_p_(ptr, pos, &check);
	test(! check, "clos_generic_p1");

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_generic_p_(ptr, pos, &check);
	test(! check, "clos_generic_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_generic_p_(ptr, pos, &check);
	test(check, "clos_generic_p3");

	GetConst(CLOS_GENERIC_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_generic_p_(ptr, pos, &check);
	test(check, "clos_generic_p4");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_generic_p_(ptr, pos, &check);
	test(! check, "clos_generic_p5");
	clos_set_funcall(pos);
	clos_generic_p_(ptr, pos, &check);
	test(! check, "clos_generic_p6");

	RETURN;
}

static int test_clos_method_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_method_p_(ptr, pos, &check);
	test(! check, "clos_method_p1");

	GetConst(CLOS_STANDARD_METHOD, &pos);
	clos_method_p_(ptr, pos, &check);
	test(! check, "clos_method_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_method_p_(ptr, pos, &check);
	test(check, "clos_method_p3");

	GetConst(CLOS_METHOD, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_method_p_(ptr, pos, &check);
	test(check, "clos_method_p4");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_method_p_(ptr, pos, &check);
	test(! check, "clos_method_p5");

	RETURN;
}

static int test_clos_define_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(check, "clos_define_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(check, "clos_define_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_combination_p9");

	RETURN;
}

static int test_clos_define_long_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(check, "clos_define_long_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_long_combination_p9");

	RETURN;
}

static int test_clos_define_short_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(check, "clos_define_short_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_define_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_define_short_combination_p9");

	RETURN;
}

static int test_clos_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(check, "clos_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(! check, "clos_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_combination_p_(ptr, pos, &check);
	test(check, "clos_combination_p9");

	RETURN;
}

static int test_clos_long_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(check, "clos_long_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_long_combination_p_(ptr, pos, &check);
	test(! check, "clos_long_combination_p9");

	RETURN;
}

static int test_clos_short_combination_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p1");

	GetConst(CLOS_DEFINE_LONG_METHOD_COMBINATION, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p3");

	GetConst(CLOS_DEFINE_SHORT_METHOD_COMBINATION, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p4");
	clos_instance_heap_(ptr, pos, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p5");

	GetConst(CLOS_LONG_METHOD_COMBINATION, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p6");
	clos_instance_heap_(ptr, pos, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p7");

	GetConst(CLOS_SHORT_METHOD_COMBINATION, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(! check, "clos_short_combination_p8");
	clos_instance_heap_(ptr, pos, &pos);
	clos_short_combination_p_(ptr, pos, &check);
	test(check, "clos_short_combination_p9");

	RETURN;
}

static int test_clos_specializer_p(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	GetConst(CLOS_STANDARD_CLASS, &pos);
	clos_specializer_p_(ptr, pos, &check);
	test(! check, "clos_specializer_p1");

	GetConst(CLOS_EQL_SPECIALIZER, &pos);
	clos_specializer_p_(ptr, pos, &check);
	test(! check, "clos_specializer_p2");
	clos_instance_heap_(ptr, pos, &pos);
	clos_specializer_p_(ptr, pos, &check);
	test(check, "clos_specializer_p3");

	GetConst(CLOS_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	clos_specializer_p_(ptr, pos, &check);
	test(! check, "clos_specializer_p4");

	RETURN;
}

static int test_funcallp(void)
{
	int check;
	addr pos;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;

	funcallp_(ptr, T, &check);
	test(! check, "funcallp1");
	GetConst(COMMON_CAR, &pos);
	funcallp_(ptr, pos, &check);
	test(! check, "funcallp2");
	GetFunctionSymbol(pos, &pos);
	funcallp_(ptr, pos, &check);
	test(check, "funcallp3");

	GetConst(CLOS_METHOD, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	funcallp_(ptr, pos, &check);
	test(! check, "funcallp4");
	clos_set_funcall(pos);
	funcallp_(ptr, pos, &check);
	test(check, "funcallp5");

	GetConst(CLOS_STANDARD_GENERIC_FUNCTION, &pos);
	clos_instance_heap_(ptr, pos, &pos);
	funcallp_(ptr, pos, &check);
	test(check, "funcallp6");

	RETURN;
}


/*
 *  make-instance
 */
static int test_clos_instance_alloc(void)
{
	addr slots, pos, name, one, clos;
	fixnum value, version;
	Execute ptr;

	ptr = Execute_Thread;
	/* direct-slots */
	slot_vector_heap(&slots, 3);

	slot_heap_(ptr, &pos);
	internchar_keyword_debug("HELLO", &name);
	setname_slot_(ptr, pos, name);
	fixnum_heap(&one, 111);
	setform_slot_(ptr, pos, one);
	setlocation_slot_(ptr, pos, 0);
	SetSlotVector(slots, 0, pos);

	slot_heap_(ptr, &pos);
	internchar_keyword_debug("AAA", &name);
	setname_slot_(ptr, pos, name);
	fixnum_heap(&one, 222);
	setform_slot_(ptr, pos, one);
	setlocation_slot_(ptr, pos, 1);
	SetSlotVector(slots, 1, pos);

	slot_heap_(ptr, &pos);
	internchar_keyword_debug("BBB", &name);
	setname_slot_(ptr, pos, name);
	fixnum_heap(&one, 333);
	setform_slot_(ptr, pos, one);
	setlocation_slot_(ptr, pos, 2);
	SetSlotVector(slots, 2, pos);

	/* clos */
	clos_stdclass_slots_(ptr, &pos);
	clos_heap(&clos, pos);
	SetClassOfClos(clos, Nil);
	stdset_class_slots_(ptr, clos, slots);
	stdset_class_finalized_p_(ptr, clos, T);

	/* test */
	clos_instance_heap_(ptr, clos, &clos);
	clos_getelt(clos, 0, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "clos_instance_heap1");
	clos_getelt(clos, 1, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "clos_instance_heap2");
	clos_getelt(clos, 2, &pos);
	GetFixnum(pos, &value);
	test(value == 333, "clos_instance_heap3");

	internchar_keyword_debug("HELLO", &name);
	clos_check_(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 111, "clos_instance_heap4");
	internchar_keyword_debug("AAA", &name);
	clos_check_(clos, name, &pos);
	GetFixnum(pos, &value);
	test(value == 222, "clos_instance_heap5");
	internchar_keyword_debug("BBB", &name);
	clos_check_(clos, name, &pos);
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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* test */
	clos_supers_local(local, &clos, NULL);
	clos_precedence_classes_(ptr, clos, &right);
	test(GetType(right) == LISPTYPE_CONS, "clos_precedence_classes1");
	GetCons(right, &left, &right);
	test(left == clos, "clos_precedence_classes2");
	test(right != Nil, "clos_precedence_classes3");
	GetCons(right, &left, &right);
	test(left == Unbound, "clos_precedence_classes4");
	test(right == Nil, "clos_precedence_classes5");

	clos_supers_local(local, &clos, Nil, T, NULL);
	clos_precedence_classes_(ptr, clos, &right);
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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* (clos unbound) -> ((clos . unbound)) */
	clos_supers_local(local, &clos, NULL);
	clos_precedence_pair_(ptr, clos, &right);

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
	clos_precedence_pair_(ptr, clos, &cons);
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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	clos_supers_local(local, &clos, NULL);
	clos_precedence_super_(ptr, clos, &right, Nil, Nil);
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
	clos_precedence_super_(ptr, clos, &right, Nil, Nil);
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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair_(ptr, clos, &right);

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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* (t . Unbound) */
	clos_supers_local(local, &t, NULL);
	list_local(local, &cons, t, NULL);
	clos_precedence_chain_(ptr, cons, &cons);
	test(length_list_unsafe(cons) == 1, "clos_precedence_chain1");
	test(test_find_cons_chain_check(t, Unbound, cons), "clos_precedence_chain2");

	clos_supers_local(local, &t, NULL);
	clos_supers_local(local, &a, t, NULL);
	clos_supers_local(local, &b, t, NULL);
	clos_supers_local(local, &c, a, b, t, NULL);
	clos_supers_local(local, &d, t, NULL);
	clos_supers_local(local, &e, t, NULL);
	clos_supers_local(local, &clos, d, c, e, t, NULL);
	clos_precedence_super_(ptr, clos, &cons, Nil, Nil);
	clos_precedence_chain_(ptr, cons, &cons);
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
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair_(ptr, clos, &right);
	clos_precedence_top_(right, &left);
	test(left == clos, "clos_precedence_top1");

	rollback_local(local, stack);

	RETURN;
}

static int test_clos_precedence_remove(void)
{
	addr clos, left, right, a, b, c, check;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	/* (clos a b unbound) -> ((b . unbound) (a . b) (clos . a)) */
	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	clos_supers_local(local, &clos, a, b, NULL);
	clos_precedence_pair_(ptr, clos, &right);
	clos_precedence_remove_(b, right, &right);
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
	clos_precedence_remove_(a, right, &right);
	test(right == Nil, "clos_precedence_remove6");

	/* ((A . b) (b . a)) */
	cons_local(local, &left, b, a);
	conscar_local(local, &right, left);
	cons_local(local, &left, a, b);
	cons_local(local, &right, left, right);
	clos_precedence_remove_(a, right, &right);
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
	clos_precedence_remove_(a, right, &right);
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
		if (GetStatusDynamic(right))
			return 0;
		GetCdr(right, &right);
	}

	return 1;
}

static int test_clos_precedence_result(void)
{
	addr clos, left, right;
	Execute ptr;
	LocalRoot local;
	LocalStack stack;

	ptr = Execute_Thread;
	local = Local_Thread;
	push_local(local, &stack);

	clos_supers_heap(&clos, NULL);
	clos_precedence_result_(ptr, clos, &right, Unbound, Nil);
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
	Execute ptr;

	ptr = Execute_Thread;
	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&food, object, NULL);
	clos_supers_heap(&spice, food, object, NULL);
	clos_supers_heap(&fruit, food, object, NULL);
	clos_supers_heap(&cinnamon, spice, object, NULL);
	clos_supers_heap(&apple, fruit, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	clos_precedence_list_(ptr, pie, &cons);
	test(no_dynamic_check(cons), "clos_precedence_list1");
	test(cons_check(cons, pie, apple, fruit, cinnamon, spice, food, object, t, NULL),
			"clos_precedence_list2");

	clos_supers_heap(&t, NULL);
	clos_supers_heap(&object, t, NULL);
	clos_supers_heap(&cinnamon, object, NULL);
	clos_supers_heap(&apple, object, NULL);
	clos_supers_heap(&pastry, cinnamon, apple, object, NULL);
	clos_supers_heap(&pie, apple, cinnamon, object, NULL);

	clos_precedence_list_(ptr, pie, &cons);
	test(no_dynamic_check(cons), "clos_precedence_list3");
	test(cons_check(cons, pie, apple, cinnamon, object, t, NULL),
			"clos_precedence_list4");

	clos_precedence_list_(ptr, pastry, &cons);
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
	int check;
	addr slot1, slot2, name, pos, cons;
	Execute ptr;

	ptr = Execute_Thread;
	check = 0;
	internchar_debug(LISP_COMMON_USER, "HELLO", &name);

	clos_slots_name_(ptr, name, Nil, &cons, &check);
	test(check == 0, "clos_slots_name1");

	slot_heap_(ptr, &slot1);
	internchar_debug(LISP_COMMON_USER, "AAA", &pos);
	setname_slot_(ptr, slot1, pos);

	slot_heap_(ptr, &slot2);
	internchar_debug(LISP_COMMON_USER, "BBB", &pos);
	setname_slot_(ptr, slot2, pos);

	list_heap(&cons, slot1, slot2, NULL);
	clos_slots_name_(ptr, name, cons, &cons, &check);
	test(check == 0, "clos_slots_name2");

	slot_heap_(ptr, &slot2);
	internchar_debug(LISP_COMMON_USER, "HELLO", &pos);
	setname_slot_(ptr, slot2, pos);

	list_heap(&cons, slot1, slot2, NULL);
	clos_slots_name_(ptr, name, cons, &cons, &check);
	test(check, "clos_slots_name3");
	test(cons == slot2, "clos_slots_name4");

	RETURN;
}

static int test_clos_slots_push(void)
{
	addr a, b, list, v;
	Execute ptr;

	ptr = Execute_Thread;
	slot_heap_(ptr, &a);
	slot_heap_(ptr, &b);
	setargs_slot_(ptr, a, Nil);
	setargs_slot_(ptr, b, Nil);
	clos_slots_push_(ptr, a, b);
	getargs_slot_(ptr, a, &list);
	test(list == Nil, "clos_slots_push1");

	v = readr_debug("(aaa bbb ccc)");
	setargs_slot_(ptr, a, v);
	setargs_slot_(ptr, b, Nil);
	clos_slots_push_(ptr, a, b);
	getargs_slot_(ptr, a, &list);
	test(equal_debug(list, v), "clos_slots_push2");

	v = readr_debug("(aaa bbb ccc)");
	setargs_slot_(ptr, a, Nil);
	setargs_slot_(ptr, b, v);
	clos_slots_push_(ptr, a, b);
	getargs_slot_(ptr, a, &list);
	v = readr_debug("(ccc bbb aaa)");
	test(equal_debug(list, v), "clos_slots_push3");

	v = readr_debug("(aaa bbb ccc)");
	setargs_slot_(ptr, a, v);
	v = readr_debug("(ddd bbb ccc eee)");
	setargs_slot_(ptr, b, v);
	clos_slots_push_(ptr, a, b);
	getargs_slot_(ptr, a, &list);
	v = readr_debug("(eee ddd aaa bbb ccc)");
	test(equal_debug(list, v), "clos_slots_push4");

	RETURN;
}

#if 0
static void test_makeclos_slotname_heap(addr *ret, const char *name)
{
	addr symbol;
	slot_heap_(ptr, ret);
	internchar_debug(LISP_COMMON_USER, name, &symbol);
	setname_slot_(ptr, *ret, symbol);
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
		test_makeclos_slotname_heap(&slot, name);
		cons_heap(&cons, slot, cons);
	}
	va_end(args);
	nreverse(&cons, cons);

	/* make clos */
	slot_vector_heap(&slots, i);
	for (i = 0; cons != Nil; i++) {
		GetCons(cons, &slot, &cons);
		SetSlotVector(slots, i, slot);
	}
	clos_stdclass_slots_(ptr, &temp);
	clos_heap(&clos, temp);
	SetClassOfClos(clos, Nil);
	stdset_class_direct_slots_(ptr, clos, slots);
	list_heap(&temp, clos, NULL);
	stdset_class_precedence_list_(ptr, clos, temp);
	*ret = clos;
}

static int slotnamecheck(addr slot, const char *name)
{
	addr check;
	getname_slot_(ptr, slot, &slot);
	internchar_debug(LISP_COMMON_USER, name, &check);
	return check == slot;
}

static int test_clos_slots_loop(void)
{
	addr clos1, clos2, cons, check, aaa;
	LocalRoot local;
	size_t size;


	local = Local_Thread;
	test_makeclos_heap(&clos1, "HELLO", "AAA", "BBB", NULL);
	clos_slots_loop(local, clos1, &cons, &size);
	test(cons != Nil, "clos_slots_loop1");
	test(size == 3, "clos_slots_loop2");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "clos_slots_loop3");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "AAA"), "clos_slots_loop4");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "clos_slots_loop5");
	test(cons == Nil, "clos_slots_loop6");

	test_makeclos_heap(&clos2, "AAA", "CCC", NULL);
	list_heap(&cons, clos2, clos1, NULL);
	stdset_class_precedence_list_(ptr, clos2, cons);
	clos_slots_loop(local, clos2, &cons, &size);
	test(size == 4, "clos_slots_loop7");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "HELLO"), "clos_slots_loop8");
	GetCons(cons, &check, &cons);
	aaa = check;
	test(slotnamecheck(check, "AAA"), "clos_slots_loop9");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "BBB"), "clos_slots_loop10");
	GetCons(cons, &check, &cons);
	test(slotnamecheck(check, "CCC"), "clos_slots_loop11");
	test(cons == Nil, "clos_slots_loop12");

	stdget_class_direct_slots_(ptr, clos2, &cons);
	GetSlotVector(cons, 0, &cons);
	test(cons == aaa, "clos_slots_loop13");

	RETURN;
}

static int test_clos_compute_slots(void)
{
	LocalRoot local;
	addr clos1, clos2, cons, check, aaa;
	size_t size;

	local = Local_Thread;
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
	stdset_class_precedence_list_(ptr, clos2, cons);
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

	stdget_class_direct_slots_(ptr, clos2, &cons);
	GetSlotVector(cons, 0, &cons);
	test(cons != aaa, "clos_compute_slots11");

	RETURN;
}
#endif


/*
 *  main
 */
static int testcase_clos_instance(void)
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
	TestBreak(test_stdget_class_default_initargs);
	TestBreak(test_stdget_class_direct_default_initargs);
	TestBreak(test_stdget_class_version);
	TestBreak(test_stdget_class_documentation);
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
	TestBreak(test_clos_slots_push);
#if 0
	TestBreak(test_clos_slots_loop);
	TestBreak(test_clos_compute_slots);
#endif

	return 0;
}

static void testinit_clos_instance(Execute ptr)
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

int test_clos_instance(void)
{
	DegradeTitle;
	return DegradeCode(clos_instance);
}

