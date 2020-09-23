#include "extern_instance.c"
#include "character.h"
#include "clos.h"
#include "code.h"
#include "common.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "declare.h"
#include "extern_control.h"
#include "extern_sequence.h"
#include "extern_object.h"
#include "extern_type.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "package_symbol.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  eval
 */
static int test_lisp_find_class(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_find_class(&pos, Nil);
	test(pos == Nil, "lisp_find_class.1");

	fixnum_heap(&pos, 10);
	lisp0_find_class(&pos, pos);
	test(pos == Nil, "lisp_find_class.2");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp0_find_class(&pos, pos);
	test(closp(pos), "lisp_find_class.3");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp0_find_class(&pos, x);
	test(closp(pos), "lisp_find_class.4");

	GetConst(COMMON_ASSOC, &pos);
	lisp0_find_class(&pos, pos);
	test(pos == Nil, "lisp_find_class.5");

	lisp_find_class(x, Nil);
	test(lisp_nil_p(x), "lisp_find_class.6");

	fixnum_heap(&pos, 10);
	lisp_find_class(x, pos);
	test(lisp_nil_p(x), "lisp_find_class.7");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_find_class(x, pos);
	test(lisp_clos_p(x), "lisp_find_class.8");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp_find_class(x, x);
	test(lisp_clos_p(x), "lisp_find_class.9");

	GetConst(COMMON_ASSOC, &pos);
	lisp_find_class(x, pos);
	test(lisp_nil_p(x), "lisp_find_class.10");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp0_find_class_(&pos, pos);
	test(closp(pos), "lisp_find_class.11");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp0_find_class_(&pos, x);
	test(closp(pos), "lisp_find_class.12");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_find_class_(x, pos);
	test(lisp_clos_p(x), "lisp_find_class.13");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp_find_class_(x, x);
	test(lisp_clos_p(x), "lisp_find_class.14");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp0_find_class_(&pos, pos);
	test(closp(pos), "lisp_find_class.15");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp0_find_class_(&pos, x);
	test(closp(pos), "lisp_find_class.16");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_find_class_(x, pos);
	test(lisp_clos_p(x), "lisp_find_class.17");

	GetConst(COMMON_STANDARD_CLASS, &pos);
	lisp_hold_set(x, pos);
	lisp_find_class_(x, x);
	test(lisp_clos_p(x), "lisp_find_class.18");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_find_class8(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_find_class8_(&pos, "CLASS");
	test(closp(pos), "lisp_find_class8.1");

	lisp_find_class8_(x, "CLASS");
	test(lisp_clos_p(pos), "lisp_find_class8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_find_class16(void)
{
	addr control, x, pos;
	const byte16 name[] = { 'C', 'L', 'A', 'S', 'S', 0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_find_class16_(&pos, name);
	test(closp(pos), "lisp_find_class16.1");

	lisp_find_class16_(x, name);
	test(lisp_clos_p(pos), "lisp_find_class16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_find_class32(void)
{
	addr control, x, pos;
	const unicode name[] = { 'C', 'L', 'A', 'S', 'S', 0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp0_find_class32_(&pos, name);
	test(closp(pos), "lisp_find_class32.1");

	lisp_find_class32_(x, name);
	test(lisp_clos_p(pos), "lisp_find_class32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  make-instance
 */
static int test_lisp_instance(void)
{
	addr control, x, y, z, pos, key, value;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-instance1 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	/* lisp0-instance */
	lisp0_intern8_(&pos, NULL, "TEST-INSTANCE1");
	lisp0_instance_(&pos, pos, NULL);
	test(closp(pos), "lisp_instance.1");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance.2");

	lisp0_intern8_(&pos, NULL, "TEST-INSTANCE1");
	lisp0_intern8_(&key, "KEYWORD", "AAA");
	fixnum_heap(&value, 20);
	lisp0_instance_(&pos, pos, key, value, NULL);
	test(closp(pos), "lisp_instance.3");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance.4");

	lisp_intern8_(x, NULL, "TEST-INSTANCE1");
	lisp_intern8_(y, "KEYWORD", "AAA");
	lisp_fixnum(z, 30);
	lisp0_instance_(&pos, x, y, z, NULL);
	test(closp(pos), "lisp_instance.5");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance.6");

	/* lisp-instance */
	lisp0_intern8_(&pos, NULL, "TEST-INSTANCE1");
	lisp_instance_(x, pos, NULL);
	test(lisp_clos_p(x), "lisp_instance.7");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance.8");

	lisp0_intern8_(&pos, NULL, "TEST-INSTANCE1");
	lisp0_intern8_(&key, "KEYWORD", "AAA");
	fixnum_heap(&value, 20);
	lisp_instance_(x, pos, key, value, NULL);
	test(lisp_clos_p(x), "lisp_instance.9");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance.10");

	lisp_intern8_(x, NULL, "TEST-INSTANCE1");
	lisp_intern8_(y, "KEYWORD", "AAA");
	lisp_fixnum(z, 30);
	lisp_instance_(x, x, y, z, NULL);
	test(lisp_clos_p(x), "lisp_instance.11");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance.12");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_instance8(void)
{
	addr control, x, z, pos, key, value;

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-instance2 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	/* lisp0-instance */
	lisp0_instance8_(&pos, "test-instance2", NULL);
	test(closp(pos), "lisp_instance8.1");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance8.2");

	fixnum_heap(&value, 20);
	lisp0_instance8_(&pos, "test-instance2", ":aaa", value, NULL);
	test(closp(pos), "lisp_instance8.3");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance8.4");

	lisp_fixnum(z, 30);
	lisp0_instance8_(&pos, "test-instance2", ":AAA", z, NULL);
	test(closp(pos), "lisp_instance8.5");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance8.6");

	/* lisp-instance */
	lisp_instance8_(x, "test-instance2", NULL);
	test(lisp_clos_p(x), "lisp_instance8.7");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance8.8");

	fixnum_heap(&value, 20);
	lisp_instance8_(x, "test-instance2", ":aaa", value, NULL);
	test(lisp_clos_p(x), "lisp_instance8.9");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance8.10");

	lisp_fixnum(z, 30);
	lisp_instance8_(x, "test-instance2", ":aaa", z, NULL);
	test(lisp_clos_p(x), "lisp_instance8.11");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance8.12");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_instance16(void)
{
	addr control, x, z, pos, key, value;
	const byte16 name[] = { 't','e','s','t','-','i','n','s','t','a','n','c','e','3',0 };
	const byte16 word[] = { ':','a','a','a',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-instance3 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	/* lisp0-instance */
	lisp0_instance16_(&pos, name, NULL);
	test(closp(pos), "lisp_instance16.1");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance16.2");

	fixnum_heap(&value, 20);
	lisp0_instance16_(&pos, name, word, value, NULL);
	test(closp(pos), "lisp_instance16.3");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance16.4");

	lisp_fixnum(z, 30);
	lisp0_instance16_(&pos, name, word, z, NULL);
	test(closp(pos), "lisp_instance16.5");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance16.6");

	/* lisp-instance */
	lisp_instance16_(x, name, NULL);
	test(lisp_clos_p(x), "lisp_instance16.7");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance16.8");

	fixnum_heap(&value, 20);
	lisp_instance16_(x, name, word, value, NULL);
	test(lisp_clos_p(x), "lisp_instance16.9");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance16.10");

	lisp_fixnum(z, 30);
	lisp_instance16_(x, name, word, z, NULL);
	test(lisp_clos_p(x), "lisp_instance16.11");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance16.12");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_instance32(void)
{
	addr control, x, z, pos, key, value;
	const unicode name[] = { 't','e','s','t','-','i','n','s','t','a','n','c','e','4',0 };
	const unicode word[] = { ':','a','a','a',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-instance4 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	/* lisp0-instance */
	lisp0_instance32_(&pos, name, NULL);
	test(closp(pos), "lisp_instance32.1");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance32.2");

	fixnum_heap(&value, 20);
	lisp0_instance32_(&pos, name, word, value, NULL);
	test(closp(pos), "lisp_instance32.3");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance32.4");

	lisp_fixnum(z, 30);
	lisp0_instance32_(&pos, name, word, z, NULL);
	test(closp(pos), "lisp_instance32.5");
	lisp0_intern8_(&key, NULL, "AAA");
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance32.6");

	/* lisp-instance */
	lisp_instance32_(x, name, NULL);
	test(lisp_clos_p(x), "lisp_instance32.7");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &value);
	test(RefFixnum(value) == 10, "lisp_instance32.8");

	fixnum_heap(&value, 20);
	lisp_instance32_(x, name, word, value, NULL);
	test(lisp_clos_p(x), "lisp_instance32.9");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 20, "lisp_instance32.10");

	lisp_fixnum(z, 30);
	lisp_instance32_(x, name, word, z, NULL);
	test(lisp_clos_p(x), "lisp_instance32.11");
	lisp0_intern8_(&key, NULL, "AAA");
	hold_value(x, &pos);
	clos_check_(pos, key, &pos);
	test(RefFixnum(pos) == 30, "lisp_instance32.12");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  slot-exists
 */
static int test_lisp_slot_exists(void)
{
	int check;
	addr control, pos, key;

	lisp_push_control(&control);

	lisp_eval8_(NULL,
			"(defclass test-slot-exists1 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-exists1", NULL);
	lisp0_intern8_(&key, NULL, "HELLO");
	check = 999;
	lisp_slot_exists_(pos, key, &check);
	test(check == 0, "lisp_slot_exists.1");

	lisp0_intern8_(&key, NULL, "AAA");
	lisp_slot_exists_(pos, key, &check);
	test(check, "lisp_slot_exists.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_exists8(void)
{
	int check;
	addr control, pos;
	const char key1[] = "HELLO";
	const char key2[] = "AAA";

	lisp_push_control(&control);

	lisp_eval8_(NULL,
			"(defclass test-slot-exists2 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-exists2", NULL);
	check = 999;
	lisp_slot_exists8_(pos, key1, &check);
	test(check == 0, "lisp_slot_exists8.1");

	lisp_slot_exists8_(pos, key2, &check);
	test(check, "lisp_slot_exists8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_exists16(void)
{
	int check;
	addr control, pos;
	const byte16 key1[] = { 'H','E','L','L','O',0 };
	const byte16 key2[] = { 'A','A','A',0 };

	lisp_push_control(&control);

	lisp_eval8_(NULL,
			"(defclass test-slot-exists3 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-exists3", NULL);
	check = 999;
	lisp_slot_exists16_(pos, key1, &check);
	test(check == 0, "lisp_slot_exists16.1");

	lisp_slot_exists16_(pos, key2, &check);
	test(check, "lisp_slot_exists16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_exists32(void)
{
	int check;
	addr control, pos;
	const unicode key1[] = { 'H','E','L','L','O',0 };
	const unicode key2[] = { 'A','A','A',0 };

	lisp_push_control(&control);

	lisp_eval8_(NULL,
			"(defclass test-slot-exists3 ()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-exists3", NULL);
	check = 999;
	lisp_slot_exists32_(pos, key1, &check);
	test(check == 0, "lisp_slot_exists32.1");

	lisp_slot_exists32_(pos, key2, &check);
	test(check, "lisp_slot_exists32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  slot-boundp
 */
static int test_lisp_slot_boundp(void)
{
	int check;
	addr control, pos, key;

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-boundp1 ()"
			"  ((aaa :initarg :aaa)"
			"   (bbb :initarg :bbb :initform 10)))");
	lisp0_instance8_(&pos, "test-slot-boundp1", NULL);
	lisp0_intern8_(&key, NULL, "AAA");
	check = 999;
	lisp_slot_boundp_(pos, key, &check);
	test(check == 0, "lisp_slot_boundp.1");

	lisp0_intern8_(&key, NULL, "BBB");
	lisp_slot_boundp_(pos, key, &check);
	test(check, "lisp_slot_boundp.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_boundp8(void)
{
	int check;
	addr control, pos;
	const char key1[] = "AAA";
	const char key2[] = "BBB";

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-boundp2 ()"
			"  ((aaa :initarg :aaa)"
			"   (bbb :initarg :bbb :initform 10)))");
	lisp0_instance8_(&pos, "test-slot-boundp2", NULL);
	check = 999;
	lisp_slot_boundp8_(pos, key1, &check);
	test(check == 0, "lisp_slot_boundp8.1");

	lisp_slot_boundp8_(pos, key2, &check);
	test(check, "lisp_slot_boundp8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_boundp16(void)
{
	int check;
	addr control, pos;
	const byte16 key1[] = { 'A','A','A',0 };
	const byte16 key2[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-boundp3 ()"
			"  ((aaa :initarg :aaa)"
			"   (bbb :initarg :bbb :initform 10)))");
	lisp0_instance8_(&pos, "test-slot-boundp3", NULL);
	check = 999;
	lisp_slot_boundp16_(pos, key1, &check);
	test(check == 0, "lisp_slot_boundp16.1");

	lisp_slot_boundp16_(pos, key2, &check);
	test(check, "lisp_slot_boundp16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_boundp32(void)
{
	int check;
	addr control, pos;
	const unicode key1[] = { 'A','A','A',0 };
	const unicode key2[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-boundp4 ()"
			"  ((aaa :initarg :aaa)"
			"   (bbb :initarg :bbb :initform 10)))");
	lisp0_instance8_(&pos, "test-slot-boundp4", NULL);
	check = 999;
	lisp_slot_boundp32_(pos, key1, &check);
	test(check == 0, "lisp_slot_boundp32.1");

	lisp_slot_boundp32_(pos, key2, &check);
	test(check, "lisp_slot_boundp32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  slot-makunbound
 */
static int test_lisp_slot_makunbound(void)
{
	int check;
	addr control, pos, key;

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-makunbound1()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-makunbound1", NULL);
	lisp0_intern8_(&key, NULL, "AAA");
	lisp_slot_makunbound_(pos, key);
	check = 999;
	lisp_slot_boundp8_(pos, "AAA", &check);
	test(check == 0, "lisp_slot_makunbound.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_makunbound8(void)
{
	int check;
	addr control, pos;
	const char key[] = "AAA";

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-makunbound2()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-makunbound2", NULL);
	lisp_slot_makunbound8_(pos, key);
	check = 999;
	lisp_slot_boundp8_(pos, "AAA", &check);
	test(check == 0, "lisp_slot_makunbound8.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_makunbound16(void)
{
	int check;
	addr control, pos;
	const byte16 key[] = { 'A','A','A',0 };

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-makunbound3()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-makunbound3", NULL);
	lisp_slot_makunbound16_(pos, key);
	check = 999;
	lisp_slot_boundp8_(pos, "AAA", &check);
	test(check == 0, "lisp_slot_makunbound16.1");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_makunbound32(void)
{
	int check;
	addr control, pos;
	const unicode key[] = { 'A','A','A',0 };

	lisp_push_control(&control);
	lisp_eval8_(NULL,
			"(defclass test-slot-makunbound4()"
			"  ((aaa :initarg :aaa :initform 10)))");

	lisp0_instance8_(&pos, "test-slot-makunbound4", NULL);
	lisp_slot_makunbound32_(pos, key);
	check = 999;
	lisp_slot_boundp8_(pos, "AAA", &check);
	test(check == 0, "lisp_slot_makunbound32.1");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  slot-value
 */
static int test_lisp_slot_value(void)
{
	addr control, x, pos, key, value;

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-value1()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-value1", NULL);
	lisp0_intern8_(&key, NULL, "BBB");
	lisp0_slot_value_(&value, pos, key);
	test(RefFixnum(value) == 20, "lisp_slot_value.1");

	lisp_intern8_(x, NULL, "AAA");
	lisp_slot_value_(x, pos, x);
	hold_value(x, &value);
	test(RefFixnum(value) == 10, "lisp_slot_value.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_value8(void)
{
	addr control, x, pos, value;
	const char key1[] = { 'A','A','A',0 };
	const char key2[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-value2()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-value2", NULL);
	lisp0_slot_value8_(&value, pos, key2);
	test(RefFixnum(value) == 20, "lisp_slot_value8.1");

	lisp_slot_value8_(x, pos, key1);
	hold_value(x, &value);
	test(RefFixnum(value) == 10, "lisp_slot_value8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_value16(void)
{
	addr control, x, pos, value;
	const byte16 key1[] = { 'A','A','A',0 };
	const byte16 key2[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-value2()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-value2", NULL);
	lisp0_slot_value16_(&value, pos, key2);
	test(RefFixnum(value) == 20, "lisp_slot_value16.1");

	lisp_slot_value16_(x, pos, key1);
	hold_value(x, &value);
	test(RefFixnum(value) == 10, "lisp_slot_value16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_value32(void)
{
	addr control, x, pos, value;
	const unicode key1[] = { 'A','A','A',0 };
	const unicode key2[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-value2()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-value2", NULL);
	lisp0_slot_value32_(&value, pos, key2);
	test(RefFixnum(value) == 20, "lisp_slot_value32.1");

	lisp_slot_value32_(x, pos, key1);
	hold_value(x, &value);
	test(RefFixnum(value) == 10, "lisp_slot_value32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  slot-setf
 */
static int test_lisp_slot_setf(void)
{
	addr control, x, y, z, pos, key, value;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-setf1()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-setf1", NULL);
	lisp0_intern8_(&key, NULL, "BBB");
	fixnum_heap(&value, 100);
	lisp_slot_setf_(pos, key, value);
	lisp0_slot_value8_(&value, pos, "BBB");
	test(RefFixnum(value) == 100, "lisp_slot_setf.1");

	lisp_instance8_(x, "test-slot-setf1", NULL);
	lisp_intern8_(y, NULL, "BBB");
	lisp_fixnum(z, 200);
	lisp_slot_setf_(x, y, z);
	lisp0_slot_value8_(&value, x, "BBB");
	test(RefFixnum(value) == 200, "lisp_slot_setf.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_setf8(void)
{
	addr control, x, z, pos, value;
	const char key[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-setf2()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-setf2", NULL);
	fixnum_heap(&value, 100);
	lisp_slot_setf8_(pos, key, value);
	lisp0_slot_value8_(&value, pos, "BBB");
	test(RefFixnum(value) == 100, "lisp_slot_setf8.1");

	lisp_instance8_(x, "test-slot-setf2", NULL);
	lisp_fixnum(z, 200);
	lisp_slot_setf8_(x, key, z);
	lisp0_slot_value8_(&value, x, "BBB");
	test(RefFixnum(value) == 200, "lisp_slot_setf8.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_setf16(void)
{
	addr control, x, z, pos, value;
	const byte16 key[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-setf3()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-setf3", NULL);
	fixnum_heap(&value, 100);
	lisp_slot_setf16_(pos, key, value);
	lisp0_slot_value8_(&value, pos, "BBB");
	test(RefFixnum(value) == 100, "lisp_slot_setf16.1");

	lisp_instance8_(x, "test-slot-setf3", NULL);
	lisp_fixnum(z, 200);
	lisp_slot_setf16_(x, key, z);
	lisp0_slot_value8_(&value, x, "BBB");
	test(RefFixnum(value) == 200, "lisp_slot_setf16.2");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_slot_setf32(void)
{
	addr control, x, z, pos, value;
	const unicode key[] = { 'B','B','B',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	z = Lisp_hold();

	lisp_eval8_(NULL,
			"(defclass test-slot-setf3()"
			"  ((aaa :initarg :aaa :initform 10)"
			"   (bbb :initarg :bbb :initform 20)))");
	lisp0_instance8_(&pos, "test-slot-setf3", NULL);
	fixnum_heap(&value, 100);
	lisp_slot_setf32_(pos, key, value);
	lisp0_slot_value8_(&value, pos, "BBB");
	test(RefFixnum(value) == 100, "lisp_slot_setf32.1");

	lisp_instance8_(x, "test-slot-setf3", NULL);
	lisp_fixnum(z, 200);
	lisp_slot_setf32_(x, key, z);
	lisp0_slot_value8_(&value, x, "BBB");
	test(RefFixnum(value) == 200, "lisp_slot_setf32.2");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_instance(void)
{
	/* find-class */
	TestBreak(test_lisp_find_class);
	TestBreak(test_lisp_find_class8);
	TestBreak(test_lisp_find_class16);
	TestBreak(test_lisp_find_class32);
	/* make-instance */
	TestBreak(test_lisp_instance);
	TestBreak(test_lisp_instance8);
	TestBreak(test_lisp_instance16);
	TestBreak(test_lisp_instance32);
	/* slot-exists */
	TestBreak(test_lisp_slot_exists);
	TestBreak(test_lisp_slot_exists8);
	TestBreak(test_lisp_slot_exists16);
	TestBreak(test_lisp_slot_exists32);
	/* slot-boundp */
	TestBreak(test_lisp_slot_boundp);
	TestBreak(test_lisp_slot_boundp8);
	TestBreak(test_lisp_slot_boundp16);
	TestBreak(test_lisp_slot_boundp32);
	/* slot-makunbound */
	TestBreak(test_lisp_slot_makunbound);
	TestBreak(test_lisp_slot_makunbound8);
	TestBreak(test_lisp_slot_makunbound16);
	TestBreak(test_lisp_slot_makunbound32);
	/* slot-value */
	TestBreak(test_lisp_slot_value);
	TestBreak(test_lisp_slot_value8);
	TestBreak(test_lisp_slot_value16);
	TestBreak(test_lisp_slot_value32);
	/* slot-setf */
	TestBreak(test_lisp_slot_setf);
	TestBreak(test_lisp_slot_setf8);
	TestBreak(test_lisp_slot_setf16);
	TestBreak(test_lisp_slot_setf32);

	return 0;
}

static void testinit_extern_instance(Execute ptr)
{
	build_lisproot(ptr);
	build_constant();
	build_object();
	build_character();
	build_package();
	build_stream();
	build_symbol();
	build_clos(ptr);
	build_condition(ptr);
	build_type();
	build_syscall();
	build_common();
	build_reader();
	build_pathname();
	build_declare();
	build_code();
}

int test_extern_instance(void)
{
	DegradeTitle;
	return DegradeCode(extern_instance);
}

