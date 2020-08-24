#include "extern_sequence.c"
#include "condition.h"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "extern_type.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  make sequence
 */
static int test_lisp_cons(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_cons(&y, NULL, T);
	test(consp(y), "lisp_cons.1");
	GetCar(y, &z);
	test(z == Nil, "lisp_cons.2");
	GetCdr(y, &z);
	test(z == T, "lisp_cons.3");

	lisp_cons(x, T, NULL);
	test(lisp_cons_p(x), "lisp_cons.4");
	hold_value(x, &y);
	GetCar(y, &z);
	test(z == T, "lisp_cons.5");
	GetCdr(y, &z);
	test(z == Nil, "lisp_cons.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_vector(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_vector(&y, 10);
	test(! lisp_hold_p(y), "lisp_vector.1");
	test(lisp_vector_p(y), "lisp_vector.2");

	lisp_vector(x, 20);
	test(lisp_vector_p(x), "lisp_vector.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_list(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_list(&y, NULL);
	test(y == Nil, "lisp_list.1");

	fixnum_heap(&y, 10);
	lisp_hold(&y, y);
	lisp_list(x, T, y, NULL);
	test(lisp_cons_p(x), "lisp_list.2");

	hold_value(x, &y);
	test(length_list_unsafe(y) == 2, "lisp_list.3");
	GetCar(y, &y);
	test(y == T, "lisp_list.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_lista(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_lista(&y, NULL);
	test(y == Nil, "lisp_lista.1");

	lisp0_lista(&y, T, NULL);
	test(y == T, "lisp_lista.2");

	fixnum_heap(&y, 10);
	lisp_hold(&y, y);
	lisp_lista(x, T, y, NULL);
	test(lisp_cons_p(x), "lisp_lista.3");

	hold_value(x, &x);
	GetCons(x, &x, &y);
	test(x == T, "lisp_lista.4");
	test(fixnump(y), "lisp_lista.5");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  access sequence
 */
static int test_lisp_getelt(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z, v, pos;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	fixnum_heap(&y, 10);
	fixnum_heap(&z, 20);
	vector_heap(&v, 4);
	setarray(v, 0, T);
	setarray(v, 1, y);
	setarray(v, 2, Nil);
	setarray(v, 3, z);

	lisp0_getelt_(&pos, v, 0);
	test(pos == T, "lisp_getelt.1");

	lisp_getelt_(x, v, 1);
	test(Lisp_holdv(x) == y, "lisp_getelt.2");
	lisp_getelt_(x, v, 2);
	test(Lisp_holdv(x) == Nil, "lisp_getelt.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_setelt(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, v;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	vector_heap(&v, 4);
	lisp_hold_set(x, v);

	lisp_setelt_(v, 0, T);
	lisp_getelt_(y, v, 0);
	test(Lisp_holdv(y) == T, "lisp_setelt.1");

	lisp_hold_set(x, v);
	fixnum_heap(&v, 10);
	lisp_hold_set(y, v);
	lisp_setelt_(x, 2, y);
	lisp_hold_set(y, Nil);
	lisp_getelt_(y, x, 2);
	lisp_hold_value(y, &y);
	test(fixnump(y), "lisp_setelt.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_length(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, v;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	vector_heap(&v, 4);
	size = 0;
	lisp_length_(v, &size);
	test(size == 4, "lisp_length.1");

	vector_heap(&v, 4);
	lisp_hold(&v, v);
	size = 0;
	lisp_length_(v, &size);
	test(size == 4, "lisp_length.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_reverse(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, c, v, z;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	list_heap(&v, T, a, b, c, NULL);

	lisp0_reverse_(&z, v);
	lisp_length_(z, &size);
	test(size == 4, "lisp_reverse.1");
	lisp_length_(v, &size);
	test(size == 4, "lisp_reverse.2");
	GetCar(z, &z);
	test(z == c, "lisp_reverse.3");

	lisp_hold_set(y, v);
	lisp_reverse_(x, y);
	lisp_length_(x, &size);
	test(size == 4, "lisp_reverse.4");
	lisp_length_(y, &size);
	test(size == 4, "lisp_reverse.5");
	lisp_hold_value(x, &z);
	GetCar(z, &z);
	test(z == c, "lisp_reverse.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_nreverse(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, c, v, z;
	size_t size;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);

	list_heap(&v, T, a, b, c, NULL);
	lisp0_nreverse_(&z, v);
	lisp_length_(z, &size);
	test(size == 4, "lisp_nreverse.1");
	lisp_length_(v, &size);
	test(size != 4, "lisp_nreverse.2");
	GetCar(z, &z);
	test(z == c, "lisp_nreverse.3");

	list_heap(&v, T, a, b, c, NULL);
	lisp_hold_set(y, v);
	lisp_nreverse_(x, y);
	lisp_length_(x, &size);
	test(size == 4, "lisp_nreverse.4");
	lisp_length_(y, &size);
	test(size != 4, "lisp_nreverse.5");
	lisp_hold_value(x, &z);
	GetCar(z, &z);
	test(z == c, "lisp_nreverse.6");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  cons
 */
static int test_lisp_car(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, v, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);

	cons_heap(&v, a, b);
	lisp0_car(&z, v);
	test(z == a, "lisp_car.1");

	lisp_hold_set(y, v);
	lisp_car(x, y);
	test(Lisp_holdv(x) == a, "lisp_car.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_cdr(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, v, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);

	cons_heap(&v, a, b);
	lisp0_cdr(&z, v);
	test(z == b, "lisp_cdr.1");

	lisp_hold_set(y, v);
	lisp_cdr(x, y);
	test(Lisp_holdv(x) == b, "lisp_cdr.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_carcdr(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, v, z, w;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);

	cons_heap(&v, a, b);
	lisp0_carcdr(&z, &w, v);
	test(z == a, "lisp_carcdr.1");
	test(w == b, "lisp_carcdr.2");

	lisp_hold_set(y, v);
	lisp_carcdr(x, y, y);
	test(Lisp_holdv(x) == a, "lisp_carcdr.3");
	test(Lisp_holdv(y) == b, "lisp_carcdr.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_setf_car(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, v, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);

	cons_heap(&v, a, b);
	lisp_setf_car(v, T);
	lisp0_car(&z, v);
	test(z == T, "lisp_setf_car.1");

	cons_heap(&v, a, b);
	lisp_hold_set(x, v);
	lisp_hold_set(y, T);
	lisp_setf_car(x, y);
	lisp0_car(&z, v);
	test(z == T, "lisp_setf_car.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_setf_cdr(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, v, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);

	cons_heap(&v, a, b);
	lisp_setf_cdr(v, T);
	lisp0_cdr(&z, v);
	test(z == T, "lisp_setf_cdr.1");

	cons_heap(&v, a, b);
	lisp_hold_set(x, v);
	lisp_hold_set(y, T);
	lisp_setf_cdr(x, y);
	lisp0_cdr(&z, v);
	test(z == T, "lisp_setf_cdr.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_setf_carcdr(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, a, b, c, d, v, z, w;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	fixnum_heap(&c, 30);
	fixnum_heap(&d, 40);

	cons_heap(&v, a, b);
	lisp_setf_carcdr(v, c, d);
	lisp0_car(&w, v);
	test(w == c, "lisp_setf_carcdr.1");
	lisp0_cdr(&w, v);
	test(w == d, "lisp_setf_carcdr.2");

	cons_heap(&v, a, b);
	lisp_hold_set(x, v);
	lisp_hold_set(y, c);
	lisp_hold_set(z, d);
	lisp_setf_carcdr(x, y, z);
	lisp0_car(&w, v);
	test(w == c, "lisp_setf_carcdr.3");
	lisp0_cdr(&w, v);
	test(w == d, "lisp_setf_carcdr.4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  string
 */
static int test_lisp_string8(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_string8_(&y, "ABC");
	test(strvectp(y), "lisp_string8.1");
	test(strvect_equal_char(y, "ABC"), "lisp_string8.2");

	lisp_string8_(x, "ABC");
	lisp_hold_value(x, &y);
	test(strvectp(y), "lisp_string8.3");
	test(strvect_equal_char(y, "ABC"), "lisp_string8.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_string16(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	const byte16 str[] = { 'A', 'B', 'C', 0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_string16_(&y, (const void *)str);
	test(strvectp(y), "lisp_string16.1");
	test(strvect_equal_char(y, "ABC"), "lisp_string16.2");

	lisp_string16_(x, (const void *)str);
	lisp_hold_value(x, &y);
	test(strvectp(y), "lisp_string16.3");
	test(strvect_equal_char(y, "ABC"), "lisp_string16.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_string32(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;
	const unicode str[] = { 'A', 'B', 'C', 0 };

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	lisp0_string32_(&y, (const void *)str);
	test(strvectp(y), "lisp_string32.1");
	test(strvect_equal_char(y, "ABC"), "lisp_string32.2");

	lisp_string32_(x, (const void *)str);
	lisp_hold_value(x, &y);
	test(strvectp(y), "lisp_string32.3");
	test(strvect_equal_char(y, "ABC"), "lisp_string32.4");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_sequence(void)
{
	/* make sequence */
	TestBreak(test_lisp_cons);
	TestBreak(test_lisp_vector);
	TestBreak(test_lisp_list);
	TestBreak(test_lisp_lista);
	/* access sequence */
	TestBreak(test_lisp_getelt);
	TestBreak(test_lisp_setelt);
	TestBreak(test_lisp_length);
	TestBreak(test_lisp_reverse);
	TestBreak(test_lisp_nreverse);
	/* cons */
	TestBreak(test_lisp_car);
	TestBreak(test_lisp_cdr);
	TestBreak(test_lisp_carcdr);
	TestBreak(test_lisp_setf_car);
	TestBreak(test_lisp_setf_cdr);
	TestBreak(test_lisp_setf_carcdr);
	/* string */
	TestBreak(test_lisp_string8);
	TestBreak(test_lisp_string16);
	TestBreak(test_lisp_string32);

	return 0;
}

static void testinit_extern_sequence(Execute ptr)
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
}

int test_extern_sequence(void)
{
	DegradeTitle;
	return DegradeCode(extern_sequence);
}

