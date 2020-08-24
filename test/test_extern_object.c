#include "extern_object.c"
#include "character.h"
#include "clos.h"
#include "common.h"
#include "constant.h"
#include "control.h"
#include "degrade.h"
#include "integer.h"
#include "object.h"
#include "package.h"
#include "pathname.h"
#include "reader.h"
#include "stream.h"
#include "symbol.h"
#include "syscall.h"
#include "type.h"
#include "type_table.h"

/*
 *  hold
 */
static int test_lisp_hold_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr pos;

	test(! lisp_hold_p(NULL), "lisp_hold_p.1");
	test(! lisp_hold_p(Unbound), "lisp_hold_p.2");
	test(! lisp_hold_p(Nil), "lisp_hold_p.3");
	test(! lisp_hold_p(T), "lisp_hold_p.4");

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &pos, T);
	test(lisp_hold_p(pos), "lisp_hold_p.5");
	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold_value(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	lisp_hold_value(x, &x);
	test(x == T, "lisp_hold_value.1");

	lisp_hold_value(Nil, &x);
	test(x == Nil, "lisp_hold_value.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold_set(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	lisp_hold_set(x, Nil);
	lisp_hold_value(x, &y);
	test(y == Nil, "lisp_hold_set.1");

	lisp_hold_set(x, T);
	lisp_hold_value(x, &y);
	test(y == T, "lisp_hold_set.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_Lisp_holdv(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	hold_local(local, &x, T);
	test(Lisp_holdv(x) == T, "Lisp_holdv.1");
	hold_local(local, &x, Nil);
	test(Lisp_holdv(x) == Nil, "Lisp_holdv.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_hold(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	lisp_hold(&x, T);
	test(Lisp_holdv(x) == T, "lisp_hold.1");
	lisp_hold(&x, x);
	test(Lisp_holdv(x) == T, "lisp_hold.2");

	rollback_local(local, stack);

	RETURN;
}

static int test_Lisp_hold(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();
	test(Lisp_holdv(x) == Nil, "Lisp_hold.1");
	rollback_local(local, stack);

	RETURN;
}


/*
 *  nil, t
 */
static int test_lisp_nil(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);

	lisp0_nil(&x);
	test(x == Nil, "lisp_nil.1");

	test(Lisp_nil() == Nil, "lisp_nil.2");

	lisp_hold(&x, T);
	lisp_nil(x);
	test(Lisp_holdv(x) == Nil, "lisp_nil.3");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_t(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);

	lisp0_t(&x);
	test(x == T, "lisp_t.1");

	test(Lisp_t() == T, "lisp_t.2");

	lisp_hold(&x, Nil);
	lisp_t(x);
	test(Lisp_holdv(x) == T, "lisp_t.3");

	rollback_local(local, stack);

	RETURN;
}


/*
 *  type
 */
static int test_lisp_nil_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_nil_p(Nil), "lisp_nil_p.1");
	test(! lisp_nil_p(T), "lisp_nil_p.2");
	lisp_hold_set(x, Nil);
	test(lisp_nil_p(x), "lisp_nil_p.3");
	lisp_hold_set(x, T);
	test(! lisp_nil_p(x), "lisp_nil_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_t_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_t_p(T), "lisp_t_p.1");
	test(! lisp_t_p(Nil), "lisp_t_p.2");
	lisp_hold_set(x, T);
	test(lisp_t_p(x), "lisp_t_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_t_p(x), "lisp_t_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_null_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	test(lisp_null_p(NULL), "lisp_null_p.1");
	test(! lisp_null_p(Nil), "lisp_null_p.2");
	lisp_hold_set(x, NULL);
	test(lisp_null_p(x), "lisp_null_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_null_p(x), "lisp_null_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_character_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	character_heap(&y, 'A');
	test(lisp_character_p(y), "lisp_character_p.1");
	test(! lisp_character_p(Nil), "lisp_character_p.2");
	lisp_hold_set(x, y);
	test(lisp_character_p(x), "lisp_character_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_character_p(x), "lisp_character_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_cons_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	consnil_heap(&y);
	test(lisp_cons_p(y), "lisp_cons_p.1");
	test(! lisp_cons_p(Nil), "lisp_cons_p.2");
	lisp_hold_set(x, y);
	test(lisp_cons_p(x), "lisp_cons_p.3");
	lisp_hold_set(x, Nil);
	test(! lisp_cons_p(x), "lisp_cons_p.4");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_list_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	consnil_heap(&y);
	test(lisp_list_p(y), "lisp_list_p.1");
	test(lisp_list_p(Nil), "lisp_list_p.2");
	test(! lisp_list_p(T), "lisp_list_p.3");
	lisp_hold_set(x, y);
	test(lisp_list_p(x), "lisp_list_p.4");
	lisp_hold_set(x, Nil);
	test(lisp_list_p(x), "lisp_list_p.5");
	lisp_hold_set(x, T);
	test(! lisp_list_p(x), "lisp_list_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_string_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "YYY");
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_string_p(y), "lisp_string_p.1");
	test(lisp_string_p(z), "lisp_string_p.2");
	test(! lisp_string_p(T), "lisp_string_p.3");
	lisp_hold_set(x, y);
	test(lisp_string_p(x), "lisp_string_p.4");
	lisp_hold_set(x, z);
	test(lisp_string_p(x), "lisp_string_p.5");
	lisp_hold_set(x, T);
	test(! lisp_string_p(x), "lisp_string_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_strvect_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strvect_char_heap(&y, "YYY");
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_strvect_p(y), "lisp_strvect_p.1");
	test(! lisp_strvect_p(z), "lisp_strvect_p.2");
	test(! lisp_strvect_p(T), "lisp_strvect_p.3");
	lisp_hold_set(x, y);
	test(lisp_strvect_p(x), "lisp_strvect_p.4");
	lisp_hold_set(x, z);
	test(! lisp_strvect_p(x), "lisp_strvect_p.5");
	lisp_hold_set(x, T);
	test(! lisp_strvect_p(x), "lisp_strvect_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_array_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	strarray_char_heap_(&y, "YYY");
	strvect_char_heap(&z, "ZZZ");
	test(lisp_array_p(y), "lisp_array_p.1");
	test(! lisp_array_p(z), "lisp_array_p.2");
	test(! lisp_array_p(T), "lisp_array_p.3");
	lisp_hold_set(x, y);
	test(lisp_array_p(x), "lisp_array_p.4");
	lisp_hold_set(x, z);
	test(! lisp_array_p(x), "lisp_array_p.5");
	lisp_hold_set(x, T);
	test(! lisp_array_p(x), "lisp_array_p.6");

	rollback_local(local, stack);

	RETURN;
}

static int test_lisp_vector_p(void)
{
	LocalRoot local;
	LocalStack stack;
	addr x, y, z;

	local = Local_Thread;
	push_local(local, &stack);
	x = Lisp_hold();

	vector_heap(&y, 10);
	strarray_char_heap_(&z, "ZZZ");
	test(lisp_vector_p(y), "lisp_vector_p.1");
	test(! lisp_vector_p(z), "lisp_vector_p.2");
	test(! lisp_vector_p(T), "lisp_vector_p.3");
	lisp_hold_set(x, y);
	test(lisp_vector_p(x), "lisp_vector_p.4");
	lisp_hold_set(x, z);
	test(! lisp_vector_p(x), "lisp_vector_p.5");
	lisp_hold_set(x, T);
	test(! lisp_vector_p(x), "lisp_vector_p.6");

	rollback_local(local, stack);

	RETURN;
}


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
static int test_lisp_string8_(void)
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

static int test_lisp_string16_(void)
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

static int test_lisp_string32_(void)
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
static int testcase_extern_object(void)
{
	/* hold */
	TestBreak(test_lisp_hold_p);
	TestBreak(test_lisp_hold_value);
	TestBreak(test_lisp_hold_set);
	TestBreak(test_Lisp_holdv);
	TestBreak(test_lisp_hold);
	TestBreak(test_Lisp_hold);
	/* nil, t */
	TestBreak(test_lisp_nil);
	TestBreak(test_lisp_t);
	/* type */
	TestBreak(test_lisp_nil_p);
	TestBreak(test_lisp_t_p);
	TestBreak(test_lisp_null_p);
	TestBreak(test_lisp_character_p);
	TestBreak(test_lisp_cons_p);
	TestBreak(test_lisp_list_p);
	TestBreak(test_lisp_string_p);
	TestBreak(test_lisp_strvect_p);
	TestBreak(test_lisp_array_p);
	TestBreak(test_lisp_vector_p);
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
	TestBreak(test_lisp_string8_);
	TestBreak(test_lisp_string16_);
	TestBreak(test_lisp_string32_);

	return 0;
}

static void testinit_extern_object(Execute ptr)
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

int test_extern_object(void)
{
	DegradeTitle;
	return DegradeCode(extern_object);
}

