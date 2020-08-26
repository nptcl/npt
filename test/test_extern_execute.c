#include "extern_execute.c"
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
static int test_lisp_eval(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_reader8_(&pos, "(+ 10 20)");
	lisp0_eval_(&pos, pos);
	test(fixnump(pos), "lisp_eval.1");
	test(RefFixnum(pos) == 30, "lisp_eval.2");

	lisp0_reader8_(&pos, "(setq lisp-eval-test (+ 30 40))");
	lisp0_eval_(NULL, pos);
	lisp0_reader8_(&pos, "lisp-eval-test");
	lisp0_eval_(&pos, pos);
	test(RefFixnum(pos) == 70, "lisp_eval.3");

	lisp_reader8_(x, "(/ 4 5)");
	lisp_eval_(x, x);
	test(lisp_ratio_p(x), "lisp_eval.4");

	lisp_reader8_(x, "(setq lisp-eval-test #\\a)");
	lisp_eval_(NULL, x);
	lisp_reader8_(x, "lisp-eval-test");
	lisp_eval_(x, x);
	test(lisp_character_p(x), "lisp_eval.5");

	RETURN;
}

static int test_lisp_eval8(void)
{
	addr control, x, pos;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_eval8_(&pos, "(+ 10 20)");
	test(fixnump(pos), "lisp_eval8.1");
	test(RefFixnum(pos) == 30, "lisp_eval8.2");

	lisp0_eval8_(NULL, "(setq lisp-eval8-test (+ 30 40))");
	lisp0_eval8_(&pos, "lisp-eval8-test");
	test(RefFixnum(pos) == 70, "lisp_eval8.3");

	lisp_eval8_(x, "(/ 4 5)");
	test(lisp_ratio_p(x), "lisp_eval8.4");

	lisp_eval8_(NULL, "(setq lisp-eval8-test #\\a)");
	lisp_eval8_(x, "lisp-eval8-test");
	test(lisp_character_p(x), "lisp_eval8.5");

	RETURN;
}

static int test_lisp_eval16(void)
{
	addr control, x, pos;
	const byte16 str[] = { 't',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_eval16_(&pos, (const void *)str);
	test(pos == T, "lisp_eval16.1");

	lisp_eval16_(x, (const void *)str);
	test(lisp_t_p(x), "lisp_eval16.2");

	RETURN;
}

static int test_lisp_eval32(void)
{
	addr control, x, pos;
	const unicode str[] = { 't',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp0_eval32_(&pos, (const void *)str);
	test(pos == T, "lisp_eval32.1");

	lisp_eval32_(x, (const void *)str);
	test(lisp_t_p(x), "lisp_eval32.2");

	RETURN;
}


/*
 *  funcall
 */
static int test_lisp_funcall(void)
{
	addr control, x, y, z, a, b, c;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_intern8_(&c, NULL, "*");
	lisp0_funcall_(&a, c, a, b, NULL);
	test(fixnump(a), "lisp_funcall.1");
	test(RefFixnum(a) == 200, "lisp_funcall.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_intern8_(z, NULL, "+");
	lisp0_funcall_(&a, z, x, y, NULL);
	test(fixnump(a), "lisp_funcall.3");
	test(RefFixnum(a) == 50, "lisp_funcall.4");

	lisp0_funcall_(NULL, z, x, y, NULL);
	test(1, "lisp_funcall.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_intern8_(&c, NULL, "*");
	lisp_funcall_(x, c, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall.6");
	test(RefFixnum(a) == 200, "lisp_funcall.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_intern8_(z, NULL, "+");
	lisp_funcall_(x, z, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall.8");
	test(RefFixnum(a) == 50, "lisp_funcall.9");

	lisp_funcall_(NULL, z, x, y, NULL);
	test(1, "lisp_funcall.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_funcall8(void)
{
	addr control, x, y, a, b;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_funcall8_(&a, "*", a, b, NULL);
	test(fixnump(a), "lisp_funcall8.1");
	test(RefFixnum(a) == 200, "lisp_funcall8.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp0_funcall8_(&a, "+", x, y, NULL);
	test(fixnump(a), "lisp_funcall8.3");
	test(RefFixnum(a) == 50, "lisp_funcall8.4");

	lisp0_funcall8_(NULL, "+", x, y, NULL);
	test(1, "lisp_funcall8.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp_funcall8_(x, "*", a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall8.6");
	test(RefFixnum(a) == 200, "lisp_funcall8.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_funcall8_(x, "+", x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall8.8");
	test(RefFixnum(a) == 50, "lisp_funcall8.9");

	lisp_funcall8_(NULL, "+", x, y, NULL);
	test(1, "lisp_funcall8.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_funcall16(void)
{
	addr control, x, y, a, b;
	const byte16 plus[] = { '+',0 };
	const byte16 multi[] = { '*',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_funcall16_(&a, multi, a, b, NULL);
	test(fixnump(a), "lisp_funcall16.1");
	test(RefFixnum(a) == 200, "lisp_funcall16.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp0_funcall16_(&a, plus, x, y, NULL);
	test(fixnump(a), "lisp_funcall16.3");
	test(RefFixnum(a) == 50, "lisp_funcall16.4");

	lisp0_funcall16_(NULL, plus, x, y, NULL);
	test(1, "lisp_funcall16.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp_funcall16_(x, multi, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall16.6");
	test(RefFixnum(a) == 200, "lisp_funcall16.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_funcall16_(x, plus, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall16.8");
	test(RefFixnum(a) == 50, "lisp_funcall16.9");

	lisp_funcall16_(NULL, plus, x, y, NULL);
	test(1, "lisp_funcall16.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_funcall32(void)
{
	addr control, x, y, a, b;
	const unicode plus[] = { '+',0 };
	const unicode multi[] = { '*',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_funcall32_(&a, multi, a, b, NULL);
	test(fixnump(a), "lisp_funcall32.1");
	test(RefFixnum(a) == 200, "lisp_funcall32.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp0_funcall32_(&a, plus, x, y, NULL);
	test(fixnump(a), "lisp_funcall32.3");
	test(RefFixnum(a) == 50, "lisp_funcall32.4");

	lisp0_funcall32_(&a, plus, x, y, NULL);
	test(1, "lisp_funcall32.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp_funcall32_(x, multi, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall32.6");
	test(RefFixnum(a) == 200, "lisp_funcall32.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_funcall32_(x, plus, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_funcall32.8");
	test(RefFixnum(a) == 50, "lisp_funcall32.9");

	lisp_funcall32_(NULL, plus, x, y, NULL);
	test(1, "lisp_funcall32.10");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  apply
 */
static int test_lisp_apply(void)
{
	addr control, x, y, z, a, b, c;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_intern8_(&c, NULL, "*");
	lisp0_apply_(&a, c, a, b, NULL);
	test(fixnump(a), "lisp_apply.1");
	test(RefFixnum(a) == 200, "lisp_apply.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_intern8_(z, NULL, "+");
	lisp0_apply_(&a, z, x, y, NULL);
	test(fixnump(a), "lisp_apply.3");
	test(RefFixnum(a) == 50, "lisp_apply.4");

	lisp0_apply_(NULL, z, x, y, NULL);
	test(1, "lisp_apply.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_intern8_(&c, NULL, "*");
	lisp_apply_(x, c, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply.6");
	test(RefFixnum(a) == 200, "lisp_apply.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_intern8_(z, NULL, "+");
	lisp_apply_(x, z, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply.8");
	test(RefFixnum(a) == 50, "lisp_apply.9");

	lisp_apply_(NULL, z, x, y, NULL);
	test(1, "lisp_apply.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_apply8(void)
{
	addr control, x, y, a, b;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_apply8_(&a, "*", a, b, NULL);
	test(fixnump(a), "lisp_apply8.1");
	test(RefFixnum(a) == 200, "lisp_apply8.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp0_apply8_(&a, "+", x, y, NULL);
	test(fixnump(a), "lisp_apply8.3");
	test(RefFixnum(a) == 50, "lisp_apply8.4");

	lisp0_apply8_(NULL, "+", x, y, NULL);
	test(1, "lisp_apply8.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp_apply8_(x, "*", a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply8.6");
	test(RefFixnum(a) == 200, "lisp_apply8.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_apply8_(x, "+", x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply8.8");
	test(RefFixnum(a) == 50, "lisp_apply8.9");

	lisp_apply8_(NULL, "+", x, y, NULL);
	test(1, "lisp_apply8.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_apply16(void)
{
	addr control, x, y, a, b;
	const byte16 plus[] = { '+',0 };
	const byte16 multi[] = { '*',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_apply16_(&a, multi, a, b, NULL);
	test(fixnump(a), "lisp_apply16.1");
	test(RefFixnum(a) == 200, "lisp_apply16.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp0_apply16_(&a, plus, x, y, NULL);
	test(fixnump(a), "lisp_apply16.3");
	test(RefFixnum(a) == 50, "lisp_apply16.4");

	lisp0_apply16_(NULL, plus, x, y, NULL);
	test(1, "lisp_apply16.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp_apply16_(x, multi, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply16.6");
	test(RefFixnum(a) == 200, "lisp_apply16.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_apply16_(x, plus, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply16.8");
	test(RefFixnum(a) == 50, "lisp_apply16.9");

	lisp_apply16_(NULL, plus, x, y, NULL);
	test(1, "lisp_apply16.10");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_apply32(void)
{
	addr control, x, y, a, b;
	const unicode plus[] = { '+',0 };
	const unicode multi[] = { '*',0 };

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_apply32_(&a, multi, a, b, NULL);
	test(fixnump(a), "lisp_apply32.1");
	test(RefFixnum(a) == 200, "lisp_apply32.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp0_apply32_(&a, plus, x, y, NULL);
	test(fixnump(a), "lisp_apply32.3");
	test(RefFixnum(a) == 50, "lisp_apply32.4");

	lisp0_apply32_(&a, plus, x, y, NULL);
	test(1, "lisp_apply32.5");

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp_apply32_(x, multi, a, b, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply32.6");
	test(RefFixnum(a) == 200, "lisp_apply32.7");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_apply32_(x, plus, x, y, NULL);
	lisp_hold_value(x, &a);
	test(fixnump(a), "lisp_apply32.8");
	test(RefFixnum(a) == 50, "lisp_apply32.9");

	lisp_apply32_(NULL, plus, x, y, NULL);
	test(1, "lisp_apply32.10");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  lowlevel
 */
static int test_lisp_eval_control(void)
{
	addr control, x, y;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_reader8_(x, "(+ 1 2 3)");
	lisp_eval_control_(x);
	getresult_control(Execute_Thread, &y);
	test(fixnump(y), "lisp_eval_control.1");
	test(RefFixnum(y) == 6, "lisp_eval_control.2");

	lisp_reader8_(x, "(* 1 2 3 4)");
	lisp_hold_value(x, &y);
	lisp_eval_control_(y);
	getresult_control(Execute_Thread, &y);
	test(fixnump(y), "lisp_eval_control.3");
	test(RefFixnum(y) == 24, "lisp_eval_control.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_eval_string_control(void)
{
	addr control, x, y;

	lisp_push_control(&control);
	x = Lisp_hold();
	lisp_string8_(x, "(+ 1 2 3)");
	lisp_eval_string_control_(x);
	getresult_control(Execute_Thread, &y);
	test(fixnump(y), "lisp_eval_string_control.1");
	test(RefFixnum(y) == 6, "lisp_eval_string_control.2");

	lisp_string8_(x, "(* 1 2 3 4)");
	lisp_hold_value(x, &y);
	lisp_eval_string_control_(y);
	getresult_control(Execute_Thread, &y);
	test(fixnump(y), "lisp_eval_string_control.3");
	test(RefFixnum(y) == 24, "lisp_eval_string_control.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_funcall_control(void)
{
	addr control, x, y, z, a, b, c;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	lisp0_intern8_(&c, NULL, "*");
	lisp_funcall_control_(c, a, b, NULL);
	getresult_control(Execute_Thread, &a);
	test(fixnump(a), "lisp_funcall_control.1");
	test(RefFixnum(a) == 200, "lisp_funcall_control.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_intern8_(z, NULL, "+");
	lisp_funcall_control_(z, x, y, NULL);
	getresult_control(Execute_Thread, &a);
	test(fixnump(a), "lisp_funcall_control.3");
	test(RefFixnum(a) == 50, "lisp_funcall_control.4");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_apply_control(void)
{
	addr control, x, y, z, a, b, c;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();
	z = Lisp_hold();

	fixnum_heap(&a, 10);
	fixnum_heap(&b, 20);
	conscar_heap(&b, b);
	lisp0_intern8_(&c, NULL, "*");
	lisp_apply_control_(c, a, b, NULL);
	getresult_control(Execute_Thread, &a);
	test(fixnump(a), "lisp_apply_control.1");
	test(RefFixnum(a) == 200, "lisp_apply_control.2");

	lisp_fixnum(x, 20);
	lisp_fixnum(y, 30);
	lisp_cons(y, y, NULL);
	lisp_intern8_(z, NULL, "+");
	lisp_apply_control_(z, x, y, NULL);
	getresult_control(Execute_Thread, &a);
	test(fixnump(a), "lisp_apply_control.3");
	test(RefFixnum(a) == 50, "lisp_apply_control.4");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  values
 */
static int test_lisp_result_control(void)
{
	Execute ptr;
	addr control, x, y, z;

	lisp_push_control(&control);
	x = Lisp_hold();

	ptr = Execute_Thread;
	fixnum_heap(&y, 10);

	setresult_control(ptr, Nil);
	lisp0_result_control(&z);
	test(z == Nil, "lisp_result_control.1");
	lisp_result_control(x);
	lisp_hold_value(x, &y);
	test(y == Nil, "lisp_result_control.2");

	setresult_control(ptr, y);
	lisp0_result_control(&z);
	test(z == y, "lisp_result_control.3");
	lisp_result_control(x);
	lisp_hold_value(x, &y);
	test(y == y, "lisp_result_control.4");

	setvalues_nil_control(ptr);
	lisp0_result_control(&z);
	test(z == Nil, "lisp_result_control.5");
	lisp_result_control(x);
	lisp_hold_value(x, &y);
	test(y == Nil, "lisp_result_control.6");

	RETURN;
}

static int test_lisp_result2_control(void)
{
	Execute ptr;
	addr control, x, y, a, b;

	lisp_push_control(&control);
	x = Lisp_hold();
	y = Lisp_hold();

	ptr = Execute_Thread;

	setvalues_nil_control(ptr);
	lisp0_result2_control(&a, &b);
	test(a == Nil, "lisp_result2_control.1");
	test(b == Nil, "lisp_result2_control.2");
	lisp_result2_control(x, y);
	test(lisp_nil_p(x), "lisp_result2_control.3");
	test(lisp_nil_p(y), "lisp_result2_control.4");

	setvalues_control(ptr, fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp0_result2_control(&a, &b);
	test(RefFixnum(a) == 1, "lisp_result2_control.5");
	test(RefFixnum(b) == 2, "lisp_result2_control.6");
	lisp_result2_control(x, y);
	lisp_hold_value(x, &a);
	lisp_hold_value(y, &b);
	test(RefFixnum(a) == 1, "lisp_result2_control.7");
	test(RefFixnum(b) == 2, "lisp_result2_control.8");

	RETURN;
}

static int test_lisp_values_control(void)
{
	Execute ptr;
	addr control, x, a;

	lisp_push_control(&control);
	x = Lisp_hold();

	ptr = Execute_Thread;

	setvalues_nil_control(ptr);
	lisp0_values_control(&a);
	test(a == Nil, "lisp_values_control.1");
	lisp_values_control(x);
	test(lisp_nil_p(x), "lisp_values_control.2");

	setvalues_control(ptr, fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp0_values_control(&a);
	test(consp(a), "lisp_values_control.3");
	test(length_list_unsafe(a) == 4, "lisp_values_control.4");

	lisp_values_control(x);
	test(lisp_cons_p(x), "lisp_values_control.5");
	lisp_hold_value(x, &a);
	GetCar(a, &a);
	test(RefFixnum(a) == 1, "lisp_values_control.6");

	RETURN;
}

static int test_lisp_nth_value_control(void)
{
	Execute ptr;
	addr control, x, a;

	lisp_push_control(&control);
	x = Lisp_hold();

	ptr = Execute_Thread;

	setvalues_control(ptr, fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp0_nth_value_control(&a, 1);
	test(fixnump(a), "lisp_nth_value_control.1");
	test(RefFixnum(a) == 2, "lisp_nth_value_control.2");

	lisp0_nth_value_control(&a, 6);
	test(a == Nil, "lisp_nth_value_control.3");

	lisp_nth_value_control(x, 3);
	lisp_hold_value(x, &a);
	test(RefFixnum(a) == 4, "lisp_nth_value_control.4");

	lisp_nth_value_control(x, 4);
	test(lisp_nil_p(x), "lisp_nth_value_control.5");

	RETURN;
}

static int test_lisp_set_result_control(void)
{
	addr control, x, a, b;

	lisp_push_control(&control);
	x = Lisp_hold();
	setvalues_control(Execute_Thread,
			fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp_set_result_control(fixnumh(10));
	lisp0_result2_control(&a, &b);
	test(fixnump(a), "lisp_set_result_control.1");
	test(RefFixnum(a) == 10, "lisp_set_result_control.2");
	test(b == Nil, "lisp_set_result_control.3");

	lisp_fixnum(x, 20);
	lisp_set_result_control(x);
	lisp0_result2_control(&a, &b);
	test(fixnump(a), "lisp_set_result_control.4");
	test(RefFixnum(a) == 20, "lisp_set_result_control.5");
	test(b == Nil, "lisp_set_result_control.6");

	lisp_pop_control_(control);

	RETURN;
}

static int test_lisp_set_values_control(void)
{
	addr a, b;

	setvalues_nil_control(Execute_Thread);
	lisp_set_values_control(fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp0_result2_control(&a, &b);
	test(fixnump(a), "lisp_set_values_control.1");
	test(RefFixnum(a) == 1, "lisp_set_values_control.2");
	test(fixnump(b), "lisp_set_values_control.3");
	test(RefFixnum(b) == 2, "lisp_set_values_control.4");
	lisp0_nth_value_control(&a, 4);
	test(a == Nil, "lisp_set_values_control.5");

	RETURN;
}

static int test_lisp_set_values_nil_control(void)
{
	addr a;

	lisp_set_values_control(fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp_set_values_nil_control();
	lisp0_values_control(&a);
	test(a == Nil, "lisp_set_values_nil_control.1");

	RETURN;
}

static int test_lisp_set_values_list_control(void)
{
	addr control, x, a;

	lisp_push_control(&control);
	x = Lisp_hold();

	list_heap(&a, fixnumh(1), fixnumh(2), fixnumh(3), fixnumh(4), NULL);
	lisp_set_values_list_control(a);

	lisp0_values_control(&a);
	test(consp(a), "lisp_set_values_list_control.1");
	test(length_list_unsafe(a) == 4, "lisp_set_values_list_control.2");
	GetCar(a, &a);
	test(RefFixnum(a) == 1, "lisp_set_values_list_control.3");

	lisp_list(x, fixnumh(5), fixnumh(6), fixnumh(7), NULL);
	lisp_set_values_list_control(x);

	lisp0_values_control(&a);
	test(consp(a), "lisp_set_values_list_control.4");
	test(length_list_unsafe(a) == 3, "lisp_set_values_list_control.5");
	GetCar(a, &a);
	test(RefFixnum(a) == 5, "lisp_set_values_list_control.6");

	lisp_pop_control_(control);

	RETURN;
}


/*
 *  others
 */
static int test_lisp_escape_control(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	normal_throw_control(ptr);
	test(! lisp_escape_control(), "lisp_escape_control.1");

	ptr->throw_value = throw_handler_case;
	test(lisp_escape_control(), "lisp_escape_control.2");
	normal_throw_control(ptr);

	RETURN;
}

static int test_lisp_escape_reset_control(void)
{
	Execute ptr;

	ptr = Execute_Thread;
	normal_throw_control(ptr);
	ptr->throw_value = throw_handler_case;
	lisp_escape_reset_control();
	test(! lisp_escape_control(), "lisp_escape_reset_control.1");
	normal_throw_control(ptr);

	RETURN;
}

static int test_lisp_escape_type_control(void)
{
	enum lisp_escape v;
	Execute ptr;

	ptr = Execute_Thread;
	lisp_escape_reset_control();
	v = lisp_escape_type_control();
	test(v == lisp_escape_normal, "lisp_escape_type_control.1");

	ptr->throw_value = throw_tagbody;
	v = lisp_escape_type_control();
	test(v == lisp_escape_tagbody, "lisp_escape_type_control.2");

	ptr->throw_value = throw_block;
	v = lisp_escape_type_control();
	test(v == lisp_escape_block, "lisp_escape_type_control.3");

	ptr->throw_value = throw_catch;
	v = lisp_escape_type_control();
	test(v == lisp_escape_catch, "lisp_escape_type_control.4");

	ptr->throw_value = throw_handler_case;
	v = lisp_escape_type_control();
	test(v == lisp_escape_handler_case, "lisp_escape_type_control.5");

	ptr->throw_value = throw_restart_case;
	v = lisp_escape_type_control();
	test(v == lisp_escape_restart_case, "lisp_escape_type_control.6");

	normal_throw_control(ptr);

	RETURN;
}


/*
 *  Main
 */
static int testcase_extern_execute(void)
{
	/* eval */
	TestBreak(test_lisp_eval);
	TestBreak(test_lisp_eval8);
	TestBreak(test_lisp_eval16);
	TestBreak(test_lisp_eval32);
	/* funcall */
	TestBreak(test_lisp_funcall);
	TestBreak(test_lisp_funcall8);
	TestBreak(test_lisp_funcall16);
	TestBreak(test_lisp_funcall32);
	/* apply */
	TestBreak(test_lisp_apply);
	TestBreak(test_lisp_apply8);
	TestBreak(test_lisp_apply16);
	TestBreak(test_lisp_apply32);
	/* lowlevel */
	TestBreak(test_lisp_eval_control);
	TestBreak(test_lisp_eval_string_control);
	TestBreak(test_lisp_funcall_control);
	TestBreak(test_lisp_apply_control);
	/* values */
	TestBreak(test_lisp_result_control);
	TestBreak(test_lisp_result2_control);
	TestBreak(test_lisp_values_control);
	TestBreak(test_lisp_nth_value_control);
	TestBreak(test_lisp_set_result_control);
	TestBreak(test_lisp_set_values_control);
	TestBreak(test_lisp_set_values_nil_control);
	TestBreak(test_lisp_set_values_list_control);
	/* others */
	TestBreak(test_lisp_escape_control);
	TestBreak(test_lisp_escape_reset_control);
	TestBreak(test_lisp_escape_type_control);

	return 0;
}

static void testinit_extern_execute(Execute ptr)
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

int test_extern_execute(void)
{
	DegradeTitle;
	return DegradeCode(extern_execute);
}

