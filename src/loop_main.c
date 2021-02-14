#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "execute.h"
#include "loop_main.h"
#include "loop_symbol.h"
#include "loop_special.h"
#include "symbol.h"

/*
 *  initially
 */
static int loop_main_initially_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_init_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  finally
 */
static int loop_main_finally_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_final_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  do
 */
static int loop_main_do_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(push_form_loop_(ptr, pos));
	}

	return 0;
}


/*
 *  return
 */
static int loop_main_return_(Execute ptr, addr list)
{
	addr return_from, named, expr, x;

	GetConst(COMMON_RETURN_FROM, &return_from);
	Return(getnamed_loop_(ptr, &named));
	Return(list_bind_(list, &expr, NULL));

	/* `(return-from ,named ,expr) */
	list_heap(&x, return_from, named, expr, NULL);
	return push_form_loop_(ptr, x);
}


/*
 *  if, unless
 */
static int loop_main_if_unless_then_(Execute ptr, addr car, addr form, addr expr1)
{
	/* `(setq it-loop ,form)
	 *  (if/unless it-loop
	 *    (go #:end-if))
	 *  ,@expr1
	 *  #:end-if
	 */
	addr setq, go, it_loop, end_if, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	make_symbolchar(&end_if, "END-IF");

	list_heap(&x, setq, it_loop, form, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, go, end_if, NULL);
	list_heap(&x, car, it_loop, x, NULL);
	Return(push_form_loop_(ptr, x));
	Return(loop_main_(ptr, expr1));
	return push_form_loop_(ptr, end_if);
}

static int loop_main_if_unless_else_(Execute ptr,
		addr car, addr form, addr expr1, addr expr2)
{
	/* `(setq it-loop ,form)
	 *  (car it-loop (go #:else-if))
	 *    ,@expr1
	 *  (go #:end-if)
	 *  #:else-if
	 *  ,@expr2
	 *  #:end-if
	 */
	addr setq, go, it_loop, else_if, end_if, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_IT_LOOP, &it_loop);
	make_symbolchar(&else_if, "ELSE-IF");
	make_symbolchar(&end_if, "END-IF");

	list_heap(&x, setq, it_loop, form, NULL);
	Return(push_form_loop_(ptr, x));
	list_heap(&x, go, else_if, NULL);
	list_heap(&x, car, it_loop, x, NULL);
	Return(push_form_loop_(ptr, x));
	Return(loop_main_(ptr, expr1));
	list_heap(&x, go, end_if, NULL);
	Return(push_form_loop_(ptr, x));
	Return(push_form_loop_(ptr, else_if));
	Return(loop_main_(ptr, expr2));
	return push_form_loop_(ptr, end_if);
}

static int loop_main_if_unless_(Execute ptr, addr list, constindex index)
{
	addr car, form, expr1, expr2;

	GetConstant(index, &car);
	Return(list_bind_(list, &form, &expr1, &expr2, NULL));
	if (expr2 == Unbound)
		return loop_main_if_unless_then_(ptr, car, form, expr1);
	else
		return loop_main_if_unless_else_(ptr, car, form, expr1, expr2);
}

static int loop_main_if_(Execute ptr, addr list)
{
	return loop_main_if_unless_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_unless_(Execute ptr, addr list)
{
	return loop_main_if_unless_(ptr, list, CONSTANT_COMMON_IF);
}


/*
 *  collect
 */
static int loop_main_collect_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse;
	addr push, value_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_PUSH, &push);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(push ,form value-loop) */
	list_heap(&x, push, form, value_loop, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_collect_into_(Execute ptr, addr form, addr into)
{
	/* `(setq ,into (append ,into (list ,form))) */
	addr setq, append, list, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_APPEND, &append);
	GetConst(COMMON_LIST, &list);
	Return(push_vars_loop_(ptr, into));
	list_heap(&list, list, form, NULL);
	list_heap(&append, append, into, list, NULL);
	list_heap(&x, setq, into, append, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_collect_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_collect_nointo_(ptr, form);
	else
		return loop_main_collect_into_(ptr, form, into);
}


/*
 *  append
 */
static int loop_main_append_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse;
	addr do_symbol, car, cdr, null, push;
	addr value_loop, g, x, y, z;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_DO, &do_symbol);
	GetConst(COMMON_CAR, &car);
	GetConst(COMMON_CDR, &cdr);
	GetConst(COMMON_NULL, &null);
	GetConst(COMMON_PUSH, &push);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(do ((x ,form (cdr x)))
	 *    ((null x))
	 *    (push (car x) value-loop))
	 */
	make_symbolchar(&g, "X");
	list_heap(&x, cdr, g, NULL);
	list_heap(&x, g, form, x, NULL);
	list_heap(&x, x, NULL);
	list_heap(&y, null, g, NULL);
	list_heap(&y, y, NULL);
	list_heap(&z, car, g, NULL);
	list_heap(&z, push, z, value_loop, NULL);
	list_heap(&x, do_symbol, x, y, z, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_append_into_(Execute ptr, addr form, addr into)
{
	addr setq, append, x;

	/* `(setq ,into (append ,into ,form)) */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_APPEND, &append);

	Return(push_vars_loop_(ptr, into));
	list_heap(&x, append, into, form, NULL);
	list_heap(&x, setq, into, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_append_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_append_nointo_(ptr, form);
	else
		return loop_main_append_into_(ptr, form, into);
}


/*
 *  nconc
 */
static int loop_main_nconc_nointo_(Execute ptr, addr form)
{
	addr setq, function_loop, function_symbol, nreverse, nreconc, value_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_FUNCTION, &function_symbol);
	GetConst(COMMON_NREVERSE, &nreverse);
	GetConst(COMMON_NRECONC, &nreconc);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq function-loop (function nreverse)) */
	list_heap(&x, function_symbol, nreverse, NULL);
	list_heap(&x, setq, function_loop, x, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(setq value-loop (nreconc ,form value-loop)) */
	list_heap(&x, nreconc, form, value_loop, NULL);
	list_heap(&x, setq, value_loop, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_nconc_into_(Execute ptr, addr form, addr into)
{
	addr setq, nconc, x;

	/* `(setq ,into (nconc ,into ,form)) */
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_NCONC, &nconc);

	Return(push_vars_loop_(ptr, into));
	list_heap(&x, nconc, into, form, NULL);
	list_heap(&x, setq, into, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_nconc_(Execute ptr, addr list)
{
	addr form, into;

	Return(list_bind_(list, &form, &into, NULL));
	if (into == Unbound)
		return loop_main_nconc_nointo_(ptr, form);
	else
		return loop_main_nconc_into_(ptr, form, into);
}


/*
 *  count
 */
static int loop_main_count_nointo_(Execute ptr, addr form)
{
	addr setq, value_loop, function_loop, if_symbol, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);

	/* `(setq value_loop 0 function-loop nil) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, value_loop, x, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(if ,form (incf value_loop)) */
	list_heap(&incf, incf, value_loop, NULL);
	list_heap(&x, if_symbol, form, incf, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_count_into_(Execute ptr, addr form, addr into)
{
	addr setq, if_symbol, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_INCF, &incf);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into 0) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, into, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(if ,form (incf ,into)) */
	list_heap(&incf, incf, into, NULL);
	list_heap(&x, if_symbol, form, incf, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_count_(Execute ptr, addr list)
{
	addr form, into, type;

	Return(list_bind_(list, &form, &into, &type, NULL));
	if (into == Unbound)
		return loop_main_count_nointo_(ptr, form);
	else
		return loop_main_count_into_(ptr, form, into);
}


/*
 *  sum
 */
static int loop_main_sum_nointo_(Execute ptr, addr form)
{
	addr setq, value_loop, function_loop, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);
	GetConst(COMMON_INCF, &incf);

	/* `(setq value-loop 0 function-loop nil) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, value_loop, x, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(incf value_loop ,form) */
	list_heap(&x, incf, value_loop, form, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_sum_into_(Execute ptr, addr form, addr into)
{
	addr setq, incf, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_INCF, &incf);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into 0) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, into, x, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(incf ,into ,form) */
	list_heap(&x, incf, into, form, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_sum_(Execute ptr, addr list)
{
	addr form, into, type;

	Return(list_bind_(list, &form, &into, &type, NULL));
	if (into == Unbound)
		return loop_main_sum_nointo_(ptr, form);
	else
		return loop_main_sum_into_(ptr, form, into);
}


/*
 *  maximize, minimize
 */
static int loop_main_maxmin_form_(Execute ptr, addr form, addr into, addr maxmin)
{
	/* `(let ((g ,form))
	 *    (if value-loop
	 *      (setq value-loop (maxmin value-loop ,g))
	 *      (setq value-loop ,g)))
	 */
	addr let, if_symbol, setq;
	addr g, x, y, z;

	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_SETQ, &setq);

	make_symbolchar(&g, "G");
	list_heap(&form, g, form, NULL);
	list_heap(&form, form, NULL);
	list_heap(&maxmin, maxmin, into, g, NULL);
	list_heap(&y, setq, into, maxmin, NULL);
	list_heap(&z, setq, into, g, NULL);
	list_heap(&if_symbol, if_symbol, into, y, z, NULL);
	list_heap(&x, let, form, if_symbol, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_maxmin_nointo_(Execute ptr, addr form, addr maxmin)
{
	addr setq, value_loop, function_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);

	/* `(setq value-loop nil function-loop nil) */
	list_heap(&x, setq, value_loop, Nil, function_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ...) */
	return loop_main_maxmin_form_(ptr, form, value_loop, maxmin);
}

static int loop_main_maxmin_into_(Execute ptr, addr form, addr into, addr maxmin)
{
	addr setq, function_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_FUNCTION_LOOP, &function_loop);

	Return(push_vars_loop_(ptr, into));
	/* `(setq ,into nil) */
	list_heap(&x, setq, into, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ...) */
	return loop_main_maxmin_form_(ptr, form, into, maxmin);
}

static int loop_main_maxmin_(Execute ptr, addr list, constindex index)
{
	addr form, into, type, maxmin;

	Return(list_bind_(list, &form, &into, &type, NULL));
	GetConstant(index, &maxmin);
	if (into == Unbound)
		return loop_main_maxmin_nointo_(ptr, form, maxmin);
	else
		return loop_main_maxmin_into_(ptr, form, into, maxmin);
}

static int loop_main_maximize_(Execute ptr, addr list)
{
	return loop_main_maxmin_(ptr, list, CONSTANT_COMMON_MAX);
}

static int loop_main_minimize_(Execute ptr, addr list)
{
	return loop_main_maxmin_(ptr, list, CONSTANT_COMMON_MIN);
}


/*
 *  while, until
 */
static int loop_main_while_until_(Execute ptr, addr list, constindex index)
{
	addr expr, x, go, end_loop;

	Return(list_bind_(list, &expr, NULL));
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);
	GetConstant(index, &x);

	/* `(unless ,expr (go end-loop)) */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&x, x, expr, go, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_while_(Execute ptr, addr list)
{
	return loop_main_while_until_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_until_(Execute ptr, addr list)
{
	return loop_main_while_until_(ptr, list, CONSTANT_COMMON_WHEN);
}


/*
 *  always, never
 */
static int loop_main_terminate_(Execute ptr, addr list, constindex index)
{
	addr expr, setq, value_loop, check, return_from, named, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);
	GetConst(COMMON_RETURN_FROM, &return_from);
	GetConstant(index, &check);

	Return(list_bind_(list, &expr, NULL));
	/* `(setq value-loop t) */
	list_heap(&x, setq, value_loop, T, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(unless ,expr (return-from named nil)) */
	Return(getnamed_loop_(ptr, &named));
	list_heap(&x, return_from, named, Nil, NULL);
	list_heap(&x, check, expr, x, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_always_(Execute ptr, addr list)
{
	return loop_main_terminate_(ptr, list, CONSTANT_COMMON_UNLESS);
}

static int loop_main_never_(Execute ptr, addr list)
{
	return loop_main_terminate_(ptr, list, CONSTANT_COMMON_WHEN);
}


/*
 *  thereis
 */
static int loop_main_thereis_(Execute ptr, addr list)
{
	addr expr, setq, value_loop, let, if_symbol, return_from, named, x, g;

	Return(list_bind_(list, &expr, NULL));
	GetConst(COMMON_SETQ, &setq);
	GetConst(SYSTEM_VALUE_LOOP, &value_loop);

	/* `(setq value-loop nil) */
	list_heap(&x, setq, value_loop, Nil, NULL);
	Return(push_init_loop_(ptr, x));
	/* `(let ((,g ,expr))
	 *    (if ,g (return-from named ,g)))
	 */
	GetConst(COMMON_LET, &let);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_RETURN_FROM, &return_from);
	make_symbolchar(&g, "G");
	Return(getnamed_loop_(ptr, &named));
	list_heap(&x, return_from, named, g, NULL);
	list_heap(&if_symbol, if_symbol, g, x, NULL);
	list_heap(&g, g, expr, NULL);
	list_heap(&g, g, NULL);
	list_heap(&x, let, g, if_symbol, NULL);
	return push_form_loop_(ptr, x);
}


/*
 *  repeat
 */
static int loop_main_repeat_form_(Execute ptr, addr expr, addr a, addr b)
{
	addr setq, if_symbol, less, incf, go, end_loop, x;

	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_IF, &if_symbol);
	GetConst(COMMON_NUMBER_LESS, &less);
	GetConst(COMMON_INCF, &incf);
	GetConst(COMMON_GO, &go);
	GetConst(SYSTEM_END_LOOP, &end_loop);

	/* `(setq ,a 0 ,b ,expr) */
	fixnum_heap(&x, 0);
	list_heap(&x, setq, a, x, b, expr, NULL);
	Return(push_init_loop_(ptr, x));

	/* `(if (< ,a ,b)
	 *    (incf ,a)
	 *    (go end-loop))
	 */
	list_heap(&go, go, end_loop, NULL);
	list_heap(&incf, incf, a, NULL);
	list_heap(&less, less, a, b, NULL);
	list_heap(&x, if_symbol, less, incf, go, NULL);
	return push_form_loop_(ptr, x);
}

static int loop_main_repeat_(Execute ptr, addr list)
{
	addr expr, a, b;

	Return(list_bind_(list, &expr, &a, &b, NULL));
	Return(push_vars_loop_(ptr, a));
	Return(push_vars_loop_(ptr, b));
	return loop_main_repeat_form_(ptr, expr, a, b);
}


/*
 *  loop_main_
 */
static int loop_main_type_(Execute ptr, addr list)
{
	addr pos;

	GetCons(list, &pos, &list);

	/* initially, finally */
	if (loop_symbol_initially_p(pos))
		return loop_main_initially_(ptr, list);
	if (loop_symbol_finally_p(pos))
		return loop_main_finally_(ptr, list);

	/* unconditional */
	if (loop_symbol_do_p(pos))
		return loop_main_do_(ptr, list);
	if (loop_symbol_return_p(pos))
		return loop_main_return_(ptr, list);

	/* conditional */
	if (loop_symbol_if_p(pos))
		return loop_main_if_(ptr, list);
	if (loop_symbol_unless_p(pos))
		return loop_main_unless_(ptr, list);

	/* list-accumulation */
	if (loop_symbol_collect_p(pos))
		return loop_main_collect_(ptr, list);
	if (loop_symbol_append_p(pos))
		return loop_main_append_(ptr, list);
	if (loop_symbol_nconc_p(pos))
		return loop_main_nconc_(ptr, list);

	/* numeric-accumulation */
	if (loop_symbol_count_p(pos))
		return loop_main_count_(ptr, list);
	if (loop_symbol_sum_p(pos))
		return loop_main_sum_(ptr, list);
	if (loop_symbol_maximize_p(pos))
		return loop_main_maximize_(ptr, list);
	if (loop_symbol_minimize_p(pos))
		return loop_main_minimize_(ptr, list);

	/* termination */
	if (loop_symbol_while_p(pos))
		return loop_main_while_(ptr, list);
	if (loop_symbol_until_p(pos))
		return loop_main_until_(ptr, list);
	if (loop_symbol_always_p(pos))
		return loop_main_always_(ptr, list);
	if (loop_symbol_never_p(pos))
		return loop_main_never_(ptr, list);
	if (loop_symbol_thereis_p(pos))
		return loop_main_thereis_(ptr, list);
	if (loop_symbol_repeat_p(pos))
		return loop_main_repeat_(ptr, list);

	/* error */
	return fmte_("Invalid loop operator ~S.", pos, NULL);
}

int loop_main_(Execute ptr, addr list)
{
	addr pos;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		Return(loop_main_type_(ptr, pos));
	}

	return 0;
}

