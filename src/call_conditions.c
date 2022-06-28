#include "call_conditions.h"
#include "clos.h"
#include "closget_class.h"
#include "condition.h"
#include "condition_debugger.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "execute_object.h"
#include "format.h"
#include "function.h"
#include "hold.h"
#include "pointer.h"
#include "print_write.h"
#include "restart.h"
#include "setf.h"
#include "strtype.h"
#include "strvect.h"
#include "symbol.h"
#include "type_parse.h"
#include "type_object.h"

/*
 *  assert
 */
static int assert_retry_common_(addr test, addr *ret)
{
	/* (tagbody
	 *   loop
	 *   (restart-bind
	 *     ((continue (lambda () (go loop))
	 *        :report-function
	 *        (lambda (s)
	 *          (princ "Retry assersion." s))))
	 *     (unless test
	 *       (error "Failed assersion ~A." 'test))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr princ, unless, quote, error;
	addr loop, s, str1, str2, a;

	/* variable */
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&s, "STREAM");
	strvect_char_heap(&str1, "Retry assersion.");
	strvect_char_heap(&str2, "Failed assersion ~A.");
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_CONTINUE, &cont);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_QUOTE, &quote);
	/* expand */
	list_heap(&a, quote, test, NULL);
	list_heap(&error, error, str2, a, NULL);
	list_heap(&unless, unless, test, error, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&princ, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&lambda, lambda, Nil, go, NULL);
	list_heap(&cont, cont, lambda, report, princ, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(ret, tagbody, loop, restart, NULL);

	return 0;
}

static int assert_prompt_common_(Execute ptr, addr env, addr *ret, addr place)
{
	/* (multiple-value-bind (a b g w r) (get-setf-expansion PLACE)
	 *   (declare (ignore r))
	 *   `(when (y-or-n-p "Do you want to set a new value in ~A? " ',place)
	 *      (let* ((,a1 ,b1) (,a2 ,b2) ... ,g)
	 *        (declare (ignorable ,a1 ,a2 ...))
	 *        (setq ,g (eval (prompt-for t "Input ~A> " ',place)))
	 *        ,w)))
	 */
	addr a, b, g, w, r;
	addr when, yornp, leta, declare, ignorable, setq, eval, prompt, quote;
	addr str1, str2, root, x, y;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	Return_getcar(g, &g);
	strvect_char_heap(&str1, "Do you want to setq a new value in ~A?");
	strvect_char_heap(&str2, "Input ~A> ");
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_Y_OR_N_P, &yornp);
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_QUOTE, &quote);
	/* expand */
	list_heap(&place, quote, place, NULL);
	list_heap(&prompt, prompt, T, str2, place, NULL);
	list_heap(&prompt, eval, prompt, NULL);
	list_heap(&setq, setq, g, prompt, NULL);
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	/* let* */
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(&leta, leta, root, declare, setq, w, NULL);
	/* when */
	list_heap(&yornp, yornp, str1, place, NULL);
	list_heap(ret, when, yornp, leta, NULL);

	return 0;
}

static int assert_list_common_(Execute ptr, addr env,
		addr test, addr places, addr output, addr *ret)
{
	/* (tagbody
	 *   loop
	 *   (restart-bind
	 *     ((continue (lambda ()
	 *                  (assert-prompt place1)
	 *                  (assert-prompt place2)
	 *                  ...
	 *                  (go loop))
	 *        :report-function
	 *        (lambda (s)
	 *          (format s "Retry assersion with new value ~{~A~^,~}." places))))
	 *     (unless test
	 *       (error  "Failed assersion ~A." 'test))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr format, unless, error, quote;
	addr loop, s, str, a, b, c;
	LocalHold hold;

	/* places */
	hold = LocalHold_array(ptr, 1);
	for (a = Nil; places != Nil; ) {
		Return_getcons(places, &b, &places);
		Return(assert_prompt_common_(ptr, env, &b, b));
		cons_heap(&a, b, a);
		localhold_set(hold, 0, a);
	}
	localhold_end(hold);

	/* variable */
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&s, "STREAM");
	strvect_char_heap(&str, "Retry assersion with new value ~{~A~^,~}.");
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_CONTINUE, &cont);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(COMMON_FORMAT, &format);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_QUOTE, &quote);
	/* format */
	if (output == Nil) {
		strvect_char_heap(&output, "Failed assersion ~A");
		list_heap(&c, quote, test, NULL);
		list_heap(&output, output, c, NULL);
	}
	/* expand */
	cons_heap(&error, error, output);
	list_heap(&unless, unless, test, error, NULL);
	list_heap(&quote, quote, places, NULL);
	list_heap(&format, format, s, str, quote, NULL);
	list_heap(&s, s, NULL);
	list_heap(&format, lambda, s, format, NULL);
	/* prompt */
	list_heap(&go, go, loop, NULL);
	cons_heap(&a, go, a);
	nreverse(&a, a);
	lista_heap(&lambda, lambda, Nil, a, NULL);
	list_heap(&cont, cont, lambda, report, format, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(ret, tagbody, loop, restart, NULL);

	return 0;
}

int assert_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, test, list;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &test, &args))
		goto error;
	if (args == Nil)
		return assert_retry_common_(test, ret);
	if (! consp_getcons(args, &list, &args))
		goto error;
	if (list == Nil && args == Nil)
		return assert_retry_common_(test, ret);
	else
		return assert_list_common_(ptr, env, test, list, args, ret);

error:
	return fmte_("ASSERT arguments ~S must be "
			"(test &optional places format args) form.", form, NULL);
}


/*
 *  error
 */
static int error_datum_common_(Execute ptr, addr datum, addr rest, addr *ret)
{
	int check;
	addr make;

	/* symbol -> (make-instance symbol ...) */
	if (symbolp(datum)) {
		Return(clos_find_class_(datum, &datum));
		Return(conditionp_(datum, &check));
		if (! check) {
			*ret = Nil;
			return fmte_("The class ~S is not a condition subclass.", datum, NULL);
		}
		GetConst(COMMON_MAKE_INSTANCE, &make);
		return applya1_control_(ptr, ret, make, datum, rest, NULL);
	}

	/* condition -> (error condition) */
	Return(condition_instance_p_(datum, &check));
	if (! check) {
		*ret = Nil;
		return fmte_("Invalid datum argument ~S.", datum, NULL);
	}
	if (rest != Nil) {
		*ret = Nil;
		return fmte_("The datum argument ~S must be a nil "
				"if first argument is condition type.", datum, NULL);
	}

	return Result(ret, datum);
}

int error_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-error */
		return call_simple_error_(ptr, datum, rest);
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
		return error_function_(ptr, datum);
	}
}


/*
 *  cerror
 */
static int cerror_restart_common_(Execute ptr, addr *ret, addr format, addr args)
{
	addr inst, pos;

	Return(format_string_lisp_(ptr, format, args, &format));
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	setreport_restart(inst, format);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);

	return Result(ret, inst);
}

int cerror_common_(Execute ptr, addr restart, addr datum, addr rest)
{
	addr control;

	push_control(ptr, &control);
	Return(cerror_restart_common_(ptr, &restart, restart, rest));
	pushrestart_control(ptr, restart);

	/* (error ...) */
	(void)error_common_(ptr, datum, rest);
	if (ptr->throw_value == throw_normal)
		goto escape;
	if (ptr->throw_control != control)
		goto escape;

	/* continue */
	if (ptr->throw_handler == restart) {
		normal_throw_control(ptr);
		goto escape;
	}

escape:
	return pop_control_(ptr, control);
}


/*
 *  check-type
 */
static int check_type_expand_common_(Execute ptr, addr env, addr *ret,
		addr place, addr type, addr string)
{
	/* (let* ((a1 b1) (a2 b2) ... (value r) g)
	 *   (declare (ignorable a1 a2 ...))
	 *   (tagbody
	 *     loop
	 *     (restart-bind
	 *       ((store-value
	 *          (lambda (v) (setq g v value v) w (go loop))
	 *          :report-function
	 *            (lambda (s)
	 *              (princ "Retry check-type with new value xx." s))
	 *          :interactive-function
	 *            (lambda ()
	 *              (list (eval (prompt-for t "Input xx> "))))))
	 *       (unless (typep value 'type)
	 *         (error
	 *           (make-condition 'simple-type-error
	 *             :datum value
	 *             :expected-type 'type
	 *             :format-control "The value of xx, ~A, is not ~(~A~)."
	 *             :format-arguments (list value string)))))))
	 */
	addr a, b, g, r, w, v, s, str1, str2, str3;
	addr leta, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, unless;
	addr typep, quote, invoke, make, simple, datum, expect, control, arguments;
	addr x, y, root;
	LocalHold hold;

	Return(get_setf_expansion_(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);
	Return_getcar(g, &g);

	Return(format_string_(ptr, &str1,
				"Retry check-type with new value ~A.", place, NULL));
	localhold_push(hold, str1);

	Return(format_string_(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);

	Return(format_string_(ptr, &str3,
				"The value of ~A, ~~A, is not ~~A.", place, NULL));
	localhold_push(hold, str3);

	if (string == Nil) {
		Return(parse_type_(ptr, &string, type, env));
		localhold_push(hold, string);
		Return(type_object_(&string, string));
		localhold_push(hold, string);
		Return(princ_string_heap_(ptr, &string, string));
	}
	localhold_end(hold);

	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_DECLARE, &declare);
	GetConst(COMMON_IGNORABLE, &ignorable);
	GetConst(COMMON_TAGBODY, &tagbody);
	GetConst(COMMON_RESTART_BIND, &restart);
	GetConst(COMMON_STORE_VALUE, &store);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_SETQ, &setq);
	GetConst(COMMON_GO, &go);
	GetConst(KEYWORD_REPORT_FUNCTION, &report);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &inter);
	GetConst(COMMON_PRINC, &princ);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_EVAL, &eval);
	GetConst(SYSTEM_PROMPT_FOR, &prompt);
	GetConst(COMMON_UNLESS, &unless);
	GetConst(COMMON_TYPEP, &typep);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_ERROR, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_SIMPLE_TYPE_ERROR, &simple);
	GetConst(KEYWORD_DATUM, &datum);
	GetConst(KEYWORD_EXPECTED_TYPE, &expect);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	/* expand */
	list_heap(&x, list, value, string, NULL);
	list_heap(&type, quote, type, NULL);
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple,
			datum, value, expect, type, control, str3, arguments, x, NULL);
	list_heap(&invoke, invoke, make, NULL);
	list_heap(&typep, typep, value, type, NULL);
	list_heap(&unless, unless, typep, invoke, NULL);
	list_heap(&prompt, prompt, T, str2, NULL);
	list_heap(&eval, eval, prompt, NULL);
	list_heap(&list, list, eval, NULL);
	list_heap(&x, lambda, Nil, list, NULL);
	list_heap(&princ, princ, str1, s, NULL);
	list_heap(&s, s, NULL);
	list_heap(&y, lambda, s, princ, NULL);
	list_heap(&go, go, loop, NULL);
	list_heap(&setq, setq, g, v, value, v, NULL);
	list_heap(&v, v, NULL);
	list_heap(&lambda, lambda, v, setq, w, go, NULL);
	list_heap(&store, store, lambda, report, y, inter, x, NULL);
	list_heap(&store, store, NULL);
	list_heap(&restart, restart, store, unless, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	/* let* */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		Return_getcons(a, &x, &a);
		Return_getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

int check_type_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, type, string;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &place, &args))
		goto error;
	if (! consp_getcons(args, &type, &args))
		goto error;
	if (args == Nil)
		string = Nil;
	else {
		if (! consp_getcons(args, &string, &args))
			goto error;
		if (args != Nil)
			goto error;
	}
	return check_type_expand_common_(ptr, env, ret, place, type, string);

error:
	return fmte_("CHECK-TYPE arguments ~S must be "
			"(place type &optional string) form.", form, NULL);
}


/*
 *  invalid-method-error
 */
int invalid_method_error_common_(Execute ptr, addr method, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method error: ~S~%~?");
	list_heap(&arguments, method, format, args, NULL);
	Return(instance_simple_error_(&pos, control, arguments));
	return error_function_(ptr, pos);
}


/*
 *  method-combination-error
 */
int method_combination_error_common_(Execute ptr, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method-Combination error:~%~?");
	list_heap(&arguments, format, args, NULL);
	Return(instance_simple_error_(&pos, control, arguments));
	return error_function_(ptr, pos);
}


/*
 *  signal
 */
int signal_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-condition */
		Return(instance_simple_condition_(&datum, datum, rest));
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
	}
	return signal_function_(ptr, datum);
}


/*
 *  warn
 */
int warn_common_(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-warning */
		Return(instance_simple_warning_(&datum, datum, rest));
	}
	else {
		Return(error_datum_common_(ptr, datum, rest, &datum));
	}

	return warning_restart_case_(ptr, datum);
}


/*
 *  break
 */
static int break_invoke_common_(Execute ptr, addr format, addr args)
{
	addr symbol, condition;
	LocalHold hold;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	Return(instance_simple_condition_(&condition, format, args));

	hold = LocalHold_local_push(ptr, condition);
	Return(invoke_debugger_(ptr, condition));
	localhold_end(hold);

	return 0;
}

static int break_restart_common_(Execute ptr, addr restart, addr format, addr args)
{
	addr control;

	push_control(ptr, &control);
	(void)restart2_control_(ptr, restart, break_invoke_common_, format, args);
	return pop_control_(ptr, control);
}

static void break_make_common(addr *ret)
{
	static const char *message = "Return from BREAK.";
	addr inst, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	GetConst(FUNCTION_NIL, &pos);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

int break_common_(Execute ptr, addr format, addr args)
{
	/* (defun break (&optional (format-control "Break") &rest args)
	 *   (with-simple-restart (continue "Return from BREAK.")
	 *     (let ((*debugger-hook* nil))
	 *       (invoke-debugger
	 *         (make-condition 'simple-condition
	 *           :format-control format :format-arguments args))))
	 *   nil)
	 */
	addr restart;
	LocalHold hold;

	hold = LocalHold_local(ptr);
	if (format == Unbound) {
		strvect_char_heap(&format, "Break");
		localhold_push(hold, format);
		args = Nil;
	}
	break_make_common(&restart);
	localhold_push(hold, restart);
	Return(break_restart_common_(ptr, restart, format, args));
	localhold_end(hold);

	return 0;
}


/*
 *  handler-bind
 */
static int handler_bind_clauses_common_(addr form, addr *ret)
{
	addr cons, symbol, quote, root, name, lambda, temp;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		/* (name lambda) */
		Return(lista_bind_(cons, &name, &lambda, &temp, NULL));
		if (temp != Nil) {
			return fmte_("handler-bind argument ~S"
					" must be a (name lambda) form.", cons, NULL);
		}
		/* (push 'name root) */
		list_heap(&name, quote, name, NULL);
		cons_heap(&root, name, root);
		/* (push (lambda ...) root) */
		cons_heap(&root, lambda, root);
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

int handler_bind_common_(addr form, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &form, &body))
		return fmte_("Too few handler-bind argument.", NULL);
	if (form == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_HANDLER, &symbol);
		Return(handler_bind_clauses_common_(form, &form));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, form, body, NULL);
	}
	return 0;
}


/*
 *  handler-case
 */
static int handler_case_gensym_common_(Execute ptr, addr form, addr *ret)
{
	addr args, declare, pos, cons, gensym;

	/* (lambda (gensym)
	 *   (declare (ignore gensym))
	 *   form)
	 */
	/* (gensym) */
	Return(make_gensym_(ptr, &gensym));
	conscar_heap(&args, gensym);
	/* (declare (ignore gensym)) */
	GetConst(COMMON_IGNORE, &pos);
	list_heap(&cons, pos, gensym, NULL);
	GetConst(COMMON_DECLARE, &pos);
	list_heap(&declare, pos, cons, NULL);
	/* (lambda ...) */
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, declare, form, NULL);

	return 0;
}

static void handler_case_lambda_common(addr args, addr form, addr *ret)
{
	addr pos;
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, form, NULL);
}

static int handler_case_noerror_common_(addr *no_error, addr args, addr form)
{
	addr lambda;

	if (*no_error) {
		return fmtw_("There are multiple :no-error clauses ~S"
				" in handler-case.", args, NULL);
	}
	else {
		GetConst(COMMON_LAMBDA, &lambda);
		lista_heap(no_error, lambda, args, form, NULL);
	}

	return 0;
}

static int handler_case_clauses_common_(Execute ptr, addr right, addr *ret, addr *rete)
{
	addr no_error, root, cons, name, args, form;
	addr keyword, symbol, quote;

	/* (lisp-system::handler-case name1 lambda1 ...) */
	no_error = NULL;
	GetConst(KEYWORD_NO_ERROR, &keyword);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		Return(lista_bind_(cons, &name, &args, &form, NULL));

		/* (:no-error (...) form) */
		if (name == keyword) {
			Return(handler_case_noerror_common_(&no_error, args, form));
			continue;
		}

		if (args == Nil) {
			/* (name () form) */
			Return(handler_case_gensym_common_(ptr, form, &cons));
		}
		else if (singlep(args)) {
			/* (name (c) form) */
			handler_case_lambda_common(args, form, &cons);
		}
		else {
			*ret = *rete = NULL;
			return fmte_("The argument ~S in handler-case clause "
					"must be a nil or (var) form.", args, NULL);
		}
		/* (push 'name root) */
		list_heap(&name, quote, name, NULL);
		cons_heap(&root, name, root);
		/* (push (lambda ...) root) */
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse(ret, root);
	*rete = no_error;

	return 0;
}

static void handler_case_body(addr no_error, addr expr, addr *ret)
{
	addr mvcall;

	if (no_error == NULL) {
		*ret = expr;
		return;
	}

	/* (multiple-value-call
	 *   (lambda (a b c d) ...)
	 *   expr))
	 */
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &mvcall);
	list_heap(ret, mvcall, no_error, expr, NULL);
}

int handler_case_common_(Execute ptr, addr list, addr env, addr *ret)
{
	addr symbol, expr, no_error;

	Return_getcdr(list, &list);
	if (! consp_getcons(list, &expr, &list))
		return fmte_("Too few handler-case argument.", NULL);
	if (list == Nil)
		return Result(ret, expr);

	GetConst(SYSTEM_HANDLER, &symbol);
	Return(handler_case_clauses_common_(ptr, list, &list, &no_error));
	handler_case_body(no_error, expr, &expr);
	list_heap(ret, symbol, list, expr, NULL);

	return 0;
}


/*
 *  ignore-errors
 */
int ignore_errors_common_(Execute ptr, addr form, addr env, addr *ret)
{
	/* `(handler-case (progn ,@form)
	 *    (error (g) (values nil g)))
	 */
	addr handler, progn, error, values, g;

	Return_getcdr(form, &form);
	GetConst(COMMON_HANDLER_CASE, &handler);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_VALUES, &values);
	Return(make_gensym_(ptr, &g));
	list_heap(&values, values, Nil, g, NULL);
	list_heap(&g, g, NULL);
	list_heap(&error, error, g, values, NULL);
	cons_heap(&progn, progn, form);
	list_heap(ret, handler, progn, error, NULL);

	return 0;
}


/*
 *  make-condition
 */
int make_condition_common_(Execute ptr, addr args, addr *ret)
{
	int check;
	addr condition, expected, call;

	/* type check */
	Return_getcar(args, &condition);
	if (symbolp(condition)) {
		Return(clos_find_class_(condition, &condition));
	}
	Return(conditionp_(condition, &check));
	if (! check) {
		GetConst(COMMON_CONDITION, &expected);
		return call_type_error_va_(ptr, condition, expected,
				"The argument ~S must be a condition.", condition, NULL);
	}

	/* (make-instance ...) */
	GetConst(COMMON_MAKE_INSTANCE, &call);
	Return(getfunction_global_(call, &call));
	return apply1_control_(ptr, ret, call, args);
}


/*
 *  compute-restarts
 */
int compute_restarts_common_(Execute ptr, addr pos, addr *ret)
{
	if (pos == Unbound)
		pos = Nil;
	return compute_restarts_control_(ptr, pos, ret);
}


/*
 *  find-restart
 */
int find_restart_common_(Execute ptr, addr var, addr opt, addr *ret)
{
	int check;

	if (opt == Unbound)
		opt = Nil;
	Return(find_restart_control_(ptr, var, opt, &var, &check));

	return Result(ret, check? var: Nil);
}


/*
 *  restart-bind
 */
static int restart_bind_interactive_common_(addr *args, addr *inter)
{
	if (*args == Nil) {
		return fmte_("The key :interactive-function must have a value.", NULL);
	}
	if (*inter != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, inter, args);
		if (*inter == Nil)
			return fmte_(":interactive-function ~S must be a function.", *inter, NULL);
	}

	return 0;
}

static int restart_bind_report_common_(addr *args, addr *report)
{
	if (*args == Nil) {
		return fmte_("The key :report-function must have a value.", NULL);
	}
	if (*report != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, report, args);
		if (*report == Nil)
			return fmte_(":report-function ~S must be a function.", *report, NULL);
	}

	return 0;
}

static int restart_bind_test_common_(addr *args, addr *test)
{
	if (*args == Nil) {
		return fmte_("The key :test-function must have a value.", NULL);
	}
	if (*test != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, test, args);
		if (*test == Nil)
			return fmte_(":test-function ~S must be a function.", *test, NULL);
	}

	return 0;
}

static void restart_bind_symbol_common(addr pos, addr *ret)
{
	addr funct;

	if (pos != Nil && symbolp(pos)) {
		GetConst(COMMON_FUNCTION, &funct);
		list_heap(ret, funct, pos, NULL);
	}
	else {
		*ret = pos;
	}
}

static int restart_bind_binding_common_(addr args, addr *ret)
{
	addr pos, name, lambda, inter, report, test;
	addr keyinter, keyreport, keytest;
	addr quote, list;

	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &keyinter);
	GetConst(KEYWORD_REPORT_FUNCTION, &keyreport);
	GetConst(KEYWORD_TEST_FUNCTION, &keytest);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);

	/* (name lambda
	 *   :interactive-function x
	 *   :report-function y
	 *   :test-function z)
	 */
	Return(lista_bind_(args, &name, &lambda, &args, NULL));
	inter = report = test = Nil;
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		if (pos == keyinter) {
			Return(restart_bind_interactive_common_(&args, &inter));
		}
		else if (pos == keyreport) {
			Return(restart_bind_report_common_(&args, &report));
		}
		else if (pos == keytest) {
			Return(restart_bind_test_common_(&args, &test));
		}
		else {
			return fmte_("Invalid key parameter ~S.", pos, NULL);
		}
	}
	restart_bind_symbol_common(inter, &inter);
	restart_bind_symbol_common(report, &report);
	restart_bind_symbol_common(test, &test);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, list, name, lambda, inter, report, test, NULL);

	return 0;
}

static int restart_bind_clauses_common_(addr right, addr *ret)
{
	addr cons, symbol, quote, root;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		Return(restart_bind_binding_common_(cons, &cons));
		cons_heap(&root, cons, root);
	}
	nreverse(ret, root);

	return 0;
}

int restart_bind_common_(addr right, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(right, &right);
	if (! consp_getcons(right, &right, &body))
		return fmte_("Too few restart-bind argument.", NULL);
	if (right == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_RESTART, &symbol);
		Return(restart_bind_clauses_common_(right, &right));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, right, body, NULL);
	}

	return 0;
}


/*
 *  restart-case
 */
static int restart_case_interactive_(addr *args, addr *inter)
{
	if (*args == Nil) {
		return fmte_("The key :interactive must have a value.", NULL);
	}
	if (*inter != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, inter, args);
		if (*inter == Nil)
			return fmte_(":interactive ~S must be a function.", *inter, NULL);
	}

	return 0;
}

static int restart_case_report_(addr *args, addr *report)
{
	if (*args == Nil) {
		return fmte_("The key :report must have a value.", NULL);
	}
	if (*report != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, report, args);
		if (*report == Nil)
			return fmte_(":report ~S must be a function.", *report, NULL);
	}

	return 0;
}

static int restart_case_test_(addr *args, addr *test)
{
	if (*args == Nil) {
		return fmte_("The key :test must have a value.", NULL);
	}
	if (*test != Nil) {
		Return_getcdr(*args, args);
	}
	else {
		Return_getcons(*args, test, args);
		if (*test == Nil)
			return fmte_(":test ~S must be a function.", *test, NULL);
	}

	return 0;
}

static int restart_case_parse_(addr cons,
		addr *rname, addr *rinter, addr *rreport, addr *rtest, addr *rbody)
{
	addr name, args, body, inter, report, test, pos, lambda;
	addr keyinter, keyreport, keytest;

	*rname = *rinter = *rreport = *rtest = *rbody = Nil;
	GetConst(KEYWORD_INTERACTIVE, &keyinter);
	GetConst(KEYWORD_REPORT, &keyreport);
	GetConst(KEYWORD_TEST, &keytest);

	/* (name (c) body) */
	Return(lista_bind_(cons, &name, &args, &body, NULL));
	if (! symbolp(name))
		return fmte_("Restart name ~S must be a symbol type.", name, NULL);

	/* body */
	inter = report = test = Nil;
	for (; body != Nil; body = cons) {
		Return_getcons(body, &pos, &cons);
		if (pos == keyinter) {
			Return(restart_case_interactive_(&cons, &inter));
		}
		else if (pos == keyreport) {
			Return(restart_case_report_(&cons, &report));
		}
		else if (pos == keytest) {
			Return(restart_case_test_(&cons, &test));
		}
		else {
			break;
		}
	}

	/* `(lambda ,args ,@body) */
	GetConst(COMMON_LAMBDA, &lambda);
	lista_heap(rbody, lambda, args, body, NULL);

	/* result */
	*rname = name;
	restart_bind_symbol_common(inter, rinter);
	restart_bind_symbol_common(report, rreport);
	restart_bind_symbol_common(test, rtest);

	return 0;
}


/*
 *  restart-case  [condition]
 *
 *  (let* ((#:r1 (lisp-system::make-restart 'aaa :escape t))
 *         (#:r2 (lisp-system::make-restart 'bbb :escape t))
 *         (#:r3 (lisp-system::make-restart 'ccc :escape t))
 *         (#:list (list #:r1 #:r2 #:r3))
 *         (#:instance (lisp-system:condition-restarts-make 'signal ...)))
 *    (lisp-system::restart-progn
 *      #:list
 *      (lambda ()
 *        (with-condition-restarts
 *          #:instance
 *          #:list
 *          (signal #:instance)))))
 */
static int restart_condition_clauses_(addr form, addr *ret)
{
	addr root, cons, name, body, inter, report, test;
	addr quote, make, pos;
	addr key1, key2, key3, key4;

	/* ((lisp-system::make-restart 'name1 call ...)
	 *  (lisp-system::make-restart 'name2 call ...)
	 *  ...)
	 */
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_MAKE_RESTART, &make);
	GetConst(KEYWORD_INTERACTIVE_FUNCTION, &key1);
	GetConst(KEYWORD_REPORT_FUNCTION, &key2);
	GetConst(KEYWORD_TEST_FUNCTION, &key3);
	GetConst(KEYWORD_ESCAPE, &key4);

	root = Nil;
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		Return(restart_case_parse_(cons, &name, &inter, &report, &test, &body));

		/* (lisp-system::make-restart 'name ...) */
		conscar_heap(&pos, make);
		/* name */
		list_heap(&name, quote, name, NULL);
		cons_heap(&pos, name, pos);
		/* body */
		cons_heap(&pos, body, pos);
		/* interactive */
		if (inter != Nil) {
			cons_heap(&pos, key1, pos);
			cons_heap(&pos, inter, pos);
		}
		/* report */
		if (report != Nil) {
			cons_heap(&pos, key2, pos);
			cons_heap(&pos, report, pos);
		}
		/* test */
		if (report != Nil) {
			cons_heap(&pos, key3, pos);
			cons_heap(&pos, test, pos);
		}
		/* escape */
		cons_heap(&pos, key4, pos);
		cons_heap(&pos, T, pos);
		/* result */
		nreverse(&pos, pos);
		cons_heap(&root, pos, root);
	}

	/* result */
	nreverse(ret, root);
	return 0;
}

static int restart_condition_progn_(addr expr, addr *ret)
{
	/* `(progn ,expr) */
	addr progn;

	GetConst(COMMON_PROGN, &progn);
	cons_heap(ret, progn, expr);

	return 0;
}

static int restart_condition_signal_(addr car, addr cdr, addr g, addr *ret, addr *rexpr)
{
	addr make, type;

	/* (condition-restarts-make 'signal ...) */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &make);
	quotelist_heap(&type, car);
	lista_heap(ret, make, type, cdr, NULL);

	/* (signal #:g) */
	list_heap(rexpr, car, g, NULL);

	return 0;
}

static int restart_condition_cerror_(addr car, addr cdr, addr g, addr *ret, addr *rexpr)
{
	addr make, type, first;

	/* (condition-restarts-make 'cerror ...) */
	GetConst(SYSTEM_CONDITION_RESTARTS_MAKE, &make);
	Return_getcons(cdr, &first, &cdr);
	quotelist_heap(&type, car);
	lista_heap(ret, make, type, cdr, NULL);

	/* `(cerror ,first #:g) */
	list_heap(rexpr, car, first, g, NULL);

	return 0;
}

static int restart_condition_make_(addr expr, addr g, addr *ret, addr *rexpr)
{
	addr check, car, cdr;

	/* signal */
	Return_getcons(expr, &car, &cdr);
	GetConst(COMMON_SIGNAL, &check);
	if (car == check)
		goto signal;

	/* error */
	GetConst(COMMON_ERROR, &check);
	if (car == check)
		goto signal;

	/* warn */
	GetConst(COMMON_WARN, &check);
	if (car == check)
		goto signal;

	/* cerror */
	GetConst(COMMON_CERROR, &check);
	if (car == check)
		goto cerror;

	/* error */
	*ret = Nil;
	return fmte_("Invalid format, ~S.", expr, NULL);

signal:
	return restart_condition_signal_(car, cdr, g, ret, rexpr);

cerror:
	return restart_condition_cerror_(car, cdr, g, ret, rexpr);
}

static int restart_condition_bind_(Execute ptr, addr expr, addr args, addr *ret)
{
	addr root, glist, ginst, make, pos, g, vars;
	addr list, leta, rprogn, lambda, with;

	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LETA, &leta);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_WITH_CONDITION_RESTARTS, &with);
	GetConst(SYSTEM_RESTART_PROGN, &rprogn);

	/* args */
	root = vars = Nil;
	while (args != Nil) {
		GetCons(args, &pos, &args);
		Return(make_gensym_(ptr, &g));
		cons_heap(&vars, g, vars);
		list_heap(&pos, g, pos, NULL);
		cons_heap(&root, pos, root);
	}
	nreverse(&vars, vars);

	/* (#:glist (list #:r1 #:r2 #:r3)) */
	Return(make_gensym_(ptr, &glist));
	cons_heap(&list, list, vars);
	list_heap(&list, glist, list, NULL);
	cons_heap(&root, list, root);

	/* (#:ginst ,make) */
	Return(make_gensym_(ptr, &ginst));
	Return(restart_condition_make_(expr, ginst, &make, &expr));
	list_heap(&pos, ginst, make, NULL);
	cons_heap(&root, pos, root);

	/* (let* (,list)
	 *   (lisp-system::restart-progn
	 *     #:glist
	 *     (lambda ()
	 *       (with-condition-restarts
	 *         #:ginst
	 *         #:glist
	 *         (signal #:ginst)))))
	 */
	list_heap(&with, with, ginst, glist, expr, NULL);
	list_heap(&lambda, lambda, Nil, with, NULL);
	list_heap(&rprogn, rprogn, glist, with, NULL);
	nreverse(&root, root);
	list_heap(ret, leta, root, rprogn, NULL);

	return 0;
}

static int restart_condition_(Execute ptr, addr form, addr expr, addr *ret)
{
	addr args;

	Return(restart_condition_clauses_(form, &args));
	if (args == Nil)
		return restart_condition_progn_(expr, ret);

	return restart_condition_bind_(ptr, expr, args, ret);
}


/*
 *  restart-case  [normal]
 */
static int restart_case_expr_(addr expr, addr *ret)
{
	addr x, y;

	if (! consp_getcar(expr, &x))
		return Result(ret, Nil);

	/* signal, error, warn */
	GetConst(COMMON_SIGNAL, &y);
	if (x == y)
		return Result(ret, x);
	GetConst(COMMON_ERROR, &y);
	if (x == y)
		return Result(ret, x);
	GetConst(COMMON_WARN, &y);
	if (x == y)
		return Result(ret, x);

	/* cerror */
	GetConst(COMMON_CERROR, &y);
	if (x == y)
		return Result(ret, x);

	/* otherwise */
	return Result(ret, Nil);
}

static int restart_case_clauses_(addr form, addr *ret)
{
	addr root, cons, name, body, inter, report, test;
	addr quote, symbol, list;

	/* (lisp-system::restart-case (list ...) (list ...) ...) */
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		Return(restart_case_parse_(cons, &name, &inter, &report, &test, &body));

		/* (list 'name (lambda ...) ...) */
		list_heap(&name, quote, name, NULL);
		list_heap(&cons, list, name, body, inter, report, test, NULL);
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse(ret, root);

	return 0;
}

int restart_case_common_(Execute ptr, addr form, addr env, addr *ret)
{
	addr symbol, expr, car;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &expr, &form))
		return fmte_("Too few restart-case argument.", NULL);
	if (form == Nil)
		return Result(ret, expr);

	GetConst(SYSTEM_RESTART, &symbol);
	Return(restart_case_expr_(expr, &car));
	if (car != Nil)
		return restart_condition_(ptr, form, expr, ret);
	Return(restart_case_clauses_(form, &form));
	list_heap(ret, symbol, form, expr, NULL);

	return 0;
}


/*
 *  with-condition-restarts
 */
static void with_condition_restarts_expander(addr *ret,
		addr condition_form, addr restarts_form, addr body)
{
	/* `(let ((,condition ,condition-form)
	 *        (,restarts ,restarts-form))
	 *    (unwind-protect
	 *      (progn
	 *        (lisp-system::condition-restarts-push ,condition ,restarts)
	 *        ,@body)
	 *      (lisp-system::condition-restarts-pop ,condition ,restarts)))
	 */
	addr let, push, pop, unwind, progn;
	addr condition, restarts;

	GetConst(COMMON_LETA, &let);
	GetConst(COMMON_UNWIND_PROTECT, &unwind);
	GetConst(COMMON_PROGN, &progn);
	GetConst(SYSTEM_CONDITION_RESTARTS_PUSH, &push);
	GetConst(SYSTEM_CONDITION_RESTARTS_POP, &pop);
	make_symbolchar(&condition, "CONDITION");
	make_symbolchar(&restarts, "RESTARTS");

	list_heap(&push, push, condition, restarts, NULL);
	lista_heap(&progn, progn, push, body, NULL);
	list_heap(&pop, pop, condition, restarts, NULL);
	list_heap(&unwind, unwind, progn, pop, NULL);
	list_heap(&condition, condition, condition_form, NULL);
	list_heap(&restarts, restarts, restarts_form, NULL);
	list_heap(&condition, condition, restarts, NULL);
	list_heap(ret, let, condition, unwind, NULL);
}

int with_condition_restarts_common_(addr form, addr *ret)
{
	addr condition, list;

	Return_getcdr(form, &form);
	if (! consp_getcons(form, &condition, &form))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	if (! consp_getcons(form, &list, &form))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	if (form == Nil)
		consnil_heap(&form);

	with_condition_restarts_expander(ret, condition, list, form);
	return 0;
}


/*
 *  with-simple-restart
 */
int with_simple_restart_common_(addr form, addr env, addr *ret)
{
	/* (defmacro with-simple-restart ((name &rest args) &body body)
	 *   `(restart-case
	 *      (progn ,@body)
	 *      (,name ()
	 *        :report (lambda (s) (format s ,@args))
	 *        (values nil t))))
	 */
	addr args, body, name;
	addr restart, progn, report, lambda, s, format, values;

	/* parse */
	Return_getcdr(form, &form);
	if (! consp_getcons(form, &args, &body))
		goto error;
	if (! consp_getcons(args, &name, &args))
		goto error;
	if (! consp(args))
		goto error;

	/* macro */
	GetConst(COMMON_RESTART_CASE, &restart);
	GetConst(COMMON_PROGN, &progn);
	GetConst(KEYWORD_REPORT, &report);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_FORMAT, &format);
	make_symbolchar(&s, "STREAM");
	GetConst(COMMON_VALUES, &values);
	list_heap(&values, values, Nil, T, NULL);
	lista_heap(&format, format, s, args, NULL);
	list_heap(&s, s, NULL);
	list_heap(&lambda, lambda, s, format, NULL);
	list_heap(&name, name, Nil, report, lambda, values, NULL);
	cons_heap(&progn, progn, body);
	list_heap(ret, restart, progn, name, NULL);
	return 0;

error:
	return fmte_("WITH-SIMPLE-RESTART arguments ~S must be "
			"((name format ...) &body body) form.", form, NULL);
}


/*
 *  abort
 */
int abort_common_(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_ABORT, &pos);
	Return(find_restart_control_error_(ptr, pos, opt, &opt));

	return invoke_restart_control_(ptr, opt, Nil);
}


/*
 *  continue
 */
int continue_common_(Execute ptr, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_CONTINUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		Return(invoke_restart_control_(ptr, opt, Nil));
	}

	return 0;
}


/*
 *  muffle-warning
 */
int muffle_warning_common_(Execute ptr, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check)
		return invoke_restart_control_(ptr, opt, Nil);
	else
		return call_control_error_(ptr);
}


/*
 *  store-value
 */
int store_value_common_(Execute ptr, addr var, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_STORE_VALUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		list_local(ptr->local, &var, var, NULL);
		Return(invoke_restart_control_(ptr, opt, var));
	}

	return 0;
}


/*
 *  use-value
 */
int use_value_common_(Execute ptr, addr var, addr opt)
{
	int check;
	addr pos;

	if (opt == Unbound)
		opt = Nil;
	GetConst(COMMON_USE_VALUE, &pos);
	Return(find_restart_control_(ptr, pos, opt, &opt, &check));
	if (check) {
		list_local(ptr->local, &var, var, NULL);
		Return(invoke_restart_control_(ptr, opt, var));
	}

	return 0;
}

