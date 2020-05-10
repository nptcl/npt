#include "clos.h"
#include "clos_class.h"
#include "condition.h"
#include "condition_debugger.h"
#include "cons.h"
#include "cons_list.h"
#include "constant.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "format.h"
#include "function.h"
#include "gc.h"
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
static int assert_retry_common(addr test, addr *ret)
{
	/* (tagbody
	 *   loop
	 *   (restart-bind
	 *     ((continue (lambda () (go loop))
	 *        :report-function
	 *        (lambda (s)
	 *          (princ "Retry assersion." s))))
	 *     (unless test
	 *       (invoke-debugger
	 *         (make-condition 'simple-error
	 *           :format-control "Failed assersion ~A."
	 *           :format-arguments (list 'test))))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr princ, unless, invoke, make, quote, simple, control, arguments, list;
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
	GetConst(COMMON_INVOKE_DEBUGGER, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_SIMPLE_ERROR, &simple);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	GetConst(COMMON_LIST, &list);
	/* expand */
	list_heap(&a, quote, test, NULL);
	list_heap(&list, list, a, NULL);
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple, control, str2, arguments, list, NULL);
	list_heap(&invoke, invoke, make, NULL);
	list_heap(&unless, unless, test, invoke, NULL);
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

static int assert_prompt_common(Execute ptr, addr env, addr *ret, addr place)
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

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
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
	nreverse_list_unsafe(&root, root);
	list_heap(&leta, leta, root, declare, setq, w, NULL);
	/* when */
	list_heap(&yornp, yornp, str1, place, NULL);
	list_heap(ret, when, yornp, leta, NULL);

	return 0;
}

static int assert_list_common(Execute ptr, addr env,
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
	 *       (invoke-debugger
	 *         (make-condition 'simple-error
	 *           :format-control "Failed assersion ~A."
	 *           :format-arguments (list 'test))))))
	 */
	addr tagbody, restart, cont, lambda, go, report;
	addr format, unless, invoke, make, quote, simple, control, arguments, list;
	addr loop, s, str, a, b, c;
	LocalHold hold;

	/* places */
	hold = LocalHold_array(ptr, 1);
	for (a = Nil; places != Nil; ) {
		Return_getcons(places, &b, &places);
		Return(assert_prompt_common(ptr, env, &b, b));
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
	GetConst(COMMON_INVOKE_DEBUGGER, &invoke);
	GetConst(COMMON_MAKE_CONDITION, &make);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(COMMON_SIMPLE_ERROR, &simple);
	GetConst(KEYWORD_FORMAT_CONTROL, &control);
	GetConst(KEYWORD_FORMAT_ARGUMENTS, &arguments);
	/* format */
	if (output != Nil) {
		Return_getcons(output, &output, &list);
		list_heap(&list, quote, list, NULL);
	}
	else {
		GetConst(COMMON_LIST, &list);
		strvect_char_heap(&output, "Failed assersion ~A");
		list_heap(&c, quote, test, NULL);
		list_heap(&list, list, c, NULL);
		list_heap(&c, list, test, NULL);
	}
	list_heap(&simple, quote, simple, NULL);
	list_heap(&make, make, simple, control, output, arguments, list, NULL);
	/* expand */
	list_heap(&invoke, invoke, make, NULL);
	list_heap(&unless, unless, test, invoke, NULL);
	list_heap(&quote, quote, places, NULL);
	list_heap(&format, format, s, str, quote, NULL);
	list_heap(&s, s, NULL);
	list_heap(&format, lambda, s, format, NULL);
	/* prompt */
	list_heap(&go, go, loop, NULL);
	cons_heap(&a, go, a);
	nreverse_list_unsafe(&a, a);
	lista_heap(&lambda, lambda, Nil, a, NULL);
	list_heap(&cont, cont, lambda, report, format, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(ret, tagbody, loop, restart, NULL);

	return 0;
}

_g int assert_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, test, list;

	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &test, &args);
	if (args == Nil)
		return assert_retry_common(test, ret);
	if (! consp(args))
		goto error;
	GetCons(args, &list, &args);
	if (list == Nil && args == Nil)
		return assert_retry_common(test, ret);
	else
		return assert_list_common(ptr, env, test, list, args, ret);

error:
	return fmte_("ASSERT arguments ~S must be "
			"(test &optional places format args) form.", form, NULL);
}


/*
 *  error
 */
static int error_datum_common(Execute ptr, addr datum, addr rest, addr *ret)
{
	addr make;

	/* symbol -> (make-instance symbol ...) */
	if (symbolp(datum)) {
		clos_find_class(datum, &datum);
		if (! conditionp(datum))
			return fmte_("The class ~S is not a condition subclass.", datum, NULL);
		GetConst(COMMON_MAKE_INSTANCE, &make);
		return callclang_applya(ptr, ret, make, datum, rest, NULL);
	}

	/* condition -> (error condition) */
	if (! condition_instance_p(datum))
		return fmte_("Invalid datum argument ~S.", datum, NULL);
	if (rest != Nil) {
		return fmte_("The datum argument ~S must be a nil "
				"if first argument is condition type.", datum, NULL);
	}
	*ret = datum;
	return 0;
}

_g int error_common(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-error */
		return call_simple_error_(ptr, datum, rest);
	}
	else {
		Return(error_datum_common(ptr, datum, rest, &datum));
		return error_function_(ptr, datum);
	}
}


/*
 *  cerror
 */
static int function_cerror_continue(Execute ptr)
{
	/* do nothing */
	return 0;
}

static int cerror_make_common(Execute ptr, addr *ret, addr format, addr args)
{
	addr inst, pos;

	Return(format_string_lisp(ptr, format, args, &format));
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_cerror_continue);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	setreport_restart(inst, format);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);

	return Result(ret, inst);
}

static int cerror_restart_common(Execute ptr, addr restart, addr datum)
{
	addr control;

	push_new_control(ptr, &control);
	Return(restart1_control(ptr, restart, invoke_debugger, datum));
	return free_control_(ptr, control);
}

_g int cerror_common(Execute ptr, addr restart, addr datum, addr rest)
{
	LocalHold hold;

	/* signal */
	if (stringp(datum)) {
		/* string -> simple-condition */
		instance_simple_error(&datum, datum, rest);
	}
	else {
		Return(error_datum_common(ptr, datum, rest, &datum));
	}

	/* wake condition */
	if (find_condition_control(ptr, datum))
		return signal_function_(ptr, datum);

	/* Can't handle the condition. */
	hold = LocalHold_local(ptr);
	localhold_push(hold, datum);
	Return(cerror_make_common(ptr, &restart, restart, rest));
	localhold_push(hold, restart);
	Return(cerror_restart_common(ptr, restart, datum));
	localhold_end(hold);

	return 0;
}


/*
 *  check-type
 */
static int check_type_expand_common(Execute ptr, addr env, addr *ret,
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

	Return(get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r));
	hold = LocalHold_local(ptr);
	localhold_pushva(hold, a, b, g, w, r, NULL);
	Return_getcar(g, &g);

	Return(format_string(ptr, &str1,
				"Retry check-type with new value ~A.", place, NULL));
	localhold_push(hold, str1);

	Return(format_string(ptr, &str2,
				"Input ~A> ", place, NULL));
	localhold_push(hold, str2);

	Return(format_string(ptr, &str3,
				"The value of ~A, ~~A, is not ~~(~~A~~).", place, NULL));
	localhold_push(hold, str2);

	if (string == Nil) {
		Return(parse_type(ptr, &string, type, env));
		localhold_push(hold, string);

		type_object(&string, string);
		localhold_push(hold, string);
		Return(princ_string_heap(ptr, &string, string));
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
	nreverse_list_unsafe(&root, root);
	list_heap(ret, leta, root, declare, tagbody, NULL);

	return 0;
}

_g int check_type_common(Execute ptr, addr form, addr env, addr *ret)
{
	addr args, place, type, string;

	Return_getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &place, &args);
	if (! consp(args))
		goto error;
	GetCons(args, &type, &args);
	if (args == Nil)
		string = Nil;
	else {
		if (! consp(args))
			goto error;
		GetCons(args, &string, &args);
		if (args != Nil)
			goto error;
	}
	return check_type_expand_common(ptr, env, ret, place, type, string);

error:
	return fmte_("CHECK-TYPE arguments ~S must be "
			"(place type &optional string) form.", form, NULL);
}


/*
 *  invalid-method-error
 */
_g int invalid_method_error_common(Execute ptr, addr method, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method error: ~S~%~?");
	list_heap(&arguments, method, format, args, NULL);
	instance_simple_error(&pos, control, arguments);
	return error_function_(ptr, pos);
}


/*
 *  method-combination-error
 */
_g int method_combination_error_common(Execute ptr, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method-combination error:~%~?");
	list_heap(&arguments, format, args, NULL);
	instance_simple_error(&pos, control, arguments);
	return error_function_(ptr, pos);
}


/*
 *  signal
 */
_g int signal_common(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-condition */
		instance_simple_condition(&datum, datum, rest);
	}
	else {
		Return(error_datum_common(ptr, datum, rest, &datum));
	}
	return signal_function_(ptr, datum);
}


/*
 *  warn
 */
_g int warn_common(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-warning */
		instance_simple_warning(&datum, datum, rest);
	}
	else {
		Return(error_datum_common(ptr, datum, rest, &datum));
	}
	return warning_restart_case(ptr, datum);
}


/*
 *  break
 */
static int function_break_continue(Execute ptr)
{
	/* do nothing */
	return 0;
}

static int break_invoke_common(Execute ptr, addr format, addr args)
{
	addr symbol, condition;
	LocalHold hold;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	instance_simple_condition(&condition, format, args);

	hold = LocalHold_local_push(ptr, condition);
	Return(invoke_debugger(ptr, condition));
	localhold_end(hold);

	return 0;
}

static int break_restart_common(Execute ptr, addr restart, addr format, addr args)
{
	addr control;

	push_new_control(ptr, &control);
	Return(restart2_control(ptr, restart, break_invoke_common, format, args));
	return free_control_(ptr, control);
}

static void break_make_common(addr *ret)
{
	static const char *message = "Return from BREAK.";
	addr inst, pos;

	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_break_continue);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	strvect_char_heap(&pos, message);
	setreport_restart(inst, pos);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;
}

_g int break_common(Execute ptr, addr format, addr args)
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
	Return(break_restart_common(ptr, restart, format, args));
	localhold_end(hold);

	return 0;
}


/*
 *  handler-bind
 */
static int handler_bind_clauses_common(addr form, addr *ret)
{
	addr cons, symbol, quote, root, name, lambda, temp;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (form != Nil) {
		Return_getcons(form, &cons, &form);
		/* (name lambda) */
		lista_bind(cons, &name, &lambda, &temp, NULL);
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
	nreverse_list_unsafe(ret, root);

	return 0;
}

_g int handler_bind_common(addr form, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(form, &form);
	if (! consp(form))
		return fmte_("Too few handler-bind argument.", NULL);
	GetCons(form, &form, &body);
	if (form == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_HANDLER, &symbol);
		Return(handler_bind_clauses_common(form, &form));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, form, body, NULL);
	}
	return 0;
}


/*
 *  handler-case
 */
static void handler_case_gensym_common(Execute ptr, addr form, addr *ret)
{
	addr args, declare, pos, cons, gensym;

	/* (lambda (gensym)
	 *   (declare (ignore gensym))
	 *   form)
	 */
	/* (gensym) */
	make_gensym(ptr, &gensym);
	conscar_heap(&args, gensym);
	/* (declare (ignore gensym)) */
	GetConst(COMMON_IGNORE, &pos);
	list_heap(&cons, pos, gensym, NULL);
	GetConst(COMMON_DECLARE, &pos);
	list_heap(&declare, pos, cons, NULL);
	/* (lambda ...) */
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, declare, form, NULL);
}

static void handler_case_lambda_common(addr args, addr form, addr *ret)
{
	addr pos;
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, form, NULL);
}

static int handler_case_noerror_common(addr *noerror, addr cons)
{
	if (*noerror) {
		return fmtw_("There are multiple :no-error clauses ~S"
				" in handler-case.", cons, NULL);
	}
	else {
		*noerror = cons;
	}

	return 0;
}

static int handler_case_clauses_common(Execute ptr, addr right, addr *ret, addr *rete)
{
	addr noerror, root, cons, name, args, form;
	addr keyword, symbol, quote;

	/* (list-system::handler-case name1 lambda1 ...) */
	noerror = NULL;
	GetConst(KEYWORD_NO_ERROR, &keyword);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		/* (name (c) form) */
		lista_bind(cons, &name, &args, &form, NULL);
		if (name == keyword) {
			Return(handler_case_noerror_common(&noerror, cons));
		}
		else {
			if (args == Nil)
				handler_case_gensym_common(ptr, form, &cons);
			else if (singlep(args))
				handler_case_lambda_common(args, form, &cons);
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
	}
	/* result */
	nreverse_list_unsafe(ret, root);
	*rete = noerror;

	return 0;
}

static void handler_case_body(addr noerror, addr expr, addr *ret)
{
	addr pos;

	if (! noerror) {
		*ret = expr;
		return;
	}

	/* (multiple-value-call
	 *   (lambda (a b c d) ...)
	 *   expr))
	 */
	GetConst(COMMON_LAMBDA, &pos);
	cons_heap(&noerror, pos, noerror);
	GetConst(COMMON_MULTIPLE_VALUE_CALL, &pos);
	list_heap(ret, pos, noerror, expr, NULL);
}

_g int handler_case_common(Execute ptr, addr right, addr env, addr *ret)
{
	addr symbol, expr, noerror;

	Return_getcdr(right, &right);
	if (! consp(right))
		return fmte_("Too few handler-case argument.", NULL);
	GetCons(right, &expr, &right);
	if (right == Nil)
		return Result(ret, expr);

	GetConst(SYSTEM_HANDLER, &symbol);
	Return(handler_case_clauses_common(ptr, right, &right, &noerror));
	handler_case_body(noerror, expr, &expr);
	list_heap(ret, symbol, right, expr, NULL);

	return 0;
}


/*
 *  ignore-errors
 */
_g int ignore_errors_common(Execute ptr, addr form, addr env, addr *ret)
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
	make_gensym(ptr, &g);
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
_g int make_condition_common(Execute ptr, addr args, addr *ret)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCE, &call);
	getfunctioncheck_local(ptr, call, &call);
	return callclang_apply(ptr, ret, call, args);
}


/*
 *  compute-restarts
 */
_g int compute_restarts_common_(Execute ptr, addr pos, addr *ret)
{
	if (pos == Unbound)
		pos = Nil;
	return compute_restarts_control_(ptr, pos, ret);
}


/*
 *  find-restart
 */
_g int find_restart_common_(Execute ptr, addr var, addr opt, addr *ret)
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
static int restart_bind_interactive_common(addr *args, addr *inter)
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

static int restart_bind_report_common(addr *args, addr *report)
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

static int restart_bind_test_common(addr *args, addr *test)
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

static int restart_bind_binding_common(addr args, addr *ret)
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
	lista_bind(args, &name, &lambda, &args, NULL);
	inter = report = test = Nil;
	while (args != Nil) {
		Return_getcons(args, &pos, &args);
		if (pos == keyinter) {
			Return(restart_bind_interactive_common(&args, &inter));
		}
		else if (pos == keyreport) {
			Return(restart_bind_report_common(&args, &report));
		}
		else if (pos == keytest) {
			Return(restart_bind_test_common(&args, &test));
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

static int restart_bind_clauses_common(addr right, addr *ret)
{
	addr cons, symbol, quote, root;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		Return(restart_bind_binding_common(cons, &cons));
		cons_heap(&root, cons, root);
	}
	nreverse_list_unsafe(ret, root);

	return 0;
}

_g int restart_bind_common(addr right, addr env, addr *ret)
{
	addr symbol, body;

	Return_getcdr(right, &right);
	if (! consp(right))
		return fmte_("Too few restart-bind argument.", NULL);
	GetCons(right, &right, &body);
	if (right == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(ret, symbol, body);
	}
	else {
		GetConst(SYSTEM_RESTART, &symbol);
		Return(restart_bind_clauses_common(right, &right));
		if (body == Nil)
			consnil_heap(&body);
		lista_heap(ret, symbol, right, body, NULL);
	}

	return 0;
}


/*
 *  restart-case
 */
static int restart_case_interactive(addr *args, addr *inter)
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

static int restart_case_report(addr *args, addr *report)
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

static int restart_case_test(addr *args, addr *test)
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

static int restart_case_clauses(addr right, addr *ret)
{
	addr root, cons, pos, name, ord, form, inter, report, test;
	addr keyinter, keyreport, keytest;
	addr quote, symbol, list, lambda;

	/* (list-system::restart-case (list ...) (list ...) ...) */
	GetConst(KEYWORD_INTERACTIVE, &keyinter);
	GetConst(KEYWORD_REPORT, &keyreport);
	GetConst(KEYWORD_TEST, &keytest);
	GetConst(COMMON_LIST, &list);
	GetConst(COMMON_LAMBDA, &lambda);
	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_CASE, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		Return_getcons(right, &cons, &right);
		/* (name (c) form) */
		lista_bind(cons, &name, &ord, &form, NULL);
		inter = report = test = Nil;
		for (; form != Nil; form = cons) {
			Return_getcons(form, &pos, &cons);
			if (pos == keyinter) {
				Return(restart_case_interactive(&cons, &inter));
			}
			else if (pos == keyreport) {
				Return(restart_case_report(&cons, &report));
			}
			else if (pos == keytest) {
				Return(restart_case_test(&cons, &test));
			}
			else
				break;
		}
		/* symbol */
		restart_bind_symbol_common(inter, &inter);
		restart_bind_symbol_common(report, &report);
		restart_bind_symbol_common(test, &test);
		/* (list 'name (lambda ...) ...) */
		list_heap(&name, quote, name, NULL);
		lista_heap(&form, lambda, ord, form, NULL);
		list_heap(&cons, list, name, form, inter, report, test, NULL);
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);

	return 0;
}

_g int restart_case_common(addr right, addr env, addr *ret)
{
	addr symbol, expr;

	Return_getcdr(right, &right);
	if (! consp(right))
		return fmte_("Too few restart-case argument.", NULL);
	GetCons(right, &expr, &right);
	if (right == Nil)
		return Result(ret, expr);;

	GetConst(SYSTEM_RESTART, &symbol);
	Return(restart_case_clauses(right, &right));
	list_heap(ret, symbol, right, expr, NULL);

	return 0;
}


/*
 *  with-condition-restarts
 */
_g int with_condition_restarts_common(addr right, addr env, addr *ret)
{
	addr condition, cons, symbol;

	Return_getcdr(right, &right);
	if (! consp(right))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	GetCons(right, &condition, &right);
	if (! consp(right))
		return fmte_("Too few with-condition-restarts argument.", NULL);
	GetCons(right, &cons, &right);
	if (right == Nil)
		consnil_heap(&right);

	/* (lisp-system::push-return
	 *   (lisp-system::redirect-restart condition cons)
	 *   right)
	 */
	GetConst(SYSTEM_REDIRECT_RESTART, &symbol);
	list_heap(&cons, symbol, condition, cons, NULL);
	GetConst(SYSTEM_PUSH_RETURN, &symbol);
	lista_heap(ret, symbol, cons, right, NULL);

	return 0;
}


/*
 *  with-simple-restart
 */
_g int with_simple_restart_common(addr form, addr env, addr *ret)
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
	if (! consp(form))
		goto error;
	GetCons(form, &args, &body);
	if (! consp(body))
		goto error;
	GetCons(args, &name, &args);
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
_g int abort_common(Execute ptr, addr opt)
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
_g int continue_common(Execute ptr, addr opt)
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
_g int muffle_warning_common(Execute ptr, addr opt)
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
_g int store_value_common(Execute ptr, addr var, addr opt)
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
_g int use_value_common(Execute ptr, addr var, addr opt)
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


/*
 *  initialize
 */
_g void init_call_conditions(void)
{
	SetPointerCall(defun, empty, cerror_continue);
	SetPointerCall(defun, empty, break_continue);
}

