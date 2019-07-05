/*
 *  ANSI COMMON LISP: 9. Conditions
 */
#include "condition.h"
#include "clos.h"
#include "clos_common.h"
#include "cons.h"
#include "common_header.h"
#include "format.h"
#include "hashtable.h"
#include "print.h"
#include "setf.h"
#include "strtype.h"
#include "type_parse.h"
#include "type_object.h"
#include "unicode.h"

/* (defun cell-error-name (condition) ...) -> t */
static void function_cell_error_name(Execute ptr, addr var)
{
	cell_error_name(var, &var);
	setresult_control(ptr, var);
}

static void type_cell_error_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, CellError);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_cell_error_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CELL_ERROR_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_cell_error_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cell_error_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro assert (test &optional (place*) format &rest args) ...) -> nil */
static void function_assert_retry(Execute ptr, addr test)
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
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	/* result */
	setresult_control(ptr, tagbody);
}

static int function_assert_prompt(Execute ptr, addr env, addr *ret, addr place)
{
	/* (multiple-value-bind (a b g w r) (get-setf-expansion PLACE)
	 *   (declare (ignore r))
	 *   `(when (y-or-n-p "Do you want to set a new value in ~A? " ',place)
	 *      (let ((,a1 ,b1) (,a2 ,b2) ... ,g)
	 *        (declare (ignorable ,a1 ,a2 ...))
	 *        (setq ,g (eval (prompt-for t "Input ~A> " ',place)))
	 *        ,w)))
	 */
	addr a, b, g, w, r;
	addr when, yornp, let, declare, ignorable, setq, eval, prompt, quote;
	addr str1, str2, root, x, y;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	getcar(g, &g);
	strvect_char_heap(&str1, "Do you want to setq a new value in ~A?");
	strvect_char_heap(&str2, "Input ~A> ");
	GetConst(COMMON_WHEN, &when);
	GetConst(COMMON_Y_OR_N_P, &yornp);
	GetConst(COMMON_LET, &let);
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
	/* let */
	for (root = Nil; a != Nil; ) {
		getcons(a, &x, &a);
		getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	cons_heap(&root, g, root);
	nreverse_list_unsafe(&root, root);
	list_heap(&let, let, root, declare, setq, w, NULL);
	/* when */
	list_heap(&yornp, yornp, str1, place, NULL);
	list_heap(ret, when, yornp, let, NULL);

	return 0;
}

static int function_assert_list(Execute ptr, addr env,
		addr test, addr places, addr output)
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
	addr loop, s, str, a, b;

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
		getcons(output, &output, &list);
		list_heap(&list, quote, list, NULL);
	}
	else {
		GetConst(COMMON_LIST, &list);
		strvect_char_heap(&output, "Failed assersion ~A");
		list_heap(&a, quote, test, NULL);
		list_heap(&list, list, a, NULL);
		list_heap(&a, list, test, NULL);
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
	for (a = Nil; places != Nil; ) {
		getcons(places, &b, &places);
		if (function_assert_prompt(ptr, env, &b, b))
			return 1;
		cons_heap(&a, b, a);
	}
	list_heap(&go, go, loop, NULL);
	cons_heap(&a, go, a);
	nreverse_list_unsafe(&a, a);
	lista_heap(&lambda, lambda, Nil, a, NULL);
	list_heap(&cont, cont, lambda, report, format, NULL);
	list_heap(&cont, cont, NULL);
	list_heap(&restart, restart, cont, unless, NULL);
	list_heap(&tagbody, tagbody, loop, restart, NULL);
	/* result */
	setresult_control(ptr, tagbody);
	return 0;
}

static void function_assert(Execute ptr, addr form, addr env)
{
	addr args, test, list;

	getcdr(form, &form);
	if (! consp(form))
		goto error;
	GetCons(form, &test, &args);
	if (args == Nil) {
		function_assert_retry(ptr, test);
		return;
	}
	if (! consp(args))
		goto error;
	GetCons(args, &list, &args);
	if (list == Nil && args == Nil) {
		function_assert_retry(ptr, test);
		return;
	}
	(void)function_assert_list(ptr, env, test, list, args);
	return;

error:
	fmte("ASSERT arguments ~S must be "
			"(test &optional places format args) form.", form, NULL);
}

static void defmacro_assert(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ASSERT, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_assert);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun error (datum &rest args) ...) -> nil
 *   datum  (or string symbol condition)
 *   args   (&rest t)
 *   nil    nil  ;; not null
 */
static int function_error_datum(Execute ptr, addr datum, addr rest, addr *ret)
{
	addr make;

	/* symbol -> (make-instance symbol ...) */
	if (symbolp(datum)) {
		clos_find_class(datum, &datum);
		if (! conditionp(datum))
			fmte("The class ~S is not a condition subclass.", datum, NULL);
		GetConst(COMMON_MAKE_INSTANCE, &make);
		lista_local(ptr->local, &rest, datum, rest, NULL);
		if (callclang_apply(ptr, ret, make, rest)) return 1;
		return 0;
	}

	/* condition -> (error condition) */
	if (condition_instance_p(datum)) {
		if (rest != Nil) {
			fmte("The datum argument ~S must be a nil "
					"if first argument is condition type.", datum, NULL);
		}
		*ret = datum;
		return 0;
	}
	fmte("Invalid datum argument ~S.", datum, NULL);

	return 0;
}

static void function_error(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-error */
		simple_error(datum, rest);
	}
	else {
		if (function_error_datum(ptr, datum, rest, &datum))
			return;
		if (error_common(ptr, datum))
			return;
	}

	/* The error function may not return normally. */
	setvalues_nil_control(ptr);
}

static void type_error_function(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, Error);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_error_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cerror (continue-format datum &args) ...) -> null */ 
static void function_cerror_continue(Execute ptr)
{
	/* do nothing */
}

static int function_cerror_make(Execute ptr, addr *ret, addr format, addr args)
{
	addr inst, pos;

	if (format_string_lisp(ptr, format, args, &format))
		return 1;
	GetConst(COMMON_CONTINUE, &pos);
	restart_heap(&inst, pos);
	compiled_heap(&pos, Nil);
	setcompiled_empty(pos, p_defun_cerror_continue);
	setfunction_restart(inst, pos);
	setinteractive_restart(inst, Nil);
	setreport_restart(inst, format);
	settest_restart(inst, Nil);
	setescape_restart(inst, 1);
	*ret = inst;

	return 0;
}

static int function_cerror_restart(Execute ptr, addr restart, addr datum)
{
	int check;
	addr control;
	codejump jump;

	/* execute */
	push_restart_initialize_control(ptr, &control);
	check = 0;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		pushobject_restart_control(ptr, restart);
		check = invoke_debugger(ptr, datum);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control))
			throw_switch(&jump);
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	setresult_control(ptr, Nil);
	return free_control(ptr, control);
}

static void function_cerror(Execute ptr, addr restart, addr datum, addr rest)
{
	/* signal */
	if (stringp(datum)) {
		/* string -> simple-condition */
		instance_simple_error(&datum, datum, rest);
	}
	else {
		if (function_error_datum(ptr, datum, rest, &datum))
			return;
	}
	if (signal_function(datum))
		return;

	/* Can't handle the condition. */
	if (function_cerror_make(ptr, &restart, restart, rest))
		return;
	if (function_cerror_restart(ptr, restart, datum))
		return;
	setresult_control(ptr, Nil);
}

static void type_cerror(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, String);
	GetTypeTable(&values, ConditionDesigner);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_cerror(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_cerror);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cerror(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro check-type (place type &optional string) ...) -> null */
static int function_check_type_expand(Execute ptr, addr env, addr *ret,
		addr place, addr type, addr string)
{
	/* (let ((a1 b1) (a2 b2) ... (value r) g)
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
	addr let, declare, ignorable, tagbody, loop, restart, store, lambda, setq;
	addr value, go, report, inter, princ, list, eval, prompt, unless;
	addr typep, quote, invoke, make, simple, datum, expect, control, arguments;
	addr x, y, root;

	if (get_setf_expansion(ptr, place, env, &a, &b, &g, &w, &r))
		return 1;
	getcar(g, &g);
	str1 = fmth("Retry check-type with new value ~A.", place, NULL);
	str2 = fmth("Input ~A> ", place, NULL);
	str3 = fmth("The value of ~A, ~~A, is not ~~(~~A~~).", place, NULL);
	if (string == Nil) {
		if (parse_type(ptr, &string, type, env))
			return 1;
		type_object(&string, string);
		if (princ_string(ptr, NULL, &string, string))
			return 1;
	}
	make_symbolchar(&v, "V");
	make_symbolchar(&s, "STREAM");
	make_symbolchar(&loop, "LOOP");
	make_symbolchar(&value, "VALUE");
	GetConst(COMMON_LET, &let);
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
	/* let */
	lista_heap(&ignorable, ignorable, a, NULL);
	list_heap(&declare, declare, ignorable, NULL);
	for (root = Nil; a != Nil; ) {
		getcons(a, &x, &a);
		getcons(b, &y, &b);
		list_heap(&x, x, y, NULL);
		cons_heap(&root, x, root);
	}
	list_heap(&value, value, r, NULL);
	cons_heap(&root, value, root);
	cons_heap(&root, g, root);
	nreverse_list_unsafe(&root, root);
	list_heap(ret, let, root, declare, tagbody, NULL);

	return 0;
}

static void function_check_type(Execute ptr, addr form, addr env)
{
	addr args, place, type, string;

	getcdr(form, &form);
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
	if (function_check_type_expand(ptr, env, &place, place, type, string))
		return;
	setresult_control(ptr, place);
	return;

error:
	fmte("CHECK-TYPE arguments ~S must be "
			"(place type &optional string) form.", form, NULL);
}

static void defmacro_check_type(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CHECK_TYPE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_check_type);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun invalid-method-error (method format &rest args) ...) -> null */
static void function_invalid_method_error(Execute ptr,
		addr method, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method error: ~S~%~?");
	list_heap(&arguments, method, format, args, NULL);
	instance_simple_error(&pos, control, arguments);
	if (error_common(ptr, pos))
		return;
	setresult_control(ptr, Nil);
}

static void type_invalid_method_error(addr *ret)
{
	addr args, values, type;

	GetTypeTable(&args, Method);
	GetTypeTable(&values, String);
	GetTypeTable(&type, T);
	typeargs_var2rest(&args, args, values, type);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_invalid_method_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVALID_METHOD_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_invalid_method_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invalid_method_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun method-combination-error (format &rest args) ...) -> null */
static void function_method_combination_error(Execute ptr, addr format, addr args)
{
	addr control, arguments, pos;

	strvect_char_heap(&control, "Method-combination error:~%~?");
	list_heap(&arguments, format, args, NULL);
	instance_simple_error(&pos, control, arguments);
	if (error_common(ptr, pos))
		return;
	setresult_control(ptr, Nil);
}

static void type_method_combination_error(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, T);
	typeargs_var1rest(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_method_combination_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_METHOD_COMBINATION_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_method_combination_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_method_combination_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun signal (datum &rest args) ...) -> nil */
static void function_signal(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-condition */
		instance_simple_condition(&datum, datum, rest);
	}
	else {
		if (function_error_datum(ptr, datum, rest, &datum))
			return;
	}
	if (signal_function(datum))
		return;
	setresult_control(ptr, Nil);
}

static void defun_signal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIGNAL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_signal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defun-simple-condition-format-control (condition) ...) -> t */
static void function_simple_condition_format_control(Execute ptr, addr var)
{
	simple_condition_format_control(var, &var);
	setresult_control(ptr, var);
}

static void type_simple_condition_format_control(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleCondition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, T);
	type_compiled_heap(args, values, ret);
}

static void defun_simple_condition_format_control(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_CONDITION_FORMAT_CONTROL, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_control);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_control(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-condition-format-arguments (conditino) ...) -> list */
static void function_simple_condition_format_arguments(Execute ptr, addr var)
{
	simple_condition_format_arguments(var, &var);
	setresult_control(ptr, var);
}

static void type_simple_condition_format_arguments(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, SimpleCondition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, List);
	type_compiled_heap(args, values, ret);
}

static void defun_simple_condition_format_arguments(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIMPLE_CONDITION_FORMAT_ARGUMENTS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_arguments);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_arguments(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun warn (datum &rest args) ...) -> nil */
static void function_warn(Execute ptr, addr datum, addr rest)
{
	if (stringp(datum)) {
		/* string -> simple-warning */
		instance_simple_warning(&datum, datum, rest);
	}
	else if (function_error_datum(ptr, datum, rest, &datum)) {
		/* throw */
		return;
	}
	warning_restart_case(ptr, datum);
}

static void defun_warn(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WARN, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_warn);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-debugger (condition) ...) -> nil */
static void function_invoke_debugger(Execute ptr, addr var)
{
	if (invoke_debugger(ptr, var)) return;
	setresult_control(ptr, Nil); /* Don't return value */
}

static void type_invoke_debugger(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, Condition);
	typeargs_var1(&args, args);
	GetTypeValues(&values, Nil);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_debugger(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_DEBUGGER, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_invoke_debugger);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_debugger(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun break (&optional format &rest args) ...) -> null */
static void function_break_continue(Execute ptr)
{
	/* do nothing */
}

static int function_break_invoke(Execute ptr, addr format, addr args)
{
	addr symbol, condition;

	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	pushspecial_control(ptr, symbol, Nil);
	instance_simple_condition(&condition, format, args);
	return invoke_debugger(ptr, condition);
}

static int function_break_restart(Execute ptr, addr restart, addr format, addr args)
{
	int check;
	addr control;
	codejump jump;

	/* execute */
	push_restart_initialize_control(ptr, &control);
	check = 0;
	begin_switch(ptr, &jump);
	if (codejump_run_p(&jump)) {
		pushobject_restart_control(ptr, restart);
		check = function_break_invoke(ptr, format, args);
	}
	end_switch(&jump);
	if (check)
		return 1;

	/* restart abort */
	if (jump.code == LISPCODE_CONTROL) {
		if (! equal_control_restart(ptr, control))
			throw_switch(&jump);
		ptr->signal = ExecuteControl_Run;
		return free_control(ptr, control);
	}

	/* free control */
	throw_switch(&jump);
	setresult_control(ptr, Nil);
	return free_control(ptr, control);
}

static int function_break_make(Execute ptr, addr *ret)
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

	return 0;
}

static void function_break(Execute ptr, addr format, addr args)
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

	if (format == Unbound) {
		strvect_char_heap(&format, "Break");
		args = Nil;
	}
	if (function_break_make(ptr, &restart))
		return;
	if (function_break_restart(ptr, restart, format, args))
		return;
	setresult_control(ptr, Nil);
}

static void type_break(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, String);
	GetTypeTable(&values, T);
	typeargs_opt1rest(&args, args, values);
	GetTypeValues(&values, Null);
	type_compiled_heap(args, values, ret);
}

static void defun_break(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_BREAK, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1rest(pos, p_defun_break);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_break(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defvar debugger-hook nil) */
static void type_debugger_hook(addr *ret)
{
	addr args, values;

	/* function */
	GetTypeTable(&values, Asterisk);
	typeargs_var2(&args, values, values);
	type_function_heap(args, values, &args);
	/* null */
	GetTypeTable(&values, Symbol);
	/* or */
	type2or_heap(args, values, ret);
}

static void defvar_debugger_hook(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_DEBUGGER_HOOK, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	type_debugger_hook(&type);
	settype_value_symbol(symbol, type);
}


/* (defvar break-on-signals nil) */
static void defvar_break_on_signals(void)
{
	addr symbol, type;

	/* symbol */
	GetConst(SPECIAL_BREAK_ON_SIGNALS, &symbol);
	SetValueSymbol(symbol, Nil);
	setspecial_symbol(symbol);
	/* type */
	GetTypeTable(&type, TypeSpec);
	settype_value_symbol(symbol, type);
}


/* (defmacro handler-bind (binding &body form) ...)
 * -> (lisp-system::handler
 *      (lisp-system::handler-bind name1 lambda1 ...)
 *      body...)
 */
static void handler_bind_clauses(Execute ptr, addr right, addr *ret)
{
	addr cons, symbol, quote, root, name, lambda, temp;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_HANDLER_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		getcons(right, &cons, &right);
		/* (name lambda) */
		lista_bind(cons, &name, &lambda, &temp, NULL);
		if (temp != Nil)
			fmte("handler-bind argument ~S must be a (name lambda) form.", cons, NULL);
		/* (push 'name root) */
		list_heap(&name, quote, name, NULL);
		cons_heap(&root, name, root);
		/* (push (lambda ...) root) */
		cons_heap(&root, lambda, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void function_handler_bind(Execute ptr, addr right, addr env)
{
	addr symbol, body;

	GetCdr(right, &right);
	if (! consp(right))
		fmte("Too few handler-bind argument.", NULL);
	GetCons(right, &right, &body);
	if (right == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(&right, symbol, right);
		setresult_control(ptr, right);
		return;
	}

	GetConst(SYSTEM_HANDLER, &symbol);
	handler_bind_clauses(ptr, right, &right);
	if (body == Nil)
		consnil_heap(&body);
	lista_heap(&right, symbol, right, body, NULL);
	setresult_control(ptr, right);
}

static void defmacro_handler_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_BIND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_handler_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro handler-case (expr &rest clause*) ...)
 * -> (lisp-system::handler
 *      (lisp-system::handler-case name1 lambda1 ...)
 *      expr)
 * -> (lisp-system::handler
 *      (lisp-system::handler-case name1 lambda1 ...)
 *      (multiple-value-call
 *        (lambda (a b c d) ...)
 *        expr))
 */
static void handler_case_lambda_gensym(Execute ptr, addr form, addr *ret)
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

static void handler_case_lambda(Execute ptr, addr args, addr form, addr *ret)
{
	addr pos;
	GetConst(COMMON_LAMBDA, &pos);
	lista_heap(ret, pos, args, form, NULL);
}

static void handler_case_noerror(addr *noerror, addr cons)
{
	if (*noerror)
		fmtw("There are multiple :no-error clauses ~S in handler-case.", cons, NULL);
	else
		*noerror = cons;
}

static void handler_case_clauses(Execute ptr, addr right, addr *ret, addr *reterror)
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
		getcons(right, &cons, &right);
		/* (name (c) form) */
		lista_bind(cons, &name, &args, &form, NULL);
		if (name == keyword)
			handler_case_noerror(&noerror, cons);
		else {
			if (args == Nil)
				handler_case_lambda_gensym(ptr, form, &cons);
			else if (singlep(args))
				handler_case_lambda(ptr, args, form, &cons);
			else {
				fmte("The argument ~S in handler-case clause "
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
	*reterror = noerror;
}

static void handler_case_body(addr noerror, addr expr, addr *ret)
{
	addr pos;

	if (noerror) {
		/* (multiple-value-call
		 *   (lambda (a b c d) ...)
		 *   expr))
		 */
		GetConst(COMMON_LAMBDA, &pos);
		cons_heap(&noerror, pos, noerror);
		GetConst(COMMON_MULTIPLE_VALUE_CALL, &pos);
		list_heap(ret, pos, noerror, expr, NULL);
	}
	else {
		*ret = expr;
	}
}

static void function_handler_case(Execute ptr, addr right, addr env)
{
	addr symbol, expr, noerror;

	GetCdr(right, &right);
	if (! consp(right))
		fmte("Too few handler-case argument.", NULL);
	GetCons(right, &expr, &right);
	if (right == Nil) {
		setresult_control(ptr, expr);
		return;
	}

	GetConst(SYSTEM_HANDLER, &symbol);
	handler_case_clauses(ptr, right, &right, &noerror);
	handler_case_body(noerror, expr, &expr);
	list_heap(&right, symbol, right, expr, NULL);
	setresult_control(ptr, right);
}

static void defmacro_handler_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_CASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_handler_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ignore-errors (&body form) ...) -> t */
static void function_ignore_errors(Execute ptr, addr form, addr env)
{
	/* `(handler-case (progn ,@form)
	 *    (error (g) (values nil g)))
	 */
	addr handler, progn, error, values, g;

	getcdr(form, &form);
	GetConst(COMMON_HANDLER_CASE, &handler);
	GetConst(COMMON_PROGN, &progn);
	GetConst(COMMON_ERROR, &error);
	GetConst(COMMON_VALUES, &values);
	make_gensym(ptr, &g);
	list_heap(&values, values, Nil, g, NULL);
	list_heap(&g, g, NULL);
	list_heap(&error, error, g, values, NULL);
	cons_heap(&progn, progn, form);
	list_heap(&handler, handler, progn, error, NULL);
	setresult_control(ptr, handler);
}

static void defmacro_ignore_errors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IGNORE_ERRORS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ignore_errors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-condition (name (supers) (slots) &rest option) ...) -> name */
static void function_define_condition(Execute ptr, addr form, addr env)
{
	if (define_condition_common(ptr, form, env, &form)) return;
	setresult_control(ptr, form);
}

static void defmacro_define_condition(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_CONDITION, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_condition);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*  (defun make-condition (type) &rest args) ...) -> condition */
static void function_make_condition(Execute ptr, addr args)
{
	addr call;

	GetConst(COMMON_MAKE_INSTANCE, &call);
	getfunctioncheck_local(ptr, call, &call);
	if (callclang_apply(ptr, &args, call, args)) return ;
	setresult_control(ptr, args);
}

static void type_make_condition(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, T);
	GetTypeTable(&values, ConditionDesigner);
	typeargs_var1rest(&args, values, args);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_make_condition(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MAKE_CONDITION, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_dynamic(pos, p_defun_make_condition);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_make_condition(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun compute-restarts (&optional condition) -> restarts
 *    condition  (or condition null)
 *    restarts   list
 */
static void function_compute_restarts(Execute ptr, addr pos)
{
	if (pos == Unbound) pos = Nil;
	compute_restarts_control(ptr, pos, &pos);
	setresult_control(ptr, pos);
}

static void type_compute_restarts(addr *ret)
{
	addr args, values;

	GetTypeArgs(&args, OptConditionNull);
	GetTypeTable(&values, Restart);
	typevalues_rest(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_compute_restarts(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPUTE_RESTARTS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_compute_restarts);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_compute_restarts(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun find-restart (identifier &optional condition) -> restart
 *     ideitifier  (or restart (and symbol (not null)))  ;; restart-designer
 *     condition   (or condition null)
 *     restart     (or restart null)
 */
static void function_find_restart(Execute ptr, addr var, addr opt)
{
	int check;

	if (opt == Unbound) opt = Nil;
	check = find_restart_control(ptr, var, opt, &var);
	setresult_control(ptr, check? var: Nil);
}

static void type_find_restart(addr *ret)
{
	addr condition, args, values;

	GetTypeTable(&args, RestartDesigner);
	GetTypeTable(&condition, ConditionNull);
	typeargs_var1opt1(&args, args, condition);
	/* restart */
	GetTypeTable(&values, RestartNull);
	typevalues_result(&values, values);
	type_compiled_heap(args, values, ret);
}

static void defun_find_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_RESTART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_find_restart);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_find_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-restart (restart &rest arguments) ...) -> result*
 *   restart    (or restart (and symbol (not null)))  ;; restart-designer
 *   arguments  &rest t
 *   result*    *
 */
static void function_invoke_restart(Execute ptr, addr var, addr rest)
{
	(void)invoke_restart_control(ptr, var, rest);
}

static void type_invoke_restart(addr *ret)
{
	addr args, values, restart;

	GetTypeTable(&restart, RestartDesigner);
	GetTypeTable(&args, T);
	typeargs_var1rest(&args, restart, args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1dynamic(pos, p_defun_invoke_restart);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_restart(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-restart-interactively (restart) ...) -> result*
 *   restart  (or restart (and symbol (not null)))  ;; restart-designer
 *   result*  *
 */
static void function_invoke_restart_interactively(Execute ptr, addr var)
{
	(void)invoke_restart_interactively_control(ptr, var);
}

static void type_invoke_restart_interactively(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, RestartDesigner);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(args, values, ret);
}

static void defun_invoke_restart_interactively(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART_INTERACTIVELY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_invoke_restart_interactively);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_restart_interactively(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro restart-bind (binding &body form) ...)
 * -> (lisp-system::restart
 *      (lisp-system::restart-bind (list ...) (list ...) ...)
 *      body...)
 */
static void restart_bind_interactive(addr *args, addr *inter)
{
	if (*args == Nil)
		fmte("The key :interactive-function must have a value.", NULL);
	if (*inter != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, inter, args);
		if (*inter == Nil)
			fmte(":interactive-function ~S must be a function.", *inter, NULL);
	}
}

static void restart_bind_report(addr *args, addr *report)
{
	if (*args == Nil)
		fmte("The key :report-function must have a value.", NULL);
	if (*report != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, report, args);
		if (*report == Nil)
			fmte(":report-function ~S must be a function.", *report, NULL);
	}
}

static void restart_bind_test(addr *args, addr *test)
{
	if (*args == Nil)
		fmte("The key :test-function must have a value.", NULL);
	if (*test != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, test, args);
		if (*test == Nil)
			fmte(":test-function ~S must be a function.", *test, NULL);
	}
}

static void restart_function_symbol(addr pos, addr *ret)
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

static void restart_bind_binding(addr args, addr *ret)
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
		getcons(args, &pos, &args);
		if (pos == keyinter)
			restart_bind_interactive(&args, &inter);
		else if (pos == keyreport)
			restart_bind_report(&args, &report);
		else if (pos == keytest)
			restart_bind_test(&args, &test);
		else
			fmte("Invalid key parameter ~S.", pos, NULL);
	}
	restart_function_symbol(inter, &inter);
	restart_function_symbol(report, &report);
	restart_function_symbol(test, &test);
	list_heap(&name, quote, name, NULL);
	list_heap(ret, list, name, lambda, inter, report, test, NULL);
}

static void restart_bind_clauses(Execute ptr, addr right, addr *ret)
{
	addr cons, symbol, quote, root;

	GetConst(COMMON_QUOTE, &quote);
	GetConst(SYSTEM_RESTART_BIND, &symbol);
	conscar_heap(&root, symbol);
	while (right != Nil) {
		getcons(right, &cons, &right);
		restart_bind_binding(cons, &cons);
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void function_restart_bind(Execute ptr, addr right, addr env)
{
	addr symbol, body;

	GetCdr(right, &right);
	if (! consp(right))
		fmte("Too few restart-bind argument.", NULL);
	GetCons(right, &right, &body);
	if (right == Nil) {
		GetConst(COMMON_PROGN, &symbol);
		cons_heap(&right, symbol, right);
		setresult_control(ptr, right);
		return;
	}

	GetConst(SYSTEM_RESTART, &symbol);
	restart_bind_clauses(ptr, right, &right);
	if (body == Nil)
		consnil_heap(&body);
	lista_heap(&right, symbol, right, body, NULL);
	setresult_control(ptr, right);
}

static void defmacro_restart_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_BIND, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_restart_bind);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro restart-case (expr &rest clause*) ...)
 * -> (lisp-system::restart
 *      (lisp-system::restart-case (list ...) (list ...) ...)
 *      body...)
 */
static void restart_case_interactive(addr *args, addr *inter)
{
	if (*args == Nil)
		fmte("The key :interactive must have a value.", NULL);
	if (*inter != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, inter, args);
		if (*inter == Nil)
			fmte(":interactive ~S must be a function.", *inter, NULL);
	}
}

static void restart_case_report(addr *args, addr *report)
{
	if (*args == Nil)
		fmte("The key :report must have a value.", NULL);
	if (*report != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, report, args);
		if (*report == Nil)
			fmte(":report ~S must be a function.", *report, NULL);
	}
}

static void restart_case_test(addr *args, addr *test)
{
	if (*args == Nil)
		fmte("The key :test must have a value.", NULL);
	if (*test != Nil)
		getcdr(*args, args);
	else {
		getcons(*args, test, args);
		if (*test == Nil)
			fmte(":test ~S must be a function.", *test, NULL);
	}
}

static void restart_case_clauses(addr right, addr *ret)
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
		getcons(right, &cons, &right);
		/* (name (c) form) */
		lista_bind(cons, &name, &ord, &form, NULL);
		inter = report = test = Nil;
		for (; form != Nil; form = cons) {
			getcons(form, &pos, &cons);
			if (pos == keyinter)
				restart_case_interactive(&cons, &inter);
			else if (pos == keyreport)
				restart_case_report(&cons, &report);
			else if (pos == keytest)
				restart_case_test(&cons, &test);
			else
				break;
		}
		/* symbol */
		restart_function_symbol(inter, &inter);
		restart_function_symbol(report, &report);
		restart_function_symbol(test, &test);
		/* (list 'name (lambda ...) ...) */
		list_heap(&name, quote, name, NULL);
		lista_heap(&form, lambda, ord, form, NULL);
		list_heap(&cons, list, name, form, inter, report, test, NULL);
		cons_heap(&root, cons, root);
	}
	/* result */
	nreverse_list_unsafe(ret, root);
}

static void function_restart_case(Execute ptr, addr right, addr env)
{
	addr symbol, expr;

	GetCdr(right, &right);
	if (! consp(right))
		fmte("Too few restart-case argument.", NULL);
	GetCons(right, &expr, &right);
	if (right == Nil) {
		setresult_control(ptr, expr);
		return;
	}

	GetConst(SYSTEM_RESTART, &symbol);
	restart_case_clauses(right, &right);
	list_heap(&right, symbol, right, expr, NULL);
	setresult_control(ptr, right);
}

static void defmacro_restart_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_CASE, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_restart_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun restart-name (restart) ...) -> symbol */
static void function_restart_name(Execute ptr, addr var)
{
	getname_restart(var, &var);
	setresult_control(ptr, var);
}

static void type_restart_name(addr *ret)
{
	addr args, values;

	GetTypeTable(&args, RestartDesigner);
	typeargs_var1(&args, args);
	GetTypeTable(&values, Symbol);
	type_compiled_heap(args, values, ret);
}

static void defun_restart_name(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_RESTART_NAME, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, p_defun_restart_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_restart_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defmacro with-condition-restarts
 *    (condition-form restarts-form &rest form) ...) -> result*
 *    condition-form  list
 *    restarts-form   list
 *    form            &rest t
 *    result*         *
 */
static void function_with_condition_restarts(Execute ptr, addr right, addr env)
{
	addr condition, cons, symbol;

	GetCdr(right, &right);
	if (! consp(right))
		fmte("Too few with-condition-restarts argument.", NULL);
	GetCons(right, &condition, &right);
	if (! consp(right))
		fmte("Too few with-condition-restarts argument.", NULL);
	GetCons(right, &cons, &right);
	if (right == Nil) {
		consnil_heap(&right);
	}
	/* (lisp-system::push-return
	 *   (lisp-system::redirect-restart condition cons)
	 *   right)
	 */
	GetConst(SYSTEM_REDIRECT_RESTART, &symbol);
	list_heap(&cons, symbol, condition, cons, NULL);
	GetConst(SYSTEM_PUSH_RETURN, &symbol);
	lista_heap(&cons, symbol, cons, right, NULL);
	setresult_control(ptr, cons);
}

static void defmacro_with_condition_restarts(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_CONDITION_RESTARTS, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_condition_restarts);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro with-simple-restart ((name format &rest args) &body body) ...)
 *     -> result
 *   name    symbol  ;; restart-name
 *   format  format-control
 *   args    format-arguments
 *   body    progn
 *   result  (values t t)
 */
static void function_with_simple_restart(Execute ptr, addr form, addr env)
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
	getcdr(form, &form);
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
	list_heap(&restart, restart, progn, name, NULL);
	setresult_control(ptr, restart);
	return;

error:
	fmte("WITH-SIMPLE-RESTART arguments ~S must be "
			"((name format ...) &body body) form.", form, NULL);
}

static void defmacro_with_simple_restart(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SIMPLE_RESTART, &symbol);
	compiled_macro_heap(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_simple_restart);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  (defun abort (&optional condition) ...) -> |
 */
static void function_abort(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_ABORT, &pos);
	if (! find_restart_control(ptr, pos, opt, &opt))
		fmte("The restart name ~S is not found.", pos, NULL);
	(void)invoke_restart_control(ptr, opt, Nil);
}

static void defun_abort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ABORT, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_abort);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Abort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun continue (&optional condition) ...) -> nil
 */
static void function_continue(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_CONTINUE, &pos);
	if (find_restart_control(ptr, pos, opt, &opt)) {
		if (invoke_restart_control(ptr, opt, Nil)) {
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_continue(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONTINUE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_continue);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Continue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun muffle-warning (&optional condition) ...) -> |
 */
static void function_muffle_warning(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_MUFFLE_WARNING, &pos);
	if (! find_restart_control(ptr, pos, opt, &opt))
		control_error();
	(void)invoke_restart_control(ptr, opt, Nil);
}

static void defun_muffle_warning(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MUFFLE_WARNING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, p_defun_muffle_warning);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Abort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun store-value (object &optional condition) ...) -> nil
 */
static void function_store_value(Execute ptr, addr var, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_STORE_VALUE, &pos);
	if (find_restart_control(ptr, pos, opt, &opt)) {
		list_local(ptr->local, &var, var, NULL);
		if (invoke_restart_control(ptr, opt, var)) {
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_store_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STORE_VALUE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_store_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StoreValue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun use-value (&optional condition) ...) -> nil
 */
static void function_use_value(Execute ptr, addr var, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_USE_VALUE, &pos);
	if (find_restart_control(ptr, pos, opt, &opt)) {
		list_local(ptr->local, &var, var, NULL);
		if (invoke_restart_control(ptr, opt, var)) {
			return;
		}
	}
	setresult_control(ptr, Nil);
}

static void defun_use_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_VALUE, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, p_defun_use_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, StoreValue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  function
 */
_g void init_common_conditions(void)
{
	SetPointerCall(defun, var1, cell_error_name);
	SetPointerCall(defmacro, macro, assert);
	SetPointerCall(defun, var1rest, error);
	SetPointerCall(defun, var2rest, cerror);
	SetPointerCall(defun, empty, cerror_continue);
	SetPointerCall(defmacro, macro, check_type);
	SetPointerCall(defun, var2rest, invalid_method_error);
	SetPointerCall(defun, var1rest, method_combination_error);
	SetPointerCall(defun, var1rest, signal);
	SetPointerCall(defun, var1, simple_condition_format_control);
	SetPointerCall(defun, var1, simple_condition_format_arguments);
	SetPointerCall(defun, var1rest, warn);
	SetPointerCall(defun, var1, invoke_debugger);
	SetPointerCall(defun, opt1rest, break);
	SetPointerCall(defun, empty, break_continue);
	SetPointerCall(defmacro, macro, handler_bind);
	SetPointerCall(defmacro, macro, handler_case);
	SetPointerCall(defmacro, macro, ignore_errors);
	SetPointerCall(defmacro, macro, define_condition);
	SetPointerCall(defun, dynamic, make_condition);
	SetPointerCall(defun, opt1, compute_restarts);
	SetPointerCall(defun, var1opt1, find_restart);
	SetPointerCall(defun, var1dynamic, invoke_restart);
	SetPointerCall(defun, var1, invoke_restart_interactively);
	SetPointerCall(defmacro, macro, restart_bind);
	SetPointerCall(defmacro, macro, restart_case);
	SetPointerCall(defun, var1, restart_name);
	SetPointerCall(defmacro, macro, with_condition_restarts);
	SetPointerCall(defmacro, macro, with_simple_restart);
	SetPointerCall(defun, opt1, abort);
	SetPointerCall(defun, opt1, continue);
	SetPointerCall(defun, opt1, muffle_warning);
	SetPointerCall(defun, var1opt1, store_value);
	SetPointerCall(defun, var1opt1, use_value);
}

_g void build_common_conditions(void)
{
	defun_cell_error_name();
	defmacro_assert();
	defun_error();
	defun_cerror();
	defmacro_check_type();
	defun_invalid_method_error();
	defun_method_combination_error();
	defun_signal();
	defun_simple_condition_format_control();
	defun_simple_condition_format_arguments();
	defun_warn();
	defun_invoke_debugger();
	defun_break();
	defvar_debugger_hook();
	defvar_break_on_signals();
	defmacro_handler_bind();
	defmacro_handler_case();
	defmacro_ignore_errors();
	defmacro_define_condition();
	defun_make_condition();
	defun_compute_restarts();
	defun_find_restart();
	defun_invoke_restart();
	defun_invoke_restart_interactively();
	defmacro_restart_bind();
	defmacro_restart_case();
	defun_restart_name();
	defmacro_with_condition_restarts();
	defmacro_with_simple_restart();
	defun_abort();
	defun_continue();
	defun_muffle_warning();
	defun_store_value();
	defun_use_value();
}

