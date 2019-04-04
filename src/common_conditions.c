/*
 *  ANSI COMMON LISP: 9. Conditions
 */
#include "clos.h"
#include "clos_common.h"
#include "cons.h"
#include "common_header.h"
#include "hashtable.h"
#include "strtype.h"
#include "type_parse.h"
#include "unicode.h"

/* (defun error (datum &rest args) ...) -> nil
 *   datum  (or string symbol condition)
 *   args   (&rest t)
 *   nil    nil  ;; not null
 */
static void function_error(Execute ptr, addr datum, addr rest)
{
	addr make_instance;

	/* string -> simple-error */
	if (stringp(datum)) {
		simple_error(datum, rest);
		goto abort;
	}

	/* symbol -> (make-instance symbol ...) */
	if (symbolp(datum)) {
		clos_find_class(datum, &datum);
		if (! conditionp(datum))
			fmte("The class ~S is not a condition subclass.", datum, NULL);
		GetConst(COMMON_MAKE_INSTANCE, &make_instance);
		if (callclang_funcall(ptr, &datum, make_instance, datum, rest, NULL)) return;
		error_function(datum);
		goto abort;
	}

	/* condition -> (error condition) */
	if (condition_instance_p(datum)) {
		if (rest != Nil)
			fmte("The error argument ~S must be a nil "
					"if first argument is condition type.", datum, NULL);
		error_function(datum);
		goto abort;
	}
	fmte("Invalid error argument ~S.", datum, NULL);

abort:
	/* The error function may not return normally. */
	setvalues_nil_control(ptr);
}

static void type_error_function(addr *ret)
{
	addr arg, values, string, symbol, condition, rest;

	GetTypeTable(&string, String);
	GetTypeTable(&symbol, Symbol);
	GetTypeTable(&condition, Condition);
	type3or_heap(string, symbol, condition, &arg);
	GetTypeTable(&rest, T);
	typeargs_var1rest(&arg, arg, rest);
	GetTypeValues(&values, Nil);
	type_compiled_heap(arg, values, ret);
}

static void defun_error(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ERROR, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, function_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_error_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
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
	setcompiled_macro(pos, function_handler_bind);
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
	setcompiled_macro(pos, function_handler_case);
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
	setcompiled_macro(pos, function_define_condition);
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
	GetTypeTable(&values, Condition);
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
	setcompiled_dynamic(pos, function_make_condition);
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
	addr arg, values;

	GetTypeArgs(&arg, OptConditionNull);
	GetTypeTable(&values, Restart);
	typevalues_rest(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_compute_restarts(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_COMPUTE_RESTARTS, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, function_compute_restarts);
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
	addr condition, arg, values;

	GetTypeTable(&arg, RestartDesigner);
	GetTypeTable(&condition, ConditionNull);
	typeargs_var1opt1(&arg, arg, condition);
	/* restart */
	GetTypeTable(&values, RestartNull);
	typevalues_result(&values, values);
	type_compiled_heap(arg, values, ret);
}

static void defun_find_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_FIND_RESTART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1opt1(pos, function_find_restart);
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
static void invoke_restart_function(Execute ptr, addr var, addr rest)
{
	(void)invoke_restart_control(ptr, var, rest);
}

static void type_invoke_restart(addr *ret)
{
	addr arg, values, restart;

	GetTypeTable(&restart, RestartDesigner);
	GetTypeTable(&arg, T);
	typeargs_var1rest(&arg, restart, arg);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(arg, values, ret);
}

static void defun_invoke_restart(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1rest(pos, invoke_restart_function);
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
static void invoke_restart_interactively_function(Execute ptr, addr var)
{
	(void)invoke_restart_interactively_control(ptr, var);
}

static void type_invoke_restart_interactively(addr *ret)
{
	addr arg, values, restart;

	GetTypeTable(&restart, RestartDesigner);
	typeargs_var1(&arg, restart);
	GetTypeTable(&values, Asterisk);
	type_compiled_heap(arg, values, ret);
}

static void defun_invoke_restart_interactively(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_INVOKE_RESTART_INTERACTIVELY, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_var1(pos, invoke_restart_interactively_function);
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
	setcompiled_macro(pos, function_restart_bind);
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
	setcompiled_macro(pos, function_restart_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
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
	setcompiled_macro(pos, function_with_condition_restarts);
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
	setcompiled_opt1(pos, function_abort);
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
	setcompiled_opt1(pos, function_continue);
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
		fmte("The restart name ~S is not found.", pos, NULL);
	(void)invoke_restart_control(ptr, opt, Nil);
}

static void defun_muffle_warning(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MUFFLE_WARNING, &symbol);
	compiled_heap(&pos, symbol);
	setcompiled_opt1(pos, function_muffle_warning);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Abort);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun store-value (&optional condition) ...) -> nil
 */
static void function_store_value(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_STORE_VALUE, &pos);
	if (find_restart_control(ptr, pos, opt, &opt)) {
		if (invoke_restart_control(ptr, opt, Nil)) {
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
	setcompiled_opt1(pos, function_store_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Continue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  (defun use-value (&optional condition) ...) -> nil
 */
static void function_use_value(Execute ptr, addr opt)
{
	addr pos;

	if (opt == Unbound) opt = Nil;
	GetConst(COMMON_USE_VALUE, &pos);
	if (find_restart_control(ptr, pos, opt, &opt)) {
		if (invoke_restart_control(ptr, opt, Nil)) {
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
	setcompiled_opt1(pos, function_use_value);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Continue);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/*
 *  intern
 */
void intern_common_conditions(void)
{
	/* defun_cell_error_name(); */
	/* defmacro_assert(); */
	defun_error();
	/* defun_cerror(); */
	/* defmacro_check_type(); */
	/* defun_invalid_method_error(); */
	/* defun_method_combination_error(); */
	/* defun_signal(); */
	/* defun_simple_condition_format_control(); */
	/* defun_simple_condition_format_arguments(); */
	/* defun_warn(); */
	/* defun_invoke_debugger(); */
	/* defun_break(); */
	/* defvar_debugger_hook(); */
	/* defvar_break_on_signals(); */
	defmacro_handler_bind();
	defmacro_handler_case();
	/* defmacro_ignore_errors(); */
	defmacro_define_condition();
	defun_make_condition();
	defun_compute_restarts();
	defun_find_restart();
	defun_invoke_restart();
	defun_invoke_restart_interactively();
	defmacro_restart_bind();
	defmacro_restart_case();
	/* defun_restart_name(); */
	defmacro_with_condition_restarts();
	/* defmacro_with_simple_restart(); */
	defun_abort();
	defun_continue();
	defun_muffle_warning();
	defun_store_value();
	defun_use_value();
}

