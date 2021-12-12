/*
 *  ANSI COMMON LISP: 9. Conditions
 */
#include "call_conditions.h"
#include "call_objects.h"
#include "condition.h"
#include "condition_debugger.h"
#include "common_header.h"
#include "restart.h"

/* (defun cell-error-name (condition) ...) -> t */
static int function_cell_error_name(Execute ptr, addr var)
{
	Return(cell_error_name_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_cell_error_name);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cell_error_name(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro assert (test &optional (place*) format &rest args) ...) -> nil */
static int function_assert(Execute ptr, addr form, addr env)
{
	Return(assert_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_assert(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_ASSERT, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_error(Execute ptr, addr datum, addr rest)
{
	Return(error_common_(ptr, datum, rest));
	setvalues_nil_control(ptr);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_error_function(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun cerror (continue-format datum &args) ...) -> null */
static int function_cerror(Execute ptr, addr restart, addr datum, addr rest)
{
	Return(cerror_common_(ptr, restart, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_cerror);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_cerror(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defmacro check-type (place type &optional string) ...) -> null */
static int function_check_type(Execute ptr, addr form, addr env)
{
	Return(check_type_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_check_type(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_CHECK_TYPE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_check_type);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun invalid-method-error (method format &rest args) ...) -> null */
static int function_invalid_method_error(Execute ptr,
		addr method, addr format, addr args)
{
	Return(invalid_method_error_common_(ptr, method, format, args));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var2rest(pos, p_defun_invalid_method_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invalid_method_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun method-combination-error (format &rest args) ...) -> null */
static int function_method_combination_error(Execute ptr, addr format, addr args)
{
	Return(method_combination_error_common_(ptr, format, args));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_method_combination_error);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_method_combination_error(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun signal (datum &rest args) ...) -> nil */
static int function_signal(Execute ptr, addr datum, addr rest)
{
	Return(signal_common_(ptr, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_signal(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_SIGNAL, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_signal);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun defun-simple-condition-format-control (condition) ...) -> t */
static int function_simple_condition_format_control(Execute ptr, addr var)
{
	Return(simple_condition_format_control_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_control);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_control(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun simple-condition-format-arguments (conditino) ...) -> list */
static int function_simple_condition_format_arguments(Execute ptr, addr var)
{
	Return(simple_condition_format_arguments_(var, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_simple_condition_format_arguments);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_simple_condition_format_arguments(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun warn (datum &rest args) ...) -> nil */
static int function_warn(Execute ptr, addr datum, addr rest)
{
	Return(warn_common_(ptr, datum, rest));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_warn(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_WARN, &symbol);
	compiled_system(&pos, symbol);
	setcompiled_var1rest(pos, p_defun_warn);
	SetFunctionCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, Signal);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun invoke-debugger (condition) ...) -> nil */
static int function_invoke_debugger(Execute ptr, addr var)
{
	Return(invoke_debugger_(ptr, var));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
	setcompiled_var1(pos, p_defun_invoke_debugger);
	SetFunctionCommon(symbol, pos);
	/* type */
	type_invoke_debugger(&type);
	settype_function(pos, type);
	settype_function_symbol(symbol, type);
}


/* (defun break (&optional format &rest args) ...) -> null */
static int function_break(Execute ptr, addr format, addr args)
{
	Return(break_common_(ptr, format, args));
	setresult_control(ptr, Nil);
	return 0;
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
	compiled_system(&pos, symbol);
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
	addr type1, type2;

	GetTypeTable(&type1, Function);
	GetTypeTable(&type2, Symbol);
	type2or_heap(type1, type2, ret);
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
static int function_handler_bind(Execute ptr, addr form, addr env)
{
	Return(handler_bind_common_(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_handler_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_handler_case(Execute ptr, addr right, addr env)
{
	Return(handler_case_common_(ptr, right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_handler_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_HANDLER_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_handler_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro ignore-errors (&body form) ...) -> t */
static int function_ignore_errors(Execute ptr, addr form, addr env)
{
	Return(ignore_errors_common_(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_ignore_errors(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_IGNORE_ERRORS, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_ignore_errors);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defmacro define-condition (name (supers) (slots) &rest option) ...) -> name */
static int function_define_condition(Execute ptr, addr form, addr env)
{
	Return(define_condition_common(ptr, form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_define_condition(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_DEFINE_CONDITION, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_define_condition);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*  (defun make-condition (type) &rest args) ...) -> condition */
static int function_make_condition(Execute ptr, addr args)
{
	Return(make_condition_common(ptr, args, &args));
	setresult_control(ptr, args);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_compute_restarts(Execute ptr, addr pos)
{
	Return(compute_restarts_common_(ptr, pos, &pos));
	setresult_control(ptr, pos);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_find_restart(Execute ptr, addr var, addr opt)
{
	Return(find_restart_common_(ptr, var, opt, &var));
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_invoke_restart(Execute ptr, addr var, addr rest)
{
	return invoke_restart_control_(ptr, var, rest);
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
	compiled_system(&pos, symbol);
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
static int function_invoke_restart_interactively(Execute ptr, addr var)
{
	return invoke_restart_interactively_control_(ptr, var);
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
	compiled_system(&pos, symbol);
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
static int function_restart_bind(Execute ptr, addr right, addr env)
{
	Return(restart_bind_common(right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_restart_bind(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_BIND, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_restart_case(Execute ptr, addr right, addr env)
{
	Return(restart_case_common(right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_restart_case(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_RESTART_CASE, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_restart_case);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/* (defun restart-name (restart) ...) -> symbol */
static int function_restart_name(Execute ptr, addr var)
{
	getname_restart(var, &var);
	setresult_control(ptr, var);
	return 0;
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
	compiled_system(&pos, symbol);
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
static int function_with_condition_restarts(Execute ptr, addr right, addr env)
{
	Return(with_condition_restarts_common(right, env, &right));
	setresult_control(ptr, right);
	return 0;
}

static void defmacro_with_condition_restarts(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_CONDITION_RESTARTS, &symbol);
	compiled_macro_system(&pos, symbol);
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
static int function_with_simple_restart(Execute ptr, addr form, addr env)
{
	Return(with_simple_restart_common(form, env, &form));
	setresult_control(ptr, form);
	return 0;
}

static void defmacro_with_simple_restart(void)
{
	addr symbol, pos, type;

	GetConst(COMMON_WITH_SIMPLE_RESTART, &symbol);
	compiled_macro_system(&pos, symbol);
	setcompiled_macro(pos, p_defmacro_with_simple_restart);
	SetMacroCommon(symbol, pos);
	/* type */
	GetTypeCompiled(&type, MacroFunction);
	settype_function(pos, type);
}


/*
 *  (defun abort (&optional condition) ...) -> |
 */
static int function_abort(Execute ptr, addr opt)
{
	return abort_common(ptr, opt);
}

static void defun_abort(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_ABORT, &symbol);
	compiled_system(&pos, symbol);
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
static int function_continue(Execute ptr, addr opt)
{
	Return(continue_common(ptr, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_continue(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_CONTINUE, &symbol);
	compiled_system(&pos, symbol);
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
static int function_muffle_warning(Execute ptr, addr opt)
{
	return muffle_warning_common(ptr, opt);
}

static void defun_muffle_warning(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_MUFFLE_WARNING, &symbol);
	compiled_system(&pos, symbol);
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
static int function_store_value(Execute ptr, addr var, addr opt)
{
	Return(store_value_common(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_store_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_STORE_VALUE, &symbol);
	compiled_system(&pos, symbol);
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
static int function_use_value(Execute ptr, addr var, addr opt)
{
	Return(use_value_common(ptr, var, opt));
	setresult_control(ptr, Nil);
	return 0;
}

static void defun_use_value(void)
{
	addr symbol, pos, type;

	/* function */
	GetConst(COMMON_USE_VALUE, &symbol);
	compiled_system(&pos, symbol);
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
void init_common_conditions(void)
{
	SetPointerCall(defun, var1, cell_error_name);
	SetPointerCall(defmacro, macro, assert);
	SetPointerCall(defun, var1rest, error);
	SetPointerCall(defun, var2rest, cerror);
	SetPointerCall(defmacro, macro, check_type);
	SetPointerCall(defun, var2rest, invalid_method_error);
	SetPointerCall(defun, var1rest, method_combination_error);
	SetPointerCall(defun, var1rest, signal);
	SetPointerCall(defun, var1, simple_condition_format_control);
	SetPointerCall(defun, var1, simple_condition_format_arguments);
	SetPointerCall(defun, var1rest, warn);
	SetPointerCall(defun, var1, invoke_debugger);
	SetPointerCall(defun, opt1rest, break);
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

void build_common_conditions(void)
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

