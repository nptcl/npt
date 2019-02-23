#include "code.h"
#include "condition.h"
#include "cons.h"
#include "constant.h"
#include "control.h"
#include "copy.h"
#include "eval_declare.h"
#include "eval_table.h"
#include "function.h"
#include "format.h"
#include "heap.h"
#include "info.h"
#include "integer.h"
#include "lisp.h"
#include "number.h"
#include "object.h"
#include "package.h"
#include "sequence.h"
#include "symbol.h"
#include "type.h"
#include "type_value.h"

static void check_type_error(addr value, addr type)
{
	type_error(value, type);
}


/*****************************************************************************
 *  code function
 *****************************************************************************/
/*
 *  system
 */
/* ARGSUSED0 */
static void hello_code(Execute ptr, addr right)
{
	info("Hello code.");
}

/* ARGSUSED0 */
static void nop_code(Execute ptr, addr right)
{
	/* do nothing */
}

/* ARGSUSED0 */
static void abort_code(Execute ptr, addr right)
{
	Abort("abort-code");
}

/* ARGSUSED0 */
static void error_code(Execute ptr, addr right)
{
	fmte("error-code", NULL);
}

static void info_code(Execute ptr, addr right)
{
	GetCar(right, &right);
	infobit(right);
}

static void print_code(Execute ptr, addr right)
{
	GetCar(right, &right);
	infoprint(right);
}

/* ARGSUSED0 */
static void end_code_operator(Execute ptr, addr right)
{
	ptr->signal = ExecuteControl_End;
}

static void execute_code(Execute ptr, addr right)
{
	runcode_control(ptr, right);
}


/*
 *  object
 */
static void set_code(Execute ptr, addr right)
{
	setresult_control(ptr, right);
}

static void push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, right);
}

static void push_result_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	pushargs_control(ptr, right);
}

static void push_values_code(Execute ptr, addr right)
{
	pushargs_allvalues(ptr);
}

static void nil_set_code(Execute ptr, addr right)
{
	setresult_control(ptr, Nil);
}

static void nil_push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, Nil);
}

static void t_set_code(Execute ptr, addr right)
{
	setresult_control(ptr, T);
}

static void t_push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, T);
}


/*
 *  declaim
 */
static void declaim_special_code(Execute ptr, addr right)
{
	setspecial_symbol(right);
}

static void declaim_type_value_code(Execute ptr, addr right)
{
	addr symbol;

	List_bind(right, &symbol, &right, NULL);
	settype_value_symbol(symbol, right);
}

static void declaim_type_function_code(Execute ptr, addr right)
{
	addr key, symbol;

	List_bind(right, &key, &right, NULL);
	GetCallName(key, &symbol);
	if (symbol_callname_p(key))
		settype_function_symbol(symbol, right);
	else
		settype_setf_symbol(symbol, right);
}

static void declaim_inline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbol_callname_p(right))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);
}

static void declaim_notinline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbol_callname_p(right))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);
}

static void declaim_compilation_code(Execute ptr, addr right)
{
	apply_compilation_speed_declaim((OptimizeType)RefFixnum(right));
}

static void declaim_debug_code(Execute ptr, addr right)
{
	apply_debug_declaim((OptimizeType)RefFixnum(right));
}

static void declaim_safety_code(Execute ptr, addr right)
{
	apply_safety_declaim((OptimizeType)RefFixnum(right));
}

static void declaim_space_code(Execute ptr, addr right)
{
	apply_space_declaim((OptimizeType)RefFixnum(right));
}

static void declaim_speed_code(Execute ptr, addr right)
{
	apply_speed_declaim((OptimizeType)RefFixnum(right));
}

static void declaim_declaration_code(Execute ptr, addr right)
{
	push_declaration_declaim(right);
}


/*
 *  local
 */
static void local_alloc_code(Execute ptr, addr right)
{
	array_data_control(ptr, RefIndex(right));
}

static void local_result_code(Execute ptr, addr right)
{
	addr value;
	getresult_control(ptr, &value);
	setdata_array_control(ptr, RefIndex(right), value);
}


/*
 *  let
 */
static void let_lexical_code(Execute ptr, addr right)
{
	addr symbol;

	List_bind(right, &symbol, &right, NULL);
	getdata_array_control(ptr, RefIndex(right), &right);
	pushlexical_control(ptr, symbol, right);
}

static void let_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, value, type;

	List_bind(right, &symbol, &value, &type, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	if (! typep_clang(value, type))
		check_type_error(value, type);
	pushlexical_control(ptr, symbol, value);
}

static void let_special_code(Execute ptr, addr right)
{
	addr symbol;

	List_bind(right, &symbol, &right, NULL);
	getdata_array_control(ptr, RefIndex(right), &right);
	pushspecial_control(ptr, symbol, right);
}

static void let_special_type_code(Execute ptr, addr right)
{
	addr symbol, value, type;

	List_bind(right, &symbol, &value, &type, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	if (! typep_clang(value, type))
		check_type_error(value, type);
	pushspecial_control(ptr, symbol, value);
}

static void leta_lexical_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushlexical_control(ptr, right, value);
}

static void leta_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getresult_control(ptr, &right);
	if (! typep_clang(right, type))
		check_type_error(right, type);
	pushlexical_control(ptr, symbol, right);
}

static void leta_special_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushspecial_control(ptr, right, value);
}

static void leta_special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getresult_control(ptr, &right);
	if (! typep_clang(right, type))
		check_type_error(right, type);
	pushspecial_control(ptr, symbol, right);
}


/*
 *  symbol
 */
static void lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getlexical_local(ptr, symbol, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void lexical_set_code(Execute ptr, addr right)
{
	getlexicalcheck_local(ptr, right, &right);
	setresult_control(ptr, right);
}

static void lexical_set_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getlexicalcheck_local(ptr, symbol, &right);
	setresult_control(ptr, right);
}

static void lexical_push_code(Execute ptr, addr right)
{
	getlexicalcheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
}

static void lexical_push_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getlexicalcheck_local(ptr, symbol, &right);
	pushargs_control(ptr, right);
}

static void special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getspecial_local(ptr, symbol, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void special_set_code(Execute ptr, addr right)
{
	getspecialcheck_local(ptr, right, &right);
	setresult_control(ptr, right);
}

static void special_set_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getspecialcheck_local(ptr, symbol, &right);
	setresult_control(ptr, right);
}

static void special_push_code(Execute ptr, addr right)
{
	getspecialcheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
}

static void special_push_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getspecialcheck_local(ptr, symbol, &right);
	pushargs_control(ptr, right);
}


/*
 *  setq
 */
static void check_readonly_variable(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		fmte("Cannot set value to the constant variable ~S.", symbol, NULL);
}

static void setq_lexical_code(Execute ptr, addr right)
{
	addr value;
	check_readonly_variable(right);
	getresult_control(ptr, &value);
	setlexical_local(ptr, right, value);
}

static void setq_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	check_readonly_variable(symbol);
	getresult_control(ptr, &right);
	if (! typep_clang(right, type))
		check_type_error(right, type);
	setlexical_local(ptr, symbol, right);
}

static void setq_special_code(Execute ptr, addr right)
{
	addr value;
	check_readonly_variable(right);
	getresult_control(ptr, &value);
	setspecial_local(ptr, right, value);
}

static void setq_special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	check_readonly_variable(symbol);
	getresult_control(ptr, &right);
	if (! typep_clang(right, type))
		check_type_error(right, type);
	setspecial_local(ptr, symbol, right);
}


/*
 *  function
 */
static void function_global_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	function_global_restart(ptr, call, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void function_global_set_code(Execute ptr, addr right)
{
	function_global_restart(ptr, right, &right);
	setresult_control(ptr, right);
}

static void function_global_push_code(Execute ptr, addr right)
{
	function_global_restart(ptr, right, &right);
	pushargs_control(ptr, right);
}

static void function_local_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	getfunction_local(ptr, call, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void function_local_set_code(Execute ptr, addr right)
{
	getfunctioncheck_local(ptr, right, &right);
	setresult_control(ptr, right);
}

static void function_local_push_code(Execute ptr, addr right)
{
	getfunctioncheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
}

static void setf_global_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	getsetf_symbol(call, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void setf_global_set_code(Execute ptr, addr right)
{
	getsetfcheck_symbol(right, &right);
	setresult_control(ptr, right);
}

static void setf_global_push_code(Execute ptr, addr right)
{
	getsetfcheck_symbol(right, &right);
	pushargs_control(ptr, right);
}

static void setf_local_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	getsetf_local(ptr, call, &right);
	if ((right != Unbound) && (! typep_clang(right, type)))
		check_type_error(right, type);
}

static void setf_local_set_code(Execute ptr, addr right)
{
	getsetfcheck_local(ptr, right, &right);
	setresult_control(ptr, right);
}

static void setf_local_push_code(Execute ptr, addr right)
{
	getsetfcheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
}


/*
 *  lambda
 */
static void lambda_code(Execute ptr, addr right)
{
	addr name, code, type, doc, form;

	List_bind(right, &name, &code, &type, &doc, &form, NULL);
	function_heap(&code, name, code);
	settype_function(code, type);
	setdocumentation_function(code, doc);
	setlambda_expression_function(code, form);
	setresult_control(ptr, code);
}

static void lambda_self_code(Execute ptr, addr right)
{
	addr name, code, type, doc, form;

	List_bind(right, &name, &code, &type, &doc, &form, NULL);
	function_heap(&code, name, code);
	settype_function(code, type);
	setdocumentation_function(code, doc);
	setlambda_expression_function(code, form);
	setrecursive_function(code);
	setresult_control(ptr, code);
}

static void lambda_value_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	conslexicalcheck_local(ptr, right, &value);
	pushclosure_value_function(pos, right, value);
}

static void lambda_function_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	getcallnamecheck_local(ptr, right, &value);
	pushclosure_function_function(pos, right, value);
}

static void lambda_tagbody_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	gettagbody_execute(ptr, &value, right);
	pushclosure_tagbody_function(pos, right, value);
}

static void lambda_block_code(Execute ptr, addr right)
{
	addr pos;

	getresult_control(ptr, &pos);
	getblock_execute(ptr, &right, right);
	pushclosure_block_function(pos, right);
}

static void bind_variable(Execute ptr, addr left, addr right, int ignore)
{
	int specialp;
	addr type;

	/* type check */
	if (! ignore && getcheck_tablevalue(left)) {
		gettype_tablevalue(left, &type);
		if (! typep_clang(right, right))
			check_type_error(right, type);
	}

	/* push */
	specialp = getspecialp_tablevalue(left);
	getname_tablevalue(left, &left);
	if (specialp)
		pushspecial_control(ptr, left, right);
	else
		pushlexical_control(ptr, left, right);
}

static int bind_initialize(Execute ptr, addr var, addr init)
{
	addr control;

	/* push */
	push_close_control(ptr, &control);
	/* code */
	if (runcode_control(ptr, init))
		return runcode_free_control(ptr, control);
	getresult_control(ptr, &init);
	if (free_control(ptr, control)) return 1;
	bind_variable(ptr, var, init, 0);

	return 0;
}

static void lambda_bind_var(Execute ptr, addr cons, addr *args)
{
	addr left, right;

	while (cons != Nil) {
		if (*args == Nil)
			fmte("Too few argument.", NULL);
		getcons(cons, &left, &cons);
		getcons(*args, &right, args);
		bind_variable(ptr, left, right, 0);
	}
}

static int lambda_bind_opt(Execute ptr, addr cons, addr *args)
{
	addr left, right, var, init, svar;

	while (cons != Nil) {
		getcons(cons, &left, &cons);
		List_bind(left, &var, &init, &svar, NULL);
		if (*args != Nil) {
			getcons(*args, &right, args);
			bind_variable(ptr, var, right, 0);
			if (svar != Nil)
				bind_variable(ptr, svar, T, 1);
		}
		else {
			if (bind_initialize(ptr, var, init))
				return 1;
			if (svar != Nil)
				bind_variable(ptr, svar, Nil, 1);
		}
	}

	return 0;
}

static void lambda_bind_rest(Execute ptr, addr rest, addr args)
{
	/*copylist_force_heap(args, &args);*/
	copy_list_heap_unsafe(&args, args);
	bind_variable(ptr, rest, args, 1);
}

static int find_name_key(addr key, addr cons)
{
	addr name;

	while (cons != Nil) {
		getcons(cons, &name, &cons);
		/* (var name init svar) */
		getcdr(name, &name);
		getcar(name, &name);
		if (key == name) return 1;
	}

	return 0;
}

static int lambda_bind_key(Execute ptr, addr cons, int allow, addr args)
{
	addr left, right, var, name, init, svar, value;

	/* format check */
	for (right = args; right != Nil; ) {
		getcons(right, &left, &right);
		if (right == Nil)
			fmte("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			fmte("&key name ~S must be a symbol.", left, NULL);
		if (! allow && ! find_name_key(left, cons))
			fmte("&key name ~S don't accept.", left, NULL);
		getcons(right, &left, &right);
	}

	/* bind */
	while (cons != Nil) {
		getcons(cons, &var, &cons);
		List_bind(var, &var, &name, &init, &svar, NULL);
		if (getplist(args, name, &value) == 0) {
			bind_variable(ptr, var, value, 0);
			if (svar != Nil)
				bind_variable(ptr, svar, T, 1);
		}
		else {
			if (bind_initialize(ptr, var, init))
				return 1;
			if (svar != Nil)
				bind_variable(ptr, svar, Nil, 1);
		}
	}

	return 0;
}

static void lambda_bind_allow(Execute ptr, addr args)
{
	addr left;

	while (args != Nil) {
		getcons(args, &left, &args);
		if (args == Nil)
			fmte("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			fmte("&key name ~S must be a symbol.", left, NULL);
		getcons(args, &left, &args); /* ignore */
	}
}

static int lambda_bind_aux(Execute ptr, addr cons)
{
	addr var, init;

	while (cons != Nil) {
		getcons(cons, &var, &cons);
		List_bind(var, &var, &init, NULL);
		if (bind_initialize(ptr, var, init)) return 1;
	}

	return 0;
}

static int lambda_bind_code_check(Execute ptr, addr right)
{
	addr var, opt, rest, key, allow, aux, args;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, NULL);
	getargs_list_control_unsafe(ptr, 0, &args);
	lambda_bind_var(ptr, var, &args);
	if (lambda_bind_opt(ptr, opt, &args)) return 1;
	if (rest != Nil)
		lambda_bind_rest(ptr, rest, args);
	else if (key == Nil && allow == Nil && args != Nil)
		fmte("Too many argument.", NULL);
	if (key != Nil) {
		if (lambda_bind_key(ptr, key, allow != Nil, args)) return 1;
	}
	else if (allow != Nil)
		lambda_bind_allow(ptr, args);
	if (lambda_bind_aux(ptr, aux)) return 1;

	return 0;
}

static void lambda_bind_code(Execute ptr, addr right)
{
	(void)lambda_bind_code_check(ptr, right);
}

static void macro_bind_whole(Execute ptr, addr whole, addr form)
{
	if (whole != Nil) {
		copy_list_heap_unsafe(&form, form);
		bind_variable(ptr, whole, form, 1);
	}
}

static void macro_bind_env(Execute ptr, addr env, addr argenv)
{
	if (env != Nil)
		bind_variable(ptr, env, argenv, 1);
}

static int macro_bind_list(Execute ptr, addr right, addr args);
static int macro_bind_var(Execute ptr, addr var, addr *args)
{
	addr left, right;

	while (var != Nil) {
		if (*args == Nil)
			fmte("Too few argument.", NULL);
		getcons(var, &left, &var);
		getcons(*args, &right, args);
		if (consp(left)) {
			if (macro_bind_list(ptr, left, right))
				return 1;
		}
		else {
			bind_variable(ptr, left, right, 0);
		}
	}

	return 0;
}

static void macro_bind_rest(Execute ptr, addr rest, addr args)
{
	addr var;

	getcons(rest, &var, &rest);  /* (value . &rest) */
	copy_list_heap_unsafe(&args, args);
	bind_variable(ptr, var, args, 1);
}

static int macro_bind_args(Execute ptr, addr args,
		addr var, addr opt, addr rest, addr key, addr allow, addr aux)
{
	if (macro_bind_var(ptr, var, &args)) return 1;
	if (lambda_bind_opt(ptr, opt, &args)) return 1;
	if (rest != Nil)
		macro_bind_rest(ptr, rest, args);
	else if (key == Nil && allow == Nil && args != Nil)
		fmte("Too many argument.", NULL);
	if (key != Nil) {
		if (lambda_bind_key(ptr, key, allow != Nil, args)) return 1;
	}
	else if (allow != Nil)
		lambda_bind_allow(ptr, args);
	if (lambda_bind_aux(ptr, aux)) return 1;

	return 0;
}

static int macro_bind_list(Execute ptr, addr right, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;
	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	macro_bind_whole(ptr, whole, args);
	return macro_bind_args(ptr, args, var, opt, rest, key, allow, aux);
}

static int macro_bind_call(Execute ptr, addr right, addr args, addr argenv)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	macro_bind_whole(ptr, whole, args);
	getcdr(args, &args);
	macro_bind_env(ptr, env, argenv);
	return macro_bind_args(ptr, args, var, opt, rest, key, allow, aux);
}

static void macro_bind_code(Execute ptr, addr right)
{
	addr args, form, env;

	getargs_list_control_unsafe(ptr, 0, &args);
	/* (lambda (form env) ...) */
	if (! consp(args))
		fmte("Too few argument in macro function.", NULL);
	getcons(args, &form, &args);
	if (! consp(args))
		fmte("Too few argument in macro function.", NULL);
	getcons(args, &env, &args);
	if (args != Nil)
		fmte("Too many argument in macro function.", NULL);
	(void)macro_bind_call(ptr, right, form, env);
}

static void destructuring_bind_code(Execute ptr, addr right)
{
	addr args;
	getargs_control(ptr, 0, &args);
	(void)macro_bind_list(ptr, right, args);
}

static void macro_lambda_code(Execute ptr, addr right)
{
	addr form;

	List_bind(right, &form, &right, NULL);
	macro_heap(&form, Nil, form);
	setdocumentation_function(form, right);
	setresult_control(ptr, form);
}

static void defmacro_code(Execute ptr, addr right)
{
	addr symbol;

	List_bind(right, &symbol, &right, NULL);
	setmacro_symbol(symbol, right);
	setresult_control(ptr, symbol);
}

static void define_symbol_macro_code(Execute ptr, addr right)
{
	addr symbol, eval, form;

	List_bind(right, &symbol, &eval, &form, NULL);
	Check(! symbolp(symbol), "type error");
	setsymbol_macro_symbol(symbol, eval, form);
	setresult_control(ptr, symbol);
}

static void defun_code(Execute ptr, addr right)
{
	addr pos, call, symbol;

	getresult_control(ptr, &pos);
	GetNameFunction(pos, &call);
	GetCallName(call, &symbol);

	if (symbol_callname_p(call)) {
		SetFunctionSymbol(symbol, pos);
		setresult_control(ptr, symbol);
	}
	else {
		setsetf_symbol(symbol, pos);
		GetConst(COMMON_SETF, &pos);
		list_heap(&pos, pos, symbol, NULL);
		setresult_control(ptr, pos);
	}
}

static void flet_code(Execute ptr, addr right)
{
	addr call;

	List_bind(right, &call, &right, NULL);
	getdata_array_control(ptr, RefIndex(right), &right);
	pushcallname_control(ptr, call, right);
}

static void labels_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushcallname_control(ptr, right, value);
}

static void call_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	execute_control(ptr, right);
}

static void call_type_code(Execute ptr, addr right)
{
	addr value;

	getargs_tail_control(ptr, &value);
	if (! typep_clang(value, right))
		check_type_error(value, right);
}

static void values_nil_code(Execute ptr, addr right)
{
	setvalues_nil_control(ptr);
}

static void values_set_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	setvalues_list_control(ptr, right);
}

static void the_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	if (! typep_clang(value, right))
		check_type_error(value, right);
}

static void if_nil_code(Execute ptr, addr right)
{
	addr check;

	getresult_control(ptr, &check);
	if (check == Nil)
		goto_control(ptr, RefIndex(right));
}

static void if_t_code(Execute ptr, addr right)
{
	addr check;

	getresult_control(ptr, &check);
	if (check != Nil)
		goto_control(ptr, RefIndex(right));
}

static void goto_code(Execute ptr, addr right)
{
	goto_control(ptr, RefIndex(right));
}

static void go_code(Execute ptr, addr right)
{
	go_control(ptr, right);
}

static void return_from_code(Execute ptr, addr right)
{
	return_from_control(ptr, right);
}

static void catch_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	catch_control(ptr, right);
}

static void throw_code_operator(Execute ptr, addr right)
{
	getargs_control(ptr, 0, &right);
	throw_control(ptr, right);
}

static void push_handler_code(Execute ptr, addr right, int escape)
{
	addr args, symbol, lambda;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &symbol, &args);
		GetCons(args, &lambda, &args);
		pushhandler_control(ptr, symbol, lambda, escape);
	}
	reverse_handler_control(ptr);
}

static void handler_bind_code(Execute ptr, addr right)
{
	push_handler_code(ptr, right, 0);
}

static void handler_case_code(Execute ptr, addr right)
{
	push_handler_code(ptr, right, 1);
}

static void push_restart_code(Execute ptr, addr right, int escape)
{
	addr args, list;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &list, &args);
		pushbind_restart_control(ptr, list, escape);
	}
	reverse_restart_control(ptr);
}

static void restart_bind_code(Execute ptr, addr right)
{
	push_restart_code(ptr, right, 0);
}

static void restart_case_code(Execute ptr, addr right)
{
	push_restart_code(ptr, right, 1);
}

static void multiple_value_bind_code(Execute ptr, addr right)
{
	addr list, var, value;

	getvalues_list_control_local(ptr, &list);
	while (right != Nil) {
		GetCons(right, &var, &right);
		GetCons(list, &value, &list);
		bind_variable(ptr, var, value, 0);
	}
}

static void funcall_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	(void)call_control(ptr, right);
}

static void nth_value_code(Execute ptr, addr right)
{
	addr nth;
	size_t index;

	getargs_control(ptr, 0, &nth);
	if (! integerp(nth))
		fmte("NTH-VALUE argument ~S must be integer type.", nth, NULL);
	if (! zerop_or_plusp_integer(nth))
		fmte("NTH-VALUE argument ~S must be greater than equal to 0.", nth, NULL);
	if (getindex_integer(nth, &index)) {
		setresult_control(ptr, Nil);
	}
	else {
		getvalues_control(ptr, index, &right);
		setresult_control(ptr, (right == Unbound)? Nil: right);
	}
}

static void progv_code(Execute ptr, addr right)
{
	addr symbols, values, symbol, value;

	getargs_control(ptr, 0, &symbols);
	getargs_control(ptr, 1, &values);
	while (symbols != Nil) {
		if (! consp(symbols))
			fmte("PROGV form ~S must be a cons.", symbols, NULL);
		GetCons(symbols, &symbol, &symbols);
		if (! symbolp(symbol))
			fmte("PROGV argument ~S must be a symbol.", symbol, NULL);
		if (values == Nil) {
			pushspecial_control(ptr, symbol, Unbound);
		}
		else if (consp(values)) {
			GetCons(values, &value, &values);
			pushspecial_control(ptr, symbol, value);
		}
		else {
			fmte("PROGV form ~S must be a cons.", values, NULL);
		}
	}
}


/* defcode */
static void defcode_constant(constindex index, calltype call)
{
	addr symbol, system;

	GetConstant(index, &symbol);
	Check(symbol == Nil || symbol == Unbound, "constant error");
	GetFunctionSymbol(symbol, &system);
	Check(system != Unbound, "code-function already exists.");
	compiled_heap(&system, symbol);
	setcompiled_system(system, call);
	setsystem_function(system);
	SetFunctionSymbol(symbol, system);
}

#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, (y))
void build_code(void)
{
	/* system */
	defcode(HELLO, hello_code);
	defcode(NOP, nop_code);
	defcode(ABORT, abort_code);
	defcode(ERROR, error_code);
	defcode(INFO, info_code);
	defcode(PRINT, print_code);
	defcode(EXECUTE, execute_code);
	defcode(END, end_code_operator);

	/* object */
	defcode(SET, set_code);
	defcode(PUSH, push_code);
	defcode(PUSH_RESULT, push_result_code);
	defcode(PUSH_VALUES, push_values_code);
	defcode(NIL_SET, nil_set_code);
	defcode(NIL_PUSH, nil_push_code);
	defcode(T_SET, t_set_code);
	defcode(T_PUSH, t_push_code);

	/* declaim */
	defcode(DECLAIM_SPECIAL, declaim_special_code);
	defcode(DECLAIM_TYPE_VALUE, declaim_type_value_code);
	defcode(DECLAIM_TYPE_FUNCTION, declaim_type_function_code);
	defcode(DECLAIM_INLINE, declaim_inline_code);
	defcode(DECLAIM_NOTINLINE, declaim_notinline_code);
	defcode(DECLAIM_COMPILATION, declaim_compilation_code);
	defcode(DECLAIM_DEBUG, declaim_debug_code);
	defcode(DECLAIM_SAFETY, declaim_safety_code);
	defcode(DECLAIM_SPACE, declaim_space_code);
	defcode(DECLAIM_SPEED, declaim_speed_code);
	defcode(DECLAIM_DECLARATION, declaim_declaration_code);

	/* local */
	defcode(LOCAL_ALLOC, local_alloc_code);
	defcode(LOCAL_RESULT, local_result_code);

	/* let */
	defcode(LET_LEXICAL, let_lexical_code);
	defcode(LET_LEXICAL_TYPE, let_lexical_type_code);
	defcode(LET_SPECIAL, let_special_code);
	defcode(LET_SPECIAL_TYPE, let_special_type_code);
	defcode(LETA_LEXICAL, leta_lexical_code);
	defcode(LETA_LEXICAL_TYPE, leta_lexical_type_code);
	defcode(LETA_SPECIAL, leta_special_code);
	defcode(LETA_SPECIAL_TYPE, leta_special_type_code);

	/* symbol */
	defcode(LEXICAL_TYPE, lexical_type_code);
	defcode(LEXICAL_SET, lexical_set_code);
	defcode(LEXICAL_SET_TYPE, lexical_set_type_code);
	defcode(LEXICAL_PUSH, lexical_push_code);
	defcode(LEXICAL_PUSH_TYPE, lexical_push_type_code);
	defcode(SPECIAL_TYPE, special_type_code);
	defcode(SPECIAL_SET, special_set_code);
	defcode(SPECIAL_PUSH, special_push_code);
	defcode(SPECIAL_SET_TYPE, special_set_type_code);
	defcode(SPECIAL_PUSH_TYPE, special_push_type_code);

	/* setq */
	defcode(SETQ_LEXICAL, setq_lexical_code);
	defcode(SETQ_LEXICAL_TYPE, setq_lexical_type_code);
	defcode(SETQ_SPECIAL, setq_special_code);
	defcode(SETQ_SPECIAL_TYPE, setq_special_type_code);

	/* function */
	defcode(FUNCTION_GLOBAL_TYPE, function_global_type_code);
	defcode(FUNCTION_GLOBAL_SET, function_global_set_code);
	defcode(FUNCTION_GLOBAL_PUSH, function_global_push_code);
	defcode(FUNCTION_LOCAL_TYPE, function_local_type_code);
	defcode(FUNCTION_LOCAL_SET, function_local_set_code);
	defcode(FUNCTION_LOCAL_PUSH, function_local_push_code);
	defcode(SETF_GLOBAL_TYPE, setf_global_type_code);
	defcode(SETF_GLOBAL_SET, setf_global_set_code);
	defcode(SETF_GLOBAL_PUSH, setf_global_push_code);
	defcode(SETF_LOCAL_TYPE, setf_local_type_code);
	defcode(SETF_LOCAL_SET, setf_local_set_code);
	defcode(SETF_LOCAL_PUSH, setf_local_push_code);

	/* lambda */
	defcode(LAMBDA, lambda_code);
	defcode(LAMBDA_SELF, lambda_self_code);
	defcode(LAMBDA_VALUE, lambda_value_code);
	defcode(LAMBDA_FUNCTION, lambda_function_code);
	defcode(LAMBDA_TAGBODY, lambda_tagbody_code);
	defcode(LAMBDA_BLOCK, lambda_block_code);
	defcode(LAMBDA_BIND, lambda_bind_code);
	defcode(MACRO_BIND, macro_bind_code);
	defcode(DESTRUCTURING_BIND, destructuring_bind_code);
	defcode(DEFUN, defun_code);
	defcode(MACRO_LAMBDA, macro_lambda_code);
	defcode(DEFMACRO, defmacro_code);
	defcode(DEFINE_SYMBOL_MACRO, define_symbol_macro_code);
	defcode(FLET, flet_code);
	defcode(LABELS, labels_code);
	defcode(CALL, call_code);
	defcode(CALL_TYPE, call_type_code);

	defcode(VALUES_NIL, values_nil_code);
	defcode(VALUES_SET, values_set_code);
	defcode(THE, the_code);
	defcode(IF_NIL, if_nil_code);
	defcode(IF_T, if_t_code);
	defcode(GOTO, goto_code);
	defcode(GO, go_code);
	defcode(RETURN_FROM, return_from_code);
	defcode(CATCH, catch_code);
	defcode(THROW, throw_code_operator);
	defcode(HANDLER_BIND, handler_bind_code);
	defcode(HANDLER_CASE, handler_case_code);
	defcode(RESTART_BIND, restart_bind_code);
	defcode(RESTART_CASE, restart_case_code);

	defcode(MULTIPLE_VALUE_BIND, multiple_value_bind_code);
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);
}


/*****************************************************************************
 *  code object
 *****************************************************************************/
enum Code_Index {
	Code_Array,
	Code_Call,
	Code_Argument,
	Code_Info,
	Code_Size
};

struct code_struct {
	enum CodeType type;
	const calltype *call;
	size_t size;
};

#define RefArrayCode		RefArraySS
#define GetArrayCode		GetArraySS
#define SetArrayCode		SetArraySS
#define PtrBodyCode(p)		PtrBodySSa((p), Code_Size)
#define StructCode(p)		((struct code_struct *)PtrBodyCode(p))
#define PtrCallCode(p)		((calltype *)PtrBodyB4(p))
#define RefArgumentCall		RefArrayA4
#define GetArgumentCall		GetArrayA4
#define SetArgumentCall		SetArrayA4

static void alloc_code(LocalRoot local, addr *ret)
{
	alloc_smallsize(local, ret, LISPTYPE_CODE,
			Code_Size, sizeoft(struct code_struct));
}

static void code_call_alloc(LocalRoot local, addr *ret, size_t size)
{
	alloc_body4(local, ret, LISPSYSTEM_CODE, sizeoft(calltype) * size);
}

static void make_code_call(LocalRoot local, addr codeA4, size_t size,
		addr *retcall, addr *retargs, const calltype **retptr)
{
	size_t i;
	addr left, right, call, args;
	calltype *ptr, check;

	code_call_alloc(local, &call, size);
	vector4_alloc(local, &args, size);
	ptr = PtrCallCode(call);
	for (i = 0; i < size; i++) {
		GetArrayA4(codeA4, i, &left);
		copylocal_object(local, &left, left);
		GetCons(left, &left, &right);
		GetFunctionSymbol(left, &left);
		/* call */
		Check(left == NULL || left == Unbound, "unbound error");
		Check(! compiled_funcall_function_p(left), "type error");
		getcompiled_system(left, &check);
		Check(check == NULL, "address error");
		ptr[i] = check;
		/* args */
		SetArgumentCall(args, i, right);
	}
	*retcall = call;
	*retargs = args;
	*retptr = ptr;
}

static void code_alloc(LocalRoot local, addr *ret, addr codeA4)
{
	addr pos, call, args;
	struct code_struct *ptr;
	const calltype *callptr;
	size_t size;

	alloc_code(local, &pos);
	ptr = StructCode(pos);
	CheckType(pos, LISPTYPE_CODE);
	CheckType(codeA4, LISPTYPE_VECTOR);
	LenArrayA4(codeA4, &size);
	make_code_call(local, codeA4, size, &call, &args, &callptr);
	SetArrayCode(pos, Code_Array, codeA4);
	SetArrayCode(pos, Code_Call, call);
	SetArrayCode(pos, Code_Argument, args);
	ptr->type = CodeType_Default;
	ptr->call = callptr;
	ptr->size = size;
	*ret = pos;
}

void code_heap(addr *ret, addr codeA4)
{
	code_alloc(NULL, ret, codeA4);
}

const calltype *getcalltype_code(addr pos)
{
	CheckType(pos, LISPTYPE_CODE);
	return StructCode(pos)->call;
}

void getarray_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Array, ret);
}

void getargs_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Argument, ret);
}

enum CodeType gettype_code(addr pos)
{
	CheckType(pos, LISPTYPE_CODE);
	return StructCode(pos)->type;
}

void settype_code(addr pos, enum CodeType type)
{
	CheckType(pos, LISPTYPE_CODE);
	StructCode(pos)->type = type;
}

void getinfo_code(addr pos, addr *ret)
{
	CheckType(pos, LISPTYPE_CODE);
	GetArrayCode(pos, Code_Info, ret);
}

void setinfo_code(addr pos, addr value)
{
	CheckType(pos, LISPTYPE_CODE);
	SetArrayCode(pos, Code_Info, value);
}

void syscall_code(LocalRoot local, addr *ret, callbind_empty call, addr value)
{
	addr pos, code, list;

	/* function */
	compiled_local(local, &pos, Nil);
	setcompiled_empty(pos, call);
	SetDataFunction(pos, value);
	/* code
	 *   (set . function)
	 *   (call)
	 *   (end)
	 */
	vector4_local(local, &code, 3);
	GetConst(CODE_SET, &list);
	cons_local(local, &list, list, pos);
	setarray(code, 0, list);
	GetConst(CODE_CALL, &list);
	conscar_local(local, &list, list);
	setarray(code, 1, list);
	GetConst(CODE_END, &list);
	conscar_local(local, &list, list);
	setarray(code, 2, list);
	code_alloc(local, &code, code);
	settype_code(code, CodeType_Close);
	*ret = code;
}

