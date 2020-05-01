#include "call_eval.h"
#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "execute.h"
#include "eval_declare.h"
#include "eval_table.h"
#include "function.h"
#include "gc.h"
#include "info.h"
#include "integer.h"
#include "lambda.h"
#include "restart_value.h"
#include "symbol.h"
#include "type_deftype.h"

/*****************************************************************************
 *  code function
 *****************************************************************************/
/*
 *  system
 */
static int hello_code(Execute ptr, addr right)
{
	info("Hello code.");
	return 0;
}

static int nop_code(Execute ptr, addr right)
{
	/* do nothing */
	return 0;
}

static int abort_code(Execute ptr, addr right)
{
	Abort("abort-code");
	return 0;
}

static int error_code(Execute ptr, addr right)
{
	return fmte_("error-code", NULL);
}

static int info_code(Execute ptr, addr right)
{
	GetCar(right, &right);
	infobit(right);
	return 0;
}

static int print_code(Execute ptr, addr right)
{
	GetCar(right, &right);
	infoprint(right);
	return 0;
}

static int execute_code(Execute ptr, addr right)
{
	return runcode_control(ptr, right);
}

static int execute_switch_code(Execute ptr, addr right)
{
	return runcode_switch(ptr, right);
}


/*
 *  object
 */
static int set_code(Execute ptr, addr right)
{
	setresult_control(ptr, right);
	return 0;
}

static int push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, right);
	return 0;
}

static int push_result_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	pushargs_control(ptr, right);
	return 0;
}

static int push_values_code(Execute ptr, addr right)
{
	pushargs_allvalues(ptr);
	return 0;
}

static int nil_set_code(Execute ptr, addr right)
{
	setresult_control(ptr, Nil);
	return 0;
}

static int nil_push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, Nil);
	return 0;
}

static int t_set_code(Execute ptr, addr right)
{
	setresult_control(ptr, T);
	return 0;
}

static int t_push_code(Execute ptr, addr right)
{
	pushargs_control(ptr, T);
	return 0;
}


/*
 *  declaim
 */
static int declaim_special_code(Execute ptr, addr right)
{
	setspecial_symbol(right);
	return 0;
}

static int declaim_type_value_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	settype_value_symbol(symbol, type);
	return 0;
}

static int declaim_type_function_code(Execute ptr, addr right)
{
	addr key, symbol, type;

	List_bind(right, &key, &type, NULL);
	GetCallName(key, &symbol);
	if (symbol_callname_p(key))
		settype_function_symbol(symbol, type);
	else
		settype_setf_symbol(symbol, type);

	return 0;
}

static int declaim_inline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbol_callname_p(right))
		setinline_function_symbol(symbol);
	else
		setinline_setf_symbol(symbol);

	return 0;
}

static int declaim_notinline_code(Execute ptr, addr right)
{
	addr symbol;

	GetCallName(right, &symbol);
	if (symbol_callname_p(right))
		setnotinline_function_symbol(symbol);
	else
		setnotinline_setf_symbol(symbol);

	return 0;
}

static int declaim_compilation_code(Execute ptr, addr right)
{
	apply_compilation_speed_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_debug_code(Execute ptr, addr right)
{
	apply_debug_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_safety_code(Execute ptr, addr right)
{
	apply_safety_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_space_code(Execute ptr, addr right)
{
	apply_space_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_speed_code(Execute ptr, addr right)
{
	apply_speed_declaim((OptimizeType)RefFixnum(right));
	return 0;
}

static int declaim_declaration_code(Execute ptr, addr right)
{
	push_declaration_declaim(right);
	return 0;
}


/*
 *  local
 */
static int local_alloc_code(Execute ptr, addr right)
{
	array_data_control(ptr, RefIndex(right));
	return 0;
}

static int local_result_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	setdata_array_control(ptr, RefIndex(right), value);

	return 0;
}


/*
 *  let
 */
static int let_lexical_code(Execute ptr, addr right)
{
	addr symbol, value;

	List_bind(right, &symbol, &value, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	pushlexical_control(ptr, symbol, value);

	return 0;
}

static int typep_unbound_error(Execute ptr, addr value, addr type)
{
	return (value == Unbound)? 0: typep_error(ptr, value, type);
}

static int let_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, value, type;

	List_bind(right, &symbol, &value, &type, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	Return(typep_error(ptr, value, type));
	pushlexical_control(ptr, symbol, value);

	return 0;
}

static int let_special_code(Execute ptr, addr right)
{
	addr symbol, value;

	List_bind(right, &symbol, &value, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	pushspecial_control(ptr, symbol, value);

	return 0;
}

static int let_special_type_code(Execute ptr, addr right)
{
	addr symbol, value, type;

	List_bind(right, &symbol, &value, &type, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	Return(typep_error(ptr, value, type));
	pushspecial_control(ptr, symbol, value);

	return 0;
}

static int leta_lexical_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushlexical_control(ptr, right, value);

	return 0;
}

static int leta_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getresult_control(ptr, &right);
	Return(typep_error(ptr, right, type));
	pushlexical_control(ptr, symbol, right);

	return 0;
}

static int leta_special_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushspecial_control(ptr, right, value);

	return 0;
}

static int leta_special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getresult_control(ptr, &right);
	Return(typep_error(ptr, right, type));
	pushspecial_control(ptr, symbol, right);

	return 0;
}


/*
 *  symbol
 */
static int lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getlexical_local(ptr, symbol, &right);
	return typep_unbound_error(ptr, right, type);
}

static int lexical_set_code(Execute ptr, addr right)
{
	Return(symbol_lexical_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int lexical_set_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(symbol_lexical_restart(ptr, symbol, &right));
	setresult_control(ptr, right);

	return 0;
}

static int lexical_push_code(Execute ptr, addr right)
{
	Return(symbol_lexical_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int lexical_push_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(symbol_lexical_restart(ptr, symbol, &right));
	pushargs_control(ptr, right);

	return 0;
}

static int lexical_remove_code(Execute ptr, addr right)
{
	return symbol_lexical_restart(ptr, right, &right);
}

static int special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getspecial_local(ptr, symbol, &right);
	return typep_unbound_error(ptr, right, type);
}

static int special_set_code(Execute ptr, addr right)
{
	Return(symbol_special_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int special_set_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(symbol_special_restart(ptr, symbol, &right));
	setresult_control(ptr, right);

	return 0;
}

static int special_push_code(Execute ptr, addr right)
{
	Return(symbol_special_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int special_push_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(symbol_special_restart(ptr, symbol, &right));
	pushargs_control(ptr, right);

	return 0;
}

static int special_remove_code(Execute ptr, addr right)
{
	return symbol_special_restart(ptr, right, &right);
}


/*
 *  setq
 */
static int check_readonly_variable_(addr symbol)
{
	if (GetStatusReadOnly(symbol))
		return fmte_("Cannot set value to the constant variable ~S.", symbol, NULL);
	return 0;
}

static int setq_lexical_code(Execute ptr, addr right)
{
	addr value;

	Return(check_readonly_variable_(right));
	getresult_control(ptr, &value);
	setlexical_local(ptr, right, value);

	return 0;
}

static int setq_lexical_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(check_readonly_variable_(symbol));
	getresult_control(ptr, &right);
	Return(typep_error(ptr, right, type));
	setlexical_local(ptr, symbol, right);

	return 0;
}

static int setq_special_code(Execute ptr, addr right)
{
	addr value;

	Return(check_readonly_variable_(right));
	getresult_control(ptr, &value);
	setspecial_local(ptr, right, value);

	return 0;
}

static int setq_special_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	Return(check_readonly_variable_(symbol));
	getresult_control(ptr, &right);
	Return(typep_error(ptr, right, type));
	setspecial_local(ptr, symbol, right);

	return 0;
}


/*
 *  function
 */
static int function_global_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	GetFunctionSymbol(symbol, &right);
	return typep_unbound_error(ptr, right, type);
}

static int function_global_set_code(Execute ptr, addr right)
{
	Return(function_global_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int function_global_push_code(Execute ptr, addr right)
{
	Return(function_global_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int function_local_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	getfunction_local(ptr, call, &right);
	return typep_unbound_error(ptr, right, type);
}

static int function_local_set_code(Execute ptr, addr right)
{
	getfunctioncheck_local(ptr, right, &right);
	setresult_control(ptr, right);
	return 0;
}

static int function_local_push_code(Execute ptr, addr right)
{
	getfunctioncheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
	return 0;
}

static int setf_global_type_code(Execute ptr, addr right)
{
	addr symbol, type;

	List_bind(right, &symbol, &type, NULL);
	getsetf_symbol(symbol, &right);
	return typep_unbound_error(ptr, right, type);
}

static int setf_global_set_code(Execute ptr, addr right)
{
	Return(setf_global_restart(ptr, right, &right));
	setresult_control(ptr, right);
	return 0;
}

static int setf_global_push_code(Execute ptr, addr right)
{
	Return(setf_global_restart(ptr, right, &right));
	pushargs_control(ptr, right);
	return 0;
}

static int setf_local_type_code(Execute ptr, addr right)
{
	addr call, type;

	List_bind(right, &call, &type, NULL);
	getsetf_local(ptr, call, &right);
	return typep_unbound_error(ptr, right, type);
}

static int setf_local_set_code(Execute ptr, addr right)
{
	getsetfcheck_local(ptr, right, &right);
	setresult_control(ptr, right);
	return 0;
}

static int setf_local_push_code(Execute ptr, addr right)
{
	getsetfcheck_local(ptr, right, &right);
	pushargs_control(ptr, right);
	return 0;
}


/*
 *  lambda
 */
static int lambda_code(Execute ptr, addr right)
{
	addr name, code, type, doc, form, defun;

	List_bind(right, &name, &code, &type, &doc, &form, &defun, NULL);
	function_heap(&code, name, code);
	settype_function(code, type);
	setdocumentation_function(code, doc);
	setlambda_expression_function(code, form);
	if (defun != Nil)
		setdefunform_function(code, defun);
	setresult_control(ptr, code);

	return 0;
}

static int lambda_self_code(Execute ptr, addr right)
{
	addr name, code, type, doc, form, defun;

	List_bind(right, &name, &code, &type, &doc, &form, &defun, NULL);
	function_heap(&code, name, code);
	settype_function(code, type);
	setdocumentation_function(code, doc);
	setlambda_expression_function(code, form);
	if (defun != Nil)
		setdefunform_function(code, defun);
	setrecursive_function(code);
	setresult_control(ptr, code);

	return 0;
}

static int lambda_value_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	conslexicalcheck_local(ptr, right, &value);
	pushclosure_value_function(pos, right, value);

	return 0;
}

static int lambda_function_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	getcallnamecheck_local(ptr, right, &value);
	pushclosure_function_function(pos, right, value);

	return 0;
}

static int lambda_tagbody_code(Execute ptr, addr right)
{
	addr pos, value;

	getresult_control(ptr, &pos);
	gettagbody_execute(ptr, &value, right);
	pushclosure_tagbody_function(pos, right, value);

	return 0;
}

static int lambda_block_code(Execute ptr, addr right)
{
	addr pos;

	getresult_control(ptr, &pos);
	getblock_execute(ptr, &right, right);
	pushclosure_block_function(pos, right);

	return 0;
}

static int bind_variable_(Execute ptr, addr left, addr right, int ignore)
{
	int specialp;
	addr type;

	/* type check */
	if (! ignore && getcheck_tablevalue(left)) {
		gettype_tablevalue(left, &type);
		Return(typep_error(ptr, right, type));
	}

	/* push */
	specialp = getspecialp_tablevalue(left);
	getname_tablevalue(left, &left);
	if (specialp)
		pushspecial_control(ptr, left, right);
	else
		pushlexical_control(ptr, left, right);

	return 0;
}

static int bind_initialize_(Execute ptr, addr var, addr init)
{
	addr control;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_close_control(ptr, &control);
	/* code */
	Return(runcode_control(ptr, init));
	getresult_control(ptr, &init);
	localhold_set(hold, 0, init);
	Return(free_control_(ptr, control));
	localhold_end(hold);
	return bind_variable_(ptr, var, init, 0);
}

static int lambda_bind_var_(Execute ptr, addr cons, addr *args)
{
	addr left, right;

	while (cons != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(cons, &left, &cons);
		Return_getcons(*args, &right, args);
		Return(bind_variable_(ptr, left, right, 0));
	}

	return 0;
}

static int lambda_bind_opt_(Execute ptr, addr cons, addr *args)
{
	addr left, right, var, init, svar;

	while (cons != Nil) {
		Return_getcons(cons, &left, &cons);
		List_bind(left, &var, &init, &svar, NULL);
		if (*args != Nil) {
			Return_getcons(*args, &right, args);
			Return(bind_variable_(ptr, var, right, 0));
			if (svar != Nil) {
				Return(bind_variable_(ptr, svar, T, 1));
			}
		}
		else {
			Return(bind_initialize_(ptr, var, init));
			if (svar != Nil) {
				Return(bind_variable_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_bind_rest_(Execute ptr, addr rest, addr args)
{
	/*copylist_force_heap(args, &args);*/
	/*copy_list_heap_safe(&args, args);*/
	copyheap(&args, args);
	return bind_variable_(ptr, rest, args, 1);
}

static int lambda_bind_key_find_(addr key, addr list, int *ret)
{
	addr name;

	while (list != Nil) {
		Return_getcons(list, &name, &list);
		/* (var name init svar) */
		Return_getcdr(name, &name);
		Return_getcar(name, &name);
		if (key == name)
			return Result(ret, 1);
	}

	return Result(ret, 0);
}

static int lambda_bind_key_(Execute ptr, addr cons, int allow, addr args)
{
	int check;
	addr left, right, var, name, init, svar, value;

	/* format check */
	for (right = args; right != Nil; ) {
		Return_getcons(right, &left, &right);
		if (right == Nil)
			return fmte_("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			return fmte_("&key name ~S must be a symbol.", left, NULL);
		if (! allow) {
			Return(lambda_bind_key_find_(left, cons, &check));
			if (! check) {
				if (find_keyword_allow_other_keys(args))
					allow = 1;
				else
					return fmte_("&key name ~S don't accept.", left, NULL);
			}
		}
		Return_getcons(right, &left, &right);
	}

	/* bind */
	while (cons != Nil) {
		Return_getcons(cons, &right, &cons);
		List_bind(right, &var, &name, &init, &svar, NULL);
		if (getplist(args, name, &value) == 0) {
			Return(bind_variable_(ptr, var, value, 0));
			if (svar != Nil) {
				Return(bind_variable_(ptr, svar, T, 1));
			}
		}
		else {
			Return(bind_initialize_(ptr, var, init));
			if (svar != Nil) {
				Return(bind_variable_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_bind_allow_(Execute ptr, addr args)
{
	addr left;

	while (args != Nil) {
		Return_getcons(args, &left, &args);
		if (args == Nil)
			return fmte_("&key argument after ~S must be a pair form.", left, NULL);
		if (! symbolp(left))
			return fmte_("&key name ~S must be a symbol.", left, NULL);
		Return_getcons(args, &left, &args); /* ignore */
	}

	return 0;
}

static int lambda_bind_aux_(Execute ptr, addr cons)
{
	addr list, var, init;

	while (cons != Nil) {
		Return_getcons(cons, &list, &cons);
		List_bind(list, &var, &init, NULL);
		Return(bind_initialize_(ptr, var, init));
	}

	return 0;
}

static int lambda_bind_code(Execute ptr, addr right)
{
	addr var, opt, rest, key, allow, aux, args;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, NULL);
	getargs_list_control_unsafe(ptr, 0, &args);
	Return(lambda_bind_var_(ptr, var, &args));
	Return(lambda_bind_opt_(ptr, opt, &args));
	if (rest != Nil) {
		Return(lambda_bind_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	if (key != Nil) {
		Return(lambda_bind_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_bind_allow_(ptr, args));
	}
	return lambda_bind_aux_(ptr, aux);
}

static int macro_bind_whole_(Execute ptr, addr whole, addr form)
{
	if (whole != Nil) {
		copy_list_heap_safe(&form, form);
		Return(bind_variable_(ptr, whole, form, 1));
	}

	return 0;
}

static int macro_bind_env_(Execute ptr, addr env, addr argenv)
{
	if (env != Nil) {
		Return(bind_variable_(ptr, env, argenv, 1));
	}
	return 0;
}

static int macro_bind_list_(Execute ptr, addr right, addr args);
static int macro_bind_var_(Execute ptr, addr var, addr *args)
{
	addr left, right;

	while (var != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(var, &left, &var);
		Return_getcons(*args, &right, args);
		if (consp(left)) {
			Return(macro_bind_list_(ptr, left, right));
		}
		else {
			Return(bind_variable_(ptr, left, right, 0));
		}
	}

	return 0;
}

static int macro_bind_rest_(Execute ptr, addr rest, addr args)
{
	addr var;

	Return_getcons(rest, &var, &rest);  /* (value . &rest) */
	copy_list_heap_safe(&args, args);
	return bind_variable_(ptr, var, args, 1);
}

static int macro_bind_args(Execute ptr, addr args,
		addr var, addr opt, addr rest, addr key, addr allow, addr aux)
{
	Return(macro_bind_var_(ptr, var, &args));
	Return(lambda_bind_opt_(ptr, opt, &args));
	if (rest != Nil) {
		Return(macro_bind_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	if (key != Nil) {
		Return(lambda_bind_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_bind_allow_(ptr, args));
	}
	return lambda_bind_aux_(ptr, aux);
}

static int macro_bind_list_(Execute ptr, addr right, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_bind_whole_(ptr, whole, args));

	return macro_bind_args(ptr, args, var, opt, rest, key, allow, aux);
}

static int macro_bind_call_(Execute ptr, addr right, addr args, addr argenv)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_bind_whole_(ptr, whole, args));
	Return_getcdr(args, &args);
	Return(macro_bind_env_(ptr, env, argenv));

	return macro_bind_args(ptr, args, var, opt, rest, key, allow, aux);
}

static int macro_bind_code(Execute ptr, addr right)
{
	addr args, form, env;

	getargs_list_control_unsafe(ptr, 0, &args);
	/* (lambda (form env) ...) */
	if (! consp(args))
		return fmte_("Too few argument in macro function.", NULL);
	Return_getcons(args, &form, &args);
	if (! consp(args))
		return fmte_("Too few argument in macro function.", NULL);
	Return_getcons(args, &env, &args);
	if (args != Nil)
		return fmte_("Too many argument in macro function.", NULL);
	return macro_bind_call_(ptr, right, form, env);
}

static int destructuring_bind_code(Execute ptr, addr right)
{
	addr args;
	getargs_control(ptr, 0, &args);
	return macro_bind_list_(ptr, right, args);
}

static int macro_lambda_code(Execute ptr, addr right)
{
	addr form, value;

	List_bind(right, &form, &value, NULL);
	macro_heap(&form, Nil, form);
	setdocumentation_function(form, value);
	setresult_control(ptr, form);

	return 0;
}

static int defmacro_code(Execute ptr, addr right)
{
	addr symbol, value;

	List_bind(right, &symbol, &value, NULL);
	setmacro_symbol(symbol, value);
	setresult_control(ptr, symbol);

	return 0;
}

static int deftype_code(Execute ptr, addr right)
{
	addr pos, symbol, doc;

	List_bind(right, &symbol, &doc, NULL);
	getresult_control(ptr, &pos);
	setdocumentation_function(pos, doc);
	setdeftype(symbol, pos);
	setresult_control(ptr, symbol);

	return 0;
}

static int define_compiler_macro_code(Execute ptr, addr right)
{
	addr pos, name, doc;

	List_bind(right, &name, &doc, NULL);
	getresult_control(ptr, &pos);
	setdocumentation_function(pos, doc);
	set_define_compiler_macro(name, pos);
	name_callname_heap(name, &name);
	setresult_control(ptr, name);

	return 0;
}

static int define_symbol_macro_code(Execute ptr, addr right)
{
	addr symbol, eval, form;

	List_bind(right, &symbol, &eval, &form, NULL);
	Check(! symbolp(symbol), "type error");
	setsymbol_macro_symbol(symbol, eval, form);
	setresult_control(ptr, symbol);

	return 0;
}

static int defun_code(Execute ptr, addr right)
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

	return 0;
}

static int flet_code(Execute ptr, addr right)
{
	addr call, value;

	List_bind(right, &call, &value, NULL);
	getdata_array_control(ptr, RefIndex(value), &value);
	pushcallname_control(ptr, call, value);

	return 0;
}

static int labels_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	pushcallname_control(ptr, right, value);

	return 0;
}

static int call_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	return execute_control(ptr, right);
}

static int call_type_code(Execute ptr, addr right)
{
	addr value;

	getargs_tail_control(ptr, &value);
	return typep_error(ptr, value, right);
}

static int values_nil_code(Execute ptr, addr right)
{
	setvalues_nil_control(ptr);
	return 0;
}

static int values_set_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	setvalues_list_control(ptr, right);
	return 0;
}

static int the_code(Execute ptr, addr right)
{
	addr value;

	getresult_control(ptr, &value);
	return typep_error(ptr, value, right);
}

static int if_nil_code(Execute ptr, addr right)
{
	addr check;

	getresult_control(ptr, &check);
	if (check == Nil)
		return goto_control_(ptr, RefIndex(right));
	return 0;
}

static int if_t_code(Execute ptr, addr right)
{
	addr check;

	getresult_control(ptr, &check);
	if (check != Nil)
		return goto_control_(ptr, RefIndex(right));
	return 0;
}

static int goto_code(Execute ptr, addr right)
{
	return goto_control_(ptr, RefIndex(right));
}

static int go_code(Execute ptr, addr right)
{
	return go_control_(ptr, right);
}

static int return_from_code(Execute ptr, addr right)
{
	return return_from_control_(ptr, right);
}

static int catch_code(Execute ptr, addr right)
{
	getresult_control(ptr, &right);
	catch_control(ptr, right);
	return 0;
}

static int throw_operator_code(Execute ptr, addr right)
{
	getargs_control(ptr, 0, &right);
	return throw_control_(ptr, right);
}

static void push_handler_code(Execute ptr, int escape)
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

static int handler_bind_code(Execute ptr, addr right)
{
	push_handler_code(ptr, 0);
	return 0;
}

static int handler_case_code(Execute ptr, addr right)
{
	push_handler_code(ptr, 1);
	return 0;
}

static void push_restart_code(Execute ptr, int escape)
{
	addr args, list;

	getargs_list_control_unsafe(ptr, 0, &args);
	while (args != Nil) {
		GetCons(args, &list, &args);
		pushbind_restart_control(ptr, list, escape);
	}
	reverse_restart_control(ptr);
}

static int restart_bind_code(Execute ptr, addr right)
{
	push_restart_code(ptr, 0);
	return 0;
}

static int restart_case_code(Execute ptr, addr right)
{
	push_restart_code(ptr, 1);
	return 0;
}

static int multiple_value_bind_code(Execute ptr, addr right)
{
	addr list, var, value;

	getvalues_list_control_local(ptr, &list);
	while (right != Nil) {
		GetCons(right, &var, &right);
		GetCons(list, &value, &list);
		Return(bind_variable_(ptr, var, value, 0));
	}

	return 0;
}

static int funcall_code(Execute ptr, addr right)
{
	getargs_list_control_unsafe(ptr, 0, &right);
	return call_control(ptr, right);
}

static int nth_value_code(Execute ptr, addr right)
{
	addr nth;
	size_t index;

	getargs_control(ptr, 0, &nth);
	if (! integerp(nth))
		return fmte_("NTH-VALUE argument ~S must be integer type.", nth, NULL);
	if (! zerop_or_plusp_integer(nth))
		return fmte_("NTH-VALUE argument ~S must be greater than equal to 0.", nth, NULL);
	if (GetIndex_integer(nth, &index)) {
		setresult_control(ptr, Nil);
	}
	else {
		getvalues_control(ptr, index, &right);
		setresult_control(ptr, (right == Unbound)? Nil: right);
	}

	return 0;
}

static int progv_code(Execute ptr, addr right)
{
	addr symbols, values, symbol, value;

	getargs_control(ptr, 0, &symbols);
	getargs_control(ptr, 1, &values);
	while (symbols != Nil) {
		if (! consp(symbols))
			return fmte_("PROGV form ~S must be a cons.", symbols, NULL);
		GetCons(symbols, &symbol, &symbols);
		if (! symbolp(symbol))
			return fmte_("PROGV argument ~S must be a symbol.", symbol, NULL);
		if (values == Nil) {
			pushspecial_control(ptr, symbol, Unbound);
		}
		else if (consp(values)) {
			GetCons(values, &value, &values);
			pushspecial_control(ptr, symbol, value);
		}
		else {
			return fmte_("PROGV form ~S must be a cons.", values, NULL);
		}
	}

	return 0;
}

static int taginfo_code(Execute ptr, addr list)
{
	set_taginfo_control(ptr, list);
	return 0;
}

static int blockinfo_code(Execute ptr, addr pos)
{
	set_blockinfo_control(ptr, pos);
	return 0;
}

static int unwind_protect_code(Execute ptr, addr pos)
{
	set_protect_control(ptr, pos);
	return 0;
}


/*
 *  initialize
 */
_g void defcode_constant(constindex index, pointer p)
{
	addr symbol, pos;

	GetConstant(index, &symbol);
	Check(symbol == Nil || symbol == Unbound, "constant error");
	GetFunctionSymbol(symbol, &pos);
	Check(pos != Unbound, "code-function already exists.");
	compiled_heap(&pos, symbol);
	setcompiled_code(pos, p);
	setsystem_function(pos);
	SetFunctionSymbol(symbol, pos);
}
#define defcode(x,y) defcode_constant(CONSTANT_CODE_##x, p_##y)
#define initcode(x) SetPointer_code(p_##x##_code, x##_code)

_g void init_code_function(void)
{
	/* system */
	initcode(hello);
	initcode(nop);
	initcode(abort);
	initcode(error);
	initcode(info);
	initcode(print);
	initcode(execute);
	initcode(execute_switch);

	/* object */
	initcode(set);
	initcode(push);
	initcode(push_result);
	initcode(push_values);
	initcode(nil_set);
	initcode(nil_push);
	initcode(t_set);
	initcode(t_push);

	/* declaim */
	initcode(declaim_special);
	initcode(declaim_type_value);
	initcode(declaim_type_function);
	initcode(declaim_inline);
	initcode(declaim_notinline);
	initcode(declaim_compilation);
	initcode(declaim_debug);
	initcode(declaim_safety);
	initcode(declaim_space);
	initcode(declaim_speed);
	initcode(declaim_declaration);

	/* local */
	initcode(local_alloc);
	initcode(local_result);

	/* let */
	initcode(let_lexical);
	initcode(let_lexical_type);
	initcode(let_special);
	initcode(let_special_type);
	initcode(leta_lexical);
	initcode(leta_lexical_type);
	initcode(leta_special);
	initcode(leta_special_type);

	/* symbol */
	initcode(lexical_type);
	initcode(lexical_set);
	initcode(lexical_set_type);
	initcode(lexical_push);
	initcode(lexical_push_type);
	initcode(lexical_remove);
	initcode(special_type);
	initcode(special_set);
	initcode(special_push);
	initcode(special_set_type);
	initcode(special_push_type);
	initcode(special_remove);

	/* setq */
	initcode(setq_lexical);
	initcode(setq_lexical_type);
	initcode(setq_special);
	initcode(setq_special_type);

	/* function */
	initcode(function_global_type);
	initcode(function_global_set);
	initcode(function_global_push);
	initcode(function_local_type);
	initcode(function_local_set);
	initcode(function_local_push);
	initcode(setf_global_type);
	initcode(setf_global_set);
	initcode(setf_global_push);
	initcode(setf_local_type);
	initcode(setf_local_set);
	initcode(setf_local_push);

	/* lambda */
	initcode(lambda);
	initcode(lambda_self);
	initcode(lambda_value);
	initcode(lambda_function);
	initcode(lambda_tagbody);
	initcode(lambda_block);
	initcode(lambda_bind);
	initcode(macro_bind);
	initcode(destructuring_bind);
	initcode(defun);
	initcode(macro_lambda);
	initcode(defmacro);
	initcode(deftype);
	initcode(define_compiler_macro);
	initcode(define_symbol_macro);
	initcode(flet);
	initcode(labels);
	initcode(call);
	initcode(call_type);

	initcode(values_nil);
	initcode(values_set);
	initcode(the);
	initcode(if_nil);
	initcode(if_t);
	initcode(goto);
	initcode(go);
	initcode(return_from);
	initcode(catch);
	initcode(throw_operator);
	initcode(handler_bind);
	initcode(handler_case);
	initcode(restart_bind);
	initcode(restart_case);

	initcode(multiple_value_bind);
	initcode(funcall);
	initcode(nth_value);
	initcode(progv);

	initcode(taginfo);
	initcode(blockinfo);
	initcode(unwind_protect);
}

_g void build_code_function(void)
{
	/* system */
	defcode(HELLO, hello_code);
	defcode(NOP, nop_code);
	defcode(ABORT, abort_code);
	defcode(ERROR, error_code);
	defcode(INFO, info_code);
	defcode(PRINT, print_code);
	defcode(EXECUTE, execute_code);
	defcode(EXECUTE_SWITCH, execute_switch_code);

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
	defcode(LEXICAL_REMOVE, lexical_remove_code);
	defcode(SPECIAL_TYPE, special_type_code);
	defcode(SPECIAL_SET, special_set_code);
	defcode(SPECIAL_PUSH, special_push_code);
	defcode(SPECIAL_SET_TYPE, special_set_type_code);
	defcode(SPECIAL_PUSH_TYPE, special_push_type_code);
	defcode(SPECIAL_REMOVE, lexical_remove_code);

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
	defcode(DEFTYPE, deftype_code);
	defcode(DEFINE_COMPILER_MACRO, define_compiler_macro_code);
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
	defcode(THROW, throw_operator_code);
	defcode(HANDLER_BIND, handler_bind_code);
	defcode(HANDLER_CASE, handler_case_code);
	defcode(RESTART_BIND, restart_bind_code);
	defcode(RESTART_CASE, restart_case_code);

	defcode(MULTIPLE_VALUE_BIND, multiple_value_bind_code);
	defcode(FUNCALL, funcall_code);
	defcode(NTH_VALUE, nth_value_code);
	defcode(PROGV, progv_code);

	defcode(TAGINFO, taginfo_code);
	defcode(BLOCKINFO, blockinfo_code);
	defcode(UNWIND_PROTECT, unwind_protect_code);
}

