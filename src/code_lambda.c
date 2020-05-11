#include "condition.h"
#include "cons.h"
#include "cons_list.h"
#include "cons_plist.h"
#include "control_execute.h"
#include "control_object.h"
#include "control_operator.h"
#include "copy.h"
#include "eval_table.h"
#include "execute.h"
#include "function.h"
#include "lambda.h"
#include "scope_object.h"
#include "symbol.h"

/*
 *  free-declare
 */
static int let_free_value(Execute ptr, addr pos, addr type)
{
	int specialp;

	specialp = getspecialp_tablevalue(pos);
	getname_tablevalue(pos, &pos);
	if (specialp)
		getspecial_local(ptr, pos, &pos);
	else
		getlexical_local(ptr, pos, &pos);

	return typep_unbound_error(ptr, pos, type);
}

static int let_free_function(Execute ptr, addr pos, addr type)
{
	int globalp, symbolp;

	globalp = getglobalp_tablefunction(pos);
	symbolp = (RefCallNameType(pos) == CALLNAME_SYMBOL);
	getname_tablefunction(pos, &pos);
	if (globalp) {
		if (symbolp)
			GetFunctionSymbol(pos, &pos);
		else
			getsetf_symbol(pos, &pos);
	}
	else {
		if (symbolp)
			getfunction_local(ptr, pos, &pos);
		else
			getsetf_local(ptr, pos, &pos);
	}

	return typep_unbound_error(ptr, pos, type);
}

_g int let_free_code(Execute ptr, addr list)
{
	addr pos, type;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(pos, &pos, &type);
		if (eval_tablevalue_p(pos)) {
			Return(let_free_value(ptr, pos, type));
			continue;
		}
		if (eval_tablefunction_p(pos)) {
			Return(let_free_function(ptr, pos, type));
			continue;
		}
		Abort("type error");
	}

	return 0;
}


/*
 *  lambda object
 */
static void lambda_closure_code(Execute ptr, addr pos, addr vector)
{
	addr x, list, value;

	/* value */
	GetArrayA2(vector, EvalClosure_Value, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		conslexicalcheck_local(ptr, x, &value);
		pushclosure_value_function(pos, x, value);
	}

	/* function */
	GetArrayA2(vector, EvalClosure_Function, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		getcallnamecheck_local(ptr, x, &value);
		pushclosure_function_function(pos, x, value);
	}

	/* tagbody */
	GetArrayA2(vector, EvalClosure_TagBody, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		gettagbody_execute(ptr, &value, x);
		pushclosure_tagbody_function(pos, x, value);
	}

	/* block */
	GetArrayA2(vector, EvalClosure_Block, &list);
	while (list != Nil) {
		GetCons(list, &x, &list);
		getblock_execute(ptr, &x, x);
		pushclosure_block_function(pos, x);
	}
}

static void lambda_info_code(Execute ptr, addr scope, addr pos)
{
	addr value;

	/* type */
	GetEvalScopeIndex(scope, EvalLambda_The, &value);
	settype_function(pos, value);
	/* documentation */
	GetEvalScopeIndex(scope, EvalLambda_Doc, &value);
	setdocumentation_function(pos, value);
	/* form */
	GetEvalScopeIndex(scope, EvalLambda_Form, &value);
	setlambda_expression_function(pos, value);
	/* defun */
	GetEvalScopeIndex(scope, EvalLambda_Defun, &value);
	if (value != Nil)
		setdefunform_function(pos, value);
	/* self */
	GetEvalScopeIndex(scope, EvalLambda_Self, &value);
	if (value != Nil)
		setrecursive_function(pos);
	/* closure */
	GetEvalScopeIndex(scope, EvalLambda_Clos, &value);
	if (value != Nil)
		lambda_closure_code(ptr, pos, value);
}

static void lambda_object_code(Execute ptr, addr scope, addr *ret)
{
	addr pos, value;

	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	GetEvalScopeIndex(scope, EvalLambda_Code, &value);
	function_heap(&pos, pos, value);
	lambda_info_code(ptr, scope, pos);
	*ret = pos;
}

_g int lambda_set_code(Execute ptr, addr pos)
{
	lambda_object_code(ptr, pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

_g int lambda_push_code(Execute ptr, addr pos)
{
	lambda_object_code(ptr, pos, &pos);
	pushargs_control(ptr, pos);
	return 0;
}


/*
 *  execute
 */
static int lambda_args_value_(Execute ptr, addr left, addr right, int ignore)
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

static int lambda_args_init_(Execute ptr, addr var, addr init)
{
	addr control;
	LocalHold hold;

	/* push */
	hold = LocalHold_array(ptr, 1);
	push_new_control(ptr, &control);
	/* code */
	Return(runcode_control(ptr, init));
	getresult_control(ptr, &init);
	localhold_set(hold, 0, init);
	Return(free_control_(ptr, control));
	localhold_end(hold);
	return lambda_args_value_(ptr, var, init, 0);
}

static int lambda_args_var_(Execute ptr, addr cons, addr *args)
{
	addr left, right;

	while (cons != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(cons, &left, &cons);
		Return_getcons(*args, &right, args);
		Return(lambda_args_value_(ptr, left, right, 0));
	}

	return 0;
}

static int lambda_args_opt_(Execute ptr, addr cons, addr *args)
{
	addr left, right, var, init, svar;

	while (cons != Nil) {
		Return_getcons(cons, &left, &cons);
		List_bind(left, &var, &init, &svar, NULL);
		if (*args != Nil) {
			Return_getcons(*args, &right, args);
			Return(lambda_args_value_(ptr, var, right, 0));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, T, 1));
			}
		}
		else {
			Return(lambda_args_init_(ptr, var, init));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_args_rest_(Execute ptr, addr rest, addr args)
{
	/*copylist_force_heap(args, &args);*/
	/*copy_list_heap_safe(&args, args);*/
	copyheap(&args, args);
	return lambda_args_value_(ptr, rest, args, 1);
}

static int lambda_args_key_find_(addr key, addr list, int *ret)
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

static int lambda_args_key_(Execute ptr, addr cons, int allow, addr args)
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
			Return(lambda_args_key_find_(left, cons, &check));
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
			Return(lambda_args_value_(ptr, var, value, 0));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, T, 1));
			}
		}
		else {
			Return(lambda_args_init_(ptr, var, init));
			if (svar != Nil) {
				Return(lambda_args_value_(ptr, svar, Nil, 1));
			}
		}
	}

	return 0;
}

static int lambda_args_allow_(Execute ptr, addr args)
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

static int lambda_args_aux_(Execute ptr, addr cons)
{
	addr list, var, init;

	while (cons != Nil) {
		Return_getcons(cons, &list, &cons);
		List_bind(list, &var, &init, NULL);
		Return(lambda_args_init_(ptr, var, init));
	}

	return 0;
}

static int lambda_args_code(Execute ptr, addr list)
{
	addr var, opt, rest, key, allow, aux, args;

	List_bind(list, &var, &opt, &rest, &key, &allow, &aux, NULL);
	getargs_list_control_unsafe(ptr, 0, &args);
	Return(lambda_args_var_(ptr, var, &args));
	Return(lambda_args_opt_(ptr, opt, &args));
	if (rest != Nil) {
		Return(lambda_args_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	if (key != Nil) {
		Return(lambda_args_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_args_allow_(ptr, args));
	}
	return lambda_args_aux_(ptr, aux);
}

_g int lambda_execute_code(Execute ptr, addr scope)
{
	addr control, list;

	push_args_control(ptr, &control);
	/* declare */
	GetEvalScopeIndex(scope, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	Return(lambda_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}


/*
 *  macro object
 */
static void macro_object_code(Execute ptr, addr scope, addr *ret)
{
	addr pos, value;

	GetEvalScopeIndex(scope, EvalLambda_Call, &pos);
	GetEvalScopeIndex(scope, EvalLambda_Code, &value);
	macro_heap(&pos, pos, value);
	lambda_info_code(ptr, scope, pos);
	*ret = pos;
}

_g int macro_set_code(Execute ptr, addr pos)
{
	macro_object_code(ptr, pos, &pos);
	setresult_control(ptr, pos);
	return 0;
}

_g int macro_push_code(Execute ptr, addr pos)
{
	macro_object_code(ptr, pos, &pos);
	pushargs_control(ptr, pos);
	return 0;
}


/*
 *  macro-execute
 */
static int macro_args_whole_(Execute ptr, addr whole, addr form)
{
	if (whole != Nil) {
		copy_list_heap_safe(&form, form);
		Return(lambda_args_value_(ptr, whole, form, 1));
	}

	return 0;
}

static int macro_args_env_(Execute ptr, addr env, addr argenv)
{
	if (env != Nil) {
		Return(lambda_args_value_(ptr, env, argenv, 1));
	}
	return 0;
}

static int macro_args_list_(Execute ptr, addr right, addr args);
static int macro_args_var_(Execute ptr, addr var, addr *args)
{
	addr left, right;

	while (var != Nil) {
		if (*args == Nil)
			return fmte_("Too few argument.", NULL);
		Return_getcons(var, &left, &var);
		Return_getcons(*args, &right, args);
		if (consp(left)) {
			Return(macro_args_list_(ptr, left, right));
		}
		else {
			Return(lambda_args_value_(ptr, left, right, 0));
		}
	}

	return 0;
}

static int macro_args_rest_(Execute ptr, addr rest, addr args)
{
	addr var;

	Return_getcons(rest, &var, &rest);  /* (value . &rest) */
	copy_list_heap_safe(&args, args);
	return lambda_args_value_(ptr, var, args, 1);
}

static int macro_args_args(Execute ptr, addr args,
		addr var, addr opt, addr rest, addr key, addr allow, addr aux)
{
	Return(macro_args_var_(ptr, var, &args));
	Return(lambda_args_opt_(ptr, opt, &args));
	if (rest != Nil) {
		Return(macro_args_rest_(ptr, rest, args));
	}
	else if (key == Nil && allow == Nil && args != Nil) {
		return fmte_("Too many argument.", NULL);
	}
	if (key != Nil) {
		Return(lambda_args_key_(ptr, key, allow != Nil, args));
	}
	else if (allow != Nil) {
		Return(lambda_args_allow_(ptr, args));
	}
	return lambda_args_aux_(ptr, aux);
}

static int macro_args_list_(Execute ptr, addr right, addr args)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_args_whole_(ptr, whole, args));

	return macro_args_args(ptr, args, var, opt, rest, key, allow, aux);
}

static int macro_args_call_(Execute ptr, addr right, addr args, addr argenv)
{
	addr var, opt, rest, key, allow, aux, whole, env;

	List_bind(right, &var, &opt, &rest, &key, &allow, &aux, &whole, &env, NULL);
	Return(macro_args_whole_(ptr, whole, args));
	Return_getcdr(args, &args);
	Return(macro_args_env_(ptr, env, argenv));

	return macro_args_args(ptr, args, var, opt, rest, key, allow, aux);
}

static int macro_args_code(Execute ptr, addr right)
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
	return macro_args_call_(ptr, right, form, env);
}

_g int macro_execute_code(Execute ptr, addr scope)
{
	addr control, list;

	push_args_control(ptr, &control);
	/* declare */
	GetEvalScopeIndex(scope, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	Return(macro_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}


/*
 *  destructuring-bind
 */
static int bind_args_code(Execute ptr, addr args)
{
	addr list;
	getresult_control(ptr, &list);
	return macro_args_list_(ptr, args, list);
}

_g int bind_set_code(Execute ptr, addr scope)
{
	addr control, list;

	push_new_control(ptr, &control);
	/* declare */
	GetEvalScopeIndex(scope, EvalLambda_Free, &list);
	Return(let_free_code(ptr, list));
	/* args */
	GetEvalScopeIndex(scope, EvalLambda_Args, &list);
	Return(bind_args_code(ptr, list));
	/* body */
	GetEvalScopeIndex(scope, EvalLambda_Cons, &list);
	Return(runcode_simple(ptr, list));
	/* free */
	return free_control_(ptr, control);
}

_g int bind_push_code(Execute ptr, addr pos)
{
	Return(bind_set_code(ptr, pos));
	getresult_control(ptr, &pos);
	pushargs_control(ptr, pos);

	return 0;
}


/*
 *  flet
 */
static int flet_args_code(Execute ptr, addr list)
{
	addr args, pos, value;

	/* value */
	args = list;
	while (args != Nil) {
		GetCdr(args, &args);
		GetCons(args, &value, &args);
		Return(runcode_simple(ptr, value));
	}

	/* bind */
	getargs_list_control_unsafe(ptr, 0, &args);
	while (list != Nil) {
		GetCons(args, &value, &args);
		GetCons(list, &pos, &list);
		GetCdr(list, &list);
		pushcallname_control(ptr, pos, value);
	}
	setargs_nil_control(ptr);

	return 0;
}

_g int flet_set_code(Execute ptr, addr list)
{
	addr control, args, body, free;

	List_bind(list, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(flet_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int flet_push_code(Execute ptr, addr list)
{
	Return(flet_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}


/*
 *  labels
 */
static int labels_args_code(Execute ptr, addr list)
{
	addr pos, value;

	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(list, &value, &list);
		Return(runcode_simple(ptr, value));
		getresult_control(ptr, &value);
		pushcallname_control(ptr, pos, value);
	}

	return 0;
}

_g int labels_set_code(Execute ptr, addr list)
{
	addr control, args, body, free;

	List_bind(list, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(labels_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int labels_push_code(Execute ptr, addr list)
{
	Return(labels_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}


/*
 *  locally
 */
_g int locally_declare_code(Execute ptr, addr pos)
{
	return let_free_code(ptr, pos);
}


/*
 *  multiple-value-bind
 */
static int bind_values_args_code(Execute ptr, addr list)
{
	addr values, pos, value;

	getvalues_list_control_local(ptr, &values);
	while (list != Nil) {
		GetCons(list, &pos, &list);
		GetCons(values, &value, &values);
		Return(lambda_args_value_(ptr, pos, value, 0));
	}

	return 0;
}

_g int bind_values_set_code(Execute ptr, addr list)
{
	addr control, args, body, free;

	List_bind(list, &args, &body, &free, NULL);
	push_new_control(ptr, &control);
	Return(let_free_code(ptr, free));
	Return(bind_values_args_code(ptr, args));
	Return(runcode_simple(ptr, body));
	return free_control_(ptr, control);
}

_g int bind_values_push_code(Execute ptr, addr list)
{
	Return(bind_values_set_code(ptr, list));
	getresult_control(ptr, &list);
	pushargs_control(ptr, list);

	return 0;
}

